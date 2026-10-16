library(SAILDBUtils)
library(dplyr)
library(data.table)
library(lsummary)
library(lubridate)

login <- SAILDBUtils::SAILConnect()

# get data
exposed_raw <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.exposed")
ctrl_raw    <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.ctrl_spells_women
                                               WHERE YEAR(START_DATE) < 2024
                                                 AND YEAR(END_DATE) >= 1996")

deaths_raw <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.death_women_1row")
meno_raw  <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.MENO_DEMOGRAPHICS")



# =========================================================
# MATCHING FROM SCRATCH
# =========================================================

# 1. Loads:
#    - exposed women table
#    - control spell table
#    - one-row-per-woman death table
#    - raw menopause demographics table
#
# 2. Builds a clean menopause summary with:
#    - first FIRST_KNOWN_MENOPAUSE_START_DT = INDEX_DT
#    - first MENOPAUSE_TYPE (e.g. premature, surgical, symptomatic)
#
# 3. Builds a spell-level pool:
#    - one row per registration spell
#    - joins death
#    - joins menopause summary
#
# 4. Builds case_info:
#    - for each exposed woman, finds the spell covering her own INDEX_DT
#
# 5. Matches separately by menopause type (Premature, Symptomatic, Surgical):
#    - exact YOB
#    - exact Townsend
#    - exact GP registration lookback category at the case index date
#    - control must be alive at case index date
#    - control must be registered at case index date
#    - control must not be in SAIL MENO
#
# 6. Same control can be reused across subgroups to enhance eligibility pool per group,
#    but not within the same subgroup.
#
# Notes
# - This matches on lookback category.
# - It keeps spell-level data and deduplicates to one eligible spell per woman per case.
# =========================================================


# ---------------------------------------------------------
# 1) type conversion
# ---------------------------------------------------------
setDT(exposed_raw)
setDT(ctrl_raw)
setDT(deaths_raw)
setDT(meno_raw)

exposed_raw[, INDEX_DT := as.IDate(INDEX_DT)]

ctrl_raw[, `:=`(
  START_DATE = as.IDate(START_DATE),
  END_DATE   = as.IDate(END_DATE)
)]

deaths_raw[, DEATH_DT := as.IDate(DEATH_DT)]

meno_raw[, FIRST_KNOWN_MENOPAUSE_START_DT := as.IDate(FIRST_KNOWN_MENOPAUSE_START_DT)]

# ---------------------------------------------------------
# 2) clean exposed
# ---------------------------------------------------------
exposed <- exposed_raw[
  !is.na(EXPOSED_ALF) &
    !is.na(EXPOSED_YOB) &
    !is.na(INDEX_DT),
  .(EXPOSED_ALF, EXPOSED_YOB, INDEX_DT)
]



# ---------------------------------------------------------
# 3) summarise menopause table to one row per woman
#    using first menopause date and first menopause type
# ---------------------------------------------------------


setorder(exposed, EXPOSED_ALF, INDEX_DT, EXPOSED_YOB)


exposed <- exposed[
  !is.na(EXPOSED_ALF),
  .(
    INDEX_DT = if (all(is.na(INDEX_DT))) as.IDate(NA)
    else first(na.omit(INDEX_DT)),
    EXPOSED_YOB = if (all(is.na(EXPOSED_YOB ))) NA_character_
    else first(na.omit(EXPOSED_YOB))
  ),
  by = EXPOSED_ALF
]


setorder(meno_raw, ALF_PE, FIRST_KNOWN_MENOPAUSE_START_DT)

meno_one <- meno_raw[
  !is.na(ALF_PE),
  .(
    INDEX_DT = if (all(is.na(FIRST_KNOWN_MENOPAUSE_START_DT))) as.IDate(NA)
    else first(na.omit(FIRST_KNOWN_MENOPAUSE_START_DT)),
    MENOPAUSE_TYPE = if (all(is.na(MENOPAUSE_TYPE))) NA_character_
    else first(na.omit(MENOPAUSE_TYPE))
  ),
  by = ALF_PE
]


exposed <- exposed[!is.na(INDEX_DT)]



# only keep women with a menopause index date
exposed <- exposed[!is.na(INDEX_DT)]

# add menopause type to exposed from menopause summary
setkey(exposed, EXPOSED_ALF)
setkey(meno_one, ALF_PE)

exposed2 <- meno_one[
  exposed,
  on = .(ALF_PE = EXPOSED_ALF),
  nomatch = 0L
][
  ,
  .(
    EXPOSED_ALF = ALF_PE,
    EXPOSED_YOB,
    INDEX_DT = i.INDEX_DT,
    MENOPAUSE_TYPE
  )
]


# ---------------------------------------------------------
# 4) clean deaths to one row per woman
# ---------------------------------------------------------
deaths <- deaths_raw[
  !is.na(ALF_PE),
  .(
    DEATH_DT = if (all(is.na(DEATH_DT))) as.IDate(NA) else min(DEATH_DT, na.rm = TRUE)
  ),
  by = ALF_PE
]

# ---------------------------------------------------------
# 5) clean control spells
# ---------------------------------------------------------
ctrl <- ctrl_raw[
  !is.na(ALF_PE) &
    !is.na(YOB) &
    !is.na(START_DATE) &
    !is.na(END_DATE) &
    !is.na(TOWNSEND),
  .(ALF_PE, YOB, START_DATE, END_DATE, TOWNSEND)
]


ctrl <- unique(ctrl)

# remove impossible spells
ctrl <- ctrl[END_DATE >= START_DATE]
# ---------------------------------------------------------
# 6) build pool at spell level
# ---------------------------------------------------------
setkey(ctrl, ALF_PE)
setkey(deaths, ALF_PE)
setkey(meno_one, ALF_PE)

pool <- deaths[ctrl]     # ctrl LEFT JOIN deaths
pool <- meno_one[pool]   # pool LEFT JOIN menopause summary

pool[, IN_SAIL_MENO := !is.na(INDEX_DT)]
pool[, MENO_GROUP := MENOPAUSE_TYPE]

# rename menopause index date in pool to avoid confusion with case index date later
setnames(pool, "INDEX_DT", "MENO_INDEX_DT")

# remove impossible rows: death before spell starts
pool <- pool[is.na(DEATH_DT) | DEATH_DT >= START_DATE]

# unique spell id
pool[, SPELL_ID := .I]

# ---------------------------------------------------------
# 7) build case_info using a safe merge + filter
#    This avoids the problematic non-equi join.
# ---------------------------------------------------------
exp2 <- copy(exposed)
setnames(exp2, "INDEX_DT", "CASE_INDEX_DT")

case_spell_join <- merge(
  exp2,
  pool,
  by.x = "EXPOSED_ALF",
  by.y = "ALF_PE",
  allow.cartesian = TRUE
)

# keep only spells covering the case's own index date

# keep only cases alive at own index date
case_spell_join <- case_spell_join[
  is.na(DEATH_DT) | DEATH_DT >= CASE_INDEX_DT
]

n_distinct(case_spell_join$EXPOSED_ALF)
# if more than one spell covers the case index date,
# keep the latest-starting spell that still covers the date
setorder(case_spell_join, EXPOSED_ALF, CASE_INDEX_DT, START_DATE, END_DATE)

case_info <- case_spell_join[
  ,
  .SD[1],
  by = .(EXPOSED_ALF, CASE_INDEX_DT)
][
  ,
  .(
    EXPOSED_ALF,
    EXPOSED_YOB,
    INDEX_DT        = CASE_INDEX_DT,
    MENOPAUSE_TYPE= MENO_GROUP,
    CASE_TOWNSEND   = TOWNSEND,
    CASE_START_DATE = START_DATE,
    CASE_END_DATE   = END_DATE,
    CASE_DEATH_DT   = DEATH_DT,
    CASE_MENO_DT    = MENO_INDEX_DT,
    CASE_SPELL_ID   = SPELL_ID
  )
]


# ---------------------------------------------------------
# 8) create lookback category
# ---------------------------------------------------------
lookback_cat <- function(days_obs) {
  fifelse(
    days_obs < 365L, "<1y",
    fifelse(
      days_obs < 365L * 2L, "1_to_<2y",
      fifelse(
        days_obs < 365L * 5L, "2_to_<5y",
        "5y_plus"
      )
    )
  )
}

case_info[, CASE_LOOKBACK_DAYS := as.integer(INDEX_DT - CASE_START_DATE)]
case_info <- case_info[CASE_LOOKBACK_DAYS >= 0L]
case_info[, CASE_LOOKBACK_CAT := lookback_cat(CASE_LOOKBACK_DAYS)]


# ---------------------------------------------------------
# 9) split pool for faster matching by YOB + Townsend
# ---------------------------------------------------------
pool[, BASE_STRATUM := paste(YOB, TOWNSEND, sep = "|")]
pool_split <- split(pool, by = "BASE_STRATUM", keep.by = FALSE, sorted = FALSE)

gc()
# ---------------------------------------------------------
# 10) matching function
# ---------------------------------------------------------
set.seed(123)

match_one_group <- function(group_name, case_dt, pool_split_obj) {
  
  exp_g <- copy(case_dt[MENOPAUSE_TYPE == group_name])
  if (nrow(exp_g) == 0L) return(data.table())
  
  # rarest strata first
  exp_g[, STRATUM := paste(EXPOSED_YOB, CASE_TOWNSEND, CASE_LOOKBACK_CAT, sep = "|")]
  strata_n <- exp_g[, .N, by = STRATUM]
  setnames(strata_n, "N", "STRATUM_N")
  exp_g <- strata_n[exp_g, on = "STRATUM"]
  setorder(exp_g, STRATUM_N, STRATUM, INDEX_DT, EXPOSED_ALF)
  exp_g[, c("STRATUM", "STRATUM_N") := NULL]
  
  used_alf <- integer(0)
  matched_list <- vector("list", nrow(exp_g))
  k <- 0L
  n_exp <- nrow(exp_g)
  
  cat("\nStarting:", group_name, "| N exposed =", n_exp, "\n")
  
  for (i in seq_len(n_exp)) {
    
    if (i %% 5000L == 0L) {
      cat(format(Sys.time(), "%H:%M:%S"), "|", group_name, "| processed:", i, "of", n_exp, "\n")
      gc(FALSE)
    }
    
    case_alf    <- exp_g$EXPOSED_ALF[i]
    case_yob    <- exp_g$EXPOSED_YOB[i]
    case_town   <- exp_g$CASE_TOWNSEND[i]
    case_lb_cat <- exp_g$CASE_LOOKBACK_CAT[i]
    idx         <- exp_g$INDEX_DT[i]
    
    base_key <- paste(case_yob, case_town, sep = "|")
    elig <- pool_split_obj[[base_key]]
    
    if (is.null(elig) || nrow(elig) == 0L) next
    
    elig_lookback_days <- as.integer(idx - elig$START_DATE)
    elig_lookback_cat  <- lookback_cat(elig_lookback_days)
    
    keep <- !(elig$ALF_PE %in% used_alf) &
      elig$ALF_PE != case_alf &
      elig$START_DATE <= idx &
      elig$END_DATE >= idx &
      elig_lookback_days >= 0L &
      elig_lookback_cat == case_lb_cat &
      (is.na(elig$DEATH_DT) | elig$DEATH_DT > idx) &
      (
        !elig$IN_SAIL_MENO 
      )
    
    if (!any(keep)) next
    
    elig <- elig[keep]
    
    # if one woman has multiple eligible spells for this case,
    # keep only the earliest eligible spell
    setorder(elig, ALF_PE, START_DATE, END_DATE)
    elig <- elig[, .SD[1], by = ALF_PE]
    
    # prioritise first eligible spell
    
    elig <- elig[IN_SAIL_MENO==FALSE]
    if(nrow(elig)==0L)next
    
    j <- sample.int(nrow(elig),1)
    pick <- elig[j]
    
    used_alf <- c(used_alf, pick$ALF_PE)
    
    k <- k + 1L
    matched_list[[k]] <- data.table(
      EXPOSED_ALF     = case_alf,
      EXPOSED_GROUP   = group_name,
      INDEX_DT        = idx,
      YOB             = case_yob,
      TOWNSEND        = case_town,
      LOOKBACK_CAT    = case_lb_cat,
      CTRL_ALF        = pick$ALF_PE,
      CTRL_IN_MENO    = pick$IN_SAIL_MENO,
      CTRL_GROUP      = pick$MENO_GROUP,
      CTRL_MENO_DT    = pick$MENO_INDEX_DT,
      CTRL_START_DATE = pick$START_DATE,
      CTRL_END_DATE   = pick$END_DATE,
      CTRL_DEATH_DT   = pick$DEATH_DT,
      CTRL_SOURCE     = fifelse(pick$IN_SAIL_MENO, "SAIL_MENO", "SAIL_only")
    )
  }
  
  out <- rbindlist(matched_list[seq_len(k)], use.names = TRUE, fill = TRUE)
  
  cat("Finished:", group_name,
      "| matched =", nrow(out),
      "| unique controls =", uniqueN(out$CTRL_ALF), "\n")
  
  out
}

# ---------------------------------------------------------
# 11) run matching separately by menopause type
# ---------------------------------------------------------

gc()

symptomatic_matched <- match_one_group(
  group_name = "Symptomatic_menopause",
  case_dt = case_info,
  pool_split_obj = pool_split
)

early_matched <- match_one_group(
  group_name = "Early_menopause",
  case_dt = case_info,
  pool_split_obj = pool_split
)

surgical_matched <- match_one_group(
  group_name = "Surgical_menopause",
  case_dt = case_info,
  pool_split_obj = pool_split
)

matched_all_groups <- rbindlist(
  list(symptomatic_matched, early_matched, surgical_matched),
  use.names = TRUE,
  fill = TRUE
)


# ---------------------------------------------------------
# 12) checks
# ---------------------------------------------------------
cat("\nPool summary\n")
cat("Rows in pool:", nrow(pool), "\n")
cat("Unique women in pool:", uniqueN(pool$ALF_PE), "\n")
print(pool[, .(rows = .N, women = uniqueN(ALF_PE)), by = IN_SAIL_MENO])

cat("\nCase spell lengths\n")
print(summary(case_info$CASE_END_DATE - case_info$CASE_START_DATE))

cat("\nCases by group and lookback category\n")
print(case_info[, .N, by = .(MENOPAUSE_TYPE, CASE_LOOKBACK_CAT)][
  order(MENOPAUSE_TYPE, CASE_LOOKBACK_CAT)
])

cat("\nMatched controls by source\n")
print(matched_all_groups[, .N, by = .(EXPOSED_GROUP, CTRL_SOURCE)][
  order(EXPOSED_GROUP, CTRL_SOURCE)
])

cat("\nCheck controls are registered at matched index\n")
print(matched_all_groups[, .(
  all_ctrl_registered = all(CTRL_START_DATE <= INDEX_DT & CTRL_END_DATE >= INDEX_DT)
)])

cat("\nCheck controls are alive at matched index\n")
print(matched_all_groups[, .(
  all_ctrl_alive = all(is.na(CTRL_DEATH_DT) | CTRL_DEATH_DT > INDEX_DT)
)])

# Preliminary power check for suicide analysis:

suicide <- SAILDBUtils::runSQL(login, "SELECT * FROM sailW1448v.SUICIDEDEATHBTHGNDR")


setkey(symptomatic_matched, CTRL_ALF)
setDT(suicide)

symp_ctrl <- suicide[symptomatic_matched, on=.(ALF_PE=CTRL_ALF)] 


symp_ctrl%>%
  summarise(controls=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))

setkey(symptomatic_matched, EXPOSED_ALF)

symp_exp <- suicide[symptomatic_matched, on=.(ALF_PE=EXPOSED_ALF)] 


symp_exp%>%
  summarise(exp=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))

setkey(matched_all_groups, CTRL_ALF)
setDT(suicide)

symp_ctrl <- suicide[matched_all_groups, on=.(ALF_PE=CTRL_ALF)] 


symp_ctrl%>%
  summarise(controls=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))

setkey(matched_all_groups, EXPOSED_ALF)

symp_exp <- suicide[matched_all_groups, on=.(ALF_PE=EXPOSED_ALF)] 


symp_exp%>%
  summarise(exp=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))


# ---------------------------------------------------------
# 13) optional save
# ---------------------------------------------------------
saveRDS(symptomatic_matched, "symptomatic_matched_from_scratch.rds")
saveRDS(early_matched, "early_matched_from_scratch.rds")
saveRDS(surgical_matched, "surgical_matched_from_scratch.rds")
saveRDS(matched_all_groups, "matched_all_groups_from_scratch.rds")

# change from iDate to Date so we can save in SQL workspace:

matched_all_groups$INDEX_DT <- as.Date(matched_all_groups$INDEX_DT)
matched_all_groups$CTRL_DEATH_DT <- as.Date(matched_all_groups$CTRL_DEATH_DT)
matched_all_groups$CTRL_MENO_DT <- as.Date(matched_all_groups$CTRL_MENO_DT)
matched_all_groups$CTRL_START_DATE <- as.Date(matched_all_groups$CTRL_START_DATE)
matched_all_groups$CTRL_END_DATE <- as.Date(matched_all_groups$CTRL_END_DATE)

symptomatic_matched$INDEX_DT <- as.Date(symptomatic_matched$INDEX_DT)
symptomatic_matched$CTRL_DEATH_DT <- as.Date(symptomatic_matched$CTRL_DEATH_DT)
symptomatic_matched$CTRL_MENO_DT <- as.Date(symptomatic_matched$CTRL_MENO_DT)
symptomatic_matched$CTRL_START_DATE <- as.Date(symptomatic_matched$CTRL_START_DATE)
symptomatic_matched$CTRL_END_DATE <- as.Date(symptomatic_matched$CTRL_END_DATE)

early_matched$INDEX_DT <- as.Date(early_matched$INDEX_DT)
early_matched$CTRL_DEATH_DT <- as.Date(early_matched$CTRL_DEATH_DT)
early_matched$CTRL_MENO_DT <- as.Date(early_matched$CTRL_MENO_DT)
early_matched$CTRL_START_DATE <- as.Date(early_matched$CTRL_START_DATE)
early_matched$CTRL_END_DATE <- as.Date(early_matched$CTRL_END_DATE)

surgical_matched$INDEX_DT <- as.Date(surgical_matched$INDEX_DT)
surgical_matched$CTRL_DEATH_DT <- as.Date(surgical_matched$CTRL_DEATH_DT)
surgical_matched$CTRL_MENO_DT <- as.Date(surgical_matched$CTRL_MENO_DT)
surgical_matched$CTRL_START_DATE <- as.Date(surgical_matched$CTRL_START_DATE)
surgical_matched$CTRL_END_DATE <- as.Date(surgical_matched$CTRL_END_DATE)

# Save in SQL workspace :) 

#SAILDBUtils::create_table_from_df(login, "SAILW1448V.symptomatic_matched_random", symptomatic_matched)
#SAILDBUtils::create_table_from_df(login, "SAILW1448V.early_matched_random", early_matched)
#SAILDBUtils::create_table_from_df(login, "SAILW1448V.surgical_matched_random", surgical_matched)
SAILDBUtils::create_table_from_df(login, "SAILW1448V.matched_women_random", matched_all_groups)

