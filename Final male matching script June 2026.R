library(SAILDBUtils)
library(dplyr)
library(data.table)
library(lubridate)

login <- SAILDBUtils::SAILConnect()

# get data

ctrl_men    <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.ctrl_spells_men
                                               WHERE YEAR(START_DATE) < 2024
                                                 AND YEAR(END_DATE) >= 1996")

deaths_men <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.death_men_1row")
meno_raw  <- SAILDBUtils::runSQL(login, "SELECT * FROM SAILW1448V.MENO_DEMOGRAPHICS")

final_raw <- read.csv('final_version4.csv')

exposed <- final_raw%>%filter(grepl('exposed', role))

setDT(exposed)

exposed <- exposed[,.(ALF_PE, YOB, LOOKBACK_CAT, townsend, INDEX_DT, meno_group)]


# ---------------------------------------------------------
# 1) type conversion and tidy
# ---------------------------------------------------------

set.seed(123)

setDT(exposed)
setDT(ctrl_men)
setDT(deaths_men)


# make names consistent
if ("TOWNSEND" %in% names(ctrl_men) && !("townsend" %in% names(ctrl_men))) {
  setnames(ctrl_men, "TOWNSEND", "townsend")
}
if ("TOWNSEND" %in% names(exposed) && !("townsend" %in% names(exposed))) {
  setnames(exposed, "TOWNSEND", "townsend")
}

# force date classes
exposed[, INDEX_DT := as.IDate(INDEX_DT)]
ctrl_men[, `:=`(
  START_DATE = as.IDate(START_DATE),
  END_DATE   = as.IDate(END_DATE)
)]
deaths_men[, DEATH_DT := as.IDate(DEATH_DT)]

# ---------------------------------------------------------
# 2) clean male control spells
# ---------------------------------------------------------
ctrl_men <- ctrl_men[
  !is.na(ALF_PE) &
    !is.na(YOB) &
    !is.na(START_DATE) &
    !is.na(END_DATE) &
    !is.na(TOWNSEND),
  .(ALF_PE, YOB, START_DATE, END_DATE, TOWNSEND)
]

#

ctrl_men <- unique(ctrl_men)

# remove impossible spells
ctrl_men <- ctrl_men[END_DATE >= START_DATE]

# ---------------------------------------------------------
# 3) clean deaths to one row per man
# ---------------------------------------------------------
deaths_men <- deaths_men[
  !is.na(ALF_PE),
  .(
    DEATH_DT = if (all(is.na(DEATH_DT))) as.IDate(NA) else min(DEATH_DT, na.rm = TRUE)
  ),
  by = ALF_PE
]

# ---------------------------------------------------------
# 4) build male pool at spell level
# ---------------------------------------------------------

setkey(ctrl_men, ALF_PE)
setkey(deaths_men, ALF_PE)

all_men_pool <- deaths_men[ctrl_men]

# remove impossible rows: death before spell starts
all_men_pool <- all_men_pool[is.na(DEATH_DT) | DEATH_DT >= START_DATE]

# unique spell id
all_men_pool[, SPELL_ID := .I]


# ---------------------------------------------------------
# 5) create lookback category function
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


# ---------------------------------------------------------
# 6) split pool for faster matching by YOB + Townsend
# ---------------------------------------------------------

all_men_pool[, BASE_STRATUM := paste(YOB, townsend, sep = "|")]
men_pool_split <- split(all_men_pool, by = "BASE_STRATUM", keep.by = FALSE, sorted = FALSE)


# ---------------------------------------------------------
# 7) matching function
# ---------------------------------------------------------

set.seed(123)

match_one_male_group <- function(group_name, exposed, pool_split_obj) {
  
  exp_g <- copy(exposed[meno_group == group_name])
  if (nrow(exp_g) == 0L) return(data.table())
  
  # rarest strata first
  exp_g[, STRATUM := paste(YOB, townsend, LOOKBACK_CAT, sep = "|")]
  strata_n <- exp_g[, .N, by = STRATUM]
  setnames(strata_n, "N", "STRATUM_N")
  exp_g <- strata_n[exp_g, on = "STRATUM"]
  setorder(exp_g, STRATUM_N, STRATUM, INDEX_DT, ALF_PE)
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
    
    case_alf    <- exp_g$ALF_PE[i]
    case_yob    <- exp_g$YOB[i]
    case_town   <- exp_g$townsend[i]
    case_lb_cat <- exp_g$LOOKBACK_CAT[i]
    idx         <- exp_g$INDEX_DT[i]
    
    base_key <- paste(case_yob, case_town, sep = "|")
    elig <- pool_split_obj[[base_key]]
    
    if (is.null(elig) || nrow(elig) == 0L) next
    
    elig_lookback_days <- as.integer(as.Date(idx) - as.Date(elig$START_DATE))
    elig_lookback_cat  <- lookback_cat(elig_lookback_days)
    
    keep <- !(elig$ALF_PE %in% used_alf) &
      elig$START_DATE <= idx &
      elig$END_DATE >= idx &
      elig_lookback_days >= 0L &
      elig_lookback_cat == case_lb_cat &
      (is.na(elig$DEATH_DT) | elig$DEATH_DT > idx)
    
    if (!any(keep)) next
    
    elig <- elig[keep]
    
    # if one man has multiple eligible spells for this case,
    # keep only the earliest eligible spell
    setorder(elig, ALF_PE, START_DATE, END_DATE)
    elig <- elig[, .SD[1], by = ALF_PE]
    
    if (nrow(elig) == 0L) next
    
    j <- sample.int(nrow(elig), 1)
    pick <- elig[j]
    
    used_alf <- c(used_alf, pick$ALF_PE)
    
    k <- k + 1L
    matched_list[[k]] <- data.table(
      EXPOSED_ALF    = case_alf,
      EXPOSED_GROUP  = group_name,
      INDEX_DT       = idx,
      YOB            = case_yob,
      TOWNSEND       = case_town,
      LOOKBACK_CAT   = case_lb_cat,
      CTRL_ALF       = pick$ALF_PE,
      CTRL_START_DATE= pick$START_DATE,
      CTRL_END_DATE  = pick$END_DATE,
      CTRL_DEATH_DT  = pick$DEATH_DT,
      CTRL_SPELL_ID  = pick$SPELL_ID
    )
  }
  
  out <- rbindlist(matched_list[seq_len(k)], use.names = TRUE, fill = TRUE)
  
  cat("Finished:", group_name,
      "| matched =", nrow(out),
      "| unique controls =", uniqueN(out$CTRL_ALF), "\n")
  
  out
}


# ---------------------------------------------------------
# 8) run matching separately by menopause type
# ---------------------------------------------------------

symptomatic_male_matched <- match_one_male_group(
  group_name = "Symptomatic_menopause",
  exposed = exposed,
  pool_split_obj = men_pool_split
)

surgical_male_matched <- match_one_male_group(
  group_name = "Surgical_menopause",
  exposed = exposed,
  pool_split_obj = men_pool_split
)

early_male_matched <- match_one_male_group(
  group_name = "Early_menopause",
  exposed = exposed,
  pool_split_obj = men_pool_split
)


matched_male_groups <- rbindlist(
  list(symptomatic_male_matched, early_male_matched, surgical_male_matched),
  use.names = TRUE,
  fill = TRUE
)

# ---------------------------------------------------------
# 9) checks
# ---------------------------------------------------------


cat("\nMatched controls by source\n")
print(matched_male_groups[, .N, by = .(EXPOSED_GROUP, CTRL_SOURCE)][
  order(EXPOSED_GROUP, CTRL_SOURCE)
])

cat("\nCheck controls are registered at matched index\n")
print(matched_male_groups[, .(
  all_ctrl_registered = all(CTRL_START_DATE <= INDEX_DT & CTRL_END_DATE >= INDEX_DT)
)])

cat("\nCheck controls are alive at matched index\n")
print(matched_male_groups[, .(
  all_ctrl_alive = all(is.na(CTRL_DEATH_DT) | CTRL_DEATH_DT > INDEX_DT)
)])


# Preliminary power check for suicide analysis

suicide <- SAILDBUtils::runSQL(login, "SELECT * FROM sailW1448v.SUICIDEDEATHBTHGNDR")


setkey(symptomatic_male_matched, CTRL_ALF)
setDT(suicide)

symp_ctrl <- suicide[symptomatic_male_matched, on=.(ALF_PE=CTRL_ALF)] 


symp_ctrl%>%
  summarise(controls=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))

setkey(surgical_male_matched, EXPOSED_ALF)

symp_exp <- suicide[symptomatic_male_matched, on=.(ALF_PE=EXPOSED_ALF)] 

symp_exp <- deaths_men[matched_male_groups, on=.(ALF_PE=CTRL_ALF)]

symp_exp%>%
  summarise(exp=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              
              ALF_PE[
                !is.na(DEATH_DT)&
                  DEATH_DT>=INDEX_DT&
                  DEATH_DT<=INDEX_DT+365*5
              ]
            ))

check <- final_raw%>%filter(grepl('exposed', role))
check%>%
  summarise(exp=n_distinct(ALF_PE),
            died_5yr=n_distinct(
              
              ALF_PE[
                !is.na(as.Date(DEATH_DT))&
                  as.Date(DEATH_DT)>=as.Date(INDEX_DT)&
                  as.Date(DEATH_DT)<=as.Date(INDEX_DT)+365*5
              ]
            ))

n_distinct(matched_male_groups$CTRL_ALF)

# ---------------------------------------------------------
# 10) Save
# ---------------------------------------------------------
saveRDS(symptomatic_male_matched, "symptomatic_matched_male.rds")
saveRDS(early_male_matched, "early_matched_male.rds")
saveRDS(surgical_male_matched, "surgical_matched_male.rds")
saveRDS(matched_male_groups, "matched_all_groups_male.rds")

# save all IDates as Dates to we can save in SQL workspace: 

matched_male_groups$INDEX_DT <- as.Date(matched_male_groups$INDEX_DT)
matched_male_groups$CTRL_DEATH_DT <- as.Date(matched_male_groups$CTRL_DEATH_DT)
matched_male_groups$CTRL_MENO_DT <- as.Date(matched_male_groups$CTRL_MENO_DT)
matched_male_groups$CTRL_START_DATE <- as.Date(matched_male_groups$CTRL_START_DATE)
matched_male_groups$CTRL_END_DATE <- as.Date(matched_male_groups$CTRL_END_DATE)

symptomatic_male_matched$INDEX_DT <- as.Date(symptomatic_male_matched$INDEX_DT)
symptomatic_male_matched$CTRL_DEATH_DT <- as.Date(symptomatic_male_matched$CTRL_DEATH_DT)
symptomatic_male_matched$CTRL_MENO_DT <- as.Date(symptomatic_male_matched$CTRL_MENO_DT)
symptomatic_male_matched$CTRL_START_DATE <- as.Date(symptomatic_male_matched$CTRL_START_DATE)
symptomatic_male_matched$CTRL_END_DATE <- as.Date(symptomatic_male_matched$CTRL_END_DATE)

early_male_matched$INDEX_DT <- as.Date(early_male_matched$INDEX_DT)
early_male_matched$CTRL_DEATH_DT <- as.Date(early_male_matched$CTRL_DEATH_DT)
early_male_matched$CTRL_MENO_DT <- as.Date(early_male_matched$CTRL_MENO_DT)
early_male_matched$CTRL_START_DATE <- as.Date(early_male_matched$CTRL_START_DATE)
early_male_matched$CTRL_END_DATE <- as.Date(early_male_matched$CTRL_END_DATE)

surgical_male_matched$INDEX_DT <- as.Date(surgical_male_matched$INDEX_DT)
surgical_male_matched$CTRL_DEATH_DT <- as.Date(surgical_male_matched$CTRL_DEATH_DT)
surgical_male_matched$CTRL_MENO_DT <- as.Date(surgical_male_matched$CTRL_MENO_DT)
surgical_male_matched$CTRL_START_DATE <- as.Date(surgical_male_matched$CTRL_START_DATE)
surgical_male_matched$CTRL_END_DATE <- as.Date(surgical_male_matched$CTRL_END_DATE)

## Save to SQL workspace:

SAILDBUtils::create_table_from_df(login, "SAILW1448V.symptomatic_matched_random_men", symptomatic_male_matched)
SAILDBUtils::create_table_from_df(login, "SAILW1448V.early_matched_random_men", early_male_matched)
SAILDBUtils::create_table_from_df(login, "SAILW1448V.surgical_matched_random_men", surgical_male_matched)
SAILDBUtils::create_table_from_df(login, "SAILW1448V.matched_men_random", matched_male_groups)
