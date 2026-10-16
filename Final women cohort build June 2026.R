library('SAILDBUtils')
library('ggplot2')
library('tidyverse')
library('SnowballC')
library('dplyr') #add dplyr library
library('data.table')
library('gtsummary')
library ('lme4')
library ('lubridate')
library('survival')
library('survminer')
library('ggpubr')
library('tidyr')
library('car')
library('psych')
library('polycor')
library('gt')
library('broom')
library('labelled')
library('cowplot')

#load data

Login <- SAILDBUtils::SAILConnect()

final_women_match_random <- SAILDBUtils::runSQL(Login,"SELECT * from sailw1448v.matched_women_random")


########################### prep and merge exposed and ctrls

setDT(final_women_match_random)

final_women_match_random[,pair_id :=.I]

n_distinct(final_women_match_random$CTRL_ALF)
n_distinct(final_women_match_random$EXPOSED_ALF)

exposed_long <-final_women_match_random[,.(
  pair_id,
  ALF_PE=EXPOSED_ALF,
  role='exposed',
  townsend=TOWNSEND,
  INDEX_DT,
  YOB,
  CTRL_START_DATE,
  CTRL_END_DATE,
  LOOKBACK_CAT,
  MENOPAUSE_TYPE=EXPOSED_GROUP)]

ctrl_long <-final_women_match_random[,.(
  pair_id,
  ALF_PE=CTRL_ALF,
  role='ctrl',
  townsend=TOWNSEND,
  INDEX_DT,
  YOB,
  CTRL_START_DATE,
  CTRL_END_DATE,
  LOOKBACK_CAT,
  MENOPAUSE_TYPE=EXPOSED_GROUP)]

long <- rbind(exposed_long,ctrl_long, use.names=T)


setDT(long)
long[,.N, by=pair_id][,summary(N)]
long[,.N, by=.(pair_id, role)][, .N, by=role]

n_distinct(ctrl_long$ALF_PE)

#268432 exposed
#250330 ctrls


## Get death info 

deaths <- SAILDBUtils::runSQL(Login,"SELECT * from SAILW1448V.death_women_1row")

long <- long%>%
  left_join(deaths, by='ALF_PE')



############################### create binary died(y/n) variable

long <- long%>%
  mutate(
    DEATH_YN=
      ifelse(!is.na(DEATH_DT),
             'Y', 'N'))

long%>%
  group_by(DEATH_YN, role)%>%
  summarise(cnt=n_distinct(ALF_PE))


# Optional fixed max follow-up of 5 years after index

long$DEATH_DT=as.Date(long$DEATH_DT)

setDT(long)

long[, FU_END_5Y := INDEX_DT %m+% years(5)]

long[, death_5Y := fifelse(
  !is.na(DEATH_DT) & DEATH_DT >= INDEX_DT & DEATH_DT <=FU_END_5Y,
  1L, 0L
)]


long%>%
  group_by(death_5Y, role)%>%
  summarise(cnt=n_distinct(ALF_PE))


########################################## Get self harm info for all

selfharm <- SAILDBUtils::runSQL(Login, "SELECT * FROM SAILW1448V.SELF_HARM_NOSUD")

selfharm$SH_DT <- selfharm$EVENT_DT

#long$sh_dt_window <- NULL

longidx <- long%>%
  distinct(ALF_PE,INDEX_DT)%>%
  mutate(INDEX_DT=as.Date(INDEX_DT))%>%
  ungroup()


# create binary sh(y/n) variable (5 years from meno/baseline date)

# create binary historic_sh(y/n) variable

sh_events <- selfharm%>%
  distinct(ALF_PE,SH_DT)%>%
  ungroup()

sh_cohort <- long%>%
  left_join(sh_events, by='ALF_PE')

sh_flags <- sh_cohort%>%
  transmute(ALF_PE,SH_DT=as.Date(SH_DT))%>%
  inner_join(longidx, by='ALF_PE')%>%
  summarise(
    sh_history=if_else(any(!is.na(SH_DT)&SH_DT<INDEX_DT), 'Y', 'N'),
    sh_outcome=if_else(any(!is.na(SH_DT)&SH_DT>=INDEX_DT&SH_DT<=INDEX_DT %m+% years(5)),'Y','N'),
    .by=ALF_PE
  )
n_distinct(sh_flags$ALF_PE)

long <- long%>%
  left_join(sh_flags, by='ALF_PE')

sh_window <- selfharm%>%
  transmute(ALF_PE,SH_DT=as.Date(SH_DT))%>%
  filter(!is.na(SH_DT))%>%
  inner_join(longidx%>%mutate(INDEX_DT=as.Date(INDEX_DT)), by='ALF_PE')%>%
  filter(SH_DT>=INDEX_DT&SH_DT<=INDEX_DT %m+% years(5))%>%
  summarise(sh_dt_window=min(SH_DT),
            .by=ALF_PE)

n_distinct(sh_window$ALF_PE)

long <- long%>%
  left_join(sh_window, by='ALF_PE')


long <- long%>%
  mutate(sh_history=tidyr::replace_na(sh_history, 'N'),
         sh_outcome=tidyr::replace_na(sh_outcome,'N'))

long%>%
  group_by(role, sh_history)%>%
  summarise(cnt=n_distinct(ALF_PE))

long%>%
  group_by(role, sh_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))



############################# Get suicide info for all

suicide <- SAILDBUtils::runSQL(Login, "SELECT * FROM sailW1448v.SUICIDEDEATHBTHGNDR")

suicide <- suicide%>%
  dplyr::distinct(ALF_PE,.keep_all = T)

suicide$SUI_DT <- suicide$DEATH_DT

n_distinct(suicide$ALF_PE)

## join suicide date 

long <- long%>%
  left_join(suicide%>%
              dplyr::select(ALF_PE, SUI_DT), by='ALF_PE')


gc()

## Need to correct suicide date to match DEATH_DT.


long <- long %>%
  mutate(
    SUI_DT=if_else(
      !is.na(SUI_DT)&!is.na(DEATH_DT)&SUI_DT>DEATH_DT,
      DEATH_DT,
      SUI_DT))

rows <- long%>%
  filter(!is.na(DEATH_DT),!is.na(SUI_DT), DEATH_DT<SUI_DT)


rows%>%
  group_by(role)%>%
  summarise(cnt=n_distinct(ALF_PE))


# create binary suicide(y/n) variable 


long <- long%>%
  group_by(ALF_PE)%>%
  mutate(
    sui_outcome= case_when(
      !is.na(SUI_DT)~'Y',
      TRUE~'N'
    ))


long%>%
  group_by(role, sui_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))

long <- long%>%
  ungroup()%>%
  mutate(
    sui_outcome5= if_else(
      !is.na(SUI_DT)&SUI_DT>=INDEX_DT&SUI_DT<=INDEX_DT %m+% years(5),'Y','N'
    ))

long%>%
  group_by(role, sui_outcome5)%>%
  summarise(cnt=n_distinct(ALF_PE))


################################## Get psych info for all

psych <- SAILDBUtils::runSQL(Login, "SELECT * FROM sailw1448v.psych_diag_no_nuero")

## join psych date 

gc()

psych$PSYCH_DT <- psych$EVENT_DT

# create binary psych(y/n) variable (5 years from meno date)

# create binary historic_psych(y/n) variable

psych_events <- psych%>%
  distinct(ALF_PE,PSYCH_DT)%>%
  ungroup()

psych_flags <- psych_events%>%
  transmute(ALF_PE,PSYCH_DT=as.Date(PSYCH_DT))%>%
  inner_join(longidx, by='ALF_PE')%>%
  summarise(
    psych_history=if_else(any(!is.na(PSYCH_DT)&PSYCH_DT<INDEX_DT), 'Y', 'N'),
    psych_outcome=if_else(any(!is.na(PSYCH_DT)&PSYCH_DT>=INDEX_DT&PSYCH_DT<=INDEX_DT%m+% years(5)),'Y','N'),
    .by=ALF_PE
  )

n_distinct(psych_flags$ALF_PE)

psych_window <- psych_events%>%
  transmute(ALF_PE,PSYCH_DT=as.Date(PSYCH_DT))%>%
  filter(!is.na(PSYCH_DT))%>%
  inner_join(longidx%>%mutate(INDEX_DT=as.Date(INDEX_DT)), by='ALF_PE')%>%
  filter(PSYCH_DT>=INDEX_DT&PSYCH_DT<=INDEX_DT%m+% years(5))%>%
  summarise(psych_dt_window=min(PSYCH_DT),
            .by=ALF_PE)

n_distinct(psych_window$ALF_PE)

long <- long%>%
  left_join(psych_window, by='ALF_PE')

long <- long%>%
  left_join(psych_flags, by='ALF_PE')


long%>%
  group_by(role, psych_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))

long%>%
  group_by(role, psych_history)%>%
  summarise(cnt=n_distinct(ALF_PE))

long <- long%>%
  mutate(psych_history=tidyr::replace_na(psych_history, 'N'),
         psych_outcome=tidyr::replace_na(psych_outcome,'N'))

long%>%
  group_by(role, psych_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))

long%>%
  group_by(role, psych_history)%>%
  summarise(cnt=n_distinct(ALF_PE))



gc()

###################################### Get comorbs info for all

comorb <- SAILDBUtils::runSQL(Login, "SELECT * FROM sailW1448v.comorbs_primary_secondary")


# join comorbs vars 

setDT(comorb)
setDT(long)

# 1) Ensure dates are comparable 
long[, INDEX_DT := as.Date(INDEX_DT)]
comorb[, EVENT_DT := as.Date(EVENT_DT)]

# 2) Drop missing/NULL condition types
comorb <- comorb[
  !is.na(CONDITION_TYPE) & CONDITION_TYPE != "NULL"
][
  , EVENT_DT:=as.Date(EVENT_DT)
][
  !is.na(EVENT_DT),
  .(EVENT_DT=min(EVENT_DT)),
  by=.(ALF_PE,CONDITION_TYPE)
]

# 3) get comorbs on or before index date

comorb_before <- comorb[long, on='ALF_PE',nomatch =0 ][
  EVENT_DT<=INDEX_DT,
  .(ALF_PE,CONDITION_TYPE)
]
comorb_before <- unique(comorb_before)
comorb_before[,value:='Y']


com_wide <- dcast(
  comorb_before,
  ALF_PE~CONDITION_TYPE,
  value.var='value',
  fill='N')

long <- com_wide[long,on='ALF_PE']

long%>%
  group_by(DIABETES_DIAGNOSIS)%>%
  summarise(cnt=n_distinct(ALF_PE))

cond_cols <- setdiff(names(com_wide),'ALF_PE')
for(cc in cond_cols) set(long,which(is.na(long[[cc]])),cc,'N')

long%>%
  group_by(role,CARDIOVASCULAR_DIAGNOSIS)%>%
  summarise(cnt=n_distinct(ALF_PE))

# 4) Create n_diagnosis variable

long[,n_diagnosis2:=rowSums(.SD =='Y', na.rm=TRUE), .SDcols = c(2:12)]



############################# get substance use disorder (sud) 

sud <- SAILDBUtils::runSQL(Login, "SELECT a.alf_pe, a.event_dt FROM sailw1448v.sud_BOTHGNDR a
UNION SELECT b.alf_pe,b.event_dt FROM sailw1448v.sud_BOTHGNDR_SECONDARY b;")

## join sud date 

gc()

sud$sud_DT <- sud$EVENT_DT

# create binary sud(y/n) variable (5 years from meno date)

# create binary historic_sud(y/n) variable

sud_events <- sud%>%
  distinct(ALF_PE,sud_DT)%>%
  ungroup()


look5idx <- look5%>%
  distinct(ALF_PE,INDEX_DT)%>%
  mutate(INDEX_DT=as.Date(INDEX_DT))%>%
  ungroup()

sud_flags <- sud_events%>%
  transmute(ALF_PE,sud_DT=as.Date(sud_DT))%>%
  inner_join(look5idx, by='ALF_PE')%>%
  summarise(
    sud_history=if_else(any(!is.na(sud_DT)&sud_DT<INDEX_DT), 'Y', 'N'),
    sud_outcome=if_else(any(!is.na(sud_DT)&sud_DT>=INDEX_DT&sud_DT<=INDEX_DT%m+% years(5)),'Y','N'),
    .by=ALF_PE
  )

n_distinct(sud_flags$ALF_PE)

sud_window <- sud_events%>%
  transmute(ALF_PE,sud_DT=as.Date(sud_DT))%>%
  filter(!is.na(sud_DT))%>%
  inner_join(look5idx%>%mutate(INDEX_DT=as.Date(INDEX_DT)), by='ALF_PE')%>%
  filter(sud_DT>=INDEX_DT&sud_DT<=INDEX_DT%m+% years(5))%>%
  summarise(sud_dt_window=min(sud_DT),
            .by=ALF_PE)

n_distinct(sud_window$ALF_PE)

look5 <- look5%>%
  left_join(sud_window, by='ALF_PE')

look5 <- look5%>%
  left_join(sud_flags, by='ALF_PE')


look5%>%
  group_by(role, sud_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))

look5%>%
  group_by(role, sud_history)%>%
  summarise(cnt=n_distinct(ALF_PE))

look5 <- look5%>%
  mutate(sud_history=tidyr::replace_na(sud_history, 'N'),
         sud_outcome=tidyr::replace_na(sud_outcome,'N'))

look5%>%
  group_by(role, sud_outcome)%>%
  summarise(cnt=n_distinct(ALF_PE))

look5%>%
  group_by(role, sud_history)%>%
  summarise(cnt=n_distinct(ALF_PE))


gc()



################################ now get time to event and outcome indicator for all outcomes

# time to event must be censored at followup end (max being 2023-12-31), death, or 5 years after index date. 


sum(is.na(long$CTRL_END_DATE))#0 CTRLs missing end date derived in matching 

# get end date for SAIL MENO

end <- SAILDBUtils::runSQL(Login, "SELECT CTRL_ALF, INDEX_DT, EXPOSED_ALF, D.END_DATE AS CTRL_END_DATE, 
  C.END_DATE AS EXPOSED_END_DATE
FROM SAILW1448v.matched_women_random S
LEFT JOIN sailw1448v.CTRL_SPELLS_women D
ON D.ALF_PE=S.CTRL_ALF  
LEFT JOIN sailw1448v.MENO_DEMOGRAPHICS C
ON C.ALF_PE=S.EXPOSED_ALF
WHERE INDEX_DT<=D.END_DATE; ")

setDT(end)

reg_end <- rbindlist(list(
  end[,.(ALF_PE=EXPOSED_ALF,REG_END=EXPOSED_END_DATE)],
  end[,.(ALF_PE=CTRL_ALF,REG_END=CTRL_END_DATE)]
),use.names = TRUE,fill=TRUE
)

reg_end <- reg_end[!is.na(ALF_PE)&!is.na(REG_END)]

reg_end_max<- reg_end[,.(REG_END_MAX=max(REG_END)), by=ALF_PE]

n_distinct(long2$ALF_PE)
long <- reg_end_max[long,on='ALF_PE']

sum(!is.na(long$REG_END_MAX))
sum(is.na(long$REG_END_MAX))#0 missing end date

# recode end date to study end: 2023


long <- long%>%
  mutate( END_DATE=if_else(REG_END_MAX== as.Date('9999-01-01'),
                           as.Date('2023-12-31'),
                           REG_END_MAX))





  long[role=='ctrl', REG_END_USE:=as.IDate(CTRL_END_DATE)]
  long[role=='exposed', REG_END_USE :=END_DATE]
  
  long[,REG_END_USE:=as.IDate(NA)]
 
  sum(is.na(long$REG_END_USE))
  
  sum(is.na(long$YOB))

#365 had index dates after end_dates, remove plus pairs
  
  bad_pairs <- long[
    INDEX_DT>REG_END_USE,
    unique(pair_id)
  ]
  
  look5 <- long[
    !pair_id%in% bad_pairs
  ]
  


### Now need time to event for suicide, self harm, sud, and psychiatric diagnosis

##### SUICIDE 5 years

setDT(long)


# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "SUI_DT", 'DEATH_DT', 'REG_END_USE')
for (cc in date_cols) if (cc %in% names(long)) long[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
long[, FU_END_5Y := INDEX_DT + 365L*5L]

# Censoring date = earliest of END_DATE, NON-SUICIDAL DEATH_DT, 5y after index


long[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
     .SDcols = c('FU_END_5Y','DEATH_DT', 'REG_END_USE')]


# Define event date only when sh_event == "Y"
long[, sui_event_dt := fifelse(sui_outcome == "Y", SUI_DT, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
long[, sui_event_ind := fifelse(
  !is.na(SUI_DT) & SUI_DT>= INDEX_DT & SUI_DT <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
long[, stop_dt := fifelse(sui_event_ind == 1L, sui_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
long[, time_to_sui := as.integer(stop_dt - INDEX_DT)]


long%>%
  group_by(role,sui_event_ind)%>%
  summarise(cnt=n_distinct(ALF_PE))

## check model works

sui_all_model <- coxph(Surv(time_to_sui, sui_event_ind)~ role,
                       data = long)
summary(sui_all_model)


##### SUICIDE 10 years

# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "SUI_DT", 'REG_END_USE','DEATH_DT')
for (cc in date_cols) if (cc %in% names(long)) long[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
long[, FU_END_10Y := INDEX_DT + 365L*10L]

# Censoring date = earliest of END_DATE, NON-SUICIDAL DEATH_DT, 5y after index


long[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
      .SDcols = c('DEATH_DT', 'REG_END_USE', 'FU_END_10Y')]


# Define event date only when sh_event == "Y"
long[, sui_event_dt := fifelse(sui_outcome == "Y", SUI_DT, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
long[, sui_event_ind_10 := fifelse(
  !is.na(SUI_DT) & SUI_DT>= INDEX_DT & SUI_DT <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
long[, stop_dt := fifelse(sui_event_ind_10 == 1L, sui_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
long[, time_to_sui_10 := as.integer(stop_dt - INDEX_DT)]


#### SUICIDE all time

# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "SUI_DT", 'DEATH_DT', 'REG_END_USE')
for (cc in date_cols) if (cc %in% names(long)) long[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
long[, FU_END_5Y := INDEX_DT + 365L*30L]

# Censoring date = earliest of END_DATE, NON-SUICIDAL DEATH_DT, 5y after index


long[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
      .SDcols = c('death_dt_corrected', 'REG_END_USE', 'FU_END_5Y')]


# Define event date only when sh_event == "Y"
long[, sui_event_dt := fifelse(sui_outcome == "Y", SUI_DT, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
long[, sui_event_ind_30 := fifelse(
  !is.na(SUI_DT) & SUI_DT>= INDEX_DT & SUI_DT <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
long[, stop_dt := fifelse(sui_event_ind_30 == 1L, sui_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
long[, time_to_sui_30 := as.integer(stop_dt - INDEX_DT)]

long%>%
  group_by(role,sui_event_ind_30)%>%
  summarise(cnt=n_distinct(ALF_PE))




##### SELF HARM


# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "sh_dt_window", "DEATH_DT", "REG_END_USE")
for (cc in date_cols) if (cc %in% names(long)) long[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
long[, FU_END_5Y := INDEX_DT + 365L*5L]

# Censoring date = earliest of END_DATE, DEATH_DT, 5y after index
# pmin needs Date/IDate; na.rm=TRUE treats NA as +Inf, so it picks the non-missing minimum
long[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
     .SDcols = c("REG_END_USE", "DEATH_DT", "FU_END_5Y")]

# Define event date only when sh_event == "Y"
long[, sh_event_dt := fifelse(sh_outcome == "Y", sh_dt_window, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
long[, sh_event_ind := fifelse(
  !is.na(sh_event_dt) & sh_event_dt >= INDEX_DT & sh_event_dt <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
long[, stop_dt := fifelse(sh_event_ind == 1L, sh_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
long[, time_to_sh := as.integer(stop_dt - INDEX_DT)]

## check model works

sh_all_model <- coxph(Surv(time_to_sh, sh_event_ind)~ role,
                       data = long)
summary(sh_all_model)


##### PSYCHIACTRIC DIAGNOSIS


# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "psych_dt_window", "DEATH_DT", "REG_END_USE")
for (cc in date_cols) if (cc %in% names(long)) long[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
long[, FU_END_5Y := INDEX_DT + 365L*5L]

# Censoring date = earliest of END_DATE, DEATH_DT, 5y after index
# pmin needs Date/IDate; na.rm=TRUE treats NA as +Inf, so it picks the non-missing minimum
long[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
     .SDcols = c("REG_END_USE", "DEATH_DT", "FU_END_5Y")]

# Define event date only when sh_event == "Y"
long[, psych_event_dt := fifelse(psych_outcome == "Y", psych_dt_window, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
long[, psych_event_ind := fifelse(
  !is.na(psych_event_dt) & psych_event_dt >= INDEX_DT & psych_event_dt <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
long[, stop_dt := fifelse(psych_event_ind == 1L, psych_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
long[, time_to_psych := as.integer(stop_dt - INDEX_DT)]

# check model

psych_all_model <- coxph(Surv(time_to_psych, psych_event_ind)~ role,
                      data = long)
summary(psych_all_model)



##### Substance use disorder 

# Ensure date classes are consistent
date_cols <- c("INDEX_DT", "sud_dt_window", "DEATH_DT", "END_DATE", "REG_END_USE")
for (cc in date_cols) if (cc %in% names(look5)) look5[, (cc) := as.IDate(get(cc))]

# Optional fixed max follow-up of 5 years after index
look5[, FU_END_5Y := INDEX_DT + 365L*5L]

# Censoring date = earliest of END_DATE, DEATH_DT, 5y after index
# pmin needs Date/IDate; na.rm=TRUE treats NA as +Inf, so it picks the non-missing minimum
look5[, censor_dt := as.IDate(do.call(pmin, c(.SD, na.rm = TRUE))),
      .SDcols = c("REG_END_USE", "DEATH_DT", "FU_END_5Y")]

# Define event date only when sud_event == "Y"
look5[, sud_event_dt := fifelse(sud_outcome == "Y", sud_dt_window, as.IDate(NA))]

# Event indicator: event date exists and occurs on/before censor date and not before index
look5[, sud_event_ind := fifelse(
  !is.na(sud_event_dt) & sud_event_dt >= INDEX_DT & sud_event_dt <= censor_dt,
  1L, 0L
)]

# Stop date: event date if event, else censor date
look5[, stop_dt := fifelse(sud_event_ind == 1L, sud_event_dt, censor_dt)]

# Time-to-event in days (ensure non-negative, and handle missing censoring)
look5[, time_to_sud := as.integer(stop_dt - INDEX_DT)]

## check model works

sud_all_model <- coxph(Surv(time_to_sud, sud_event_ind)~ role,
                       data = look5)
summary(sh_all_model)


## create age variable


long$age <- long$INDEX_YR-long$YOB

summary(long$age)

sum(is.na(long$age))


## create meno group variable with controls 

long<- long%>%
  mutate(
    meno_group=case_when(
      role=='exposed'~ MENOPAUSE_TYPE,
      role=='ctrl'~ role),
    meno_group=factor(meno_group,levels=c('ctrl', 'Symptomatic_menopause', 'Early_menopause', 'Surgical_menopause'))
  )


names(long)

# check no negs

summary(long$time_to_sui)


#### save final file (before lookback filter)


#write.csv(long, 'final_sui.csv')
