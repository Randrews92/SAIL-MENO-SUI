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
library('rms')


long <- read.csv('final_sui.csv')


## exclude anyone with less than 5 years lookback

look5 <- long%>%filter(grepl('5y_plus', LOOKBACK_CAT))

look5%>%
  group_by(role)%>%
  summarise(cnt=n_distinct(ALF_PE))


## save filtered data

# write.csv(look5,'final_women_cohort3.csv')

# look5 <- read.csv('final_women_cohort3.csv')

## create meno cohorts with matched pairs 

long_symptomatic <- look5%>%filter(grepl('Symptomatic_menopause', MENOPAUSE_TYPE))
long_early <- look5%>%filter(grepl('Early_menopause', MENOPAUSE_TYPE))
long_surg <- look5%>%filter(grepl('Surgical_menopause', MENOPAUSE_TYPE))

## ensure all have 1:1 matched groups 

long_symptomatic%>%
  group_by(role)%>%
  summarise(cnt=n_distinct(ALF_PE))

long_surg%>%
  group_by(role)%>%
  summarise(cnt=n_distinct(ALF_PE))

long_early%>%
  group_by(role)%>%
  summarise(cnt=n_distinct(ALF_PE))

## check power for suicide analysis 

long_symptomatic%>%
  group_by(role,sui_event_ind)%>%
  summarise(cnt=n_distinct(ALF_PE))

long_surg%>%
  group_by(role, sui_event_ind)%>%
  summarise(cnt=n_distinct(ALF_PE))

long_early%>%
  group_by(role,sui_event_ind)%>%
  summarise(cnt=n_distinct(ALF_PE))

look5%>%
  group_by(role,sui_event_ind)%>%
  summarise(cnt=n_distinct(ALF_PE))



###################################### SUICIDE MODELS ######################################

# now run and save all models

sui_all_model <- coxph(Surv(time_to_sui, sui_event_ind)~ meno_group+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2+
                       cluster(ALF_PE),
                       data = look5)
summary(sui_all_model)

sui_all_model_final <- tbl_regression(sui_all_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_all_model_final.docx')


sui_all_model_10 <- coxph(Surv(time_to_sui_10, sui_event_ind_10)~ meno_group+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2+
                          cluster(ALF_PE),
                       data = look5)
summary(sui_all_model_10)

sui_all_model_final_10 <- tbl_regression(sui_all_model_10, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_all_model_final_10.docx')

sui_all_model_30 <- coxph(Surv(time_to_sui_30, sui_event_ind_30)~ meno_group+
                            sh_history2+
                            psych_history2+
                            sud_history+
                            age+
                            n_diagnosis2,
                          data = look5)
summary(sui_all_model_30)


sui_all_model_final_30 <- tbl_regression(sui_all_model_30, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_all_model_final_30.docx')

sui_symp_model <- coxph(Surv(time_to_sui, sui_event_ind)~ role+
                         sh_history2+
                         psych_history2+
                          sud_history+
                         age+
                        n_diagnosis2,
                       data = long_symptomatic)
summary(sui_symp_model)

sui_symp_model_final <- tbl_regression(sui_symp_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_symp_model_final.docx')

tbl_regression(sui_symp_model,exponentiate = T)

sui_symp_model_10 <- coxph(Surv(time_to_sui_10, sui_event_ind_10)~ role+
                          sh_history2+
                          psych_history2+
                          sud_history+
                          age+
                          n_diagnosis2,
                        data = long_symptomatic)
summary(sui_symp_model_10)

sui_symp_model_30 <- coxph(Surv(time_to_sui_30, sui_event_ind_30)~ role+
                             sh_history2+
                             psych_history2+
                             sud_history+
                             age+
                             n_diagnosis2,
                           data = long_symptomatic)
summary(sui_symp_model_30)


sui_early_model <- coxph(Surv(time_to_sui, sui_event_ind)~ role+
                         sh_history2+
                         psych_history2+
                           sud_history+
                         age+
                         n_diagnosis2,
                       data = long_early)
summary(sui_early_model)

sui_early_model_final <- tbl_regression(sui_early_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_early_model_final.docx')

sui_early_model_10 <- coxph(Surv(time_to_sui_10, sui_event_ind_10)~ role+
                           sh_history2+
                           psych_history2+
                           sud_history+
                           age+
                           n_diagnosis2,
                         data = long_early)
summary(sui_early_model_10)

sui_early_model_30 <- coxph(Surv(time_to_sui_30, sui_event_ind_30)~ meno_group+
                             sh_history2+
                             psych_history2+
                             age+
                             sud_history+
                             n_diagnosis2,
                           data = long_early)
summary(sui_early_model_30)

sui_surg_model <- coxph(Surv(time_to_sui, sui_event_ind)~ role+
                             sh_history2+
                             psych_history2+
                             age+
                             sud_history+
                             n_diagnosis2,
                           data = long_surg)
summary(sui_surg_model)

sui_surg_model_final <- tbl_regression(sui_surg_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sui_surg_model_final.docx')

sui_surg_model_10 <- coxph(Surv(time_to_sui_10, sui_event_ind_10)~ role+
                           sh_history2+
                          psych_history2+
                           age+
                        sud_history+
                           n_diagnosis2,
                         data = long_surg)
summary(sui_surg_model_10)

sui_surg_model_30 <- coxph(Surv(time_to_sui_30, sui_event_ind_30)~ role+
                             sh_history2+
                             psych_history2+
                             age+
                             sud_history+
                             n_diagnosis2,
                           data = long_surg)
summary(sui_surg_model_30)


################################## SELF HARM ###################################################

sh_all_model <- coxph(Surv(time_to_sh, sh_event_ind)~ meno_group+
                          sh_history2+
                          psych_history2+
                          sud_history+
                          age+
                          n_diagnosis2+cluster(ALF_PE),
                        data = look5)
summary(sh_all_model)

sh_all_model_final <- tbl_regression(sh_all_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sh_all_model_final.docx')




sh_early_model <- coxph(Surv(time_to_sh, sh_event_ind)~ role+
                          sh_history2+
                          psych_history2+
                          sud_history+
                          age+
                          n_diagnosis2,
                        data = long_early)
summary(sh_early_model)


sh_early_model_final <- tbl_regression(sh_early_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sh_early_model_final.docx')


sh_surg_model <- coxph(Surv(time_to_sh, sh_event_ind)~ role+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2,
                       data = long_surg)
summary(sh_surg_model)

sh_surg_model_final <- tbl_regression(sh_surg_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sh_surg_model_final.docx')

sh_symp_model <- coxph(Surv(time_to_sh, sh_event_ind)~ role+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2,
                       data = long_symptomatic)
summary(sh_symp_model)

sh_symp_model_final <- tbl_regression(sh_symp_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sh_symp_model_final.docx')




############################### PSYCH DIAGNOSIS #################################

psych_all_model <- coxph(Surv(time_to_psych, psych_event_ind)~ meno_group+
                             sh_history2+
                             psych_history2+
                             sud_history+
                             age+
                             n_diagnosis2+
                           cluster(ALF_PE),
                           data = look5)
summary(psych_all_model)

psych_all_model_final <- tbl_regression(psych_all_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('psych_all_model_final.docx')

psych_early_model <- coxph(Surv(time_to_psych, psych_event_ind)~ role+
                             sh_history2+
                             psych_history2+
                             sud_history+
                             age+
                             n_diagnosis2,
                           data = long_early)
summary(psych_early_model)

psych_early_model_final <- tbl_regression(psych_early_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('psych_early_model_final.docx')


psych_surg_model <- coxph(Surv(time_to_psych, psych_event_ind)~ role+
                            sh_history2+
                            psych_history2+
                            sud_history+
                            age+
                            n_diagnosis2,
                          data = long_surg)
summary(psych_surg_model)

psych_surg_model_final <- tbl_regression(psych_surg_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('psych_surg_model_final.docx')

psych_symp_model <- coxph(Surv(time_to_psych, psych_event_ind)~ role+
                            sh_history2+
                            psych_history2+
                            sud_history+
                            age+
                            n_diagnosis2,
                          data = long_symptomatic)
summary(psych_symp_model)

psych_symp_model_final <- tbl_regression(psych_symp_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('psych_symp_model_final.docx')

############################### SUD #################################

sud_all_model <- coxph(Surv(time_to_sud, sud_event_ind)~ meno_group+
                           sh_history2+
                           psych_history2+
                           sud_history+
                           age+
                           n_diagnosis2+cluster(ALF_PE),
                         data = look5)
summary(sud_all_model)

sud_all_model_final <- tbl_regression(sud_all_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sud_all_model_final.docx')

sud_early_model <- coxph(Surv(time_to_sud, sud_event_ind)~ role+
                             sh_history2+
                             psych_history2+
                             sud_history+
                             age+
                             n_diagnosis2,
                           data = long_early)
summary(sud_early_model)

sud_early_model_final <- tbl_regression(sud_early_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sud_early_model_final.docx')


sud_surg_model <- coxph(Surv(time_to_sud, sud_event_ind)~ role+
                            sh_history2+
                            psych_history2+
                            sud_history+
                            age+
                            n_diagnosis2,
                          data = long_surg)
summary(sud_surg_model)

sud_surg_model_final <- tbl_regression(sud_surg_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sud_surg_model_final.docx')

sud_symp_model <- coxph(Surv(time_to_sud, sud_event_ind)~ role+
                            sh_history2+
                            psych_history2+
                            sud_history+
                            age+
                            n_diagnosis2,
                          data = long_symptomatic)
summary(sud_symp_model)

sud_symp_model_final <- tbl_regression(sud_symp_model, exponentiate = TRUE)%>%
  as_gt()%>%
  gt::gtsave('sud_symp_model_final.docx')

cox.zph(sui_symp_model)



### get the VIF and PHs for meno_group, just do full models role v exposed

## All suicide models are fine for PH and VIF

cox.zph(sui_all_model)
vif(sui_all_model)

cox.zph(sui_symp_model)
vif(sui_symp_model)

cox.zph(sui_early_model)
vif(sui_early_model)

cox.zph(sui_surg_model)
vif(sui_surg_model)

## Minor deviation in all sh p=0.04, driven by symp group (p=0.001) but fine in early and surg models

cox.zph(sh_all_model)
vif(sh_all_model)

cox.zph(sh_symp_model)
vif(sh_symp_model)

cox.zph(sh_surg_model)
vif(sh_surg_model)

cox.zph(sh_early_model)
vif(sh_early_model)

## large deviation in psych all model (p<0.001), but driven by symp group, p fine in early and surgical models

cox.zph(psych_all_model)
vif(psych_all_model)

cox.zph(psych_symp_model)
vif(psych_symp_model)

cox.zph(psych_surg_model)
vif(psych_surg_model)

cox.zph(psych_early_model)
vif(psych_early_model)

## no deviation in sud all model or sud early and sud surgical, p=0.006 in sud symp model 

cox.zph(sud_all_model)
vif(sud_all_model)

cox.zph(sud_symp_model)
vif(sud_symp_model)

cox.zph(sud_early_model)
vif(sud_early_model)

cox.zph(sud_surg_model)
vif(sud_surg_model)

## VIFs are under 5 throughout 

## residual checks

zph <- cox.zph(sui_all_model)
plot(zph,var='meno_group')

zph <- cox.zph(psych_all_model)
plot(zph,var='meno_group')

zph <- cox.zph(sh_all_model)
plot(zph,var='meno_group')

zph <- cox.zph(sud_all_model)
plot(zph,var='meno_group')

### save the plots

png("suicide_PH.png", width = 2000, height = 1500, res = 300)
plot(cox.zph(sui_all_model), var = "meno_group")
dev.off()

png("psych_PH.png", width = 2000, height = 1500, res = 300)
plot(cox.zph(psych_all_model), var = "meno_group")
dev.off()

png("sh_PH.png", width = 2000, height = 1500, res = 300)
plot(cox.zph(sh_all_model), var = "meno_group")
dev.off()

png("sud_PH.png", width = 2000, height = 1500, res = 300)
plot(cox.zph(sud_all_model), var = "meno_group")
dev.off()


