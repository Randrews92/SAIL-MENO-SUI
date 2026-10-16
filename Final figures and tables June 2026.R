library(broom)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(stringr)

##### Create summary stats with full cohort including male controls ########

meno <- look5%>%filter(grepl('exposed', role))
women_ctrl <- look5%>%filter(grepl('ctrl', role))
men_ctrl <- male_look5%>%filter(grepl('male_ctrl', role))

# combine ctrl and male ctrls

names(meno)
names(women_ctrl)

combined_meno <- meno%>%
  select("ALF_PE", "role","age","follow_up","townsend", "n_diagnosis2", "sud_history",    
         "sh_history2","psych_history2","sud_event_ind", "time_to_sud",
         "sh_event_ind",   
         "time_to_sh", "psych_event_ind","time_to_psych",
         "sui_event_ind"  
         ,"time_to_sui","death_5Y",2:12)

combined_women <- women_ctrl%>%
  select("ALF_PE", "role","age","follow_up","townsend", "n_diagnosis2", "sud_history",    
                                "sh_history2","psych_history2","sud_event_ind", "time_to_sud",
                                "sh_event_ind",   
                                 "time_to_sh", "psych_event_ind","time_to_psych",
                                "sui_event_ind"  
                                ,"time_to_sui","death_5Y",2:12)


combined_men <- men_ctrl%>%
  select("ALF_PE", "role","age","follow_up","townsend", "n_diagnosis2", "sud_history",    
         "sh_history2","psych_history2","sud_event_ind", "time_to_sud",
         "sh_event_ind",   
         "time_to_sh", "psych_event_ind","time_to_psych",
         "sui_event_ind"  
         ,"time_to_sui","death_5Y",2:12)


combined_sum <- rbind(combined_meno,combined_women,combined_men)

long_distinct <- combined_sum%>%
  dplyr::distinct(ALF_PE,.keep_all = T)

names(long_distinct)

table <- long_distinct[,c(-1)]



summarystats <- tbl_summary(table, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**Summary Statistics: All Exposed**')%>%
  as_gt()%>%
  gt::gtsave('summary stats all groups.docx')


summarystats

# women ctrl versus meno 

table2 <- table%>%filter(!grepl('male_ctrl', role))

table2$role <- droplevels(table2$role)

summarystats <- tbl_summary(table2, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**exposed versus ctrl women**')%>%
  as_gt()%>%
  gt::gtsave('summary stats exposed versus ctrl women.docx')

summarystats

#men ctrl versus meno 

table3 <- table%>%filter(grepl('male_ctrl|exposed', role))

table3$role <- droplevels(table3$role)

summarystats <- tbl_summary(table3, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**exposed versus ctrl men**')%>%
  as_gt()%>%
  gt::gtsave('summary stats exposed versus ctrl men.docx')

summarystats

#men ctrl versus women ctrl 

table4 <- table%>%filter(!grepl('exposed', role))

table4$role <- droplevels(table4$role)

summarystats <- tbl_summary(table4, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**ctrl women versus ctrl men**')%>%
  as_gt()%>%
  gt::gtsave('ctrl women versus ctrl men.docx')

summarystats



##### Create SAIL MENO characteristics table ###########

long_distinct <- look5%>%
  dplyr::distinct(ALF_PE,.keep_all = T)

long_distinct <- long_distinct%>%filter(grepl('exposed', role))

Login <- SAILDBUtils::SAILConnect()


meno <- SAILDBUtils::runSQL(Login,"SELECT * from sailw1448v.MENO_DEMOGRAPHICS")

menoflat <- meno%>%
  dplyr::select(1, 5:26)

menoflat <- menoflat%>%
  group_by(ALF_PE)%>%
  summarise(across(1:22, ~{
    if(any(.=='Y', na.rm=TRUE)){
      'Y'
    }else if(any(.=='N', na.rm=TRUE)){
      'N'
    }else {
      NA_character_
    }}))

names(menoflat)

long_distinct <- long_distinct%>%
  left_join(menoflat, by='ALF_PE')

names(long_distinct)


table <-long_distinct%>%
  select("ALF_PE", "meno_group","age","follow_up","townsend", "n_diagnosis2", "sud_history",    
         "sh_history2","psych_history2","sud_outcome", "time_to_sud",
         "sh_outcome",   
         "time_to_sh", "psych_outcome","time_to_psych",
         "sui_outcome"  
         ,"time_to_sui","death_5Y",2:12,80:101)

table$townsend <- as.factor(table$townsend)
levels(table$townsend)

summarystats <- tbl_summary(table, 
                            by=meno_group,
                            missing='no',
                            type=all_continuous()~'continuous',
                            digits=all_continuous()~2,
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              townsend~'Deprivation Quintile'
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**Summary Statistics: All Exposed**')%>%
  as_gt()%>%
  gt::gtsave('summary stats sail meno cohort only3.docx')



summarystats




######### create tables for meno versus ctrl women

# sail meno versus women controls 

# create total follow-up variable


look5$REG_END_USE <- as.Date(look5$REG_END_USE)
look5$INDEX_DT <- as.Date(look5$INDEX_DT)

look5 <- look5%>%
  mutate( FINAL_END_DATE=if_else(REG_END_USE== as.Date('9999-01-01'),
                           as.Date('2023-12-31'),
                           REG_END_USE))

look5$FINAL_END_DATE <- as.Date(look5$FINAL_END_DATE)

look5$follow_up <- as.numeric(look5$FINAL_END_DATE-look5$INDEX_DT)
summary(look5$follow_up)


long_distinct <- look5%>%
  dplyr::distinct(ALF_PE,.keep_all = T)

names(long_distinct)

table <- long_distinct[,c(18, 59, 45,50,53,75,30, 77,2:12)]



summarystats <- tbl_summary(table, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                              
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**Summary Statistics: All Exposed**')%>%
  as_gt()

summarystats



######### create tables for meno versus ctrl men 

# sail meno versus male controls

# create total follow-up variable 

male_look5 <- male_look5%>%
  mutate( FINAL_END_DATE=if_else(END_DATE== as.IDate('9999-01-01'),
                                 as.IDate('2023-12-31'),
                                 END_DATE))

male_look5$FINAL_END_DATE <- as.Date(male_look5$FINAL_END_DATE)
male_look5$INDEX_DT <- as.Date(male_look5$INDEX_DT)
male_look5$follow_up <- as.numeric(male_look5$FINAL_END_DATE-male_look5$INDEX_DT)
summary(look5$follow_up)

long_distinct_man <- male_look5%>%
  dplyr::distinct(ALF_PE,.keep_all = T)

names(long_distinct_man)

table <- long_distinct_man[,c(18,60,52,47,43,76,30, 80, 2:12)]


summarystats <- tbl_summary(table, 
                            by=role,
                            missing='no',
                            type=all_continuous()~'continuous',
                            statistic=all_continuous()~'{mean}({sd})',
                            label=list(
                            
                            ))%>%
  add_p()%>%
  add_q(method='BH')%>%
  bold_labels()%>%
  modify_spanning_header(all_stat_cols()~'**Summary Statistics: All Exposed versus Male Controls**')%>%
  as_gt()

summarystats

######################## CREATE PLOTS ##################################


###### SUICIDE PLOT 5Y

fit <- survfit(sui_all_model,
               newdata = data.frame(
                 meno_group = c("ctrl", "Early_menopause","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p <- ggsurvplot(fit,
                data=look5,
                fun = "event",
                conf.int = TRUE,
                legend.title = "Group",
                legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                xlab = "Days since index",
                ylab = "Cumulative incidence",
                risk.table = T)

p$plot <- p$plot+(scale_y_continuous(labels = scales::percent_format
                                     (accuracy = 0.001)))

p


png('sui_5y_cumul_incidence.png',
       width=3500,
       height=2500,
       res=300)
print(p)
dev.off()

###### SUICIDE PLOT 10Y

fit <- survfit(sui_all_model_10,
               newdata = data.frame(
                 meno_group = c("ctrl", "Early_menopause","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p3 <- ggsurvplot(fit,
                data=look5,
                fun = "event",
                conf.int = TRUE,
                legend.title = "Group",
                legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                xlab = "Days since index",
                ylab = "Cumulative incidence",
                risk.table = T)

p3$plot <- p3$plot+(scale_y_continuous(labels = scales::percent_format
                                     (accuracy = 0.001)))

p3

png('sui_10y_cumul_incidence.png',
    width=3500,
    height=2500,
    res=300)
print(p3)
dev.off()

###### SUICIDE PLOT ALL TIME

fit <- survfit(sui_all_model_30,
               newdata = data.frame(
                 meno_group = c("ctrl", "Early_menopause","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p2 <- ggsurvplot(fit,
                data=look5,
                fun = "event",
                conf.int = TRUE,
                legend.title = "Group",
                legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                xlab = "Days since index",
                ylab = "Cumulative incidence",
                risk.table = T)

p2$plot <- p2$plot+(scale_y_continuous(labels = scales::percent_format
                                     (accuracy = 0.001)))

p2

png('sui_all_time_cumul_incidence.png',
    width=3500,
    height=2500,
    res=300)
print(p2)
dev.off()

##### Psych plot 5Y

fit <- survfit(psych_all_model,
               newdata = data.frame(
                 meno_group = c("ctrl", "Early_menopause","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p4 <- ggsurvplot(fit,
                data=look5,
                fun = "event",
                conf.int = TRUE,
                legend.title = "Group",
                legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                xlab = "Days since index",
                ylab = "Cumulative incidence",
                risk.table = T)

p4$plot <- p4$plot+(scale_y_continuous(labels = scales::percent_format
                                     (accuracy = 0.001)))

p4

png('psych_5y_cumul_incidence.png',
    width=3500,
    height=2500,
    res=300)
print(p4)
dev.off()

##### SH plot 5y

fit <- survfit(sh_all_model,
               newdata = data.frame(
                 meno_group = c("ctrl", "Early_menopause","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p5 <- ggsurvplot(fit,
                 data=look5,
                 fun = "event",
                 conf.int = TRUE,
                 legend.title = "Group",
                 legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                 xlab = "Days since index",
                 ylab = "Cumulative incidence",
                 risk.table = T)

p5$plot <- p5$plot+(scale_y_continuous(labels = scales::percent_format
                                       (accuracy = 0.001)))

p5

png('sh_5y_cumul_incidence.png',
    width=3500,
    height=2500,
    res=300)
print(p5)
dev.off()

##### SUD plot 5Y

fit <- survfit(sud_all_model,
               newdata = data.frame(
                 meno_group = c("ctrl", "Premature","Symptomatic_menopause","Surgical_menopause"),
                 sh_history2 = "N",
                 psych_history2 = "N",
                 sud_history="N",
                 n_diagnosis2 = median(look5$n_diagnosis2, na.rm = TRUE),
                 age = median(look5$age, na.rm = TRUE)
               ))


p6 <- ggsurvplot(fit,
                 data=look5,
                 fun = "event",
                 conf.int = TRUE,
                 legend.title = "Group",
                 legend.labs = c("Control", "Premature", "Symptomatic", "Surgical"),
                 xlab = "Days since index",
                 ylab = "Cumulative incidence",
                 risk.table = T)

p6$plot <- p6$plot+(scale_y_continuous(labels = scales::percent_format
                                       (accuracy = 0.001)))

p6

png('sud_5y_cumul_incidence.png',
    width=3500,
    height=2500,
    res=300)
print(p6)
dev.off()

##### forest plots with facet wrap so we can compare each one 


sui_all_model <- coxph(Surv(time_to_sui, sui_event_ind)~ meno_group+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2+
                         cluster(ALF_PE),
                       data = look5)
summary(sui_all_model)

sh_all_model <- coxph(Surv(time_to_sh, sh_event_ind)~ meno_group+
                        sh_history2+
                        psych_history2+
                        sud_history+
                        age+
                        n_diagnosis2+cluster(ALF_PE),
                      data = look5)
summary(sh_all_model)

psych_all_model <- coxph(Surv(time_to_psych, psych_event_ind)~ meno_group+
                           sh_history2+
                           psych_history2+
                           sud_history+
                           age+
                           n_diagnosis2+
                           cluster(ALF_PE),
                         data = look5)
summary(psych_all_model)

sud_all_model <- coxph(Surv(time_to_sud, sud_event_ind)~ meno_group+
                         sh_history2+
                         psych_history2+
                         sud_history+
                         age+
                         n_diagnosis2+cluster(ALF_PE),
                       data = look5)
summary(sud_all_model)

## now create plot comparing meno groups with ctrl_w as ref: 

extract_meno <- function(model, outcome_name) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(str_detect(term, "^meno_group")) %>%
    mutate(
      outcome = outcome_name,
      meno_group = str_remove(term, "^meno_group"),
      HR = estimate,
      CI_low = conf.low,
      CI_high = conf.high,
      p_value = p.value,
      sig_colour = case_when(
        p_value < 0.05 ~ "p < 0.05",
        p_value < 0.10 ~ "p < 0.10",
        TRUE ~ "NS"
      )
    ) %>%
    select(outcome, meno_group, HR, CI_low, CI_high, p_value, sig_colour)
}

forest_data <- bind_rows(
  extract_meno(sui_all_model, "Suicide"),
  extract_meno(sh_all_model, "Self-harm"),
  extract_meno(psych_all_model, "Psychiatric diagnosis"),
  extract_meno(sud_all_model, "Substance use disorder")
) %>%
  mutate(
    meno_group = case_when(
      str_detect(meno_group, "Early|early|Premature|premature") ~ "Premature menopause",
      str_detect(meno_group, "Symptomatic|symptomatic") ~ "Symptomatic menopause",
      str_detect(meno_group, "Surgical|surgical") ~ "Surgical menopause",
      TRUE ~ meno_group
    ),
    meno_group = factor(
      meno_group,
      levels = c(
        "Premature menopause",
        "Symptomatic menopause",
        "Surgical menopause"
      )
    ),
    outcome = factor(
      outcome,
      levels = c(
        "Suicide",
        "Self-harm",
        "Psychiatric diagnosis",
        "Substance use disorder"
      )
    )
  )

forest_plot <- ggplot(
  forest_data,
  aes(
    x = HR,
    y = meno_group,
    xmin = CI_low,
    xmax = CI_high,
    colour = sig_colour
  )
) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.18) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_log10() +
  scale_colour_manual(
    values = c(
      "p < 0.05" = "darkgreen",
      "p < 0.10" = "goldenrod2",
      "NS" = "grey60"
    ),
    name = "Significance"
  ) +
  facet_wrap(~ outcome, ncol = 2) +
  labs(
    x = "Hazard ratio (log scale)",
    y = "Menopause group",
    title = "Association between menopause healthcare contact and outcomes",
    subtitle = "Hazard ratios and 95% confidence intervals from Cox models"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

forest_plot

ggsave(
  "all_cox_forest_plot.pdf",
  forest_plot,
  width = 10,
  height = 7,
  dpi = 300
)


### and with covars (sh history, psych history, sud history etc.)


extract_terms <- function(model, outcome_name) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    
    # CHANGE THIS LINE TO CHOOSE WHICH VARIABLES APPEAR IN THE FOREST PLOT
    filter(str_detect(term, "^meno_group|sh_history2|psych_history2|sud_history|age|n_diagnosis2")) %>%
    
    mutate(
      outcome = outcome_name,
      variable = case_when(
        str_detect(term, "^meno_group") ~ str_remove(term, "^meno_group"),
        str_detect(term, "sh_history2") ~ "Previous self-harm",
        str_detect(term, "psych_history2") ~ "Previous psychiatric diagnosis",
        str_detect(term, "sud_history") ~ "Previous substance use disorder",
        term == "age" ~ "Age",
        term == "n_diagnosis2" ~ "Number of diagnoses",
        TRUE ~ term
      ),
      HR = estimate,
      CI_low = conf.low,
      CI_high = conf.high,
      p_value = p.value,
      sig_colour = case_when(
        p_value < 0.05 ~ "p < 0.05",
        p_value < 0.10 ~ "p < 0.10",
        TRUE ~ "NS"
      )
    ) %>%
    select(outcome, variable, HR, CI_low, CI_high, p_value, sig_colour)
}

forest_data <- bind_rows(
  extract_terms(sui_all_model, "Suicide"),
  extract_terms(sh_all_model, "Self-harm"),
  extract_terms(psych_all_model, "Psychiatric diagnosis"),
  extract_terms(sud_all_model, "Substance use disorder")
) %>%
  mutate(
    variable = case_when(
      str_detect(variable, "Early|early|Premature|premature") ~ "Premature menopause",
      str_detect(variable, "Symptomatic|symptomatic") ~ "Symptomatic menopause",
      str_detect(variable, "Surgical|surgical") ~ "Surgical menopause",
      TRUE ~ variable
    )
  )

forest_data <- forest_data%>%
  mutate(
    variable=factor(
      variable,
      levels=c(
  'Symptomatic menopause',
  'Surgical menopause',
  'Premature menopause',
  'Previous psychiatric diagnosis',
  'Previous self-harm',
  'Previous substance use disorder',
  'Number of diagnoses',
  'Age'
)))

forest_data$variable <- forcats::fct_rev(forest_data$variable)
forest_plot <- ggplot(
  forest_data,
  aes(
    x = HR,
    y = variable,
    xmin = CI_low,
    xmax = CI_high,
    colour = sig_colour
  )
) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.18) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_log10() +
  scale_colour_manual(
    values = c(
      "p < 0.05" = "darkgreen",
      "p < 0.10" = "goldenrod2",
      "NS" = "grey60"
    ),
    name = "Significance"
  ) +
  facet_wrap(~ outcome, ncol = 2) +
  labs(
    x = "Hazard ratio (log scale)",
    y = "Variable",
    title = "Cox model hazard ratios by outcome within 5 years from first menopause healthcare contact",
    subtitle = "Hazard ratios and 95% confidence intervals for menopause group and covariates"
  ) +
  theme_minimal(base_size = 10)

dev.off()
forest_plot

ggsave(
  "all_cox_forest_plot_wcovars.pdf",
  forest_plot,
  width = 11,
  height = 7,
  dpi = 300
)
