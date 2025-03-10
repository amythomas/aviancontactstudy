#This script accompanies the paper entitled 'Seasonal influenza vaccination in people who have contact with birds' by Amy Thomas, Suzanne Gokool, Harry Whitlow, Genevieve Clapp, Peter Moore, Richard Puleston, Louise E Smith, Riinu Pae and Ellen Brooks-Pollock  


#load packages 
library(tidyverse)
library(lubridate)
library(cowplot)
library(haven)
library(labelled) #for handling labelled variables
library(sjlabelled) #handling labelled variables
library(sjmisc) #Y #use frq() to print a summary, includes labelled vectors in frequency table
library(kableExtra)
library(table1)
library(srvyr)
library(patchwork) 
library(forcats) #re ordering geom_bar by count 

#import data
#note further information on this dataset, including access can be found in the Data Note published in Wellcome Open
#Thomas AC, Gokool S, Clapp G et al. Data note on the Avian Contact Study: a questionnaire resource for avian influenza public health planning [version 1; peer review: awaiting peer review]. Wellcome Open Res 2024, 9:604 
#https://doi.org/10.12688/wellcomeopenres.23064.1
data=readRDS('ais_data080824v1.RDS')

#change "[not completed]" entries to NA for data$avian_influenza_social_contact_survey_timestamp
data %>% mutate(survey_timestamp = ifelse(avian_influenza_social_contact_survey_timestamp == "[not completed]", NA, avian_influenza_social_contact_survey_timestamp)) -> data

#convert data$avian_influenza_social_contact_survey_timestamp into date 
ymd_hms(data$survey_timestamp,tz=Sys.timezone()) -> data$survey_timestamp
class(data$survey_timestamp)

#convert data$sec5_survey_time_started into date
data$sec5_survey_time_started
ymd_hms(data$sec5_survey_time_started,tz=Sys.timezone()) -> data$sec5_survey_time_started
class(data$sec5_survey_time_started)

#######
#questions relating to vaccination 
#######

#include individuals who responded to any 1 of the vaccine questions: - 

# flu vaccine recommendation
# sec1_q10.factor
table(data$sec1_q10.factor)

# flu vaccine uptake
# sec1_q11.factor
table(data$sec1_q11.factor)

# when were you vaccinated? 
# sec1_q11_yes_date_month
# sec1_q11_yes_date_year

# what factors influenced decision to get vaccinated? 
# sec1_q11_yes_rsn___1.factor
# sec1_q11_yes_rsn___2.factor
# sec1_q11_yes_rsn___3.factor
# sec1_q11_yes_rsn___4.factor
# sec1_q11_yes_rsn___5.factor
# sec1_q11_yes_rsn___6.factor
# sec1_q11_yes_rsn___7.factor
# sec1_q11_yes_rsn___8.factor
# sec1_q11_yes_rsn___9.factor


# what factors prevented you from getting vaccinated? 
# sec_q11_no_rsn___1.factor
# sec_q11_no_rsn___2.factor
# sec_q11_no_rsn___3.factor
# sec_q11_no_rsn___4.factor

#how many were excluded i.e. entries were blank/no data collected so filtered out
#if checked for any of the above, give notation 'vaccineq_cohort', otherwise give 'all' for variable 'vaccineq'

data %>% 
  mutate(vaccineq = ifelse(sec1_q10.factor == "Yes" | sec1_q10.factor == "No" | sec1_q10.factor == "I do not know", "vaccineq_cohort",
                             ifelse(sec1_q11.factor == "Yes as provided by the NHS" | 
                                      sec1_q11.factor == "Yes I paid for it myself" | 
                                      sec1_q11.factor == "No but I intend to get vaccinated" | 
                                      sec1_q11.factor == "No I am undecided" |
                                      sec1_q11.factor == "No I am not intending to get vaccinated" |
                                      sec1_q11.factor == "I do not know", "vaccineq_cohort",
                                    ifelse(sec1_q11_yes_rsn___1.factor == "Checked", "vaccineq_cohort",
                                           ifelse(sec1_q11_yes_rsn___2.factor == "Checked", "vaccineq_cohort",
                                                  ifelse(sec1_q11_yes_rsn___3.factor == "Checked", "vaccineq_cohort",
                                                         ifelse(sec1_q11_yes_rsn___4.factor == "Checked", "vaccineq_cohort",
                                                                ifelse(sec1_q11_yes_rsn___5.factor == "Checked", "vaccineq_cohort",
                                                                       ifelse(sec1_q11_yes_rsn___6.factor == "Checked", "vaccineq_cohort",
                                                                              ifelse(sec1_q11_yes_rsn___7.factor == "Checked", "vaccineq_cohort",
                                                                                     ifelse(sec1_q11_yes_rsn___8.factor == "Checked", "vaccineq_cohort",
                                                                                            ifelse(sec1_q11_yes_rsn___9.factor == "Checked", "vaccineq_cohort",
                                                                                                   ifelse(sec_q11_no_rsn___1.factor == "Checked", "vaccineq_cohort",
                                                                                                          ifelse(sec_q11_no_rsn___2.factor == "Checked", "vaccineq_cohort",
                                                                                                                 ifelse(sec_q11_no_rsn___3.factor == "Checked", "vaccineq_cohort",
                                                                                                                        ifelse(sec_q11_no_rsn___4.factor == "Checked", "vaccineq_cohort", "notvaccinecohort"
                                                                                                                               )))))))))))))))) -> data
table(data$vaccineq)
#2 NAs - meaning that these 2 individuals did not respond to at least one vaccine q 

data %>% 
  filter(is.na(vaccineq)) -> notvaccinecohort

#remove these IDs and proceed with reporting analysis for only individuals who responded to at least one of the five vaccine questions 

data %>% filter(!record_id == 185 & !record_id == 205) -> data


#############################
#create age categories 
#############################


#age including ≥65 year to match influenza vaccine eligibility 
data %>% mutate(age_cat65 = case_when(age_cat_5year == "20 - 24" | age_cat_5year == "25 - 29" ~ "20 - 29",
                                      age_cat_5year == "30 - 34" | age_cat_5year == "35 - 39" ~ "30 - 39",
                                      age_cat_5year == "40 - 44" | age_cat_5year == "45 - 49" ~ "40 - 49",
                                      age_cat_5year == "50 - 54" | age_cat_5year == "55 - 59" ~ "50 - 59",
                                      age_cat_5year == "60 - 64" ~ "60 - 64",
                                      age_cat_5year == "≥65" ~ "≥65")) -> data

#reorder age_cat65
data$age_cat65 <- factor(data$age_cat65, levels=c("20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 64", "≥65"))

#create 4 age categories
#"19 - 39", "40 - 59", "60 - 64", "≥65"

data %>% mutate(age_cat4 = case_when(age_cat_5year == "20 - 24" | age_cat_5year == "25 - 29" | age_cat_5year == "30 - 34" | age_cat_5year == "35 - 39" ~ "19 - 39",
                                      age_cat_5year == "40 - 44" | age_cat_5year == "45 - 49" | age_cat_5year == "50 - 54" | age_cat_5year == "55 - 59" ~ "40 - 59",
                                      age_cat_5year == "60 - 64" ~ "60 - 64",
                                      age_cat_5year == "≥65" ~ "≥65")) -> data


#reorder age_cat4
data$age_cat4 <- factor(data$age_cat4, levels=c("19 - 39", "40 - 59", "60 - 64", "≥65"))


###
#Inspect occupation 

#all occupations 
data %>% select(occupation_clean) %>% frq()

#arrange occupations in descending frequency 
data %>% group_by(occupation_clean) %>% summarise(N=n()) %>% arrange(desc(N)) %>% knitr::kable(align = "c")

#add new variable to denote top 5 occupations
data %>% mutate(top5_occupation = ifelse(occupation_clean == "Poultry farmer" | occupation_clean == "Veterinarian" | occupation_clean == "Zookeeper" | occupation_clean == "Farm manager" | occupation_clean == "Retired", "top5occ", "otherocc")) -> data


###
#Label gender variable
data$sec1_q2.factor = factor(data$sec1_q2,levels=c("1","2","3"))
levels(data$sec1_q2.factor)=c("Female","Male","Other")
label(data$sec1_q2)="What is your gender?"

###
# Demographics 

#Age 
data %>% 
  group_by(age_cat65) %>%
  summarise(N=n())

#Proportion of sample male/female
data %>% 
  group_by(sec1_q2.factor) %>%
  summarise(N=n())

#check response looks like one participant did not provide gender data
missingdata <- data %>%
  filter_all(any_vars(is.na(sec1_q2.factor)))

#Median age
#Most frequently reported occupations 
#See datanote for demographic table

#age distribution by sex 
## create median for sex grouping
#note won't run for released dataset at present - age categories released only 
#median_agesex <- data %>% group_by(sec1_q2.factor) %>% summarise(median_val=median(age))

#fig1a <- data %>% filter(sec1_q2.factor == "Female" | sec1_q2.factor == "Male") %>%
#  ggplot(aes(x = age, fill = sec1_q2.factor)) + 
#geom_histogram(position = "identity") + 
#scale_x_continuous(name="Age", breaks=seq(20,90,5)) + 
#ylab("Count") + 
#geom_vline(data = median_agesex, aes(xintercept=median_val, colour = sec1_q2.factor), linetype="dashed", show.legend = F) +
#theme_minimal_grid() + 
#guides(fill="none") + 
#facet_grid(sec1_q2.factor~.)
#fig1a


#arrange occupations in descending frequency 
data %>% group_by(occupation_clean) %>% summarise(N=n()) %>% arrange(desc(N)) %>% knitr::kable(align = "c")

# plot prop of occupations 
data %>% 
  count(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  ggplot(aes(x=reorder(occupation_clean, p), y = p, group = occupation_clean, fill = occupation_clean, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  coord_flip()

# plot count of occupations 
data %>% 
  count(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  ggplot(aes(x=reorder(occupation_clean, n), y = n, group = occupation_clean, fill = occupation_clean, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Count") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  coord_flip()



#plot occupations with >5 obs / group 
#add new variable to denote >= 5 obs/occupation 
data %>% mutate(morethan5_occ = ifelse(occupation_clean == "Poultry farmer" | occupation_clean == "Veterinarian" | occupation_clean == "Zookeeper" | occupation_clean == "Farm manager", "topocc", "otherocc")) -> data

#plot top 5 occupations
#order bars in descending order 

#raw and prop for top 4 occupations and other 
data %>% filter(morethan5_occ == "topocc") %>% 
  count(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#add 'Other' category which shows all occupations other than the top 4 

#add new variable to denote >= 5 obs/occupation 
data %>% mutate(occupation_clean2 = ifelse(morethan5_occ == "topocc", occupation_clean, "Other")) -> data

#re-order factor variable in descending order followed by Other for plotting
data$occupation_clean2 = factor(data$occupation_clean2,levels=c("Poultry farmer","Veterinarian","Zookeeper","Farm manager", "Other"))


  
fig1b <- data %>% 
    ggplot(aes(occupation_clean2, fill = occupation_clean2)) + 
    geom_bar() + 
    ylab("Count") + 
    xlab("Occupation") +
    theme_minimal_grid() + 
    theme(legend.position = "none") + 
    theme(axis.text.x=element_text(angle=-45, hjust=0))
fig1b




###
#arrange plots using patchwork 
#Figure1 <- fig1a + fig1b + plot_annotation(tag_levels = 'a') + plot_layout(ncol=2)
#Figure1
#ggsave("vaccinationfigures/Figure1.tiff", Figure1, scale = 1, width = 11, units = c("in"), dpi = 200)


###
#recommendation to receive seasonal influenza vaccine 
###

#prop of individuals for which seasonal influenza vaccine was recommended 
table(data$sec1_q10.factor)
#116 yes; 98 no; 8 I don't know 


#vaccine recommendation w/ age
fig2a <- data %>%
  count(age_cat4, sec1_q10.factor) %>%
  group_by(age_cat4) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(sec1_q10.factor == "Yes") %>%
  ggplot(aes(x = age_cat4, y = p, group = age_cat4, fill = age_cat4, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Age") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid()
fig2a

###


###
#flu recommendation by occupation 

#CIs
data %>% 
  filter(morethan5_occ == "topocc") %>%
  count(occupation_clean, sec1_q10.factor) %>%
  group_by(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#plot
fig2b <- data %>% 
  filter(morethan5_occ == "topocc") %>%
  count(occupation_clean, sec1_q10.factor) %>%
  group_by(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(sec1_q10.factor == "Yes") %>%
  ggplot(aes(x = occupation_clean, y = p, group = occupation_clean, fill = occupation_clean, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0))
fig2b  

#try coord flip to fit figure better in multipanel 
fig2b_flip <- 
  data %>% 
  filter(morethan5_occ == "topocc") %>%
  count(occupation_clean, sec1_q10.factor) %>%
  group_by(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(sec1_q10.factor == "Yes") %>%
  ggplot(aes(x = reorder(occupation_clean, p), y = p, group = occupation_clean, fill = occupation_clean, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  coord_flip() +
  theme(axis.title.y =element_blank())
fig2b_flip


#include other group for fig.2b
fig2b_flipother <- 
  data  %>%
  count(occupation_clean2, sec1_q10.factor) %>%
  group_by(occupation_clean2) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(sec1_q10.factor == "Yes") %>%
  ggplot(aes(x = reorder(occupation_clean2, -p), y = p, group = occupation_clean2, fill = occupation_clean2, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  coord_flip() +
  theme(axis.title.y =element_blank())
fig2b_flipother



data %>% 
  count(occupation_clean) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  ggplot(aes(x=reorder(occupation_clean, p), y = p, group = occupation_clean, fill = occupation_clean, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Occupation") + 
  ylab("Prop") + 
  guides(fill="none") + 
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  coord_flip()


###
#seasonal flu vaccine uptake

###
#consolidate groups into yes, no and don't know 

data %>% mutate(vaccine_uptake = case_when(sec1_q11.factor == "Yes as provided by the NHS" ~ "Vaccinated",
                                           sec1_q11.factor == "Yes I paid for it myself" ~ "Vaccinated",
                                           sec1_q11.factor == "No but I intend to get vaccinated" ~ "Intending",
                                           sec1_q11.factor == "No I am undecided" ~ "Don't know",
                                           sec1_q11.factor == "No I am not intending to get vaccinated" ~ "Unvaccinated",
                                           sec1_q11.factor == "I do not know" ~ "Don't know")) -> data

#reorder to yes, no, don't know 
data$vaccine_uptake <- factor(data$vaccine_uptak, levels = c("Vaccinated", "Unvaccinated", "Intending", "Don't know"))


###
#simplify and include only Vacc/Unvacc

#plot prop vaccine uptake for Vacc/Unvacc with 4 age categories 
fig2c <- data %>%
  count(age_cat4, vaccine_uptake) %>%
  group_by(age_cat4) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(vaccine_uptake == "Vaccinated" | vaccine_uptake == "Unvaccinated") %>%
  ggplot(aes(x = age_cat4, y = p, group = age_cat4, fill = age_cat4, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Age") + 
  ylab("Prop") + 
  theme_minimal_grid() +
  theme(axis.title.x = element_blank()) + 
  facet_grid(.~factor(vaccine_uptake)) +
  scale_x_discrete(drop=T) +
  guides(fill="none")
fig2c

#greatest prop of vaccinees aged over 60 
#n over 60: 22 + 28 = 48
#n over 60 and vaccinated: 29
#29/48; 60% 

#largest unvaccinated prop aged 20 - 39 
#n of 20 - 39 year olds: 68
#n of 20 - 39 years olds unvaccinated: 45
#45/68; 66%


###################################
#eligibility/risk and vaccine uptake 
####################################
#eligibility of sample and vaccine uptake based on eligibility of ≥65 years and occupational/contact risk 
#hard to define occupational risk inline with green book so take sample that have reported exposure to avian influenza and consider as the at-risk group 
#three groups: No risk; ≥65 years; exposed to AIV

# what proportion of survey respondents are 65 and over and eligible to have the vaccine? 
data %>% 
  filter(age_cat4 == "≥65") %>% 
  summarise(N=n())

#28 people, 28/225 (total respondents) = 12%

# what proportion of eligible respondents ≥65 years received the vaccine? sec1_q11
data %>% 
  filter(age_cat4 == "≥65") %>% 
  count(sec1_q11.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#16/28 (57%) of over 65s were vaccinated, 36% (n=10) said they were not intending to get vaccinated 

# what proportion of respondents are classed as at risk due to avian contact? 
# Green Book: People at highest risk are likely to be those undertaking culling or cleaning at confirmed
# avian influenza outbreak premises, or handling live unwell birds. Workers employed at, or
# regularly visiting, statutorily-registered poultry units and poultry processing units, may also
# be at risk if they have direct exposure to bird faeces/litter such as through initial egg
# sorting or cleaning of premises. People involved in collection of wild bird carcasses where
# avian influenza is suspected should also be considered for vaccination.

#consider exposure to AIV to define at-risk group 
#sec1_q12

#≥65 years
#exposed to AIV
#no risk = anyone else

data %>%
  mutate(riskgroup3 = ifelse(age_cat4 == "≥65", "≥65 years",
                            ifelse(sec1_q12.factor == "Yes", "Exposed to AIV", "No risk"))) -> data

table(data$riskgroup3)

#reorder risk levels 
data$riskgroup3 <- factor(data$riskgroup3, levels = c("No risk", "≥65 years", "Exposed to AIV"))

#check variable
#age of individuals by risk group 
ggplot(data, aes(x = age, fill = riskgroup3)) + 
geom_histogram(position = "identity", alpha = 0.5) + 
scale_x_continuous(name="Age", breaks=seq(20,90,5)) + 
ylab("Count") + 
scale_fill_discrete(name = "Risk group") + 
theme_minimal_grid()

#AIV exposure by age 
ggplot(data, aes(x = age, fill = sec1_q12.factor)) + 
  geom_histogram(position = "identity", alpha = 0.5) + 
  scale_x_continuous(name="Age", breaks=seq(20,90,5)) + 
  ylab("Count") + 
  scale_fill_discrete(name = "AIV exposure") + 
  theme_minimal_grid()

#how many individuals were aged ≥65 and exposed to AIV 
data %>% 
  filter(age_cat4 == "≥65" & sec1_q12.factor == "Yes") %>% 
  summarise(n=n())


data %>%
  group_by(riskgroup3) %>%
  count(vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#3 risk groups 

#restructure so that comparison is between vacc/unvacc within each 'risk' group
fig2d <- data %>%
  count(riskgroup3, vaccine_uptake) %>%
  group_by(riskgroup3) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(vaccine_uptake == "Vaccinated" | vaccine_uptake == "Unvaccinated") %>%
  ggplot(aes(x = vaccine_uptake, y = p, group = vaccine_uptake, fill = vaccine_uptake, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) + 
  xlab("Vaccine uptake") + 
  ylab("Prop") + 
  theme_minimal_grid() +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) +
  facet_grid(.~factor(riskgroup3)) +
  guides(fill="none")
fig2d

#note categories 'intending' and 'don't know' aren't shown for figure 2d for simplicity 

#vaccine uptake in ≥65 year group 
#16/28; 57%

#vaccine uptake in no risk group 
#52/(52+73+21+20)
#52/166; 31%

#vaccine uptake in aiv exposed group 
#9/28; 32%



Figure2 <- (fig2a | fig2b_flipother) / fig2c /  fig2d + plot_annotation(tag_levels = 'a') 
Figure2
#ggsave("vaccinationfigures/Figure2.tiff", Figure2, scale = 1.5, width = 6, units = c("in"), dpi = 200)





###
#Factors influencing decision to become vaccinated against seasonal influenza
###

#for denominator, group by unique IDs and look at number of unique IDs for each question

#order responses by most to least frequently reported 
#For protection against seasonal (winter) flu

data %>%
  count(sec1_q11_yes_rsn___1.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#filter to those that were vaccinated 
data %>%
  filter(vaccine_uptake == "Vaccinated") %>%
  count(sec1_q11_yes_rsn___1.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))
#63/77; 82


#Stratify by age
data %>%
  count(age_cat4, sec1_q11_yes_rsn___1.factor) %>%
  group_by(age_cat4) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

#Need to shape from wide to long to plot all responses 

#select variables for factors influencing decision to get vaccinated
my_vars2 <- function() {
  matches("sec1_q11_yes_rsn___")
}

my_vars3 <- function() {
  contains("factor")
}

#first select all variables which relate to sec1_q11_yes_rsn then drop those that are not the factor version 
influence_vaccination <- data %>% select(my_vars2()) %>% select(my_vars3())

#rename variables 
influence_vaccination %>% rename("Protection against seasonal influenza" = sec1_q11_yes_rsn___1.factor,
                                 "Convenience of appointment times" = sec1_q11_yes_rsn___2.factor,
                                 "Convenience of vaccine centre location" = sec1_q11_yes_rsn___3.factor, 
                                 "The vaccine is safe - no side effects" = sec1_q11_yes_rsn___4.factor, 
                                 "The vaccine is effective at reducing the risk of seasonal (winter) flu" = sec1_q11_yes_rsn___5.factor, 
                                 "Other people I know are vaccinated" = sec1_q11_yes_rsn___6.factor, 
                                 "The vaccine has been recommended for me" = sec1_q11_yes_rsn___7.factor,
                                 "Protection against avian influenza (bird flu)" = sec1_q11_yes_rsn___8.factor,
                                 "To protect other people" = sec1_q11_yes_rsn___9.factor) -> influence_vaccination

#reshape wide to long
influence_vaccination %>% pivot_longer(cols = 1:9, names_to = "vaccine_uptake") -> influence_vaccination_long

#number of individuals/denominator 
influence_vaccination_long %>% 
  group_by()



# prop factors influencing vaccination table
influence_vaccination_long %>%
  count(vaccine_uptake, value) %>%
  group_by(vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_influence_vaccination

#plot responses 
#plot counts not prop
fig3a <- 
  influence_vaccination_long %>%
  count(vaccine_uptake, value) %>%
  group_by(vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(vaccine_uptake, n), y = n, group = vaccine_uptake, fill = vaccine_uptake, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = -0.5) + 
  xlab("Vaccinated") + 
  ylab("Count") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  guides(fill="none") + 
  coord_flip() +
  theme_minimal_grid()
fig3a


##############
#What factors have prevented you from getting vaccinated?
#sec_q11_no_rsn

#select variables for factors that prevented vaccinated
my_vars2b <- function() {
  matches("sec_q11_no_rsn___")
}

my_vars3b <- function() {
  contains("factor")
}

#first select all variables which relate to sec1_q11_yes_rsn then drop those that are not the factor version 
prevent_vaccination <- data %>% select(my_vars2b()) %>% select(my_vars3b())

#rename variables 
prevent_vaccination %>% rename("I have not had time to get vaccinated" = sec_q11_no_rsn___1.factor,
                               "I do not know where to get vaccinated" = sec_q11_no_rsn___2.factor,
                               "It is too far/inconvenient to get vaccinated" = sec_q11_no_rsn___3.factor, 
                               "The vaccine is too expensive" = sec_q11_no_rsn___4.factor) -> prevent_vaccination

#reshape wide to long
prevent_vaccination %>% pivot_longer(cols = 1:4, names_to = "prevent_vaccine_uptake") -> prevent_vaccination_long

# prop factors preventing vaccine uptake table
prevent_vaccination_long %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_prevent_vaccination


#plot responses 
fig3b <-
  prevent_vaccination_long %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(prevent_vaccine_uptake, n), y = n, group = prevent_vaccine_uptake, fill = prevent_vaccine_uptake, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = -0.5) + 
  xlab("Unvaccinated and intending") + 
  ylab("Count") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  guides(fill="none") + 
  coord_flip() +
  theme_minimal_grid()
fig3b


##############
#What factors influenced your indecision or decision to not get vaccinated?
#sec_q11_no_rsn

#select variables for factors that prevented vaccinated
my_vars2c <- function() {
  matches("sec1_q11_no_rsn___")
}

my_vars3c <- function() {
  contains("factor")
}

#first select all variables which relate to sec1_q11_yes_rsn then drop those that are not the factor version 
prevent_vaccination2 <- data %>% select(my_vars2c()) %>% select(my_vars3c())

#rename variables 
prevent_vaccination2 %>% rename("The vaccine is too expensive" = sec1_q11_no_rsn___1.factor,
                                "I am not sure if the vaccine is effective" = sec1_q11_no_rsn___2.factor,
                                "The vaccine may cause short term side effects" = sec1_q11_no_rsn___3.factor, 
                                "The vaccine may cause long term side effects" = sec1_q11_no_rsn___4.factor, 
                                "I am not likely to catch seasonal (winter) flu" = sec1_q11_no_rsn___5.factor,
                                "I will not get very ill if I catch seasonal (winter) flu" = sec1_q11_no_rsn___6.factor, 
                                "I am allergic to the vaccine" = sec1_q11_no_rsn___7.factor,
                                "It is better to get natural immunity" = sec1_q11_no_rsn___8.factor,
                                "Other" = sec1_q11_no_rsn___9.factor) -> prevent_vaccination2



#reshape wide to long
prevent_vaccination2 %>% pivot_longer(cols = 1:9, names_to = "prevent_vaccine_uptake") -> prevent_vaccination_long2

# prop factors preventing vaccine uptake table
prevent_vaccination_long2 %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_prevent_vaccination2


#plot responses 
fig3c <- prevent_vaccination_long2 %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(prevent_vaccine_uptake, n), y = n, group = prevent_vaccine_uptake, fill = prevent_vaccine_uptake, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = -0.5) + 
  xlab("Unvaccinated, not intending") + 
  ylab("Count") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  guides(fill="none") + 
  coord_flip() +
  theme_minimal_grid()
fig3c

Figure3 <- fig3a / fig3b / fig3c + plot_annotation(tag_levels = 'a') 
Figure3
#ggsave("vaccinationfigures/Figure3.tiff", Figure3, scale = 2, width = 6, units = c("in"), dpi = 200)

#
#reshape wide to long
prevent_vaccination2 %>% pivot_longer(cols = 1:9, names_to = "prevent_vaccine_uptake") -> prevent_vaccination_long2_other

# prop factors preventing vaccine uptake table
prevent_vaccination_long2_other %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prevent_vaccination_long2_other

#denominators for vaccine uptake
table(data$vaccine_uptake)
#77 vaccinated 
#98 unvaccinated
#25 intending
#22 don't know 



#Figure 3c including thematically coded other free-text responses, matching those given as main drop down option where possible 
#subset and re-code other 
#note free-text responses not included in open access data set 

#import data
data=readRDS('ais_data080824v1_vaccinefreetext.RDS')



##############
#What factors influenced your indecision or decision to not get vaccinated?
#sec_q11_no_rsn

#select variables for factors that prevented vaccinated
my_vars2c <- function() {
  matches("sec1_q11_no_rsn___")
}

my_vars3c <- function() {
  contains("factor")
}

#first select all variables which relate to sec1_q11_yes_rsn then drop those that are not the factor version 
prevent_vaccination2 <- data %>% select(my_vars2c()) %>% select(my_vars3c()) 
prevent_vaccination3 <- data %>% (sec1_q11_no_rsn_other)

prevent_vaccination4 <- data %>% select(record_id) %>% cbind(prevent_vaccination2,prevent_vaccination3)

#rename variables 
prevent_vaccination4 %>% rename("The vaccine is too expensive" = sec1_q11_no_rsn___1.factor,
                                "I am not sure if the vaccine is effective" = sec1_q11_no_rsn___2.factor,
                                "The vaccine may cause short term side effects" = sec1_q11_no_rsn___3.factor, 
                                "The vaccine may cause long term side effects" = sec1_q11_no_rsn___4.factor, 
                                "I am not likely to catch seasonal (winter) flu" = sec1_q11_no_rsn___5.factor,
                                "I will not get very ill if I catch seasonal (winter) flu" = sec1_q11_no_rsn___6.factor, 
                                "I am allergic to the vaccine" = sec1_q11_no_rsn___7.factor,
                                "It is better to get natural immunity" = sec1_q11_no_rsn___8.factor,
                                "Other" = sec1_q11_no_rsn___9.factor) -> prevent_vaccination4



#reshape wide to long
prevent_vaccination4 %>% pivot_longer(cols = 2:10, names_to = "prevent_vaccine_uptake") -> prevent_vaccination_long4

#re-code free-text responses 
prevent_vaccination4_recoded <- prevent_vaccination_long4 %>% 
  mutate(freetextother = case_when(sec1_q11_no_rsn_other == "couldnt get it from the doctors" ~ "Couldn't access from GP",
         sec1_q11_no_rsn_other == "Doesn't want it" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "Fit, healthy, not interested" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "forgot" ~ "Forgot",
         sec1_q11_no_rsn_other == "Have not been offered it, classed as to young to recieve it " ~ "Not offered",
         sec1_q11_no_rsn_other == "Haven't got round to it" ~ "No time",
         sec1_q11_no_rsn_other == "I didn't realise i could pay to get vaccinated as i'm not eligible for a free one" ~ "Not aware",
         sec1_q11_no_rsn_other == "I have ad bad reactions including elevated blood pressure" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "I have received no information on the vaccine and I have never been offered it." ~ "Not offered",
         sec1_q11_no_rsn_other == "I haven't looked into getting it and work hasn't recommended it." ~ "Not offered",
         sec1_q11_no_rsn_other == "I haven't really thought abut it" ~ "Not aware",
         sec1_q11_no_rsn_other == "If i need it i will have it" ~ "Not aware",
         sec1_q11_no_rsn_other == "just didnt remember to, never had flu before not something i think of" ~ "Not aware",
         sec1_q11_no_rsn_other == "just dont want to" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "making an appointment didnt have time" ~ "No time",
         sec1_q11_no_rsn_other == "NEEDLES" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "Never thought about it" ~ "Not aware",
         sec1_q11_no_rsn_other == "No time" ~ "No time",
         sec1_q11_no_rsn_other == "Not interested" ~ "Doesn't want it",
         sec1_q11_no_rsn_other == "Not offered locally  " ~ "Not offered",
         sec1_q11_no_rsn_other == "Too busy" ~ "No time",
         sec1_q11_no_rsn_other == "wasn't aware of it" ~ "Not aware"))
         

table(prevent_vaccination4_recoded$freetextother)
         
         
# prop factors preventing vaccine uptake table
prevent_vaccination4_recoded %>%
  count(prevent_vaccine_uptake, value) %>%
  group_by(prevent_vaccine_uptake) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_prevent_vaccination4_recoded


#free-text responses 
prevent_vaccination4_recoded %>%
  filter(!freetextother == 'NA') %>%
  count(freetextother, value) %>%
  group_by(freetextother) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_prevent_vaccination4_recoded2
