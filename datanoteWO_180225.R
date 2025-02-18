# Code to accompany 'Data note on the Avian Contact Study: a questionnaire resource for avian influenza public health planning', Thomas et al 2025, Wellcome Open Research
# https://doi.org/10.12688/wellcomeopenres.23064.1
# Authors: Harry Whitlow and Amy Thomas
# Date of creation: 18/02/25
# R vers: 4.4.2


#load packages 
library(tidyverse)
library(lubridate)
library(cowplot)
library(haven)
library(labelled) #for handling labelled variables
library(sjlabelled) #handling labelled variables
library(sjmisc) # use frq() to print a summary, includes labelled vectors in frequency table
library(kableExtra)
library(table1)
library(srvyr)
library(patchwork)


#see data availability statement in https://doi.org/10.12688/wellcomeopenres.23064.1 for data access
#import data
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

#add labels for sec1_q2.factor - Female, Male, Other 
data$sec1_q2.factor #check class and levels
data$sec1_q2.factor = factor(data$sec1_q2,levels=c("1","2","3")) #change to factor
levels(data$sec1_q2.factor)=c("Female","Male","Other") #assign labels 


###############
#response rate 
###############

data %>% select(respondent_type) %>% frq() 
#63 online, 162 in person

#distribution of responses over time by respondent type (in-person vs online)
fig1a <- ggplot(data, aes(x = as.Date(sec5_survey_time_started), fill = respondent_type)) + 
  geom_histogram(position = "identity", alpha = 0.5) + 
  scale_x_date(name="Date") + ylab("Count") + 
  #geom_vline(aes(xintercept=median(age)), colour="black", linetype="dashed", show.legend = F) +
  theme_minimal_grid() + 
  scale_fill_discrete(name = "Survey format")
fig1a

#age distribution of survey respondents coloured by survey format

## create median by group
median_age <- data %>% group_by(respondent_type) %>% summarise(median_val=median(age))

#uses age as continuous - note not released with data set to preserve anonymity, age available as categorical 
#fig1b <- ggplot(data, aes(x = age, fill = respondent_type)) + 
# geom_histogram(position = "identity", alpha = 0.5) + 
#  scale_x_continuous(name="Age of participant", breaks=seq(20,90,5)) + 
#  ylab("Count") + 
#  #geom_vline(data = median_age, aes(xintercept=median_val, colour = respondent_type), linetype="dashed", show.legend = F) +
#  theme_minimal_grid() + 
#  scale_fill_discrete(name = "Survey format") + 
#  facet_grid(respondent_type~.)
#fig1b


#Figure1 <- fig1a + fig1b + 
#  plot_annotation(tag_levels = 'a') +
#  plot_layout(guides = 'collect') +
#  plot_layout(widths = c(0.7, 1))
  
#Figure1
#ggsave("datanotefigures/Figure1.tiff", Figure1, scale = 1, width = 9, height = 5, units = c("in"), dpi = 200)


#####
#demographics 
#####

#create new identifier that groups all respondents together for use with table1()
data %>% mutate(surveyname = ifelse(respondent_type == "in-person", "Avian Contact Study", "Avian Contact Study")) -> data

#occupation with ≥5 observations per group, otherwise 'Other'

#add new variable to denote occupations with >5 observations per group 
data %>% mutate(occupation_clean_5 = ifelse(occupation_clean == "Poultry farmer" | occupation_clean == "Veterinarian" | occupation_clean == "Zookeeper" | occupation_clean == "Retired" | occupation_clean == "Farm manager" | occupation_clean == "Mixed/Livestock/Arable farmer", "occmorethan5", "otherocc")) -> data

#add new variable to denote occupations with >5 observations per group for tallying 
data %>% mutate(occupation_clean_5_grouping = ifelse(occupation_clean_5 == "occmorethan5", occupation_clean, "Other")) -> data

#note paper includes median age using continuous age variable 

#age including ≥65 year to match influenza vaccine eligibility 
data %>% mutate(age_cat65 = case_when(age_cat_5year == "20 - 24" | age_cat_5year == "25 - 29" ~ "20 - 29",
                                      age_cat_5year == "30 - 34" | age_cat_5year == "35 - 39" ~ "30 - 39",
                                      age_cat_5year == "40 - 44" | age_cat_5year == "45 - 49" ~ "40 - 49",
                                      age_cat_5year == "50 - 54" | age_cat_5year == "55 - 59" ~ "50 - 59",
                                      age_cat_5year == "60 - 64" ~ "60 - 64",
                                      age_cat_5year == "≥65" ~ "≥65")) -> data

#reorder age_cat65
data$age_cat65 <- factor(data$age_cat65, levels=c("20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 64", "≥65"))
                  
table1(~  age_cat65 + sec1_q2.factor + occupation_clean_5_grouping + sec1_q7 + sec1_q8.factor | surveyname, overall=F, format_number = list(big.mark = ","),
       caption = "Demographic", data = data)


############
#health score
############

# report mean health rating for each age bracket
data %>% group_by(age_cat65) %>%
  summarise(raw.mean = mean(sec1_q9, na.rm = TRUE),
            sd = sd(sec1_q9, na.rm = TRUE), 
            n = n())

# mean and standard error of health score for each age category 
# include age category of ≥65
fig3 <- data %>% 
  filter(!age_cat65 == "NA") %>% 
  ggplot(aes(x=age_cat65, y = sec1_q9)) + 
  geom_point(stat = "summary", fun.y = "mean", na.rm = TRUE) + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", na.rm = TRUE, aes(width = 0.2)) + 
  labs(x = "Age", y="Mean health score") + 
  theme_minimal_grid() 
fig3

Figure3 <- fig3 + plot_annotation()
Figure3
#ggsave("datanotefigures/Figure3.tiff", Figure3, scale = 1, width = 8, height = 5, units = c("in"), dpi = 200)


##############
#To the best of your knowledge, have you ever been exposed to avian influenza (bird flu)?
##############

#sec1_q12
data %>% select(sec1_q12.factor) %>% frq()
#14% (31/225) reported known exposure to avian influenza 


##############
#Have you ever been offered a test for avian influenza (bird flu)?
##############

data %>% select(sec1_q13.factor) %>% frq()
#6 people have been previously tested 

#What was the result of this test? 
data %>% select(sec1_q13_yestest.factor) %>% frq()
#No individuals tested positive 

#Have you been tested more than once?
data %>% select(sec1_q13_yestest.factor) %>% frq()

#Have you ever been offered antiviral medication (oseltamivir®, also known as Tamiflu) for avian influenza (bird flu)? 
data %>% select(sec1_q14.factor) %>% frq()
#10 % offered antivirals, of which 8% accepted 



##################
#bird ownership/flock size
##################

#If you are a bird owner, how many birds do you own?
#sec2_q4

data %>%
  count(sec2_q4.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) -> prop_bird_ownership

fig4a <- data %>%
  count(sec2_q4.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  ggplot(aes(x = sec2_q4.factor, y = p, 
             ymax = lower, ymin = upper,
             group = sec2_q4.factor, fill = sec2_q4.factor)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -10) + 
  xlab("N birds owned") + 
  ylab("Prop") + 
  guides(fill="none") +
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0))
fig4a


##################
#contact between people and birds  
##################
#sec2_q1 - How frequently do you have direct contact* with any type of domestic or wild bird(s)? *Direct contact means being within 2 metres of a bird.

data %>%
  count(sec2_q1.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1]))

fig4b <- data %>%
  count(sec2_q1.factor) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  ggplot(aes(x = sec2_q1.factor, y = p, 
             ymax = lower, ymin = upper,
             group = sec2_q1.factor, fill = sec2_q1.factor)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -10) + 
  ylab("Prop") + 
  #xlab("Contact frequency") + 
  guides(fill="none") +
  labs(x = NULL) +
  theme_minimal_grid() +
  theme(axis.text.x=element_text(angle=-45, hjust=0))
fig4b


#############################
#contact type with birds i.e culling, plucking, etc 
#############################

#sec2_q5

bird_contacttype <- data %>% select(starts_with("sec2_q5") & is.factor)

bird_contacttype <- bird_contacttype %>% rename("Feeding"=sec2_q5___1.factor,
                                                "Handling"=sec2_q5___2.factor,
                                                "Touch waste/litter/eggs"=sec2_q5___3.factor,
                                                "Culling"=sec2_q5___4.factor,
                                                "Plucking"=sec2_q5___5.factor,
                                                "Disposal"=sec2_q5___6.factor,
                                                "Visual contact"=sec2_q5___7.factor,
                                                "Other"=sec2_q5___8.factor)
#reshape wide to long
bird_contacttype %>% 
  pivot_longer(cols = 1:8, names_to = "contact_type") -> bird_contacttype_long                                            


#plot count of type of contact with birds 
fig4c <- bird_contacttype_long %>%
  count(contact_type, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(contact_type, n), y = n, group = contact_type, fill = contact_type, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  #geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  #geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5) + 
  xlab("Contact type") + 
  ylab("Count") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  guides(fill="none") + 
  coord_flip() +
  theme_minimal_grid()
fig4c

##################
#types of birds
##################

bird_ownership <- data %>% select(starts_with("sec2_q2") & is.factor,
                                  c(sec2_q2_dombrd_othr, sec2_q2_preybrd_othr, sec2_q2_wildwtrfwl_othr,
                                    sec2_q2_seabrd_othr, sec2_q2_petbrd_othr, sec2_q2_anybrd_othr))

bird_ownership <- bird_ownership %>% rename("Chickens"=sec2_q2_dombrd___1.factor,
                                            "Turkeys"=sec2_q2_dombrd___2.factor,
                                            "Ducks"=sec2_q2_dombrd___3.factor,
                                            "Geese"=sec2_q2_dombrd___4.factor,
                                            "Guinea fowl"=sec2_q2_dombrd___5.factor,
                                            "Quail"=sec2_q2_dombrd___6.factor,
                                            "Owls"=sec2_q2_preybrd___1.factor,
                                            "Hawks"=sec2_q2_preybrd___2.factor,
                                            "Eagles"=sec2_q2_preybrd___3.factor,
                                            "Buzzards"=sec2_q2_preybrd___4.factor,
                                            "Kites"=sec2_q2_preybrd___5.factor,
                                            "Vultures"=sec2_q2_preybrd___6.factor,
                                            "Wild ducks"=sec2_q2_wildwtrfwl___1.factor,
                                            "Wild geese"=sec2_q2_wildwtrfwl___2.factor,
                                            "Wild swans"=sec2_q2_wildwtrfwl___3.factor,
                                            "Wild teal"=sec2_q2_wildwtrfwl___4.factor,
                                            "Gulls"=sec2_q2_seabrd___1.factor,
                                            "Guillemots"=sec2_q2_seabrd___2.factor,
                                            "Herons"=sec2_q2_seabrd___3.factor,
                                            "Kittiwakes"=sec2_q2_seabrd___4.factor,
                                            "Terns"=sec2_q2_seabrd___5.factor,
                                            "Budgerigars"=sec2_q2_petbrd___1.factor,
                                            "Canaries"=sec2_q2_petbrd___2.factor,
                                            "Cockatiels"=sec2_q2_petbrd___3.factor,
                                            "Doves"=sec2_q2_petbrd___4.factor,
                                            "Finches"=sec2_q2_petbrd___5.factor,
                                            "Parrots"=sec2_q2_petbrd___6.factor)


#reshape wide to long
bird_ownership %>% select(!starts_with("sec2_q2")) %>%
  pivot_longer(cols = 1:27, names_to = "species") -> bird_ownership_long


#create a higher order group for bird: domestic bird, bird of prey, wild waterfoul, seabird, pet bird, Other 
bird_ownership_long %>% 
  mutate(birdgroup = case_when(species == "Chickens" | species == "Turkeys" | species == "Ducks" | species == "Geese" |  species == "Guinea fowl" |species == "Quail" ~ "Domestic bird",
                               species == "Owls" | species == "Hawks" | species == "Eagles" | species == "Buzzards" | species == "Kites" | species == "Vultures" ~ "Birds of prey", 
                               species == "Wild ducks" | species == "Wild geese" | species == "Wild swans" | species == "Wild teal" ~ "Wild waterfowl", 
                               species == "Gulls" | species == "Guillemots" | species == "Herons" | species == "Kittiwakes" | species == "Terns" ~ "Seabirds",
                               species == "Budgerigars" | species == "Canaries" | species == "Cockatiels" | species == "Doves" | species == "Finches" | species == "Parrots" ~ "Pet birds")) -> bird_ownership_long


#reorder bird group
bird_ownership_long$birdgroup <- factor(bird_ownership_long$birdgroup, levels=c("Domestic bird",
                                                                                "Birds of prey", 
                                                                                "Wild waterfowl",
                                                                                "Seabirds",
                                                                                "Pet birds"))

# prop and n of different bird species (ignoring free-text responses)
bird_ownership_long %>%
  count(species, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") -> birdtypes


bird_ownership_long %>%
  count(species, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(species, p), y = p, group = species, fill = species, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5) + 
  xlab("Species") + 
  ylab("Prop") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  ggtitle("Bird species participants in contact with") +
  guides(fill="none") + 
  coord_flip() +
  theme_minimal_grid()

#proportion of bird species in contact with by bird group 
# add group_by(birdgroup) if proportions to be calculated within bird groups
bird_ownership_long %>%
  count(birdgroup, species, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(species, p), y = p, group = species, fill = species, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") + #order columns in descending order within bird group 
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5) + 
  xlab("Species") + 
  ylab("Prop") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  ggtitle("Bird species participants in contact with") +
  guides(fill="none") +  
  coord_flip() +
  theme_minimal_grid() + 
  facet_grid(birdgroup~., scales = "free_y") #drops unused factor levels across bird groups

#plot counts 
fig4d <- bird_ownership_long %>%
  count(birdgroup, species, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") %>% 
  ggplot(aes(x = reorder(species, n), y = n, group = species, fill = species, 
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge") + #order columns in descending order within bird group 
  #geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  #geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5) + 
  xlab("Species") + 
  ylab("Count") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.5)) + 
  guides(fill="none") +  
  coord_flip() +
  theme_minimal_grid() +
  theme(strip.text.x = element_text(size = 8)) + 
  facet_grid(birdgroup~., scales = "free_y")  #drops unused factor levels across bird groups
  fig4d

# print type of birds in contact with 
bird_ownership_long %>% group_by(species) %>% summarise(N=n()) %>% arrange(desc(N)) %>% knitr::kable(align = "c")

bird_ownership_long %>%
  count(birdgroup, species, value) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(value == "Checked") %>%
  arrange(desc(n))
  
#Figure 4

Figure4ab <- (fig4a | fig4b) + plot_annotation(tag_levels = 'a')
Figure4ab
#ggsave("datanotefigures/Figure4ab.tiff", Figure4, scale = 1.2, width = 12, units = c("in"), dpi = 200)

Figure4cd <- (fig4c | fig4d) + plot_annotation(tag_levels = 'a')
Figure4cd

Figure4 <- (fig4a | fig4b) / (fig4c | fig4d) + plot_annotation(tag_levels = 'a') + plot_layout(heights = c(1, 2))
Figure4
#ggsave("datanotefigures/Figure4.tiff", Figure4, scale = 1.2, height = 9, width = 8, units = c("in"), dpi = 200)



####
#Contact with other people 

#Number of people in direct contact with in last 24h
data %>%
  filter(!is.na(sec3_q1.factor)) %>% #remove missing responses for calculating proportions 
  count(sec3_q1.factor) %>%
  mutate(p=round(n/sum(n)*100))
#197 respondents answered question 


###
#Biosecurity and awareness of avian influenza 


#Extract biosecurity variables
biosecurity <- data %>% select(c(record_id, sec2_q3.factor), starts_with("sec2_q7___") & is.factor)

#How many respondents reported using or not using biosecurity measures?
#Assume that if all measures are 'unchecked' then question skipped i.e., missing 

biosecurity <- biosecurity %>% 
  mutate(biosecurity_q_complete = ifelse(sec2_q7___1.factor == "Unchecked" &
                                  sec2_q7___2.factor == "Unchecked" &
                                  sec2_q7___3.factor == "Unchecked" &
                                  sec2_q7___4.factor == "Unchecked" &
                                  sec2_q7___5.factor == "Unchecked" &
                                  sec2_q7___6.factor == "Unchecked" &
                                  sec2_q7___7.factor == "Unchecked" &
                                  sec2_q7___8.factor == "Unchecked" &
                                  sec2_q7___9.factor == "Unchecked" &
                                  sec2_q7___10.factor == "Unchecked" &
                                  sec2_q7___11.factor == "Unchecked" &
                                  sec2_q7___12.factor == "Unchecked" &
                                  sec2_q7___13.factor == "Unchecked" &
                                  sec2_q7___14.factor == "Unchecked" &
                                  sec2_q7___15.factor == "Unchecked" &
                                  sec2_q7___16.factor == "Unchecked" &
                                  sec2_q7___17.factor == "Unchecked", "missed_q", "completed_q"))

table(biosecurity$biosecurity_q_complete)


#count responses 
table(data$biosecurity_q)
#218/225 responded to the biosecurity question 

#Rename biosecurity variables
biosecurity <- biosecurity %>% rename("I use face masks"=sec2_q7___1.factor,
                                      "I use goggles"=sec2_q7___2.factor,
                                      "I use gloves"=sec2_q7___3.factor,
                                      "I change clothing when in contact with birds"=sec2_q7___4.factor,
                                      "I use outer garments when in contact with birds"=sec2_q7___5.factor,
                                      "I change boots"=sec2_q7___6.factor,
                                      "I use boot covers"=sec2_q7___7.factor,
                                      "I use a disinfecting footwear dip"=sec2_q7___8.factor,
                                      "I wash/clean my hands after touching birds"=sec2_q7___9.factor,
                                      "I wash/clean my hands before and after handling raw poultry meat"=sec2_q7___10.factor,
                                      "I make sure raw poultry meat is fully cooked before eating"=sec2_q7___11.factor,
                                      "I do not eat undercooked or raw poultry meat"=sec2_q7___12.factor,
                                      "I make sure eggs are thoroughly cooked"=sec2_q7___13.factor,
                                      "I do not eat raw eggs"=sec2_q7___14.factor,
                                      "I do not remember"=sec2_q7___15.factor,
                                      "None"=sec2_q7___16.factor,
                                      "Other"=sec2_q7___17.factor)

#Reshape data to long
biosecurity <- biosecurity %>% gather(key="Biosecurity measure", value="Checked/unchecked", -record_id, -sec2_q3.factor)

#Frequency of biosecurity measures reported 
biosecurity %>%
  dplyr::count(`Checked/unchecked`, `Biosecurity measure`) %>%
  group_by(`Biosecurity measure`) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(`Checked/unchecked` == "Checked") %>%
  ggplot(aes(x = reorder(`Biosecurity measure`, p), y = p, group = `Biosecurity measure`, fill=`Biosecurity measure`,
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5, size=6) + 
  xlab("Biosecurity measure") + 
  ylab("Proportion") + 
  theme_minimal_grid()+
  theme(axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=20, face="bold")) + 
  guides(fill="none") + 
  coord_flip()

#create a new variable to denote if at least one biosecurity measure was used and then sum to count how many people reported using at least one 
#assume that if all measures are 'unchecked' then the question was skipped i.e. missing data 



#Risk of AIV

#Risk to personal health
#Risk to health of people working with birds 
#Risk to health of birds 
#Risk to business/livelihood 


#Extract risk statements
riskq <- data %>% select(record_id, 
                         sec4_q3.factor, sec4_q4.factor, sec4_q5.factor, sec4_q6.factor)

#Remove observations with missing data (n=149)
#riskq <- riskq %>% filter(!is.na(sec4_q3.factor), !is.na(sec4_q4.factor),
#                          !is.na(sec4_q5.factor), !is.na(sec4_q6.factor))

#Rename columns to the statements
riskq <- riskq %>% dplyr::rename(
  "Avian influenza poses a risk to my physical health"=sec4_q3.factor,
  "Avian influenza poses a risk to people who work with birds"=sec4_q4.factor,
  "Avian influenza poses a risk to the health of my birds"=sec4_q5.factor,
  "Avian influenza poses a risk to my business/livelihood"=sec4_q6.factor)

riskq_long <- riskq

#Assume if NA for all questions then skipped AIV awareness 
falseifNA <- function(x){
  ifelse(is.na(x), FALSE, x)
}


riskq <- riskq %>% 
  mutate(riskq_q_complete = falseifNA(riskq$`Avian influenza poses a risk to my physical health`)) %>%
  mutate(riskq_q_complete2 = ifelse(riskq_q_complete == 0, "incomplete", "complete"))
                                      

#count responses 
table(riskq$riskq_q_complete2)
#198/225 responded to the AIV awareness q; 27 missing


#Extract risk statements
riskq_long <- data %>% select(record_id, sec1_q2.factor, occupation_clean, sec2_q3.factor, sec2_q4.factor,
                         sec4_q3.factor, sec4_q4.factor, sec4_q5.factor, sec4_q6.factor, sec1_q12.factor)

#Remove observations with missing data (n=149)
riskq_long <- riskq_long %>% filter(!is.na(sec4_q3.factor), !is.na(sec4_q4.factor),
                          !is.na(sec4_q5.factor), !is.na(sec4_q6.factor))

#Rename columns to the statements
riskq_long <- riskq_long %>% dplyr::rename(
  "Avian influenza poses a risk to my physical health"=sec4_q3.factor,
  "Avian influenza poses a risk to people who work with birds"=sec4_q4.factor,
  "Avian influenza poses a risk to the health of my birds"=sec4_q5.factor,
  "Avian influenza poses a risk to my business/livelihood"=sec4_q6.factor)

#Reshape to make data long
riskq_long <- riskq_long %>% gather(key="Question_num", value="Answer", -record_id, -sec2_q3.factor,
                          -sec2_q4.factor, -sec1_q2.factor, -occupation_clean, -sec1_q12.factor)

#Make answer a factor variable
riskq_long$Answer <- as.factor(riskq_long$Answer)

#Re-order factor answer
riskq_long$Answer <- factor(riskq_long$Answer, levels=c("I do not know",
                                              "No risk at all", 
                                              "Low risk",
                                              "Medium risk",
                                              "High risk",
                                              "Very high risk"))




#each risk statement 

riskq_long %>%
  dplyr::count(Answer, Question_num) %>% #answer, gender, statement
  group_by(Question_num) %>% #answer and gender
  mutate(p=n/sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(Question_num == "Avian influenza poses a risk to my physical health") %>%
  ggplot(aes(x = Answer, y = p, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to my physical health'")+
  guides(fill=guide_legend(title="Type of occupation"))+
  theme_minimal_grid()+
  theme(legend.position = "none")
#113/198 - low risk 
#39/198 - medium risk


riskq_long %>%
  dplyr::count(Answer, Question_num) %>% #answer, gender, statement
  group_by(Question_num) %>% #answer and gender
  mutate(p=n/sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(Question_num == "Avian influenza poses a risk to people who work with birds") %>%
  ggplot(aes(x = Answer, y = p, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to people who work with birds'")+
  guides(fill=guide_legend(title="Type of occupation"))+
  theme_minimal_grid()+
  theme(legend.position = "none")
#96/198 - low risk 
#61/198 - medium risk 


riskq_long %>%
  dplyr::count(Answer, Question_num) %>% #answer, gender, statement
  group_by(Question_num) %>% #answer and gender
  mutate(p=n/sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(Question_num == "Avian influenza poses a risk to the health of my birds") %>%
  ggplot(aes(x = Answer, y = p, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to the health of my birds'")+
  guides(fill=guide_legend(title="Type of occupation"))+
  theme_minimal_grid()+
  theme(legend.position = "none")
#36/198 - low risk 
#46/198 - medium risk 
#53/198 - high risk 
#49/198 - very high risk 



riskq_long %>%
  dplyr::count(Answer, Question_num) %>% #answer, gender, statement
  group_by(Question_num) %>% #answer and gender
  mutate(p=n/sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(Question_num == "Avian influenza poses a risk to my business/livelihood") %>%
  ggplot(aes(x = Answer, y = p, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to my business/livelihood'")+
  guides(fill=guide_legend(title="Type of occupation"))+
  theme_minimal_grid()+
  theme(legend.position = "none")
#45/198 - high 
#60/198 - very high


#For 'Figure 2. Spatial distribution of respondents by home and work postcode.', see separate script 'datanotemapsWO_180225.R'. 
#However, note postcode not released within open access dataset to preserve respondent anonymity, so maps cannot be re-created. 
