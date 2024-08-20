#Data Note 

#Data Release 080824v1

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
fig2 <- data %>% 
  filter(!age_cat65 == "NA") %>% 
  ggplot(aes(x=age_cat65, y = sec1_q9)) + 
  geom_point(stat = "summary", fun.y = "mean", na.rm = TRUE) + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", na.rm = TRUE, aes(width = 0.2)) + 
  labs(x = "Age", y="Mean health score") + 
  theme_minimal_grid() 
fig2

Figure2 <- fig2 + plot_annotation()
Figure2
#ggsave("datanotefigures/Figure2.tiff", Figure2, scale = 1, width = 8, height = 5, units = c("in"), dpi = 200)


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

fig3a <- data %>%
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
fig3a


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

fig3b <- data %>%
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
fig3b


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
fig3c <- bird_contacttype_long %>%
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
fig3c

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
fig3d <- bird_ownership_long %>%
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
  fig3d

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
  
#Figure 3

Figure3ab <- (fig3a | fig3b) + plot_annotation(tag_levels = 'a')
Figure3ab
#ggsave("datanotefigures/Figure3ab.tiff", Figure3, scale = 1.2, width = 12, units = c("in"), dpi = 200)

Figure3cd <- (fig3c | fig3d) + plot_annotation(tag_levels = 'a')
Figure3cd

Figure3 <- (fig3a | fig3b) / (fig3c | fig3d) + plot_annotation(tag_levels = 'a') + plot_layout(heights = c(1, 2))
Figure3
#ggsave("datanotefigures/Figure3.tiff", Figure3, scale = 1.2, height = 9, width = 8, units = c("in"), dpi = 200)
