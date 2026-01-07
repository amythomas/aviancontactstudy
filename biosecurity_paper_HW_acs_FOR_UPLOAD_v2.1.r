##Avian contact study: biosecurity paper analyses
##Author: HW
##Date of creation: 22/01/24
##Date of last edit: 06/10/25
##Data release: 080824v1

##R version: R version 4.4.2 (2024-10-31 ucrt)

##Data glossary
  #data - raw data frame with all ACS data - n=225
  #biosecurity - biosecurity data from ACS with additional groupings added - n=218
  #biosecurity_non_agg - biosecurity data from ACS without additional groupings to plot all biosecurity measures on one graph - n=218
  #biosecurity_frq - to plot the supplementary figure of biosecurity groups and each biosecurity measure in them - n=218
  #biosecurity_chi - to run test of proportions and chi-squared tests on bird contact and biosecurity uptake - n=217
  #riskq - dataframe of all respondents who provided risk perception and bird contact data (excluding I don't know) - n=191

##Packages
library(tidyverse)
library(patchwork)
library(sjmisc)
library(broom)
library(patchwork)

##Read in data:
data=readRDS("ais_data080824v1.RDS")

#change "[not completed]" entries to NA for data$avian_influenza_social_contact_survey_timestamp
data %>% mutate(survey_timestamp = ifelse(avian_influenza_social_contact_survey_timestamp == "[not completed]", NA,
  avian_influenza_social_contact_survey_timestamp)) -> data

#convert data$avian_influenza_social_contact_survey_timestamp into date 
ymd_hms(data$survey_timestamp,tz=Sys.timezone()) -> data$survey_timestamp
class(data$survey_timestamp)

#convert data$sec5_survey_time_started into date
data$sec5_survey_time_started
ymd_hms(data$sec5_survey_time_started,tz=Sys.timezone()) -> data$sec5_survey_time_started
class(data$sec5_survey_time_started)

#Create bird contact variable
data <- data %>% mutate(bird_contact=case_when(sec2_q3.factor %in% c("1 - 10", "11 - 100", "101 - 1 000")~"Low contact",
  sec2_q3.factor %in% c("1001 - 10 000", "10 001 - 100 000", "More than 100 000")~"High contact",
  is.na(sec2_q3.factor)~"Unknown"))

#Create new risk perception variables for later association analyses
data <- data %>% mutate(phys_health_risk=case_when(
    sec4_q3.factor %in% c("No risk at all", "Low risk", "Medium risk")~"no/low/medium",
    sec4_q3.factor %in% c("High risk", "Very high risk")~"high/veryhigh",
    sec4_q3=="I do not know"~NA),
  bird_health=case_when(
    sec4_q5.factor %in% c("No risk at all", "Low risk", "Medium risk")~"no/low/medium",
    sec4_q5.factor %in% c("High risk", "Very high risk")~"high/veryhigh",
    sec4_q5.factor=="I do not know"~NA),
  buisness_risk=case_when(
    sec4_q6.factor %in% c("No risk at all", "Low risk", "Medium risk")~"no/low/medium",
    sec4_q6.factor %in% c("High risk", "Very high risk")~"high/veryhigh",
    sec4_q6.factor=="I do not know"~NA)
  )

#turn new variables into factors
data$bird_contact <- factor(data$bird_contact, levels=c("Low contact", "High contact", "Unknown"))
data$phys_health_risk <- factor(data$phys_health_risk, levels=c("no/low/medium", "high/veryhigh"))
data$bird_health <- factor(data$bird_health, levels=c("no/low/medium", "high/veryhigh"))
data$business_risk <- factor(data$buisness_risk, levels=c("no/low/medium", "high/veryhigh"))

#Create gender variable
data <- data %>% mutate(gender=sec1_q2.factor)

  #Change class to factor
  data$gender <- as.factor(data$gender)

  #Assign levels
  data$gender <- recode_factor(data$gender, "1"="Female", "2"="Male", "3"="Other", "4"="Prefer not to say")

############################
#---- 1. Descriptives ------
############################

#Respondent_type
table(data$respondent_type, useNA="always")
round(table(data$respondent_type, useNA="always")/sum(table(data$respondent_type, useNA="always"))*100, 2)

#Age
table(data$age_cat_5year, useNA="always")
round(table(data$age_cat_5year, useNA="always")/sum(table(data$age_cat_5year, useNA="always"))*100, 2)

#Gender
table(data$gender, useNA="always")
round(table(data$gender, useNA="always")/sum(table(data$gender, useNA="always"))*100, 2)

#Occuaption
frq(data$occupation_clean, show.na=TRUE) #Raw categories

#Number of birds in contact with
table(data$sec2_q3.factor, useNA="always")
round(table(data$sec2_q3.factor, useNA="always")/sum(table(data$sec2_q3.factor, useNA="always"))*100, 2)

#Bird contact groups
table(data$bird_contact, useNA="always")
round(table(data$bird_contact, useNA="always")/sum(table(data$bird_contact, useNA="always"))*100, 2)

#Bird ownership
table(data$sec2_q4.factor, useNA="always")
round(table(data$sec2_q4.factor, useNA="always")/sum(table(data$sec2_q4.factor, useNA="always"))*100, 2)

#Contact with types of birds
species <- data %>% select(starts_with("sec2_q2") & is.factor,
                                  c(sec2_q2_dombrd_othr, sec2_q2_preybrd_othr, sec2_q2_wildwtrfwl_othr,
                                    sec2_q2_seabrd_othr, sec2_q2_petbrd_othr, sec2_q2_anybrd_othr))

species <- species %>% rename("Chickens"=sec2_q2_dombrd___1.factor,
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
species %>% select(!starts_with("sec2_q2")) %>%
  pivot_longer(cols = 1:27, names_to = "species") -> species_long


#create a higher order group for bird: domestic bird, bird of prey, wild waterfoul, seabird, pet bird, Other 
species_long %>% 
  mutate(birdgroup = case_when(species == "Chickens" | species == "Turkeys" | species == "Ducks" | species == "Geese" |  species == "Guinea fowl" |species == "Quail" ~ "Domestic bird",
                               species == "Owls" | species == "Hawks" | species == "Eagles" | species == "Buzzards" | species == "Kites" | species == "Vultures" ~ "Birds of prey", 
                               species == "Wild ducks" | species == "Wild geese" | species == "Wild swans" | species == "Wild teal" ~ "Wild waterfowl", 
                               species == "Gulls" | species == "Guillemots" | species == "Herons" | species == "Kittiwakes" | species == "Terns" ~ "Seabirds",
                               species == "Budgerigars" | species == "Canaries" | species == "Cockatiels" | species == "Doves" | species == "Finches" | species == "Parrots" ~ "Pet birds")) -> species_long


#reorder bird group
species_long$birdgroup <- factor(species_long$birdgroup, levels=c("Domestic bird",
                                                                                "Birds of prey", 
                                                                                "Wild waterfowl",
                                                                                "Seabirds",
                                                                                "Pet birds"))

#Prop and n of bird species
birdspecies <- species_long %>%
  filter(value=="Checked") %>%
  group_by(species) %>%
  summarise(n=n()) %>%
  mutate(p=round((n/225)*100))

#########################################
#------- 2. Biosecurity measures --------
#########################################

#Extract biosecurity variables
biosecurity <- data %>% select(c(record_id, gender, bird_contact, sec2_q3.factor, sec2_q4.factor, respondent_type, age_cat_5year, 
  sec4_q3.factor, sec4_q4.factor, sec4_q5.factor, sec4_q6.factor, phys_health_risk, bird_health, business_risk),
  starts_with("sec2_q7___") & is.factor)

#------Cleaning------#

#Function to check which rows have all 'unchecked' to identify missing data
unchecked_measures <- function(row) {
  length(unique(as.character(row))) == 1
}

#Mark the data with missing responses as a logical variable
biosecurity <- biosecurity %>%
  rowwise() %>%
  mutate(missing=unchecked_measures(c_across(cols = 15:31)))

# n = 218

#Filter the biosecurity dataframe
biosecurity <- biosecurity %>% filter(missing==FALSE)

#Ungroup the dataframe
biosecurity <- ungroup(biosecurity)

#Create combined biosecurity measures
biosecurity <- biosecurity %>% mutate(
  #create food measure category
  food_measures=case_when(sec2_q7___10.factor=="Checked" | sec2_q7___11.factor=="Checked" | sec2_q7___12.factor=="Checked" |
   sec2_q7___13.factor=="Checked" | sec2_q7___14.factor=="Checked"~"Checked"),
  #create ppe measure category
  ppe_measures=case_when(sec2_q7___1.factor=="Checked" | sec2_q7___2.factor=="Checked" | sec2_q7___3.factor=="Checked" |
   sec2_q7___4.factor=="Checked" |sec2_q7___5.factor=="Checked"~"Checked"),
  #create ppe footwear specific category
  ppe_measures_footwear=case_when(sec2_q7___6.factor=="Checked" | sec2_q7___7.factor=="Checked" |
   sec2_q7___8.factor=="Checked"~"Checked"),
  #create handwashing as logical
  hand_washing=case_when(sec2_q7___9.factor=="Checked"~"Checked"),
  #create no remember as logical
  no_remember=case_when(sec2_q7___15.factor=="Checked"~"Checked", sec2_q7___15.factor=="Unchecked"~"Unchecked"),
  #create none as logical
  none=case_when(sec2_q7___16.factor=="Checked"~"Checked", sec2_q7___16.factor=="Unchecked"~"Unchecked"),
  #create other as logical
  other=case_when(sec2_q7___17.factor=="Checked"~"Checked", sec2_q7___17.factor=="Unchecked"~"Unchecked")
  )

#replace NA with "unchecked"
biosecurity <- biosecurity %>% mutate(across(c(food_measures, ppe_measures, ppe_measures_footwear, hand_washing), 
  ~replace_na(.x, value="Unchecked")))

#make as factors
biosecurity <- biosecurity %>% mutate(across(c(ppe_measures, food_measures, ppe_measures_footwear, hand_washing),
  ~factor(.x, levels=c("Unchecked", "Checked"))))

#Create an non-aggregate dataframe
biosecurity_non_agg <- biosecurity


#########################################################################################
# ---- Create supplementary frequency distribution graph of each biosecurity group -----#
#########################################################################################

  #Susbet out old biosecurity measures to check frequency distribution
  biosecurity_frq <- biosecurity %>% select(c(record_id), starts_with("sec2_q7___") & is.factor)

  #Rename biosecurity_frq variables
  biosecurity_frq <- biosecurity_frq %>% rename("I use face masks"=sec2_q7___1.factor,
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
  biosecurity_frq <- biosecurity_frq %>% gather(key="bm", value="yes/no", -record_id)

  #Change checked and unchecked to yes and no
  biosecurity_frq$`yes/no` <- str_replace(biosecurity_frq$`yes/no`, pattern="Unchecked", replacement="No")
  biosecurity_frq$`yes/no` <- str_replace(biosecurity_frq$`yes/no`, pattern="Checked", replacement="Yes")

  #Add groupings
  biosecurity_frq <- biosecurity_frq %>% mutate(bm_cat=case_when(
    str_detect(bm, c("masks|gloves|goggles|clothing|garments")) ~ "ppe",
    str_detect(bm, c("boots|footwear|covers")) ~ "ppe_foot",
    str_detect(bm, c("raw|eggs")) ~ "food"))

  #Subset them out to plot separately for supplementary figure
  biosecurity_frq_ppe <- biosecurity_frq %>% filter(bm_cat=="ppe")
  biosecurity_frq_foot <- biosecurity_frq %>% filter(bm_cat=="ppe_foot")
  biosecurity_frq_food <- biosecurity_frq %>% filter(bm_cat=="food")

  #ppe
  p1 <- ggplot(biosecurity_frq_ppe)+
  geom_bar(aes(x=`yes/no`), fill="darkgreen")+
  facet_wrap(~ str_wrap(bm, width=25), ncol=5)+
  theme_bw()+
  labs(x="Yes/no", y="Number of respondents", title="PPE measures for the face or body")+
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.6)), 
    strip.text.x=element_text(size=rel(1.4)),
    axis.text.x=element_text(size=rel(1.6)),
    axis.text.y=element_text(size=rel(1.6)),
    axis.title=element_text(size=rel(1.6)),
    plot.tag=element_text(size=rel(1.6)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5))

  #footwear
  p2 <- ggplot(biosecurity_frq_foot)+
  geom_bar(aes(x=`yes/no`), fill="darkred")+
  facet_wrap(~ str_wrap(bm, width=25), ncol=5)+
  theme_bw()+
  labs(x="Yes/no", y="Number of respondents", title="Footwear related PPE measures")+
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.6)), 
    strip.text.x=element_text(size=rel(1.4)),
    axis.text.x=element_text(size=rel(1.6)),
    axis.text.y=element_text(size=rel(1.6)),
    axis.title=element_text(size=rel(1.6)),
    plot.tag=element_text(size=rel(1.6)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5))

  #food
  p3 <- ggplot(biosecurity_frq_food)+
  geom_bar(aes(x=`yes/no`), fill="darkblue")+
  facet_wrap(~ str_wrap(bm, width=25), ncol=5)+
  theme_bw()+
  labs(x="Yes/no", y="Number of respondents", title="Food safety measures")+
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.6)), 
    strip.text.x=element_text(size=rel(1.4)),
    axis.text.x=element_text(size=rel(1.6)),
    axis.text.y=element_text(size=rel(1.6)),
    axis.title=element_text(size=rel(1.6)),
    plot.tag=element_text(size=rel(1.6)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5))

  #Combined and export
  pc <- (p1|p2|p3) + plot_layout(nrow=3)+plot_annotation(tag_levels='a')

  #Export and save
  ggsave(filename="supplementary_figure_1_biosecurity_distribution.jpg", 
    plot=pc, width=12, height=14, units="in")

#Distinct number of respondents who used at least one measure in each group (Table S1)
distinct_ppe <- biosecurity_frq_ppe %>% 
    filter(`yes/no`=="Yes") %>% 
    distinct(record_id, .keep_all=TRUE) %>%
    group_by(bm_cat) %>%
    summarise(freq=table(`yes/no`))

distinct_food <- biosecurity_frq_food %>% 
    filter(`yes/no`=="Yes") %>% 
    distinct(record_id, .keep_all=TRUE)%>%
    group_by(bm_cat) %>%
    summarise(freq=table(`yes/no`))

distinct_foot <- biosecurity_frq_foot %>% 
    filter(`yes/no`=="Yes") %>% 
    distinct(record_id, .keep_all=TRUE)%>%
    group_by(bm_cat) %>%
    summarise(freq=table(`yes/no`))

distinct_bm_use <- bind_rows(distinct_ppe, distinct_food, distinct_foot)

#################################################################################

#Drop original biosecurity measures for analyses
biosecurity <- biosecurity %>% select(!c(missing, starts_with("sec2_q7___")))

#Create risk_bm dataframe for later chi-squared tests
risk_bm <- biosecurity %>% select(record_id, sec4_q3.factor, sec4_q4.factor, sec4_q5.factor, sec4_q6.factor, bird_contact,
  food_measures, ppe_measures, ppe_measures_footwear, hand_washing, none, other, phys_health_risk, bird_health, business_risk)

################################
#---- Test of proportions -----#
################################

#remove unknown bird contact response
biosecurity_chi <- biosecurity %>% filter(!bird_contact=="Unknown")

#Test of proportions: ppe
prop_ppe <- table(biosecurity_chi$bird_contact, biosecurity_chi$ppe_measures, useNA="always")
prop.test(x=c(prop_ppe[2,2], prop_ppe[1,2]), n=c(129, 88), conf.level = 0.95, correct = FALSE)
sqrt(34.58)

#Test of proportions: ppe footwear
prop_footwear <- prop_footwear <- table(biosecurity_chi$bird_contact, biosecurity_chi$ppe_measures_footwear, useNA="always")
prop.test(x=c(prop_footwear[2,2], prop_footwear[1,2]), n=c(129, 88), conf.level=0.95, correct = FALSE)
sqrt(42.134)

#Test of proportions: food
prop_food <- table(biosecurity_chi$bird_contact, biosecurity_chi$food_measures, useNA="always")
prop.test(x=c(prop_food[2,2], prop_food[1,2]), n=c(129, 88), conf.level=0.95, correct = FALSE)
sqrt(0.88761)

#Test of proportions: hand washing
prop_handwashing <- table(biosecurity_chi$bird_contact, biosecurity_chi$hand_washing, useNA="always")
prop.test(x=c(prop_handwashing[2,2], prop_handwashing[1,2]), n=c(129, 88), conf.level=0.95, correct = FALSE)
sqrt(1.9883)

##################################################################
#---- Chi square tests (bird contact and biosecurity uptake)-----#
##################################################################

#food measures
food <- chisq.test(biosecurity_chi$food_measures, biosecurity_chi$bird_contact, correct=FALSE)
  food$observed
  food$expected
print(food)

#ppe measures
ppe <- chisq.test(biosecurity_chi$ppe_measures, biosecurity_chi$bird_contact)
  ppe$observed
  ppe$expected
print(ppe)

#ppe footwear measures
ppe_footwear <- chisq.test(biosecurity_chi$ppe_measures_footwear, biosecurity_chi$bird_contact)
  ppe_footwear$observed #less than 5 counts in high contact unchecked
fisher.test(table(biosecurity_chi$ppe_measures_footwear, biosecurity_chi$bird_contact))

#hand washing
hand_washing <- chisq.test(biosecurity_chi$hand_washing, biosecurity_chi$bird_contact, correct=FALSE)
  hand_washing$observed
  hand_washing$expected
print(hand_washing)

#use of no measures
none <- chisq.test(biosecurity_chi$none, biosecurity_chi$bird_contact)
  none$observed
fisher.test(table(biosecurity_chi$none, biosecurity_chi$bird_contact))


###############################################
#---- Transforming data frames for plots -----#
###############################################

#Reshape aggregated data to long for plot
biosecurity <- biosecurity %>% gather(key="Biosecurity group", value="uptake", -record_id, -gender, -bird_contact, -sec2_q3.factor,
  -sec2_q4.factor, -sec4_q3.factor, -sec4_q4.factor, -sec4_q5.factor, -sec4_q6.factor, -respondent_type, -age_cat_5year, -phys_health_risk, -bird_health,
  -business_risk)

    #Rename measures in biosecurity_non_agg
    biosecurity_non_agg <- biosecurity_non_agg %>% rename("I use face masks"=sec2_q7___1.factor,
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

#Reshape non-aggregated data to long for plot
biosecurity_non_agg <- biosecurity_non_agg %>% gather(key="Biosecurity measure", value="uptake", -record_id, -gender, -bird_contact, -sec2_q3.factor,
  -sec2_q4.factor, -sec4_q3.factor, -sec4_q4.factor, -sec4_q5.factor, -sec4_q6.factor, -respondent_type, -age_cat_5year, -ppe_measures, -ppe_measures_footwear,
  -food_measures, -hand_washing, -no_remember, -missing, -none, -other, -phys_health_risk, -bird_health, -business_risk)

#Add groupings
biosecurity_non_agg <- biosecurity_non_agg %>% mutate(bm_cat=case_when(
  str_detect(`Biosecurity measure`, c("masks|gloves|goggles|clothing|garments")) ~ "PPE face and body",
  str_detect(`Biosecurity measure`, c("boots|footwear|covers")) ~ "Footwear PPE",
  str_detect(`Biosecurity measure`, c("raw|eggs")) ~ "Food safety measure",
  str_detect(`Biosecurity measure`, "Other") ~ "Other",
  str_detect(`Biosecurity measure`, "wash/clean") ~ "Hand-washing",
  str_detect(`Biosecurity measure`, "None") ~ "None",
  str_detect(`Biosecurity measure`, "remember") ~ "Don't remember"))

#############################################################################
#---- #Bar plot with prop test for biosecurity measures without grouping----#
#############################################################################

bm_group_col <- c("PPE face and body"="#3BA38E", "Footwear PPE"="#FCB54E", "Food safety measure"="#4EB7FC",
  "Other"="#C0BEA8", "Hand-washing"="#FC5A4E", "None"="#ffc0cb", "Don't remember"="black")

prop_labels <- c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")

jpeg(filename="fig1_biosecurity_measures_used.jpg", width=14, height=15, units="in", res=900)

biosecurity_non_agg %>%
  dplyr::count(uptake, `Biosecurity measure`, bm_cat) %>%
  group_by(`Biosecurity measure`) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(uptake == "Checked") %>%
  ggplot(aes(x = reorder(`Biosecurity measure`, p), y = p, group = `Biosecurity measure`,
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge", aes(fill=bm_cat))+
  geom_line(aes(x=`Biosecurity measure`, y=0, group=`Biosecurity measure`, color="95% CI"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5, size=6) + 
  scale_y_continuous(breaks=seq(0,1.0,0.10), limits=c(0,1.0), labels=prop_labels)+
  scale_color_manual(name="", values="black")+
  scale_fill_manual(name=str_wrap("Biosecurity group", width=13), values=bm_group_col)+
  xlab("Biosecurity measure") + 
  ylab("Proportion") + 
  theme_bw()+
  theme(axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=20, face="bold"),
        legend.position="bottom",
        legend.key.size=unit(3, "cm"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16, face="bold"),
        legend.key.height = unit(1, "cm"),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.ticks=element_line(linewidth=1.5)) + 
  guides(fill=guide_legend(nrow=3)) + 
  coord_flip()

dev.off()

#########################################################################################
#---- #Bar plot with prop test for biosecurity measure groups and bird contact level----#
#########################################################################################

#Change names for plot
biosecurity[biosecurity=="ppe_measures"] <- "Use of at least one PPE measure for the face or body"
biosecurity[biosecurity=="ppe_measures_footwear"] <- "Use of at least one footwear related PPE measure"
biosecurity[biosecurity=="food_measures"] <- "Use of at least one food safety measure"
biosecurity[biosecurity=="hand_washing"]  <- "Hand washing/cleaning after touching birds"
biosecurity[biosecurity=="no_remember"] <- "Respondent does not remember"
biosecurity[biosecurity=="none"] <- "None"
biosecurity[biosecurity=="other"] <- "Other"


#Plot
jpeg(filename="fig2_plots_july_biosecuritygroups.jpg", width=12, height=12, units="in", res=900)

biosecurity %>%
  dplyr::count(uptake, `Biosecurity group`, bird_contact) %>%
  group_by(`Biosecurity group`, bird_contact) %>%
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>% 
  filter(uptake == "Checked", !bird_contact=="Unknown") %>%
  ggplot(aes(x = reorder(`Biosecurity group`, p), y = p, group = bird_contact, fill=`bird_contact`,
             ymax = lower, ymin = upper)) +
  geom_col(stat = "identity", position = "dodge")+
  geom_line(aes(x=`Biosecurity group`, y=0, group=`Biosecurity group`, color="95% CI"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  scale_y_continuous(breaks=seq(0,1.0,0.10), limits=c(0,1.0), labels=prop_labels)+
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), hjust = 0.5, size=6) +
  scale_fill_manual(name="", values=c("Low contact"="#36A38D", "High contact"="#E64308", "Unknown"="#F9EA6E"),
    labels=c("Low contact"="Low contact (1-1000)", "High contact"="High contact (1001+)", "Missing"="Missing"))+
  scale_color_manual(name="", values="black")+
  xlab("Biosecurity measure") + 
  ylab("Proportion") + 
  theme_bw()+
  theme(axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=20, face="bold"),
        legend.position="bottom",
        legend.key.size=unit(3, "cm"),
        legend.text=element_text(size=16),
        legend.key.height = unit(1, "cm"),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.ticks=element_line(linewidth=1.5)) +
  coord_flip()

dev.off()


###########################
#---- Risk perception ----#
###########################

#Extract risk statements
riskq <- data %>% select(record_id, respondent_type, sec1_q2.factor, occupation_clean, sec2_q3.factor, sec2_q4.factor, bird_contact,
                         sec4_q3.factor, sec4_q4.factor, sec4_q5.factor, sec4_q6.factor, sec1_q12.factor)

#Remove observations with missing data (n=149)
riskq <- riskq %>% filter(!is.na(sec4_q3.factor), !is.na(sec4_q4.factor),
                          !is.na(sec4_q5.factor), !is.na(sec4_q6.factor))

# n = 193

  #make risk questions as factors
  riskq$sec4_q3.factor <- as.factor(riskq$sec4_q3.factor)
  riskq$sec4_q3.factor <- factor(riskq$sec4_q3.factor, levels=c("I do not know",
                                              "No risk at all", 
                                              "Low risk",
                                              "Medium risk",
                                              "High risk",
                                              "Very high risk"))

  #make risk questions as factors
  riskq$sec4_q5.factor <- as.factor(riskq$sec4_q5.factor)
  riskq$sec4_q5.factor <- factor(riskq$sec4_q5.factor, levels=c("I do not know",
                                              "No risk at all", 
                                              "Low risk",
                                              "Medium risk",
                                              "High risk",
                                              "Very high risk"))

  ##Supplementary figures
  s1a <- ggplot(riskq)+
    geom_bar(aes(x=sec4_q3.factor), fill="#36A38D")+
    labs(x="Risk response", y="Frequency", title="Risk to respondents' health")+
    scale_y_continuous(breaks=seq(0,120,10), limits=c(0,120))+
    theme_bw()+
    theme(plot.title=element_text(size=rel(1.6), face="bold", hjust=0.5),
      axis.text.x=element_text(size=rel(1.5)),
      axis.text.y=element_text(size=rel(1.5)),
      axis.title=element_text(size=rel(1.6)),
      plot.tag=element_text(size=rel(1.6)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5)
)

  s2a <- ggplot(riskq)+
    geom_bar(aes(x=sec4_q5.factor), fill="#36A38D")+
    labs(x="Risk response", y="Frequency", title="Risk to birds")+
    scale_y_continuous(breaks=seq(0,60,10), limits=c(0,60))+
    theme_bw()+
    theme(plot.title=element_text(size=rel(1.6), face="bold", hjust=0.5),
      axis.text.x=element_text(size=rel(1.5)),
      axis.text.y=element_text(size=rel(1.5)),
      axis.title=element_text(size=rel(1.6)),
      plot.tag=element_text(size=rel(1.6)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5)
)

  s <- s1a / s2a+
    plot_annotation(tag_levels='a')

  ggsave("supplementary_figure_2_risk_frequencies.jpg", s, height=12, width=10, units=c("in"), dpi=900)

#Rename columns to the statements
riskq <- riskq %>% dplyr::rename(
  "Avian influenza poses a risk to my physical health"=sec4_q3.factor,
  "Avian influenza poses a risk to people who work with birds"=sec4_q4.factor,
  "Avian influenza poses a risk to the health of my birds"=sec4_q5.factor,
  "Avian influenza poses a risk to my business/livelihood"=sec4_q6.factor)

#Reshape to make data long
riskq <- riskq %>% gather(key="Question_num", value="Answer", -record_id, -sec2_q3.factor,
                          -sec2_q4.factor, -bird_contact, -sec1_q2.factor, -occupation_clean, -sec1_q12.factor, -respondent_type)

#Make answer a factor variable
riskq$Answer <- as.factor(riskq$Answer)

#Re-order factor answer
riskq$Answer <- factor(riskq$Answer, levels=c("I do not know",
                                              "No risk at all", 
                                              "Low risk",
                                              "Medium risk",
                                              "High risk",
                                              "Very high risk"))

#Plot of risk statement 1 with prop test confidence intervals
fig3a <- riskq %>%
  dplyr::count(Answer, Question_num, bird_contact) %>% #answer, gender, statement
  group_by(Question_num, bird_contact) %>% #answer and gender
  mutate(p=n/sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1])) %>%
  filter(Question_num == "Avian influenza poses a risk to my physical health", !bird_contact=="Unknown") %>%
  ggplot(aes(x = Answer, y = p, group = bird_contact, fill = bird_contact, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5, size=rel(5)) +
  scale_y_continuous(breaks=seq(0,0.8,0.1), limits=c(0,0.8), labels=c("0%", "10%", "20%","30%","40%","50%","60%","70%","80%"))+
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to my physical health'")+
  guides(fill=guide_legend(title="Bird contact"))+
  theme_bw()+
  theme(legend.position = "none",
    axis.title=element_text(size=rel(1.5)),
    plot.title=element_text(size=rel(1.6), hjust=0.5, face="bold"),
    axis.text.x=element_text(size=rel(1.5)),
    axis.text.y=element_text(size=rel(1.5)),
    plot.tag=element_text(size=rel(1.6)),
    legend.title=element_text(size=rel(1.4)),
    legend.text=element_text(size=rel(1.3)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5))+
  scale_fill_manual(values=c("#36A38D","#E64308", "#F9EA6E"))

#Plot of risk statement 3 with prop test confidence intervals
fig3b <-  riskq %>%
  count(Answer, Question_num, bird_contact) %>% #answer, gender, statement
  group_by(Question_num, bird_contact) %>% #answer and gender
  mutate(
    p=n/sum(n),
    lower = lapply(n, prop.test, n = sum(n)), 
    upper = sapply(lower, function(x) x$conf.int[2]), 
    lower = sapply(lower, function(x) x$conf.int[1])) %>%  
  filter(Question_num == "Avian influenza poses a risk to the health of my birds", !bird_contact=="Unknown") %>%
  ggplot(aes(x = Answer, y = p, group = bird_contact, fill = bird_contact, 
             ymax = lower, ymin = upper)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.9) +
  #geom_line(aes(x=Answer, y=0, group=bird_contact, color=bird_contact))+
  geom_errorbar(position = position_dodge(preserve = "single", width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = 0.5, size=rel(5)) +
  scale_y_continuous(breaks=seq(0,0.5,0.1), limits=c(0,0.5), labels=c("0%", "10%", "20%","30%","40%","50%"))+
  xlab("Risk perception answer") + 
  ylab("Proportion") + 
  ggtitle("'Avian influenza poses a risk to the health of my birds'")+
  guides(fill=guide_legend(title="Bird contact"))+
  theme_bw()+
  theme(legend.position = "bottom",
    axis.title=element_text(size=rel(1.5)),
    plot.title=element_text(size=rel(1.6), hjust=0.5, face="bold"),
    axis.text.x=element_text(size=rel(1.5)),
    axis.text.y=element_text(size=rel(1.5)),
    plot.tag=element_text(size=rel(1.6)),
    legend.title=element_text(size=rel(1.4)),
    legend.text=element_text(size=rel(1.3)),
    axis.ticks.length  = unit(0.2, "cm"),
    axis.ticks=element_line(linewidth=1.5))+
  scale_fill_manual(values=c("#36A38D","#E64308", "#F9EA6E"))#+
  #scale_color_manual(name="", label="95% CI", values="black")

#Combine figures and export for two questions (human health, bird health)
riskfig <- fig3a / fig3b+
  plot_annotation(tag_levels='a')
riskfig
ggsave("fig3_a_b_combined_risk_statements_NEW.jpg", device="jpeg", riskfig, height=10, width=9, units=c("in"), dpi=600)

################################################
#---- Risk perception significance testing ----#
################################################

#Risk to physical health
phys_health <- riskq %>% filter(Question_num=="Avian influenza poses a risk to my physical health")
phys_health <- table(phys_health$bird_contact, phys_health$Answer)

  prop.test(x=c(10, 10), n=c(116, 75), conf.level = 0.95) #no risk at all
  prop.test(x=c(59, 52), n=c(116, 75), conf.level = 0.95) #low risk*
    sqrt(5.6483)
  prop.test(x=c(29, 10), n=c(116, 75), conf.level = 0.95) #medium risk*
    sqrt(3.6639)
  prop.test(x=c(7, 2), n=c(116, 75), conf.level = 0.95) #high
  prop.test(x=c(6, 0), n=c(116, 75), conf.level = 0.95) #very high

#Risk to bird health
bird_health <- riskq %>% filter(Question_num=="Avian influenza poses a risk to the health of my birds")
bird_health <- table(bird_health$bird_contact, bird_health$Answer)

  prop.test(x=c(0,2), n=c(116, 75), conf.level = 0.95) #very low
  prop.test(x=c(11, 24), n=c(116, 75), conf.level = 0.95) #low*
    sqrt(13.963)
  prop.test(x=c(30, 16), n=c(116, 75), conf.level = 0.95) #medium
  prop.test(x=c(32, 21), n=c(116, 75), conf.level = 0.95) #high
  prop.test(x=c(40, 8), n=c(116, 75), conf.level = 0.95) #very high*
    sqrt(12.495)


#####################################################################
#---- Chi square tests (risk perception and biosecurity uptake)-----#
#####################################################################

#Remove na in risk_bm dataframe
risk_bm <- risk_bm %>% filter(!is.na(sec4_q3.factor), !is.na(sec4_q4.factor),
                          !is.na(sec4_q5.factor), !is.na(sec4_q6.factor), !is.na(bird_contact))

#food measures
    food_risk_phys <- chisq.test(risk_bm$food_measures, risk_bm$phys_health_risk) #physical health risk
    food_risk_phys$observed
    food_risk_phys$expected
    print(food_risk_phys)

      fisher.test(table(risk_bm$food_measures, risk_bm$phys_health_risk), alternative="two.sided") #fishers test due to cell count of 4

    food_risk_bird <- chisq.test(risk_bm$food_measures, risk_bm$bird_health) #risk to bird health
    food_risk_bird$observed
    food_risk_bird$expected
    print(food_risk_bird)

#ppe measures
    ppe_risk_phys <- chisq.test(risk_bm$ppe_measures, risk_bm$phys_health_risk) #physical health risk
    ppe_risk_phys$observed
    ppe_risk_phys$expected
    print(ppe_risk_phys)

      fisher.test(table(risk_bm$ppe_measures, risk_bm$phys_health_risk), alternative="two.sided") #fishers test due to cell count of 5

    ppe_risk_bird <- chisq.test(risk_bm$ppe_measures, risk_bm$bird_health) #risk to bird health
    ppe_risk_bird$observed
    ppe_risk_bird$expected
    print(ppe_risk_bird)

#ppe footwear measures
    ppe_footwear_phys <- chisq.test(risk_bm$ppe_measures_footwear, risk_bm$phys_health_risk) #risk to physical health
    ppe_footwear_phys$observed
    ppe_footwear_phys$expected
    print(ppe_footwear_phys)

      fisher.test(table(risk_bm$ppe_measures_footwear, risk_bm$phys_health_risk), alternative="two.sided")

    ppe_footwear_bird <- chisq.test(risk_bm$ppe_measures_footwear, risk_bm$bird_health) #risk to bird health
    ppe_footwear_bird$observed
    ppe_footwear_bird$expected
    print(ppe_footwear_bird)

#hand washing
    hand_washing_phys <- chisq.test(risk_bm$hand_washing, risk_bm$phys_health_risk) #risk to physical health
    hand_washing_phys$observed
    hand_washing_phys$expected
    print(hand_washing_phys)

      fisher.test(table(risk_bm$hand_washing, risk_bm$phys_health_risk), alternative="two.sided")

    hand_washing_bird <- chisq.test(risk_bm$hand_washing, risk_bm$bird_health) #risk to bird health
    hand_washing_bird$observed
    hand_washing_bird$expected
    print(hand_washing_bird)

#use of no measures
    none_risk_phys <- chisq.test(risk_bm$none, risk_bm$phys_health_risk) #risk to physical health
    none_risk_phys$observed
    none_risk_phys$expected
    print(none_risk_phys)

      fisher.test(table(risk_bm$none, risk_bm$phys_health_risk), alternative="two.sided")

    none_risk_bird <- chisq.test(risk_bm$none, risk_bm$bird_health) #risk to bird health
    none_risk_bird$observed
    none_risk_bird$expected
    print(none_risk_bird)

      fisher.test(table(risk_bm$none, risk_bm$bird_health), alternative="two.sided")

#END