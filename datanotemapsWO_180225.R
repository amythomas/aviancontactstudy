# Code to accompany 'Data note on the Avian Contact Study: a questionnaire resource for avian influenza public health planning', Thomas et al 2025, Wellcome Open Research
# Code to produce 'Figure 2. Spatial distribution of respondents by home and work postcode.'
# Authors: Harry Whitlow, Amy Thomas
# Date of creation: 18/02/25
# R vers: 4.4.2


#Packages
library(sf)
library(tidyverse)
library(janitor)
library(ggspatial)
library(ggrepel)
library(patchwork)

#import data 
#note - this dataset is not available open access to preserve respondent anonymity
#code is given to demonstrate methodology for map generation 
data=readRDS('ais_data080824v2.RDS')


###########################################
#------ Extracting postcode data ---------#
###########################################

#Extract important columns
data <- data %>% select(record_id, avian_influenza_social_contact_survey_timestamp, 
                        sec1_q3, sec1_q4, sec1_q2, occupation_clean, sec2_q3.factor, sec2_q4.factor)

#Replace blanks in postcode variables with NA
data[data==""] <- NA

#row 11 col 3 still not appearing as NA
data[11,3]
data[data=="     "] <- NA

#Select observations with missing postcodes
missing <- data %>% filter(is.na(sec1_q3), is.na(sec1_q4))

#Remove observations that are missing - n=202
data <- data %>% filter(!is.na(sec1_q3), !is.na(sec1_q4))

#Make all postcodes upper case
data$sec1_q3 <- toupper(data$sec1_q3)
data$sec1_q4 <- toupper(data$sec1_q4)

#Function to select first four characters left to right of postcode
left = function (string,char) {
  substr(string,1,char)
}

#Apply to both postcode variables
data$sec1_q3 <- left(data$sec1_q3, 2)
data$sec1_q4 <- left(data$sec1_q4, 2)

#Extract only the letters
data$sec1_q3 <- gsub("[^a-zA-Z]", "", data$sec1_q3)
data$sec1_q4 <- gsub("[^a-zA-Z]", "", data$sec1_q4)

#Create home and work
home <- as.data.frame(table(data$sec1_q3))
work <- as.data.frame(table(data$sec1_q4))

#pcd boundaries (from: https://www.opendoorlogistics.com/data/)
pcd <- st_read("/Volumes/zoonoticTB/AI/aviancontact/data/july2024release/agepostcode/Distribution/Areas.shp")

#Find difference between home/work and pcd
missing_home <- as.data.frame(setdiff(pcd$name, home$Var1))
missing_work <- as.data.frame(setdiff(pcd$name, work$Var1)) 

#Create new frequency column
missing_home <- missing_home %>% rename("name"="setdiff(pcd$name, home$Var1)") %>% mutate(Freq=0)
missing_work <- missing_work %>% rename("name"="setdiff(pcd$name, work$Var1)") %>% mutate(Freq=0)

#Remove "P" from home/work
home <- home %>% filter(!Var1=="P")
work <- work %>% filter(!Var1=="P")

#Rename Var1 to name
home <- rename(home, "name"="Var1")
work <- rename(work, "name"="Var1")

#rbind
home_data <- rbind(home, missing_home)
work_data <- rbind(work, missing_work)

#inner_join
pcd_home <- inner_join(pcd, home_data, by="name")
pcd_work <- inner_join(pcd, work_data, by="name") #excludes three rows which are somehow created in the above - not sure what is going on

### FOR POSTCODE AREA LABELS ## 
#Create centroids
centroid <- st_centroid(pcd_home)

#Create a unique identifer by row
centroid$id <- seq.int(nrow(centroid))

#Extract coordinates
coords <- st_coordinates(centroid)

#col bind them together
centroid <- cbind(centroid, coords)

#check that unique identifiers match row numbers to ensure successful col bind
all(centroid$id == seq.int(nrow(centroid)))

#Plot for home pcd

homemap <- ggplot()+
  geom_sf(data=pcd_home, mapping=aes(fill=Freq))+
  #geom_text_repel(data=centroid, aes(x=X, y=Y, label=name))+
  scale_fill_gradient(high="#ab0202", low="#ffffff", name=str_wrap("Number of respondents", width=10),
                      breaks=c(0,2,4,6,8,10,12,14))+
  labs(title=str_wrap("Home", width=40),
       caption="This reconstructed dataset is copyright © 2015 by OpenDoorLogistics (www.opendoorlogistics.com)\nContains Royal Mail data © Royal Mail copyright and database right 2015\nContains National Statistics data © Crown copyright and database right 2015")+
  theme_void()+
  theme(legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        plot.title=element_text(size=22, face="bold", hjust=0.5))+
  annotation_north_arrow(location="bl", which_north = "true", height=unit(3, "cm"), width=unit(3, "cm"))+
  annotation_scale(location="br", height=unit(1, "cm"), width=unit(3, "cm"), text_cex=1.5)
homemap

#Plot for work pcd

workmap <- ggplot()+
  geom_sf(data=pcd_work, mapping=aes(fill=Freq))+
  #geom_text_repel(data=centroid, aes(x=X, y=Y, label=name))+
  scale_fill_gradient(high="#3E1D78", low="#ffffff", name=str_wrap("Number of respondents", width=10),
                      breaks=c(0,2,4,6,8,10,12,14))+
  labs(title=str_wrap("Work", width=40),
       caption="This reconstructed dataset is copyright © 2015 by OpenDoorLogistics (www.opendoorlogistics.com)\nContains Royal Mail data © Royal Mail copyright and database right 2015\nContains National Statistics data © Crown copyright and database right 2015")+
  theme_void()+
  theme(legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=16),
        legend.text = element_text(size=15),
        plot.title=element_text(size=22, face="bold", hjust=0.5))+
  annotation_north_arrow(location="bl", which_north = "true", height=unit(3, "cm"), width=unit(3, "cm"))+
  annotation_scale(location="br", height=unit(1, "cm"), width=unit(3, "cm"), text_cex=1.5)
workmap

#save plot as multipanel
jpeg(filename="datanotefigures/maps.tiff", height=10, width=9, units="in", res=500)

homemap + workmap + plot_annotation(tag_levels = 'a') + plot_layout(guides = 'collect') + plot_layout(widths = c(0.7, 1))

dev.off()
