#ABOUT DATA
#This mortality dataset is a record of every death in the country for the year 2014, which includes detailed information about 
#causes of death and the demographic background of the deceased.

#loading of libraries
library(readr)
library(plyr)
library(Hmisc)
library(dplyr)
library(GGally)
library(readr)
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)

#loading data
deathdb <- read.csv("c:/users/alex/desktop/r/exercise data/death in US/DeathRecords.csv" , header = TRUE)

#basic outline
str(deathdb)
describe(deathdb)
summary(deathdb)
head(deathdb)
tail(crimedb)

#cleaning and preparing data

#removing duplicated columns
deathdb <- deathdb %>% select(-Education1989Revision, -EducationReportingFlag, -AgeSubstitutionFlag,-AgeRecode52,-AgeRecode27,-InfantAgeRecode22,
                              -Icd10Code,-CauseRecode358,-CauseRecode39,-CauseRecode113,-NumberOfEntityAxisConditions,
                              -NumberOfRecordAxisConditions,-RaceImputationFlag,-RaceRecode3,-HispanicOrigin,-BrigadeRaceFlag)

#plots
#Month of Death
ggplot(deathdb, aes(deathdb$MonthOfDeath)) +
  geom_bar( aes(fill = as.factor(deathdb$MonthOfDeath))) +
  scale_fill_discrete(name="Month",
                      labels=c("January","February","March","April","May","June","July","August","September",
                               "October","November","December")) + theme(axis.text.x=element_blank(),
                                                                            axis.ticks.x=element_blank()) +
  labs(x = "Month" , y= "Number of Deaths" , title = "Deaths Per Month") 

#Day of the Week of Death
ggplot(na.omit(deathdb), aes(deathdb$DayOfWeekOfDeath)) + geom_bar( aes(fill = as.factor(deathdb$DayOfWeekOfDeath))) +
  scale_fill_discrete(name="Days of the Week",
                      labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) + theme(axis.text.x=element_blank(),
                                                                         axis.ticks.x=element_blank()) +
  labs(x = "Day" , y= "Number of Deaths" , title = "Deaths Per Day of the Week") + ylim(0, 450000)
#Sex
ggplot(deathdb, aes(deathdb$Sex)) + geom_bar( aes(fill = as.factor(deathdb$Sex))) +
  scale_fill_discrete(name="Gender",
                      labels=c("Female","Male")) + theme(axis.text.x=element_blank(),
                                                      axis.ticks.x=element_blank()) +
  labs(x = "Gender" , y= "Number of Deaths" , title = "Deaths Per Gender") 

#Autopsy. Here we had to reset the levels of factor deathdb$Autopsy, since the values in it were a little messy.
levels(deathdb$Autopsy) <- c(n = "N" , "N" , "U" ,y = "Y", "Y")
ggplot(deathdb, aes(deathdb$Autopsy)) + geom_bar( aes(fill = as.factor(deathdb$Autopsy)))  +
  scale_fill_discrete(name="Autopsy",
                      labels=c("No" , "Yes", "Unknown")) + theme(axis.text.x=element_blank(),
                                                         axis.ticks.x=element_blank()) +
  labs(x= "Autopsy",y= "Frequency" , title = "Autopsy Conducted?")

#Marital Status
ggplot(deathdb, aes(deathdb$MaritalStatus)) + geom_bar(aes(fill = as.factor(deathdb$MaritalStatus)))  +
  scale_fill_discrete(name="Marital Status",
                      labels=c("Divorced", "Married","Never Married/Single","Marital Status Unknown","Widowed")) + 
                                                            theme(axis.text.x=element_blank(),
                                                                 axis.ticks.x=element_blank()) +
  labs(x= "Marital Status",y= "Frequency" , title = "Marital Status")

#Education
ggplot(deathdb, aes(deathdb$Education2003Revision)) + geom_bar(aes(fill = as.factor(deathdb$Education2003Revision))) + 
  scale_fill_discrete(name="Education",
                      labels=c("No Formal Education", "8th Grade or Less","9th - 12th Grade/No Diploma","High School Graduate/GED Completed",
                               "Some College Credit/No Degree","Associate Degree","Bachelor's Degree","Master's Degree",
                               "Doctorate or Professional Degree","Unknown")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Education",y= "Frequency" , title = "Education Status")

#Place of Death
ggplot(deathdb, aes(deathdb$PlaceOfDeathAndDecedentsStatus)) + geom_bar(aes(fill = as.factor(deathdb$PlaceOfDeathAndDecedentsStatus))) + 
  scale_fill_discrete(name="Place of Death and Decendent's Status",
                      labels=c("Hospital,Clinic or Medical Center - Inpatient","Hospital, Clinic or Medical Center
- Outpatient or admitted to Emergency Room","Hospital, Clinic or Medical Center - Dead on Arrival","Decedent's home",
                               "Hospice facility","Nursing home/long term care","Other","Place of death unknown")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Place of Death",y= "Frequency" , title = "Place of Death and Decendent's Status") + ylim(0,1000000)

#Age
ggplot(deathdb, aes(deathdb$AgeRecode12)) + geom_bar(aes(fill = as.factor(deathdb$AgeRecode12))) + 
  scale_fill_discrete(name="Age",
                      labels=c("Under 1 year","1 - 4 years","5 - 14 years","15 - 24 years","25 - 34 years",
                               "35 - 44 years","45 - 54 years","55 - 64 years","65 - 74 years","75 - 84 years",
                               "85 years and over","Age not stated")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Age",y= "Frequency" , title = "Age") + ylim(0,1000000)

#Deaths by Accidental Exposures and Lightning
barplot(deathdb$CauseRecode358[412:416] , names.arg = c("Electrical Current","Radiation",
                                                        "Smoke,Fire and Flames","Heat",
                                                     "Lightning"), main = "Deaths by Accidental Exposures and Lightning", ylim=c(0,500))

#Death by Suicide
barplot(deathdb$CauseRecode358[425:431] , names.arg = c("Drugs","Poisoning/Other",
                                                        "Gases","Hanging","Firearms",
                                                        "Jumping","Unspecified"), main = "Deaths by Suicide", ylim=c(0,500))

