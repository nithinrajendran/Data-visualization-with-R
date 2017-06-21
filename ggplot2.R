Data07 <- read.csv("C:/Users/shaba_001/Desktop/Philadelphia Crime Rate tableau/crime.csv", sep = ",", 
                   header = TRUE)
CrimeData <- na.omit(Data07)

install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)

Fraud <- subset(CrimeData, Text_General_Code == "Fraud")
str(Fraud)

Expt_Allother$Month = as.character(Expt_Allother$Month)
Expt_Allother <- cbind(Expt_Allother, year=substr(Expt_Allother$Month, 0, 4))
str(Expt_Allother)

qmplot(Lon, Lat, data = Fraud, maptype = 'toner-lite', 
       color = I('blue'), size = I(1), darken = .3)

map <- qmap('Philadelphia', zoom = 11)
map + geom_point(data = Fraud, aes(x = Lon, y = Lat), 
                 size = 0.09, color="navyblue", size=2, alpha=0.75)

theme_set(theme_bw(16))
PhiladelphiaMap <- qmap("Philadelphia", maptype = 'hybrid', zoom = 11) 
                        
PhiladelphiaMap +
  geom_point(aes(x = Lon, y = Lat), size = 0.09, color = "orange", data = Fraud)


Expt_Allother <- subset(Data07, Text_General_Code != "" & Text_General_Code != "All Other Offenses" )
Expt_Allother <- na.omit(Expt_Allother)
theme_set(theme_bw(16))
Philadelphia_Map <- qmap("philadelphia", zoom = 11, maptype = 'hybrid',
                         legend(x, y=NULL, legend, fill = NULL))
Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Expt_Allother)

Theft <- subset(Expt_Allother, Text_General_Code %in% c("Burglary Non-Residential", "Burglary Residential", "Motor Vehicle Theft",
                                                  "Robbery No Firearm", "Robbery Firearm", "Thefts", "Theft from Vehicle"))


theme_set(theme_bw(16))
Philadelphia_Map <- qmap("philadelphia", zoom = 11, 
                         legend(x, y=NULL, legend, fill = NULL))
Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Theft)


#2015-16 Data#
summary(Theft)
levels(Theft$year)
count(Theft, year)
sum(Theft, Theft$n)
Theft1516 <- subset(Theft, Theft$year %in% c('2015', '2016'))
Theft1516 <- na.omit(Theft1516)
library(dplyr) 
library(plyr)
levels(Expt_Allother$Text_General_Code)
Assault <- subset(Expt_Allother, Text_General_Code %in% 
                    c("Other Assaults","Other Sex Offenses (Not Commercialized)",
                      "Prostitution and Commercialized Vice", "Rape"))
count(Assault, Text_General_Code)
Assault <- na.omit(Assault)
Assault1516 <- subset(Assault, Assault$year %in% c('2015', '2016'))

Fraud <- subset(Expt_Allother, Text_General_Code %in% "Fraud")
Fraud1516 <- subset(Fraud, Fraud$year %in% c('2015', '2016'))
Fraud1516 <- na.omit(Fraud1516)

Firearm <- subset(Expt_Allother, Text_General_Code %in% 
                    c("Robbery Firearm", "Weapon Violations",
                      "Aggravated Assault Firearm"))
Firearm1516 <- subset(Firearm, Firearm$year %in% c('2015', '2016'))
Firearm1516 <- na.omit(Firearm1516)



Recovered <- subset(Expt_Allother, Text_General_Code %in% 
                      "Recovered Stolen Motor Vehicle")
Recovered1516 <- subset(Recovered, Recovered$year %in% c('2015', '2016'))
Recovered1516 <- na.omit(Recovered1516)


theme_set(theme_bw(16))
Philadelphia_Map <- qmap("philadelphia", zoom = 11, maptype = 'hybrid', 
                         legend(x, y=NULL, legend, fill = NULL))
Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Theft1516)

Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Assault1516)

Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Assault1516)

Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat), color = "green",
             data = Fraud1516)

Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Firearm1516)


Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat, colour = Text_General_Code),
             data = Firearm1516)

Philadelphia_Map +
  geom_point(aes(x = Lon, y = Lat), color = "violet",
             data = Recovered1516) 

#--Top offences charts
Expt_Allother0915 <- subset(Expt_Allother, Expt_Allother$year %in% c('2009', '2010',
'2011', '2012', '2013', '2014', '2015'))
Expt_Allother0915 <- na.omit(Expt_Allother0915)

TopOffences0915 <- subset(Expt_Allother0915, Text_General_Code %in% c("Thefts", "Narcotic / Drug Law Violations",
                                                                  "Theft from Vehicle",
                                                                  "Robbery Firearm", "Fraud",
                                                                  "Vandalism/Criminal Mischief",                                                                  "Fraud", "Robbery Firearm",
                                                                  "Motor Vehicle Theft",
                                                                  "Robbery No Firearm",
                                                                  "Gambling Violations",
                                                                  "Offenses Against Family and Children",
                                                                  "Other Assaults",
                                                                  "Other Sex Offenses (Not Commercialized)"))
levels(Expt_Allother0915$year)

Expt_Allother0915 <- subset(Expt_Allother0915, !(Expt_Allother0915$Text_General_Code %in% "Other Assaults"))
Robbery <- subset(Expt_Allother0915, Expt_Allother0915$Text_General_Code %in% "Robbery Fireaem")
ggplot(Expt_Allother0915, aes(Text_General_Code, fill = Text_General_Code)) +
  geom_bar(width=0.6, position = position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text=element_text(size=9), legend.position ="none")

ggplot(TopOffences0915, aes(year, fill = Text_General_Code)) +
  geom_bar(width=0.9)

ggplot(TopOffences0915, aes(Hour, fill = Text_General_Code)) +
  geom_bar(width=0.9)

