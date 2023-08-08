### Install dplyr packages ###
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("date")

### Call dplyr library ###
library(tidyverse)
library(ggplot2)
library(pracma)
library(data.table)
library(date)

### Set working directory ###
#getwd()         
#setwd("/Users/jp/Downloads/Data")
#getwd()   

### Open csv file ###
accidents <- read.csv("accidents.csv", header = TRUE)

### Check data variable formats ###
str(accidents)

### Change to adeguate date and time format and check ###
accidents$Accident.Date  <- as.Date(accidents$Accident.Date, "%d/%m/%Y")
accidents$Time..24hr. <- substr(as.POSIXct(sprintf("%04.0f", accidents$Time..24hr.), format='%H%M'), 12, 16)
str(accidents)

### Check missing values and view them separately ###
sum(is.na(accidents))
accempty <- accidents[!complete.cases(accidents),]
View(accempty)


### Fix possible translation problems in the data ###
locale <- Sys.setlocale(category = "LC_ALL", locale = "C") 

### Fix wrong values in Road Class column ###
accidents$X1st.Road.Class[accidents$X1st.Road.Class %like% "U"] <- "6"
accidents$X1st.Road.Class[accidents$X1st.Road.Class %like% "M62"] <- "1"
accidents$X1st.Road.Class[accidents$X1st.Road.Class %like% "(M)"] <- "2"
accidents$X1st.Road.Class[accidents$X1st.Road.Class %like% "A"] <- "3"
accidents$X1st.Road.Class[accidents$X1st.Road.Class %like%"B"] <- "4"

### Fix wrong values in Road Surface column ###
accidents$Road.Surface[accidents$Road.Surface %like% "Dry"] <- "1"
accidents$Road.Surface[accidents$Road.Surface %like%  "Wet"] <- "2" 
accidents$Road.Surface[accidents$Road.Surface %like% "Snow"] <- "3"
accidents$Road.Surface[accidents$Road.Surface %like% "Ice"] <- "4"

### Make fixed columns from chr to int  and check it ###
accidents$Road.Surface  <- as.integer(accidents$Road.Surface, "%i")
accidents$X1st.Road.Class  <- as.integer(accidents$X1st.Road.Class, "%i")
str(accidents)

### Delete Local.Authority column ###
accidents <- select(accidents, -Local.Authority)
View(accidents)


### Find outliers of Age of Casualties with 3 sigma ### 
accfull <- accidents[complete.cases(accidents),]
ages <- accfull$Age.of.Casualty
### Calculate mean and standard deviation ### 
resm <- mean(ages)
ressd <-sd(ages)
print(resm)
print(ressd)
### Calculate upper and lower band ### 
upbo <- resm + (3*ressd)
lobo <- resm - (3*ressd)
print(upbo)
print(lobo)
### Histogram of Age of Casualties ### 
hist(accidents$Age.of.Casualty, main="Boxplot of Age of Casualties", breaks = sqrt(nrow(accidents)))


### Find outliers of Age of Casualties with Hampel identifier ### 
resmed <- median(ages)
print(resmed)
### Calculate median and standard deviation ### 
deviation <- ages -resmed
deviation <- abs(deviation)
resmad <- median(deviation)
print(resmad)
### Calculate upper and lower band ### 
upbo2 <- resmed + (3*resmad)
lobo2 <- resmed - (3*resmad)
print(upbo2)
print(lobo2)

sum(upbo2<(ages))


### Find outliers of Age of Casualties with boxplots ### 
summary(ages)
resiqr <- IQR(ages)
### Calculate upper and lower band ### 
upbo3 <- 49 + (1.5*resiqr)
lobo3 <- 21 - (1.5*resiqr)
print(upbo3)
print(lobo3)
### Boxplot of Age of Casualties ### 
boxplot(accidents$Age.of.Casualty, main="Boxplot of Age of Casualties", ylab="Number of Casualties")
boxplot.stats(accidents$Age.of.Casualty)$out


### Create cleaned data ###
nearclean  <- subset(accidents, accidents$Age.of.Casualty<upbo3)
View(nearclean)
write.table(nearclean,file = "clean_accident.csv",append = FALSE, quote = TRUE,
            sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE,)
clean_accidents <- read.csv("clean_accident.csv", header = TRUE)
View(clean_accidents)


### Boxplot of males ### 
maleacc <- clean_accidents %>% 
  select(Sex.of.Casualty, Weather.Conditions) %>%
  filter(Sex.of.Casualty == "1")  %>%
  group_by(Weather.Conditions,Sex.of.Casualty )
View(maleacc)

### Boxplot of females ### 
femaleacc <- clean_accidents %>% 
  select(Sex.of.Casualty, Weather.Conditions) %>%
  filter(Sex.of.Casualty == "2")
View(femaleacc)

### Boxplot of males and females ### 
hist(maleacc$Weather.Conditions, breaks = 0.5:9.5 ,
     xlim=c(0,10) , col='skyblue',border=F)
hist(femaleacc$Weather.Conditions, add=T, breaks = 0.5:9.5 ,
     xlim=c(0,10), col=scales::alpha('red',.5),border=F)


### Histogram of accident during years ### 
dhist <- substring(clean_accidents$Accident.Date,0,4)
View(dhist)
dhistconv <- as.numeric(dhist)
str(dhistconv)
hist(dhistconv, main="Boxplot of accident during years", 
     breaks = 2013.5:2017.5,las=1, xlim=c(2013.5,2017.5))


### Histogram on Light conditions and severity ### 
Sevacc1 <-clean_accidents %>%
  select(Lighting.Conditions,Casualty.Severity)  %>%
  filter( Casualty.Severity == 1 )
View(Sevacc1)

Sevacc2 <-clean_accidents %>%
  select(Lighting.Conditions,Casualty.Severity)  %>%
  filter( Casualty.Severity == 2 )
View(Sevacc2)

Sevacc3 <-clean_accidents %>%
  select(Lighting.Conditions,Casualty.Severity)  %>%
  filter( Casualty.Severity == 3 )
View(Sevacc3)

hist(Sevacc3$Lighting.Conditions, breaks = 0.5:8.5, xlim = c(0,8), col = 'red')
hist(Sevacc2$Lighting.Conditions,breaks = 0.5:8.5, xlim = c(0,8),col = 'blue')
hist(Sevacc1$Lighting.Conditions, add=T,breaks = 0.5:8.5, xlim = c(0,8),col = 'yellow')


### Weather condition and number of vehicles involved ### 
weavec <-clean_accidents %>%
  select(Number.of.Vehicles,Weather.Conditions)
View(weavec)
boxplot( Number.of.Vehicles~Weather.Conditions , weavec)
boxplot.stats(weavec$Number.of.Vehicles)$out


### Regression Model ### 

### Creation train model ### 
trainmod <- clean_accidents %>%
  select(Weather.Conditions,Casualty.Class,Casualty.Severity,Type.of.Vehicle,Age.of.Casualty) %>%
  filter(!is.na(Age.of.Casualty))
View(trainmod)

### Creation test model ### 
missmod <- accidents %>%
  select(Weather.Conditions,Casualty.Class,Casualty.Severity,Type.of.Vehicle,Age.of.Casualty) %>%
  filter(is.na(Age.of.Casualty))
View(missmod)

### Creation linear model ### 
regmodel = lm(Age.of.Casualty~Weather.Conditions+Casualty.Class+Casualty.Severity+Type.of.Vehicle, data=trainmod)
summary(regmodel)

### Predicting data on test model from linear regression ### 
pred <- predict(regmodel, missmod)
missmod$Age.of.Casualty <- predict(regmodel, missmod)
missmod$Age.of.Casualty <- round(missmod$Age.of.Casualty, digits = 0)

### Create regression data ###
write.table(missmod,file = "regression.csv",append = FALSE, quote = TRUE,
            sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE,)
regression <- read.csv("regression.csv", header = TRUE)
View(regression)

print(pred)



