#########################################
#     IMPORTING & CLEANING
#########################################

library(ggplot2)
library(dplyr)
library(regclass)

data <- read.csv('NYC_Dog_Licensing_Dataset.csv')
incomedata <- read.csv('Income_Data.csv')
boroughs <- read.csv('boroughs.csv')

#cleaning - read dates as dates
data$LicenseIssuedDate <- as.Date(data$LicenseIssuedDate, format='%m/%d/%Y')
data$LicenseExpiredDate <- as.Date(data$LicenseExpiredDate, format='%m/%d/%Y')

#cleaning - create License Issue Month variable
LicenseIssuedMonth <- as.numeric(format(data$LicenseIssuedDate, '%m'))
data <- cbind(data, LicenseIssuedMonth)

#cleaning - create License Issue Year variable
LicenseIssuedYear <- as.numeric(format(data$LicenseIssuedDate, '%Y'))
data <- cbind(data, LicenseIssuedYear)

#cleaning - remove letters from beginning of Zip Codes, so only the Zip Codes remain
incomedata$Zip <- substring(incomedata$Zip, 7,11)
incomedata <- transform(incomedata, Zip=as.numeric(Zip))

#cleaning - merge income & dog data
fulldata <- full_join(data, incomedata, by=c('ZipCode'='Zip'), copy=TRUE)
fulldata <- transform(fulldata, MedianIncome=as.numeric(MedianIncome))

#cleaning - merge income, dog, & borough data
fulldata <- full_join(fulldata, boroughs, by=c('ZipCode'='Zip'), copy=TRUE)
fulldata$Borough <- factor(fulldata$Borough)
                        
#cleaning - create factored data for income
incomelevels <- c()
for(i in 1:length(fulldata$MedianIncome)) {
  if(is.na(fulldata$MedianIncome[i]))
    {incomelevels[i] = 'NA'} else {
      if(fulldata$MedianIncome[i]<25000) 
        {incomelevels[i] = '<25k'}
        if(fulldata$MedianIncome[i]>=25000 & fulldata$MedianIncome[i]<50000) 
          {incomelevels[i] = '25-50k'}
          if(fulldata$MedianIncome[i]>=50000 & fulldata$MedianIncome[i]<75000)
            {incomelevels[i] = '50-75k'}
            if(fulldata$MedianIncome[i]>=75000 & fulldata$MedianIncome[i]<100000)
              {incomelevels[i] = '75-100k'}
              if(fulldata$MedianIncome[i]>=100000 & fulldata$MedianIncome[i]<150000)
                {incomelevels[i] = '100-150k'}
                if(fulldata$MedianIncome[i]>=150000)
                  {incomelevels[i] = '150k+'}
    }
}
incomelevels <- factor(incomelevels, levels=c('<25k', '25-50k', '50-75k', '75-100k', '100-150k', '150k+'))
fulldata <- cbind(fulldata, incomelevels)

#cleaning - create factored data for population
poplevels <- c()
for(i in 1:length(fulldata$Population)) {
  if(is.na(fulldata$Population[i]))
  {poplevels[i] = 'NA'} else {
    if(fulldata$Population[i]<10000) 
    {poplevels[i] = '<10k'}
    if(fulldata$Population[i]>=10000 & fulldata$Population[i]<15000) 
    {poplevels[i] = '10-15k'}
    if(fulldata$Population[i]>=15000 & fulldata$Population[i]<20000)
    {poplevels[i] = '15-20k'}
    if(fulldata$Population[i]>=20000 & fulldata$Population[i]<25000)
    {poplevels[i] = '20-25k'}
    if(fulldata$Population[i]>=25000 & fulldata$Population[i]<30000)
    {poplevels[i] = '25-30k'}
    if(fulldata$Population[i]>=30000 & fulldata$Population[i]<35000)
    {poplevels[i] = '30-35k'}
    if(fulldata$Population[i]>=35000)
    {poplevels[i] = '35k+'}
  }
}
poplevels <- factor(poplevels, levels=c('<10k', '10-15k', '15-20k', '25-30k', '30-35k', '35k+'))
fulldata <- cbind(fulldata, poplevels)

#clear NA
cleandata <- na.omit(fulldata)

#Create Animal Age Variable
AnimalAge <- cleandata$LicenseIssuedYear - cleandata$AnimalBirthYear
cleandata <- cbind(cleandata, AnimalAge)
cleandata <- subset(cleandata, AnimalAge >=0)

#cleaning - create factored data for age
agelevels <- c()
for(i in 1:length(cleandata$AnimalAge)) {
  if(is.na(cleandata$AnimalAge[i]))
  {agelevels[i] = 'NA'} else {
    if(cleandata$AnimalAge[i]<5) 
    {agelevels[i] = 'Young'}
    if(cleandata$AnimalAge[i]>=5 & cleandata$AnimalAge[i]<10) 
    {agelevels[i] = 'Middle Aged'}
    if(cleandata$AnimalAge[i]>=10)
    {agelevels[i] = 'Elderly'}
  }
}
agelevels <- factor(agelevels, levels=c('Young','Middle Aged','Elderly'))
cleandata <- cbind(cleandata, agelevels)

#Create Seasons variable
seasons <- c()
for(i in 1:length(cleandata$LicenseIssuedMonth)){
  if(cleandata$LicenseIssuedMonth[i]<3) {seasons[i] = 'Winter'}
  if((cleandata$LicenseIssuedMonth[i]>=3) & (cleandata$LicenseIssuedMonth[i]<6)) {seasons[i]='Spring'}
  if((cleandata$LicenseIssuedMonth[i]>=6) & (cleandata$LicenseIssuedMonth[i])<9) {seasons[i]='Summer'}
  if((cleandata$LicenseIssuedMonth[i]>=9) & (cleandata$LicenseIssuedMonth[i])<12) {seasons[i]='Fall'}
  if(cleandata$LicenseIssuedMonth[i]>=12) {seasons[i]='Winter'}
}
seasons <- factor(seasons, levels=c('Spring','Summer','Fall','Winter'))
cleandata <- cbind(cleandata, seasons)

#Create a Variable that lists each unique Zip Code
  #each Zip Code's Population & Median Income
zippop1 = c()
zippop2 = c()
zippop3 = c()
for(i in 1:length(cleandata$ZipCode)){
  if(cleandata$ZipCode[i] %in% zippop1) {
    next }
  zippop1 = append(zippop1, cleandata$ZipCode[i])
  zippop2 = append(zippop2, cleandata$Population[i])
  zippop3 = append(zippop3, cleandata$MedianIncome[i])
}
zippop <- cbind(zippop1, zippop2, zippop3)
colnames(zippop) <- c('Zip Code', 'Population', 'Median Income')
zippop <- data.frame(zippop)
zippop$Median.Income <- as.numeric(zippop$Median.Income)

#Borough Dataframe
  #Total Borough Population
Boroughs_List <- c('Bronx','Brooklyn','Manhattan','Queens','Staten')
Boroughs_Pop <- c(1379946, 2590516, 1596273, 2278029, 491133)
boroughs_total <- data.frame(cbind(Boroughs_List, as.numeric(Boroughs_Pop)))
boroughs_total$V2 <- as.numeric(boroughs_total$V2)
  #Number of Dog Registrations by Borough
boroughcount <- data.frame(table(cleandata$Borough))
  #Percent of Dog Registrations by Borough Population
borough_pct <- c()
for(i in 1:dim(boroughcount)[1]){
  borough_pct[i] = boroughcount$Freq[i]/boroughs_total$V2[i]
}
borough_pct <- cbind(Boroughs_List, borough_pct)
colnames(borough_pct) <- c('Borough', 'Percent Dog Registrees')
borough_pct <- data.frame(borough_pct)
borough_pct$Percent.Dog.Registrees <- as.numeric(borough_pct$Percent.Dog.Registrees)
  #Borough Population Density
borough_density <- c(12634, 14417, 27203, 8090, 3297)
borough_density <- cbind(Boroughs_List, borough_density)
colnames(borough_density) <- c('Borough', 'Population Density')
borough_density <- data.frame(borough_density)
borough_density$Population.Density <- as.numeric(borough_density$Population.Density)
  #Borough Dog Density
borough_pctd <- c()
for(i in 1:dim(boroughs_total)[1]){
  borough_pctd[i] = boroughcount$Freq[i]/(boroughs_total$V2[i]/borough_density$Population.Density[i])
}
borough_pctd <- cbind(Boroughs_List, borough_pctd)
colnames(borough_pctd) <- c('Borough', 'Dog Percent Density')
borough_pctd <- data.frame(borough_pctd)
borough_pctd$Dog.Percent.Density <- as.numeric(borough_pctd$Dog.Percent.Density)
  #Merge
Borough_Info <- cbind(Boroughs_List, Boroughs_Pop, boroughcount$Freq, borough_pct$Percent.Dog.Registrees, borough_density$Population.Density, borough_pctd$Dog.Percent.Density)
colnames(Borough_Info) <- c('Boroughs', 'HumanPopulation', 'DogPopulation', 'HumanDensity', 'HumanDogRatio', 'DogDensity')
Borough_Info <- data.frame(Borough_Info)
for(i in 2:length(Borough_Info)) {
  Borough_Info[,i] = as.numeric(Borough_Info[,i])
}

#########################################
#     EXPLORATORY DATA ANALYSIS
#########################################

#Summary
summary(cleandata)

#Median Income - Density Plot
ggplot(cleandata, aes(x=MedianIncome)) + 
  geom_density() +
  ggtitle('Density Plot of Median Income') +
  xlab('Median Income') +
  ylab('Density')

ggplot(cleandata, aes(x=sqrt(MedianIncome))) + 
  geom_density()+
  ggtitle('Density Plot of Median Income') +
  xlab('Median Income') +
  ylab('Density')

#License Issue Date - Density Plot
ggplot(cleandata, aes(x=LicenseIssuedDate)) + 
  geom_density() +
  ggtitle('Density Plot of License Issue Date') +
  xlab('License Issue Date') +
  ylab('Density')

#License Expired Date - Density Plot
ggplot(cleandata, aes(x=LicenseExpiredDate)) + 
  geom_density() +
  ggtitle('Density Plot of License Expired Date') +
  xlab('License Expired Date') +
  ylab('Density')

#License Issue Month - Bar Plot
ggplot(cleandata, aes(x=LicenseIssuedMonth)) + 
  geom_bar() +
  ggtitle('Bar Plot of License Issue Month') +
  xlab('License Issue Month') +
  ylab('Count')

#ANIMAL BIRTH YEAR & DATE - Line
ggplot(cleandata, aes(x=LicenseIssuedDate, y=AnimalBirthYear)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle=90)) +
  xlab('License Issue Date') +
  ylab('Animal Birth Year') +
  ggtitle('Animal Birth Year by License Issue Date')

#ANIMAL AGE & DATE - Line
ggplot(cleandata, aes(x=LicenseIssuedDate, y=AnimalAge)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle=90)) +
  xlab('License Issue Date') +
  ylab('Animal Age') +
  ggtitle('Animal Age by License Issue Date')

#Animal Age - Density
ggplot(cleandata, aes(x=AnimalAge)) +
  geom_density() +
  xlab('Animal Age') +
  ylab('Density') +
  ggtitle('Density Plot of Animal Age')

#Animal Birth Year - Density
ggplot(cleandata, aes(x=AnimalBirthYear)) +
  geom_density() +
  xlab('Animal Birth Year') +
  ylab('Density') +
  ggtitle('Density Plot of Animal Birth Year')

#Boroughs & Population
ggplot(data=boroughs_total, aes(x=Boroughs_List, y=Boroughs_Pop)) + 
  geom_bar(stat='identity') +
  ggtitle('NYC Total Population by Borough') +
  xlab('Boroughs') +
  ylab('Population')
ggplot(data=boroughcount, aes(x=Var1, y=Freq)) +  
  geom_bar(stat='identity') +
  ggtitle('Dog Registrations by Borough') +
  xlab('Boroughs') +
  ylab('Count')
ggplot(data=borough_pct, aes(x=Borough, y='Percent Dog Registrees')) +  
  geom_bar(stat='identity') +
  ggtitle('Percent Dog Registrees by Borough') +
  xlab('Boroughs') +
  ylab('Percent Dog Registrees')
ggplot(data=borough_density, aes(x=Borough, y=Population.Density)) +  
  geom_bar(stat='identity') +
  ggtitle('Population Density of Humans by Borough') +
  xlab('Boroughs') +
  ylab('Population Density')
ggplot(data=borough_pctd, aes(x=Borough, y=Dog.Percent.Density)) +  
  geom_bar(stat='identity') +
  ggtitle('Population Density of Dogs by Borough') +
  xlab('Boroughs') +
  ylab('Dog Density')

#########################################
#     ASSOCIATION ANALYSIS: Y = INCOME 
#########################################

all_correlations(cleandata, interest='MedianIncome')
#Population, AnimalAge, AnimalBirthYear, ZipCode, LicenseIssuedYear

associate(MedianIncome~AnimalGender, data=cleandata, permutations = 1000, seed = 50123)
associate(MedianIncome~LicenseIssuedMonth, data=cleandata)
associate(MedianIncome~Borough, data=cleandata, permutations = 1000, seed = 50123, dev.off())

#########################################
#     ASSOCIATION ANALYSIS: Y = INCOME / GRAPHS
#########################################

#POPULATION & INCOME
ggplot(cleandata) +
  geom_point(aes(x=Population, y=MedianIncome)) +
  geom_smooth(aes(x=Population, y=MedianIncome), method='lm') +
  xlab('Population of Zip Code of Dog Registrees') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Median Income by Zip Code Population')
ggplot(cleandata, aes(x=poplevels, fill=incomelevels)) + 
  geom_bar(stat='count') +
  xlab('Population Levels') +
  ylab('Frequency') +
  theme(legend.title = element_blank())
ggplot(cleandata, aes(x=poplevels, y=MedianIncome)) + 
  geom_boxplot() +
  xlab('Population Levels') +
  ylab('Median Income') +
  ggtitle('Population by Median Income') +
  theme(legend.title = element_blank())

#ANIMAL AGE & INCOME
ggplot(cleandata, aes(x=AnimalAge, y=MedianIncome)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle=90)) +
  xlab('Animal Age') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Animal Age by Median Income')  
ggplot(cleandata, aes(x=agelevels, y=MedianIncome)) + 
  geom_boxplot() +
  xlab('Age Levels') +
  ylab('Median Income') +
  ggtitle('Animal Age by Median Income') +
  theme(legend.title = element_blank())
ggplot(cleandata) +
  geom_jitter(aes(x=AnimalAge, y=MedianIncome)) +
  geom_smooth(aes(x=AnimalAge, y=MedianIncome)) +
  xlab('Animal Age') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Median Income by Animal Age')

#LICENSE ISSUE MONTH & INCOME
ggplot(cleandata, aes(x=LicenseIssuedMonth, y=MedianIncome)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('License Issue Month') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Median Income by License Issue Month')
ggplot(cleandata, aes(x=factor(LicenseIssuedMonth), y=MedianIncome)) + 
  geom_boxplot() +
  xlab('License Issue Month') +
  ylab('Median Income') +
  ggtitle('License Issue Month & Median Income')

#LICENSE ISSUE YEAR & INCOME
ggplot(cleandata, aes(x=LicenseIssuedYear, y=MedianIncome)) +
  geom_smooth(method='lm') +
  xlab('License Issue Year') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Median Income by License Issue Year')
ggplot(cleandata, aes(x=LicenseIssuedYear, fill=incomelevels)) + 
  geom_bar(stat='count') +
  xlab('License Issue Year') +
  ylab('Frequency') +
  ggtitle('License Issue Year & Median Income') +
  theme(legend.title = element_blank())

#BOROUGH & INCOME
ggplot(cleandata, aes(x=Borough, y=MedianIncome)) +
  geom_boxplot() +
  xlab('Borough of Dog Registrees') +
  ylab('Median Income of Dog Registrees') +
  ggtitle('Median Income of Dog Registrees by Borough')
mosaic(cleandata$Borough~cleandata$incomelevels, xlab='Income Level', ylab='Borough')
ggplot(zipbor, aes(x=Borough, y=Median.Income)) +
  geom_boxplot() +
  xlab('Borough') +
  ylab('Median Income') +
  ggtitle('Median Income of Population by Borough')

#########################################
#     REGRESSION ANALYSIS: Y = INCOME
#########################################

#Median Income & Population
lmpop = lm(MedianIncome~Population, data=cleandata)
anova(lmpop)
confint(lmpop)
plot(lmpop)
#Median Income & Population - transformed
ggplot(cleandata, aes(x=sqrt(Population), y=log(MedianIncome))) +
  geom_point() +
  geom_smooth(method='lm')
lmpop = lm(log(MedianIncome)~sqrt(Population), data=cleandata)
anova(lmpop)
confint(lmpop)
plot(lmpop)

#Median Income & Animal Age 
lmage = lm(MedianIncome~AnimalAge, data=cleandata)
anova(lmage)
confint(lmage)
plot(lmage)
#Median Income & Animal Age - Transformed
ggplot(cleandata, aes(x=log(AnimalAge), y=log(MedianIncome))) +
  geom_point() +
  geom_smooth(method='lm')
lmage = lm(log(MedianIncome)~log(AnimalAge+1), data=cleandata)
anova(lmage)
confint(lmage)
plot(lmage)

#Median Income & License Issue Year
lmissueyear = lm(MedianIncome~LicenseIssuedYear, data=cleandata)
anova(lmissueyear)
confint(lmissueyear)
plot(lmissueyear)
#Median Income & License Issue Year - Transformed
ggplot(cleandata, aes(x=LicenseIssuedYear, y=sqrt(MedianIncome))) +
  geom_point() +
  geom_smooth(method='lm')
lmissueyear = lm(sqrt(MedianIncome)~LicenseIssuedYear, data=cleandata)
anova(lmissueyear)
confint(lmissueyear)
plot(lmissueyear)

#Median Income & License Issued Month
lmissuemonth = lm(MedianIncome~LicenseIssuedMonth, data=cleandata)
anova(lmissuemonth)
confint(lmissuemonth)
plot(lmissuemonth)

lmborough = lm(MedianIncome~Borough, data=cleandata)
anova(lmborough)
confint(lmborough)
plot(lmborough)

#########################################
#     OTHER TECHNIQUES
#########################################

# INCOME = BIRTH YEAR + AGE
Mlmage = lm(MedianIncome~AnimalAge + AnimalBirthYear, data=cleandata)
anova(Mlmage)
confint(Mlmage)
ggplot(cleandata, aes(x=agelevels, y=incomelevels)) +
  geom_tile(aes(fill=AnimalBirthYear))+
  xlab('Animal Age') +
  ylab('Median Income') +
  ggtitle('Median Income by Animal Age & Birth Year') +
  theme(legend.title = element_blank())

# INCOME = BIRTH YEAR + DATE
Mlmdate = lm(MedianIncome~AnimalBirthYear + LicenseIssuedDate, data=cleandata)
anova(Mlmdate)
confint(Mlmdate)
ggplot(cleandata, aes(x=LicenseIssuedDate, y=AnimalBirthYear, color=incomelevels)) +
  geom_smooth()

# INCOME = POPULATION + BOROUGH
Mlmpop = lm(MedianIncome~Population + Borough, data=cleandata)
anova(Mlmpop)
confint(Mlmpop)
ggplot(cleandata, aes(x=Borough, y=incomelevels)) +
  geom_tile(aes(fill=Population))+
  xlab('Borough') +
  ylab('Median Income') +
  ggtitle('Median Income by Borough & Population') +
  theme(legend.title = element_blank())

Bronx = subset(cleandata, Borough=='Bronx')
Brooklyn = subset(cleandata, Borough=='Brooklyn')
Manhattan = subset(cleandata, Borough=='Manhattan')
Queens = subset(cleandata, Borough=='Queens')
Staten = subset(cleandata, Borough=='Staten')
popplot <- ggplot(cleandata, aes(x=Population, y=MedianIncome, color=Borough)) +
  geom_point()
popplot +
  geom_smooth(data=Bronx, aes(x=Population, y=MedianIncome), method=lm, se=FALSE) +
  geom_smooth(data=Brooklyn, aes(x=Population, y=MedianIncome), method=lm, se=FALSE) +
  geom_smooth(data=Manhattan, aes(x=Population, y=MedianIncome), method=lm, se=FALSE) +
  geom_smooth(data=Queens, aes(x=Population, y=MedianIncome), method=lm, se=FALSE) +
  geom_smooth(data=Staten, aes(x=Population, y=MedianIncome),method=lm, se=FALSE) +
  ggtitle('Median Income by Population') +
  xlab('Population') +
  ylab('Median Income')
  
# INCOME = MONTH + YEAR
Mlmdates = lm(MedianIncome~LicenseIssuedMonth + LicenseIssuedYear, data=cleandata)
anova(Mlmdates)
confint(Mlmdates)
