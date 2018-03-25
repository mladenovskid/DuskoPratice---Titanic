#load Packages
library(ggplot2)
library(dplyr)

#Set Directory
#Laptop 
  setwd('C://Users//dmladenovski//Documents//R//DuskoPratice - Titanic')
       
#Home Pc
 #setwd('C://Users//Dusko (Guest)//Documents//GitHub//DuskoPratice---Titanic')
 
  

#List files names
  files <- list.files()

#Read in Files
  Test <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',', na.strings = "")
  GenderSubmission <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',', na.strings = "")
#TitanicInfo <- read.csv2('Titanic Info.txt', stringsAsFactors = FALSE, sep = ',')
  Train <- read.csv2('train.csv', stringsAsFactors = FALSE, sep = ',', na.strings = "")

  Full <- bind_rows(Train, Test)
  
#Change classes
  Full$Fare <- as.numeric(Full$Fare)
  Full$Age <- as.integer(Full$Age)
  Full$Survived <- as.factor(Full$Survived)
  
#Add Title Column
  Full$Title <- gsub('(.*, )|(\\..*)', '', Full$Name)
  
#Unique Titles  
  CoolTitles <- Full$Title[Full$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Rev', 'Sir', 'the Countess', 'Lady', 'Dona')]
  
#Edit Titles
  Full$Title[Full$Title == 'Mlle'] <- 'Miss'
  Full$Title[Full$Title == 'Mme'] <- 'Mrs'
  Full$Title[Full$Title == 'Ms'] <- 'Miss'
  Full$Title[Full$Title == ''] <- ''
  Full$Title[Full$Title %in% CoolTitles] <- 'Cool Title'

#Table Titles
  table(Full$Sex, Full$Title)

#Split SurNames
  Full$Surname <- sapply(Full$Name,  
                         function(x) strsplit(x, split = '[,.]')[[1]][1])
#Explore Data - Survivors by Class
  Train %>%
  group_by(Pclass, Sex) %>%
  summarize(
    mean = mean(Survived),
    Total = n(),
    Survived = sum(Survived),
    Died = sum(Total-Survived)
    )

#Table Data - Embarked
  table(Train$Embarked, Train$Survived)

# 1 Passengers do not have a point of Embarkment
  ggplot(Full, aes(x = Embarked, fill = factor(Survived)))+
    geom_histogram(bins = 10, stat = "count", position = "dodge")
  
  #Review Embarked missing data --- Missing embarkment are most likely 1st class and embarked from C
  Embarked_full <- Full %>% filter(Embarked != '')
  
#Missing embarkment paid $80
  ggplot(Embarked_full, aes( x = Embarked, y = as.numeric(Fare), fill = factor(Pclass))) + 
    geom_boxplot()+
    geom_hline(yintercept = 80, col = 'red')+
    ylab('Fare')

#Missing embarkment was 1st class and plenty of cabins in 'B' Cabin
  Full[Full$Fare >= 75 & Full$Fare <= 85 & Full$Title == 'Mr' & Full$Embarked == 'C',]

  #Manually edit Embark point for missing data as C
  Full$Embarked[is.na(Full$Embarked)] <- 'C'

#Recheck Attribute
  table(Full$Embarked, Full$Survived)

#Add Total Family Size
  Full$FamilySize <- Full$SibSp + Full$Parch + 1

#Visualize Data - Survival Based on Family Size
  ggplot(Full[1:891,], aes(x = FamilySize, fill = factor(Survived)))+
    geom_bar(stat = "count", position = 'dodge') + 
    scale_x_continuous(breaks = 1:11) +
    theme_classic()
  
#Check Survived by Age
  ggplot(Full[1:891,], aes(x = Age, fill = factor(Survived))) + 
    geom_histogram(position = 'stack')+
    facet_grid(.~Sex)

#check missing 
  for (Var in names(Full)){
    missing <- sum(is.na(Full[,Var]))
    print(c(Var, missing)) 
  }
  
#Identifying Mothers and Children
  Full$Mother <- "Not Mother"
  Full$Mother[Full$Sex == 'female' & Full$Parch > 0 & Full$Title != 'Miss' & Full$Age > 18] <- "Mother"

  Full$Child[Full$Age >= 18] <- "Adult"
  Full$Child[Full$Age < 18] <- "Child"

  table(Full$Mother, Full$Survived)
  table(Full$Child, Full$Survived)

  
#Attempting to use Decision Tree
#library('party')
  library('rpart')

#check missing data
  colSums(is.na(Full))
  colSums(Full == '')

#remove missing Age

  FullAge <- Full[Full$Age != ''  & !is.na(Full$Age),]


#predict Age
install.packages('mice')
library(mice)
install.packages('randomForest')
library('randomForest')
Train_mice <- mice(Full[, !names(Full) %in% 
                           c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Survived')], method = 'rf')


Train_complete <- complete(Train_mice)
Train$Age <- as.numeric(Train$Age)

ggplot(Train, aes(x = Age))+
  geom_line(stat = 'density', binwidth = 5, color = 'blue')+
  geom_line(data = Train_complete, aes(x = Age), stat = 'density', binwidth = 5, color = 'red')


#check predicted values against original data
par(mfrow=c(1,2))
hist(Train$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04), breaks = 16)
hist(Train_complete$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

Fit <- rpart(formula = Survived ~ Pclass + Sex  + Fare + Age + FamilySize, data = Full[1:400,], method = 'class')

rpart::plotcp(Fit)
plot(Fit, uniform = T)
text(Fit, all = T, cex = .8)

printcp(Fit)
plotcp(Fit)
rsq.rpart(Fit)
summary(Fit)
prune(Fit, Fit$cptable[which.min(Fit$cptable[,"xerror"]),"CP"])
