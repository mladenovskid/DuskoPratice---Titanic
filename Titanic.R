#load Packages
library(ggplot2)
library(dplyr)

#Set Directory
#Laptop 
# setwd('C://Users//dmladenovski//Documents//R//DuskoPratice - Titanic')
       
#Home Pc
 setwd('C://Users//Dusko (Guest)//Documents//GitHub//DuskoPratice---Titanic')
 
  files <- list.files()

#List files names
  read.csv2(list.files()) <- names(files)

#Read in Files
  Test <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',')
  GenderSubmission <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',')
#TitanicInfo <- read.csv2('Titanic Info.txt', stringsAsFactors = FALSE, sep = ',')
  Train <- read.csv2('train.csv', stringsAsFactors = FALSE, sep = ',')

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


unique(Train$SibSp)
unique(Train$Parch)
unique(Full$Title)


head(Full$Name)



#Visualize Data - Embarked
  table(Train$Embarked, Train$Survived)

# 2 Passengers do not have a point of Embarkment

  ggplot(Train, aes(x = Embarked, fill = factor(Survived)))+
    geom_histogram(bins = 10, stat = "count", position = "dodge")
  

#Review Embarked missing data --- Missing embarkment are most likely 1st class and embarked from C
Embarked_full <- Full %>% filter(Embarked != '')

ggplot(Embarked_full, aes( x = Embarked, y = as.numeric(Fare), fill = factor(Pclass))) + 
  geom_boxplot()+
  geom_hline(yintercept = 80, col = 'red')+
  ylab('Fare')

#Manually edit Embark point for missing data
Full$Embarked[Full$Embarked == ''] <- 'C'
#Recheck Attribute
table(Full$Embarked)


#Add Family Size
Full$FamilySize <- Full$SibSp + Full$Parch + 1
#Visualize Data - Survival Based on Family Size
  ggplot(Full[1:891,], aes(x = FamilySize, fill = factor(Survived)))+
    geom_bar(stat = "count", position = 'dodge') + 
    scale_x_continuous(breaks = 1:11) +
    theme_classic()
  
  #Families larger than 4 people have bad chnce of survival

Full$AgeBracket <- cut_width(Full$Age, width = 10)



#Check Survived by Age

  ggplot(Full[1:891,], aes(x = Age, fill = factor(Survived))) + 
    geom_histogram(position = 'stack')+facet_grid(.~Sex)
  
  
#check missing 
  for (Var in names(Full)){
    missing <- sum(is.na(Full[,Var]))
    print(c(Var, missing)) 
  }
  
  Full[Full$Fare == '' | is.na(Full$Fare),]
  Full[Full$Fare < 1,]

  
  
#Identifying Mothers and Children
  Full$Mother <- "Not Mother"
  Full$Mother[Full$Sex == 'female' & Full$Parch > 0 & Full$Title != 'Miss' & Full$Age > 18] <- "Mother"

  Full$Child[Full$Age >= 18] <- "Adult"
  Full$Child[Full$Age < 18] <- "Child"

  table(Full$Mother, Full$Survived)
  table(Full$Child, Full$Survived)

  
#Attempting to use Decision Tree

library('rpart')
#library('party')

  for (i in names(Full)){
  totals <- sum(is.na(Full$i))
  print(totals)
  }

  #check missing data
colSums(is.na(Full))
colSums(Full == '')

#remove missing Age

FullAge <- Full[Full$Age != ''  & !is.na(Full$Age),]


#predict Age
AgeTest <- rpart(data = FullAge[1:523, ], 
                 formula = Age ~ Pclass + SibSp + Parch + Fare + Title + FamilySize, 
                 method = 'poisson')
AgePredict <- predict(AgeTest, FullAge[524:1046, ])

plotcp(AgeTest)
summary(AgeTest)
plot(AgeTest)


rpart::






table(AgePredict)
hist(AgePredict)
hist(FullAge$Age)


plotcp(AgeTest)
  plot(AgeTest)
  text(AgeTest)

summary(AgeTest)
  
Fit <- rpart(formula = Survived ~ Pclass + Sex  + Fare + Age + FamilySize, data = Full[1:400,], method = 'class')

rpart::plotcp(Fit)
plot(Fit, uniform = T)
text(Fit, all = T, cex = .8)

printcp(Fit)
plotcp(Fit)
rsq.rpart(Fit)
summary(Fit)
prune(Fit, Fit$cptable[which.min(Fit$cptable[,"xerror"]),"CP"])

FirstPred <- predict(Fit, Full[401:891,], type = 'poisson')

table(Full[1:491, 2], FirstPred)

length(FirstPred)




