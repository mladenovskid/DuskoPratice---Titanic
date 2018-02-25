#Set Directory
  setwd('C://Users//dmladenovski//Documents//R//DuskoPratice - Titanic')
  files <- list.files()

#List files names
  read.csv2(list.files()) <- names(files)

#Read in Files
  Test <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',')
  GenderSubmission <- read.csv2('test.csv', stringsAsFactors = FALSE, sep = ',')
  TitanicInfo <- read.csv2('Titanic Info.txt', stringsAsFactors = FALSE, sep = ',')
  Train <- read.csv2('train.csv', stringsAsFactors = FALSE, sep = ',')

  Full <- bind_rows(Train, Test)
  
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
  table(Full$Sex, Full$Title, exclude = "")
  
  
#Split SurNames
  Full$Surname <- sapply(Full$Name,  
                         function(x) strsplit(x, split = '[,.]')[[1]][1])
  
  
#Add Family Size
Full$FamilySize <- Full$SibSp + Full$Parch + 1
  

glimpse(Full)

  
#load Packages
  library(ggplot2)
  library(dplyr)

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

  ggplot(Train[Train$Embarked != "",], aes(x = Embarked, fill = factor(Survived)))+
    geom_histogram(bins = 10, stat = "count", position = "dodge")
  

  #Review Embarked missing data --- Missing embarkment are most likely 1st class and embarked from C
  
  Embarked_full <- Full %>% filter(Embarked != '')
    
ggplot(Embarked_full, aes( x = Embarked, y = as.numeric(Fare), fill = factor(Pclass))) + 
  geom_boxplot()+
  geom_hline(yintercept = 80, col = 'red')+
  ylab('Fare')

#Manually edit Embark point for missing data
Full$Embarked[Full$Embarked == ''] <- 'C'


Table(Full)

#Visualize Data - Survival Based on Family Size
  ggplot(Full[1:891,], aes(x = FamilySize, fill = factor(Survived)))+
    geom_bar(stat = "count", position = 'dodge', ) + 
    scale_x_continuous(breaks = 1:11) +
    theme_classic()


mean(Full$Cabin == '')

Full$Age[is.na(Full$Age)]

sapply(Full, )
Full$Age == ''

?is.na




