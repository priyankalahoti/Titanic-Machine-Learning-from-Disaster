library(plyr)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(stringr)


rm(list=ls())
# load train and test dataset
train <- read.csv('D:/Titanic/train.csv')
test  <- read.csv('D:/Titanic/test.csv')
test$Survived <- NA

#merge test and train dataset
full <- rbind(train, test)
str(train)
summary(train)

### Data Visualisation
ggplot(train,aes(x = Pclass,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Pclass v/s Survival Rate")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")  
#First class Survival rate is far more better than the 3rd class  

## Did age influence survival?
ggplot(train, aes(x=Age, y=PassengerId, color = as.factor(Survived))) +                      
  geom_point() + 
  facet_grid(Sex ~.) +
  ggtitle("Survival vs Passenger's Age")+
  xlab("Age") 

### Visualize the 3-way relationship of sex, pclass, and survival
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Plot of sex, pclass, and survival") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")
##In the all the class female Survival rate is better than Men

## Is survival of a passenger related to his/her Pclass and port of embarkation?
ggplot(train[train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Pclass) +
  labs(fill="Survived") +
  ggtitle("Survival vs Passenger's Pclass and Port of Embarkation")

#Most of the passengers who perished had embarked from port 'S' and were travelling 3rd class.
ggplot(train[train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Sex) +
  ggtitle("Survival vs Passenger's Sex and Port of Embarkation")




###Exploratory data analysis

head(full$Name)

## Extract Title of Passengers from Name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
full[full$Title == "Capt"| full$Title == "Col"| full$Title == "Don"|full$Title == "Major"| full$Title == "Rev"|full$Title == "Jonkheer"|full$Title == "Sir",]$Title = "Mr"    
table(full$Sex, full$Title)

full[full$Title == "Dona"|full$Title == "Mlle"|full$Title == "Mme" | full$Title=="Ms",]$Title = "Miss"
full[full$Title == "Lady"| full$Title == "the Countess",]$Title = "Mrs"

# Categorise doctors as per their sex
full[full$Title == "Dr" & full$Sex == "female",]$Title = "Miss"
full[full$Title == "Dr" & full$Sex == "male",]$Title = "Mr"

full$Title = as.factor(full$Title)
summary(full$Title)

# Lets check who among Mr, Master, Miss having a better survival rate
ggplot(full[1:891,],aes(x = Title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 
#passengers with title Miss and Mrs have higher survival rate and with title Mr have lower survival rate

## FamilySize
full$FSize = ifelse(full$SibSp + full$Parch + 1 <= 3, 1,0) # Small = 1, Big = 0

## Mother
full$Mother = ifelse(full$Title=="Mrs" & full$Parch > 0, 1,0)

## Single
full$Single = ifelse(full$SibSp + full$Parch + 1 == 1, 1,0) # People travelling alone

full$Name<-as.character(full$Name)
## FamilyName
full$FamilyName = sapply(full$Name,function(x) strsplit(x,',')[[1]][1])


#Since there are possibly many people sharing same family name, it is necessary to distinguish each family separately. 
Family.Ticket = full[full$Single == 0,c("FamilyName", "Ticket")]
Family.Ticket = Family.Ticket[order(Family.Ticket$FamilyName),]
head(Family.Ticket)


#Baring few exceptions, in general, a family shared the same ticket number. This can be a good way of identifying families. Here, last three digits of the ticket is extracted and attached to family names, thereby creating unique family names for each family.
full$FamilyName  = paste(full$FamilyName , str_sub(full$Ticket,-3,-1), sep="")

## FamilySurvived
full$FamilySurvived = 0
# Dataset of passengers with family
Families = full[(full$Parch+full$SibSp) > 0,]

# Group families by their family name and number of survivals in the family
Survival.GroupByFamilyName = aggregate(as.numeric(Families$Survived), by=list("FamilyName" = Families$FamilyName), FUN=sum, na.rm=TRUE)


# Family is considered to have survived if atleast one member survived
FamilyWithSurvival = Survival.GroupByFamilyName[Survival.GroupByFamilyName$x > 0,]$FamilyName
full[apply(full, 1, function(x){ifelse(x["FamilyName"] %in% FamilyWithSurvival,TRUE,FALSE)}),]$FamilySurvived = 1


# Imputing missing data
#The Multiple Imputation by Chained Equations (MICE) package is used for multiple imputation through predictive mean matching method, specifically  for missing Age data, which ensures that imputed values are plausible. 
full$Pclass = as.factor(full$Pclass)
full$Sex = as.factor(full$Sex)
full[full$Embarked == "",]$Embarked = NA
full$Embarked = as.factor(full$Embarked)
full[full$Cabin == "",]$Cabin = NA
full$Cabin = as.factor(full$Cabin)
full$FSize = as.factor(full$FSize)
full$Mother = as.factor(full$Mother)
full$Single = as.factor(full$Single)
full$FamilyName = as.factor(full$FamilyName)

## Embarked
full$Embarked[is.na(full$Embarked)] = 'S'

## Fare
which(is.na(full$Fare))
full[1044,]
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

##age
## We will group data det by variables PClass and Title and replace missing Age value in that group by group median. 
impute.median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
full<-ddply(full, ~ Pclass+Title, transform, Age = impute.median(Age))

                         
## AgeClass
                       
full$Agegroup = ifelse(full$Age<=10,1,
                       ifelse(full$Age>10 & full$Age<=20,2,
                              ifelse(full$Age>20 & full$Age<=35,3,4)))
full$Agegroup = as.factor(full$Agegroup)
# Child
full$Child= ifelse(full$Age<10,1,0)

## Cabin
# Note: This procedure has been taken from script submitted on Kaggle.
# Extract single alphabet prefixed to each cabin number provided. Each of these letters represent the part of the deck were these cabins were located.
full$CabinNo = sapply(full$Cabin,function(x) substr(x,1,1))
full$CabinNo[full$CabinNo == ""] = NA
table(is.na(full$CabinNo))

# Dataset of all families with cabin data
familyWithCabinNo = unique(full[!is.na(full$CabinNo) & full$SibSp + full$Parch > 0,c("FamilyName", "CabinNo")])
head(familyWithCabinNo)

checkIfHasCabin <- function(familyName, CabinNo){   
  ifelse (familyName %in% familyWithCabinNo$FamilyName, familyWithCabinNo$CabinNo, CabinNo)}


# Assign same cabin number to those members of a single family, whose cabin number is missing 
full[is.na(full$CabinNo),]$CabinNo = apply(full[ is.na(full$CabinNo),c("FamilyName", "CabinNo")], 1, function(y) checkIfHasCabin(y["FamilyName"], y["CabinNo"]))

table(is.na(full$CabinNo))
table(full$CabinNo, full$Pclass)

# for first class obs
A.1 = round(22/(323-65) * 65)
B.1 = round(65/(323-65) * 65)
C.1 = round(96/(323-65) * 65)
D.1 = round(40/(323-65) * 65)
E.1 = 65 - (A.1+B.1+C.1+D.1)
# for second class
D.2 = round(6/(277-254) * 254)
E.2 = round(4/(277-254) * 254)
F.2 = 254 - (D.2+E.2)
# for third class
E.3 = round(3/(709-691) * 691)
F.3 = round(8/(709-691) * 691)
G.3 = 691 - (E.3+F.3)

set.seed(0)
full[ sample( which( full$Pclass==1 & is.na(full$CabinNo)), A.1 ) , "CabinNo"] <- rep("A", A.1)
full[ sample( which( full$Pclass==1 & is.na(full$CabinNo)), B.1 ) , "CabinNo"] <- rep("B", B.1)
full[ sample( which( full$Pclass==1 & is.na(full$CabinNo)), C.1 ) , "CabinNo"] <- rep("C", C.1)
full[ sample( which( full$Pclass==1 & is.na(full$CabinNo)), D.1 ) , "CabinNo"] <- rep("D", D.1)
full[ sample( which(full$Pclass==1 & is.na(full$CabinNo)), E.1 ) , "CabinNo"] <- rep("E", E.1)

set.seed(0)
full[ sample( which( full$Pclass==2 & is.na(full$CabinNo)), D.2 ) , "CabinNo"] <- rep("D", D.2)
full[ sample( which( full$Pclass==2 & is.na(full$CabinNo)), E.2 ) , "CabinNo"] <- rep("E", E.2)
full[ sample( which( full$Pclass==2 & is.na(full$CabinNo)), F.2 ) , "CabinNo"] <- rep("F", F.2)

set.seed(0)
full[ sample( which( full$Pclass==3 & is.na(full$CabinNo)), E.3 ) , "CabinNo"] <- rep("E", E.3)
full[ sample( which( full$Pclass==3 & is.na(full$CabinNo)), F.3 ) , "CabinNo"] <- rep("F", F.3)
full[ sample( which( full$Pclass==3 & is.na(full$CabinNo)), G.3 ) , "CabinNo"] <- rep("G", G.3)

full$CabinNo = as.factor(full$CabinNo)
table(full$CabinNo, full$Pclass)

summary(full)
full1<-full[,-c(4,9,11,17)]

###Now lets build randomforest model
training = full1[1:891,]
test = full1[892:1309,]

## Remove PassengerId variable from training data as it will not contribute in model building
training<-training[,-1]

rf<- randomForest(factor(Survived)~. , data = training)
rf                           
varImpPlot(rf)

# Predict using the test set
test$Survived <- predict(rf, test)

# Create final solution file
final_solution<-test[,c('PassengerId','Survived')]
write.csv(final_solution, file = 'final_Solution.csv', row.names = F)
                  
               

