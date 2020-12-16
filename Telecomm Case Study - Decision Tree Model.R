
#################### TELECOMM -  CASE STUDY ####################

## Important Packages : : 

install.packages("caTools")

library(dplyr)
library(ggplot2)
library(Hmisc)
library(caTools)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)


## Importing Dataset : :

CustomerData <- read.csv("E:/R Programming/Data Analysis using R CASE STUDIES/Dataset/Customer_dataset.csv", header = T, sep = ",")

Customer_Data <- data.frame(CustomerData)

## EDA : : 

dim(Customer_Data)
## There are 1000 obs, & 14 var's in this dataset

glimpse(Customer_Data)
## Only 3 vars are numeric rest all are categorical / binary

sum(is.na(Customer_Data))
## There are NO N/A values

describe(Customer_Data$Monthly_Charges)
summary(Customer_Data$Monthly_Charges)
## Min. Monthly Charges amount = 18.95
## Max. Monthly Charges amount = 116.25


(table(Customer_Data$Churn)/nrow(Customer_Data))*100
## This shows that 74 % people are churning
## This shows that 26 % people are not churning


names(Customer_Data)
## The variable names available



## VISUALIZING THE BASICS

ggplot(Customer_Data, aes(Monthly_Charges, Total_Charges, color = Sex)) + 
       geom_point()


plot(Customer_Data$International_plan, xlab = "International Plan",
     ylab = "Count of customers", col = "orange")
table(Customer_Data$International_plan)
## This produces interesting insight..
## There are TWO "YES" for International data..

plot(Customer_Data$Technical_support, xlab = "Tech Support",
     ylab = "Count of customers", col = "lightblue")
?plot

plot(Customer_Data$Sex, xlab = "Tech Support",
     ylab = "Count of customers", col = "lightgreen")
## The males appear to take more telecomm service than women
table(Customer_Data$Sex)
## However, the appears to be evenl spread



## SPLITTING THE DATA FOR FURTHER ANALYSIS : : 

set.seed(123)
Training_Sample <- sample.split(Customer_Data$Churn, SplitRatio = 0.7)
Training_Data <- subset(Customer_Data, Training_Sample==T)

Testing_Data <- subset(Customer_Data, Training_Sample==F)

## BUILDING THE FULL TREE  : : 

Model <- rpart(Churn ~., data = Training_Data, method = "class",
               parms = list(split = 'information'), cp = -1)
Model

## Method means the criteria used
## Parms means the parameter used
## Split crietria BY DEFAULT is Inform i.e GINI IMPURITY
## CP = Complexity Parameter, (-1) ensures a tree to grow fully..


## CREATING THE TREE : : 
fancyRpartPlot(Model)


## To remove OVERPLOTTING FROM THE MODEL :
Churn_Model <- rpart(Churn ~., data = Training_Data, 
                     method = "class", parms = list(split = 'information'),
                     maxdepth = 3, minsplit = 2, minbucket = 2)
Churn_Model

## Mxdepth = 3, retsricts the model to not produce > 3 nodes 
## Minsplit = 2, restricts the model to include a min. of 
## 2 obs. in the node
## Minbucket = 2, restricts the min. no. of obs in leaf nodes
## at 2..

fancyRpartPlot(Churn_Model)


## TO GET CP VALUE ::
set.seed(123)
plotcp(Churn_Model)

## It shows that a fully grown tree will have OVERFITTING ISSUES
## To rectify this error, we have to prune the tree.


## To get CROSS-VALIDATION RESULTS : :
printcp(Churn_Model)
## It shows that the training data contains (26 % ) of the 
## cases with customer churn..

## We select that CP - VALUE for pruning which has 
## Least xerror as well as the Least Std Dev..
## IN our case , Xerror = 0.93
## & the std dev = 0.06
## So, CP - Value = 0.01,
## So, the tree is pruned by considering the 
## CP-Value = 0.01


## PRUNING PROCESS : : 
Prune_Model <- prune(Churn_Model, cp = 0.02)

## Rules mentioned for decision tree:
Prune_Model


## To understand the pruned model refer to the rules mentioned
fancyRpartPlot(Prune_Model)
## We know, Out of 700 custs, (26 %) are churning
## Now, we create a model to predict which cust's churn
## on the basis of their usage.

## Decision Tree is first split using Agreement period, which
## means that Agreement period is the most sigf. var.

## Further, root node shows that 26 % people are churning as 
## against (74 %) people.

## Further, Cust with Agree. Period = Monthly,
## (42.22 %) Customers Churn..

## these are subdivided in Internet Service, which shows
## (48.75 %) churning rate of people using Internet service
## of cable.

## This is further subdidvided on the basis of monthly charges

## The churning rate = (71.87 %) for people who have
## Internet service in Cable & monthly charges > 95

## In contrast, people with Internet service = No,
## shows custromer churn rate = (9.37 %)


## SUMMARY : :
## Based on the Decision tree we say that customers with 
## high likelihood to churn are those which belong
## to the Internet  = Cable & Monthly charge > 95 category





## PREDICTING THE MODEL : :

Testing_Data$Churn_Class <- predict(Prune_Model, 
                                    newdata = Testing_Data,
                                    type = "class")

## Conf. Matrix : :

table(Testing_Data$Churn, Testing_Data$Churn_Class)
## IT Shows, ACTUAL ....... PREDICTED

Accuracy_rate <- ((218+20)/(218+20+4+58))*100
Accuracy_rate

## PREDICTING THE PROBABILITY MATRIX : : 
Prediction <- predict(Prune_Model, newdata = Testing_Data,
                      type = "prob")

Prediction_Frame <- data.frame(Prediction)
Prediction_Frame
View(Prediction_Frame)

## Probability (No) represents the probl value of customers
## not churning and
## Probability (Yes) represents the probl value of
## customer churn.

## All telecomm industries are using these predicting 
## probabilities to target the customers who are having
## high likelihood to churn with the promotional offers, 
## providing the best customer service and engagement, 
## which results in reduction of customer churn.

Top10 <- filter(Prediction_Frame) %>%
                arrange(desc(Yes)) %>%
                slice(1:10)
Top10
## It shows the Top 10 Riskiest customers with the possibilty
## To churn..

Bottom10 <- filter(Prediction_Frame) %>%
  arrange(desc(No)) %>%
  slice(1:10)
Bottom10
## It shows the Top 10 customers with the Least possibilty
## To churn..





################### END OF CASE STUDY ###################
