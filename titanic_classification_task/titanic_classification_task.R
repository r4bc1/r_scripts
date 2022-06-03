library(ggplot2)
library(tidyverse)
library(dplyr)
library("ggpubr")
library("doBy")
library(cowplot)
library(mice)
library(caTools)
library(caret)

df <- read.csv(r"{C:\Users\DELL\Documents\R scripts\data\titanic.csv}",
               stringsAsFactors=FALSE, encoding="UTF-8", sep= ",")


head(df, 1)
# PassengerId Survived Pclass                    Name  Sex Age
# 1           1        0      3 Braund, Mr. Owen Harris male  22
# SibSp Parch    Ticket Fare Cabin Embarked
# 1     1     0 A/5 21171 7.25              S


names(df)
# [1] "PassengerId" "Survived"    "Pclass"      "Name"       
# [5] "Sex"         "Age"         "SibSp"       "Parch"      
# [9] "Ticket"      "Fare"        "Cabin"       "Embarked"   

dim(df)
# [1] 891  12

str(df)
# 'data.frame':	891 obs. of  12 variables:
#   $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
# $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
# $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
# $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
# $ Sex        : chr  "male" "female" "female" "female" ...
# $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
# $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
# $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
# $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
# $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
# $ Cabin      : chr  "" "C85" "" "C123" ...
# $ Embarked   : chr  "S" "C" "S" "S" ...


summary(df)
# PassengerId       Survived          Pclass     
# Min.   :  1.0   Min.   :0.0000   Min.   :1.000  
# 1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000  
# Median :446.0   Median :0.0000   Median :3.000  
# Mean   :446.0   Mean   :0.3838   Mean   :2.309  
# 3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000  
# Max.   :891.0   Max.   :1.0000   Max.   :3.000  
# 
# Name               Sex                 Age       
# Length:891         Length:891         Min.   : 0.42  
# Class :character   Class :character   1st Qu.:20.12  
# Mode  :character   Mode  :character   Median :28.00  
# Mean   :29.70  
# 3rd Qu.:38.00  
# Max.   :80.00  
# NA's   :177    
#      SibSp           Parch           Ticket         
#  Min.   :0.000   Min.   :0.0000   Length:891        
#  1st Qu.:0.000   1st Qu.:0.0000   Class :character  
#  Median :0.000   Median :0.0000   Mode  :character  
#  Mean   :0.523   Mean   :0.3816                     
#  3rd Qu.:1.000   3rd Qu.:0.0000                     
#  Max.   :8.000   Max.   :6.0000                     
#                                                     
#       Fare           Cabin             Embarked        
#  Min.   :  0.00   Length:891         Length:891        
#  1st Qu.:  7.91   Class :character   Class :character  
#  Median : 14.45   Mode  :character   Mode  :character  
#  Mean   : 32.20                                        
#  3rd Qu.: 31.00                                        
#  Max.   :512.33      


# позбавимось NA

df = df %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))


# EDA

table(df$Sex)
# female   male
# 314    577

# погляньмо на пропорції виживжих і померлих пасажирів
prop.table(table(df$Survived))
# 0         1 
# 0.6161616 0.3838384 

summary(df$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.42   20.12   28.00   29.70   38.00   80.00     177 

# зробімо нову бінарну колонку, яка означатиме, чи є дитиною ця особа
df$Child <- 0
df$Child[df$Age < 18] <- 1


# поглянемо на розподіл по статі і цій новій колонці
aggregate(Survived ~ Child + Sex, data=df, FUN=sum)
# Child    Sex Survived
# 1     0 female      195
# 2     1 female       38
# 3     0   male       86
# 4     1   male       23

# дропнем неважливі для нас в даному випадку колонки
names(df)

drops <- c("Name", "Cabin", "Ticket", "PassengerId")
df = df[ , !(names(df) %in% drops)]



# спробуємо Random Forest

library(randomForest)

df$Survived = as_factor(df$Survived)


df$Survived


train <- sample(nrow(df), 0.8 * nrow(df), replace = FALSE)
TrainSet <- df[train,]
ValidSet <- df[-train,]


rf <- randomForest(TrainSet$Survived ~ ., data = TrainSet, importance = TRUE)

rf

# Call:
#   randomForest(formula = TrainSet$Survived ~ ., data = TrainSet,      importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 16.57%
# Confusion matrix:
#   0   1 class.error
# 0 406  32  0.07305936
# 1  86 188  0.31386861


# Тепер спробуємо логістичну регресію (класифікаційна задача)

logistic <- glm(Survived ~ ., data = TrainSet, family = "binomial")
summary(logistic)

# Call:
#   glm(formula = Survived ~ ., family = "binomial", data = TrainSet)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2739  -0.6278  -0.3872   0.5649   2.4310  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  15.271713 535.411473   0.029  0.97724    
# Pclass       -1.163924   0.165176  -7.047 1.83e-12 ***
#   Sexmale      -2.822275   0.232675 -12.130  < 2e-16 ***
#   Age          -0.021075   0.010531  -2.001  0.04536 *  
#   SibSp        -0.323172   0.127356  -2.538  0.01116 *  
#   Parch        -0.152489   0.142139  -1.073  0.28335    
# Fare          0.003239   0.002672   1.212  0.22545    
# EmbarkedC   -10.802932 535.411265  -0.020  0.98390    
# EmbarkedQ   -10.683847 535.411357  -0.020  0.98408    
# EmbarkedS   -10.936631 535.411248  -0.020  0.98370    
# Child         1.163023   0.410718   2.832  0.00463 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 948.92  on 711  degrees of freedom
# Residual deviance: 605.74  on 701  degrees of freedom
# AIC: 627.74
# 
# Number of Fisher Scoring iterations: 12


probs <- predict(logistic, newdata = ValidSet, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)

confusionMatrix(factor(pred), factor(ValidSet$Survived), positive = as.character(1))

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 104  21
# 1   5  49
# 
# Accuracy : 0.8547          
# 95% CI : (0.7945, 0.9029)
# No Information Rate : 0.6089          
# P-Value [Acc > NIR] : 5.104e-13       
# 
# Kappa : 0.682           
# 
# Mcnemar's Test P-Value : 0.003264        
#                                           
#             Sensitivity : 0.7000          
#             Specificity : 0.9541          
#          Pos Pred Value : 0.9074          
#          Neg Pred Value : 0.8320          
#              Prevalence : 0.3911          
#          Detection Rate : 0.2737          
#    Detection Prevalence : 0.3017          
#       Balanced Accuracy : 0.8271          
#                                           
#        'Positive' Class : 1               
                                  
