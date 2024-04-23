install.packages("tidyverse")
library("tidyverse")
data <- read.csv("healthdata.csv")
head(data)
newdata <- read.csv("healthdata.csv")
nrow(data)
attach(data)
names(data)

#####
smoke.factor <- factor(Smoker)
smoke.factor

gender.factor <- factor(Gender)
gender.factor


Children.factor <- factor(Children)
Children.factor

############## DATA CLEANING ####################
#Handling missing value in R
#find the mean of the columns
mean(Age)   #39.74
mean(BMI)    #31.153
mean(Charges)  #12881.01
#Charges nre ---> log10(Charges)
mean(Charges.new) #3.938
mean(Children) #1
nrow(subset(data, Gender == "NA"))
nrow(subset(data, Smoker == "NA"))

#Conclusion: There are no missing data in any of the columns, if so it would affect the accuracy of the analysis
#Next: Checking the Irregularities of the data
summary(data)
boxplot(Age, main= "Age")
boxplot(BMI, main = "BMI")
boxplot(Charges)
boxplot(Charges.new, main= log10(Charges))
#our boxplot outliers show no errors


##################################################
               #Relationship Between Age and Medical Charges


plot(Age,Charges,  main= "Age vs Medical Charges", xlab= "Age", ylab= "Medical Charges")
cor(Age, Charges) #0.31499
               #Inference about population Correlation Coeffcient 
qt(0.975, df=492)  #1.964
#Decision RUle: Reject Ho if t >= 1.964778
#t + r(sqrt((n-2)/1-r^2))  #6.980
#We reject null hypothesis. 
#There is significant evidence that at the alpha =0.05, that there is a linear association between age of the customer and medical charges
cor.test(Age, Charges, alternative = "two.sided", method = "pearson")
#Smaller the p value (0.0000000000007692) strong evidence in favor of the alternative hypothesis


                  #Regression line

xbar1 <- mean(Age)
sx1 <- sd(Age)
ybar1 <- mean(Charges)
sy1<- sd(Charges)
r <- cor(Age, Charges)
beta1 <- r*sy1/sx1
beta1
beta0 <- ybar1 - beta1*xbar1
beta0

m1 <- lm(Charges~Age)
m1
abline(m1, lty=3, col="blue")

                        ###Simple Linear Regression F-Statistic 
qf(.95, df1=1, df2 = 492)

anova(m1)
summary(m1)
#54.15 >3.88 ---> Reject Ho ---?Evidence of linear association 



######################################
                     #Relationship between health background 
plot(BMI,Charges,  main= "BMI vs Medical Charges", xlab= "BMI", ylab= "Medical Charges")
cor(BMI, Charges) #0.206

                    #Inference about population Correlation Coefficient 
qt(0.975, df=492)  #1.964
#Decision RUle: Reject Ho if t >= 1.964778
#t + r(sqrt((n-2)/1-r^2))  #
#We reject null hypothesis. 
#There is significant evidence that at the alpha =0.05, that there is a linear association between age of the customer and medical charges
cor.test(BMI, Charges, alternative = "two.sided", method = "pearson")
#Smaller the p value (0.0000000000007692) strong evidence in favor of the alternative hypothesis


                    #Regression line

xbar2 <- mean(BMI)
sx2 <- sd(BMI)
ybar2 <- mean(Charges)
sy2<- sd(Charges)
r1 <- cor(BMI, Charges)
beta1.1 <- r1*sy2/sx2
beta1.1
beta0.1 <- ybar2 - beta1.1*xbar2
beta0.1

m2 <- lm(Charges~BMI)
m2
abline(m2, lty=3, col="Green")
summary(m2)


                ###Simple Linear Regression F-Statistic 

qf(.95, df1=1, df2 = 492)

anova(m2)
summary(m2) 
#21.808 >3.88 ---> Reject Ho ---?Evidence of linear association 


########################################

#Smoker vs Non-Smoker

plot(smoke.factor, Charges, main="Smoking Effects")

#Gender--> Females & Males

plot(gender.factor, Charges, main= "Gender")
 
###########################

#Children (dependencies)
plot(Children.factor , Charges, main= "Dependencies")

                     #Multi (AGE + BMI)


m3 <- lm(Charges~ Age + BMI)    #y = -8343.5 + 262.2X(Age) + 346.7(BMI)
m3
summary(m3)
numeric.data <- subset(data, select =c(Age, Charges, BMI))
numeric.data
cor(numeric.data)




                     #To test if Age and height are significant predictors 


#Multiple R-Squared is only 0.1313






############## Data Predictions###############################
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
 
data$Charges.new<- log10(data$Charges)    #log10 the response variable to stabilize
names(data)
Charges.new

# Split the data into Training and Test Data

set.seed(122)
training.samples <- data$Charges.new %>%
  createDataPartition(p=0.8, list = FALSE)
train <- data[training.samples, ]
test <- data[-training.samples,]

train$Gender <- factor(train$Gender)
train$Smoker <- factor(train$Smoker)
train$Children <- factor(train$Children)

test$Gender <- factor(test$Gender)
test$Smoker <- factor(test$Smoker)
test$Children <- factor(test$Children)
head(train)
head(test)
glimpse(test)
glimpse(train)

nrow(subset(train, train$BMI== "NA"))

formula <- as.formula("Charges.new~ BMI + Age + Gender + Smoker + Children")
model <- lm(formula, data=train)
model
summary(model)

prediction <- model %>% predict(test)
prediction
tail(data)
#Calculating Residuals
residuals <- data$Charges.new - prediction
rmse <- sqrt(mean(residuals^2))
rmse %>%
  round(digits =3) #0.174

predictions <- model %>% predict(train)
residuals <- 10^data$Charges.new - 10^prediction
rmse1 <- sqrt(mean(residualss^2))
round(rmse1)    #7724

##################################################################


