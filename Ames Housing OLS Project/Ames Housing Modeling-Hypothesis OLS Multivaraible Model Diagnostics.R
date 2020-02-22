library(ggplot2)
library(ggplot)
library(gridExtra)
library(corrplot)
library(dplyr)
library(psych)
library(car)      
library(gvlma)

library(stargazer)
library(foreign)  
library(effects) 
library(sjPlot)  
library(MASS) 
library(relaimpo) 

mydata <- read.csv(file.path("C:/Users/syurasek/OneDrive - Constellation Brands/Documents/Northwestern/PREDICT 410/Assignment 1", "ames_housing_data.csv"),head=TRUE,sep=",")
str(mydata)
summary(mydata) 
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
#Review summary of new mydata set
summary(mydata)

cleandata <- select(mydata, -Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature) 

for (col_name in colnames(cleandata[sapply(cleandata, is.numeric) == TRUE])) {
  
  if (sum(is.na(cleandata[[col_name]])) > 0) {
    cleandata[col_name][is.na(cleandata[col_name])] <- mean(cleandata[[col_name]], na.rm = T)
    stmt <- paste('Null values of', col_name, ' Null has been replaced with mean value ', mean(cleandata[[col_name]], na.rm = T))
    print(stmt, na.print = NULL)
  }
}


## Identify categorical variables and treat them with mode, highest frequent value.  

Mode = function(x){
  ta = table(x)
  tam = max(ta)
  mod = names(ta)[ta==tam]
  return(mod)
}

for (col_name in colnames(cleandata[sapply(cleandata, is.factor) == TRUE])) {
  
  if (sum(is.na(cleandata[[col_name]])) > 0) {
    cleandata[col_name][is.na(cleandata[col_name])] <- Mode(cleandata[[col_name]])
    stmt <- paste('Null values of', col_name, ' Null has been replaced by mode value ', Mode(cleandata[[col_name]]))
    print(stmt, na.print = NULL)
  }
}


summary(cleandata)

cleandata<-filter(cleandata,BldgType == "1Fam")
cleandata<-filter(cleandata,Zoning %in% c("RH", "RL", "RM", "FV"))
cleandata<-filter(cleandata,SaleCondition == "Normal")
cleandata <- cleandata[cleandata$GrLivArea<=4000,]
cleandata <- cleandata[cleandata$TotalFloorSF<=4000,]
cleandata <- cleandata[cleandata$SalePrice<=500000,]


#pairs.panels(cleandata, col="red")


model1<-lm(formula = SalePrice ~ TotalFloorSF, data = cleandata)
summary(model1)
anova(model1)

plot(SalePrice ~ TotalFloorSF, data = cleandata, main = "SF by SalePrice" )
abline(model1, col='red')

layout(matrix(c(1,2,3,4),2,2)) 
plot(model1)

outlierTest(model1) 
#histogram studentized residuals
sresid <- studres(model1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Model 1 Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


#cook's distance

cutoff <- 4/((nrow(cleandata)-length(model1$coefficients)-2)) 
plot(model1, which=4, cook.levels=cutoff)

###Model 2

model2<-lm(formula = SalePrice ~ OverallQual, data = cleandata)
summary(model2)
anova(model2)

plot(SalePrice ~ OverallQual, data = cleandata, main = "Overall Quality by SalePrice" )
abline(model2, col='red')

layout(matrix(c(1,2,3,4),2,2)) 
plot(model2)

outlierTest(model2) 

model3<-lm(formula = SalePrice ~ TotalFloorSF+OverallQual, data = cleandata)
summary(model3)
anova(model3)

plot(SalePrice ~ TotalFloorSF+OverallQual, data = cleandata, main = "Overall Quality by SalePrice" )
abline(model3, col='red')

layout(matrix(c(1,2,3,4),2,2)) 
plot(model3)

sresid <- studres(model3) 
hist(sresid, freq=FALSE, 
     main="Distribution of Model 3 Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

outlierTest(model3)

#Model 4

model4<-lm(formula = SalePrice ~ TotalFloorSF+OverallQual+GrLivArea+TotalBsmtSF+GarageArea, data = cleandata)
summary(model4)
anova(model4)

layout(matrix(c(1,2,3,4),2,2)) 
plot(model4)

sresid <- studres(model4) 
hist(sresid, freq=FALSE, 
     main="Distribution of Model 4 Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

outlierTest(model4)

crPlots(model4)


#PART C:  Multiple Linear Regression Models on Transformed Response Variable

model1b<-lm(formula = logSalePrice ~ TotalFloorSF, data = cleandata)
summary(model1b)
anova(model1b)

plot(logSalePrice ~ TotalFloorSF, data = cleandata, main = "SF by Log SalePrice" )
abline(model1b, col='red')

layout(matrix(c(1,2,3,4),2,2)) 
plot(model1b)


model3b<-lm(formula = logSalePrice ~ TotalFloorSF+OverallQual, data = cleandata)
summary(model3b)
anova(model3b)

plot(logSalePrice ~ TotalFloorSF+OverallQual, data = cleandata, main = "Overall Quality by SalePrice" )
abline(model3b, col='red')

layout(matrix(c(1,2,3,4),2,2)) 
plot(model3b)

model4b<-lm(formula = logSalePrice ~ TotalFloorSF+OverallQual+GrLivArea+TotalBsmtSF+GarageArea, data = cleandata)
summary(model4b)
anova(model4b)

layout(matrix(c(1,2,3,4),2,2)) 
plot(model4b)

crPlots(model4b)


####deffits-Hadi plot 
plot(dffits(model4), cex=0.5)
library(olsrr)
ols_plot_dffits(model4, cex=0.5)

###
cutoff <- 4/((nrow(cleandata)-length(model4$coefficients)-2)) 
plot(model4, which=4, cook.levels=cutoff)
###

#### Diff method getting to cook SD influential point####
mod <- lm(SalePrice ~ TotalFloorSF+OverallQual+GrLivArea+TotalBsmtSF+GarageArea,data = cleandata)
cooksd <- cooks.distance(model4)
sample_size <- nrow(cleandata)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

####Removal of the influential####
cleandata_1 <- cleandata[-influential, ]

mod_1 <- lm(SalePrice ~ TotalFloorSF+OverallQual+GrLivArea+TotalBsmtSF+GarageArea,data = cleandata_1)
summary(mod_1)
sample_size_1=nrow(cleandata_1)

cooksd_1 <- cooks.distance(mod_1)

#### Replot Cook SD####
plot(cooksd_1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size_1, col="red")  # add cutoff line
text(x=1:length(cooksd_1)+1, y=cooksd_1, labels=ifelse(cooksd_1>4/sample_size_1, names(cooksd_1),""), col="red")  # add labels

layout(matrix(c(1,2,3,4),2,2)) 
plot(mod_1)

ols_plot_dffits(mod_1)

ols_plot_cooksd_bar(model4)
ols_plot_cooksd_bar(mod_1)

####Model 5 Final model

model5<-lm(formula = logSalePrice ~ TotalFloorSF+OverallQual+TotalBsmtSF+GarageArea+FullBath+LotArea+Fireplaces+GarageCars+YearRemodel, data = cleandata)
summary(model5)
anova(model5)


layout(matrix(c(1,2,3,4),2,2)) 
plot(model5)

sresid <- studres(model5) 
hist(, freq=FALSE, 
     main="Model 5 Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


ols_plot_dffits(model5)

ols_plot_cooksd_bar(model5)
ols_plot_cooksd_chart(model5)
outlierTest(model5)

##############################extra################################

# read dataset
df = mtcars

# create multiple linear model
lm_fit <- lm(mpg ~ cyl + hp, data=df)
summary(lm_fit)

# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(mpg_pred = predict(lm_fit, df), hp=df$hp)

# this is the predicted line of multiple linear regression
ggplot(data = df, aes(x = mpg, y = hp)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = predicted_df, aes(x=mpg_pred, y=hp))



confint(model_3, level=0.99)
predict(model_3)
library(psych)
install.packages("psych")
pairs.panels(M3, col="red")

M4<-select(cleandata, SalePrice,OverallQual,HouseAge,YearBuilt,TotalFloorSF,TotalBsmtSF,GrLivArea)
model_4 <- lm(formula = SalePrice ~ OverallQual+HouseAge+YearBuilt+TotalFloorSF+TotalBsmtSF+GrLivArea, data = M4)
summary(model_4)
anova(model_4)

pairs.panels(M4, col="red")
