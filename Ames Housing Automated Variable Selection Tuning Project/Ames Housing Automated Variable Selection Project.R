#Ames Housing Varible slection and tunning

library(ggplot2)
library(ggplot)
library(gridExtra)
library(corrplot)
library(dplyr)
library(corrr)
library(MASS)
library(dummies)

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)


#Intro 1
mydata <- read.csv(file.path("C:/Users/syurasek/OneDrive - Constellation Brands/Documents/Northwestern/PREDICT 410/Assignment 1", "ames_housing_data.csv"),head=TRUE,sep=",")
str(mydata)

###adding new features 
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
#Review summary of new mydata set
summary(mydata)


newdata<-filter(mydata,BldgType == "1Fam")
newdata<-filter(newdata,Zoning %in% c("RH", "RL", "RM", "FV"))
newdata<-filter(newdata,SaleCondition == "Normal")


cleandata.numeric.col <- unlist(lapply(cleandata, is.numeric))
cleandata.numeric <- cleandata[, cleandata.numeric.col]

#Great code:https://www.kaggle.com/pradeeptripathi/predicting-house-prices-using-r
  missing_row <- newdata[!complete.cases(newdata),]
  head(missing_row)
  nrow(missing_row)
  colSums(sapply(newdata, is.na))
#Step 1 remove variables with large % of missing data
cleandata <- select(newdata, -Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature) 

#Step 2 replace numeric variable missing value with mean

## Identify numeric variables and treat them with mean value if any missing / null values exists
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

## Identify numeric variables and treat them with mean value if any missing / null values exists
for (col_name in colnames(cleandata[sapply(cleandata, is.factor) == TRUE])) {
  
  if (sum(is.na(cleandata[[col_name]])) > 0) {
    cleandata[col_name][is.na(cleandata[col_name])] <- Mode(cleandata[[col_name]])
    stmt <- paste('Null values of', col_name, ' Null has been replaced by mode value ', Mode(cleandata[[col_name]]))
    print(stmt, na.print = NULL)
  }
}

#recheck the newdata for any missing values
colSums(is.na(cleandata))


#Numberial vs categorical data check
numeric_variables = which(sapply(cleandata, is.numeric))
numeric_values = cleandata[,numeric_variables]
categoriacal_vars = which(sapply(cleandata, is.factor))
paste("numeric variables are ", length(numeric_values))
paste("categoriacal variables are ", length(categoriacal_vars))

#calculate correlation of all numeric variables against each other(did not use for this project)
df <- dplyr::select_if(cleandata, is.numeric)
r <- cor(df, cleandata$saleprice)
round(r,2)


x <- df %>% 
  correlate() %>% 
  focus(SalePrice)
x

#Remove extreme outliers

cleandata <- cleandata[cleandata$GrLivArea<=4000,]
cleandata <- cleandata[cleandata$TotalFloorSF<=4000,]
cleandata <- cleandata[cleandata$SalePrice<=500000,]

# 1. Preparing the Categorical Variables  

cleandata %>%
  select_if(is.factor) %>%
  names()


summary(cleandata$Neighborhood)


ggplot(cleandata) +
  geom_bar( aes(Neighborhood) ) +
  ggtitle("Number of houses per Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))

hood_mean <-
  aggregate(SalePrice ~ Neighborhood, data = cleandata, FUN = mean)
hood_mean

m1<-lm(SalePrice~Neighborhood, data=cleandata)
summary(m1)

RoofStyle_mean <-
  aggregate(SalePrice ~ RoofStyle, data = cleandata, FUN = mean)
RoofStyle_mean

m2<-lm(SalePrice~RoofStyle, data=cleandata)
summary(m2)

Exterior1 <-
  aggregate(SalePrice ~ Exterior1, data = cleandata, FUN = mean)
Exterior1

m3<-lm(SalePrice~Exterior1, data=cleandata)
summary(m3)

MasVnrType <-
  aggregate(SalePrice ~ MasVnrType, data = cleandata, FUN = mean)
MasVnrType

m4<-lm(SalePrice~MasVnrType, data=cleandata)
summary(m4)

Foundation <-
  aggregate(SalePrice ~ Foundation, data = cleandata, FUN = mean)
Foundation

m5<-lm(SalePrice~Foundation, data=cleandata)
summary(m5)

Heating <-
  aggregate(SalePrice ~ Heating, data = cleandata, FUN = mean)
Heating

m6<-lm(SalePrice~Heating, data=cleandata)
summary(m6)

Zoning <-
  aggregate(SalePrice ~ Zoning, data = cleandata, FUN = mean)
Zoning

m7<-lm(SalePrice~Zoning, data=cleandata)
summary(m7)

ExterQual <-
  aggregate(SalePrice ~ ExterQual, data = cleandata, FUN = mean)
ExterQual

m8<-lm(SalePrice~ExterQual, data=cleandata)
summary(m8)

m9<-lm(SalePrice~BsmtQual, data=cleandata)
summary(m9)

BsmtQual <-
  aggregate(SalePrice ~ BsmtQual, data = cleandata, FUN = mean)
BsmtQual

m10<-lm(SalePrice~HouseStyle, data=cleandata)
summary(m10)

m11<-lm(SalePrice~Exterior2, data=cleandata)
summary(m11)

m12<-lm(SalePrice~KitchenQual, data=cleandata)
summary(m12)

kitchenQual <-
  aggregate(SalePrice ~ KitchenQual, data = cleandata, FUN = mean)
kitchenQual

m13<-lm(SalePrice~GarageFinish, data=cleandata)
summary(m13)

m14<-lm(SalePrice~BsmtFinType1, data=cleandata)
summary(m14)

m15<-lm(SalePrice~PavedDrive, data=cleandata)
summary(m15)

Combo<-lm(SalePrice~KitchenQual+BsmtQual+ExterQual, data=cleandata)
summary(Combo)

ggplot(cleandata, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Price by Neighborhood")+
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(cleandata, aes(x = Foundation, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Price by Foundation")+
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(cleandata, aes(x = ExterQual, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Price by Exterial Quality")+
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(cleandata, aes(x = KitchenQual, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Price by Kitchen Quality")+
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(cleandata, aes(x = BsmtQual, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Price by Basement Quality")+
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +theme(axis.text.x=element_text(angle=90,hjust=1))

# Define one new variable for later use;

cleandata$TotalSqftCalc <- cleandata$BsmtFinSF1+cleandata$BsmtFinSF2+cleandata$GrLivArea;

#Dummy creation for 4 categoricals

new<- dummy.data.frame(cleandata, names = c("KitchenQual","BsmtQual","Foundation", "ExterQual") , sep = ".")


#Train and test split
#Set the seed on the random number generator so you get the same split every time that you run the code.
set.seed(123)
new$u <- runif(n=dim(new)[1],min=0,max=1);

# Create train/test split;
train.df <- subset(new, u<0.70);
test.df  <- subset(new, u>=0.70);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(new)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

# Summary of observations
knitr::kable(data.frame(
  "DataFrame" = c("train.df", "test.df"),
  "ObsCounts" = c(nrow(train.df), nrow(test.df)),
  "PercentOfObs" = c(nrow(train.df) / nrow(new), nrow(test.df) / nrow(new))
))

keep.list <-  list('SalePrice', 'QualityIndex','TotalSqftCalc','YrSold', 'FullBath','GarageArea','GarageCars',
                   'TotRmsAbvGrd','LotArea','MasVnrArea','WoodDeckSF',
                   'BsmtQual.Ex','BsmtQual.Fa', 'BsmtQual.Gd','BsmtQual.TA', 
                   'KitchenQual.Fa','KitchenQual.Gd', 'KitchenQual.Po', 'KitchenQual.TA','KitchenQual.Ex',
                   'ExterQual.TA', 'ExterQual.Ex', 'ExterQual.Fa', 'ExterQual.Gd')

train.clean1 <-train.df[, (names(new) %in% keep.list)];

str(train.clean1)

# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean1);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean1);
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean1);
summary(sqft.lm)


# There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

#Junk model is to demonstrate the high degree of collinearity
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)

# Compute the VIF values. if you have a very large VIF value (like 20, 30, 50 etc.), then you should consider removing 
#a variable so that your variable selection models are not junk too
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

#In sample review for 4 models above and calculate AIC BIC MSE, MAE
AIC(forward.lm, k=2)
AIC(backward.lm, k=2)
AIC(stepwise.lm, k=2)
AIC(junk.lm, k=2)

BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

#mse.1 <- mean(model.1$residuals^2)
#mae.1 <- mean(abs(model.1$residuals))
# Mae and Mse calculations 
mean(abs(forward.lm$residuals))
mean(forward.lm$residuals^2)

mean(abs(backward.lm$residuals))
mean(backward.lm$residuals^2)

mean(abs(stepwise.lm$residuals))
mean(stepwise.lm$residuals^2)

mean(abs(junk.lm$residuals))
mean(junk.lm$residuals^2)

#out of sample testing evalaution-prediction accuracy
forward.test = predict(forward.lm, newdata = test.df)
summary(forward.test)

backward.test = predict(backward.lm, newdata = test.df)
summary(backward.test)

stepwise.test = predict(stepwise.lm, newdata = test.df)
summary(stepwise.test)

junk.test = predict(junk.lm, newdata = test.df)
summary(junk.test)

fwd.error = forward.test - test.df$SalePrice
back.error = backward.test - test.df$SalePrice
step.error = stepwise.test - test.df$SalePrice
junk.error = junk.test - test.df$SalePrice

mean(abs(fwd.error))
mean(fwd.error^2)

mean(abs(back.error))
mean(back.error^2)

mean(abs(step.error))
mean(step.error^2)

mean(abs(junk.error))
mean(junk.error^2)

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean1$SalePrice;

# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)
#####################

# Training Data
# Abs Pct Error
backward.pct <- abs(backward.lm$residuals)/train.clean1$SalePrice;

# Assign Prediction Grades;
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

backward.trainTable <- table(backward.PredictionGrade)
backward.trainTable/sum(backward.trainTable)

############
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean1$SalePrice;

# Assign Prediction Grades;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)

#############################

junk.pct <- abs(junk.lm$residuals)/train.clean1$SalePrice;

junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)


# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;


# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

###
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

###
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)

###
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                       ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)

###Final model review and tuning 

# KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA
# ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA
# BsmtQual. + BsmtQual.Ex + BsmtQual.Fa + BsmtQual.Gd + BsmtQual.Po + BsmtQual.TA

summary(forward.lm)

modelbase <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + FullBath + GarageArea + GarageCars + TotRmsAbvGrd + LotArea + MasVnrArea + WoodDeckSF +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
            KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
            ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary(modelbase)

#remove GarageArea

r1 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + FullBath  + GarageCars + TotRmsAbvGrd + LotArea + MasVnrArea + WoodDeckSF +
                 BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
                 KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
                 ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary(r1)

#remove wooddeck

r2 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + FullBath  + GarageCars + TotRmsAbvGrd + LotArea + MasVnrArea +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary(r2)

#remove fullbath

r3 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc  + GarageCars + TotRmsAbvGrd + LotArea + MasVnrArea +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary(r3)

#remove garagecars

r4 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + TotRmsAbvGrd + LotArea + MasVnrArea +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary(r4)

#remove totrmsabvgrd

r5 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + LotArea + MasVnrArea +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary (r5)

#remove lotarea

r6 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc + MasVnrArea +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary (r6)

#remove masvnrArea

r7 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex +
          ExterQual.Ex + ExterQual.Fa + ExterQual.Gd + ExterQual.TA, data = train.clean1)
summary (r7)


#remove ExterQual

r8 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd + 
          KitchenQual.Fa + KitchenQual.Gd + KitchenQual.Po + KitchenQual.TA + KitchenQual.Ex, data = train.clean1)
summary (r8)


#remove KitchenQual

r9 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd, data = train.clean1)
summary (r9)

#remove quality index? Close to 10% drop, bring it back into the model
r9.1 <-lm(SalePrice ~ TotalSqftCalc +
          BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd, data = train.clean1)
summary (r9.1)

r9.2 <-lm(SalePrice ~ QualityIndex+
            BsmtQual.Ex + BsmtQual.Fa + BsmtQual.TA +BsmtQual.Gd, data = train.clean1)
summary (r9.2)


#remove BasementQual- this indicating we show 12% drop, 
#so we need to keep this varaible and final model tunning can stop
r10 <-lm(SalePrice ~ QualityIndex + TotalSqftCalc, data = train.clean1)
summary (r10)


final <-lm(SalePrice ~ QualityIndex + TotalSqftCalc +
          BsmtQual.Ex + BsmtQual.Fa +BsmtQual.TA, data = train.clean1)
summary (final)

mean(abs(final$residuals))
mean(final$residuals^2)


final.test = predict(final, newdata = test.df)

final.error = final.test - test.df$SalePrice

mean(abs(final.error))
mean(final.error^2)


final.testPCT <- abs(test.df$SalePrice-final.test)/test.df$SalePrice;


# Assign Prediction Grades;
final.testPredictionGrade <- ifelse(final.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(final.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(final.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

final.testTable <-table(final.testPredictionGrade)
final.testTable/sum(final.testTable)

layout(matrix(c(1,2,3,4),2,2)) 
plot(final)
outlierTest(final)

#anova effect evaluation by diving deeper
r11 <-lm(SalePrice ~ QualityIndex+TotalSqftCalc+BsmtQual.Ex+BsmtQual.Fa+BsmtQual.TA+
            QualityIndex*BsmtQual.Ex + QualityIndex*BsmtQual.Fa + QualityIndex*BsmtQual.TA, data = train.clean1)
summary (r11)

anova(final,r11)

r9.4 <-lm(SalePrice ~ QualityIndex+TotalSqftCalc+BsmtQual.Ex+BsmtQual.Fa+BsmtQual.TA+
            TotalSqftCalc*BsmtQual.Ex + TotalSqftCalc*BsmtQual.Fa + TotalSqftCalc*BsmtQual.TA, data = train.clean1)
summary (r9.4)

anova(r9.4, final)

library(ggpubr)
ggscatter(
  cleandata, x = "QualityIndex", y = "SalePrice",
  color = "BsmtQual" , add = "reg.line")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = BsmtQual)
  )


ggscatter(
  cleandata, x = "TotalSqftCalc", y = "SalePrice",
  color = "BsmtQual" , add = "reg.line")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = BsmtQual)
  )

final.2<-lm(SalePrice ~ QualityIndex+TotalSqftCalc+
            TotalSqftCalc*BsmtQual.Ex + TotalSqftCalc*BsmtQual.Fa + TotalSqftCalc*BsmtQual.TA+
              QualityIndex*BsmtQual.Ex + QualityIndex*BsmtQual.Fa + QualityIndex*BsmtQual.TA , data = train.clean1)
summary (final.2)
anova (final.2)

###diagnostic testing
layout(matrix(c(1,2,3,4),2,2)) 
plot(final.2)
outlierTest(final.2)


train.clean1$pred_final<- predict(final.2)
train.clean1$residuals <- residuals(final.2)

hist(train.clean1$residuals)

#actual vs predict plot using totalsqft and saleprice

ggplot(train.clean1, aes(x = TotalSqftCalc, y = SalePrice)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = TotalSqftCalc, yend = pred_final), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  
  geom_point(aes(y = pred_final), shape = 1) +
  ggtitle("Plot of Residuals FinalModel Actual vs Predicted")+
  theme_bw()

#actual vs predict plot using QualityIndex and saleprice

ggplot(train.clean1, aes(x = QualityIndex, y = SalePrice)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = QualityIndex, yend = pred_final), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  
  geom_point(aes(y = pred_final), shape = 1) +
  ggtitle("Plot of Residuals FinalModel Actual vs Predicted")+
  theme_bw()

mean(abs(final.2$residuals))
mean(final.2$residuals^2)


final.2.test = predict(final.2, newdata = test.df)

final.2.error = final.2.test - test.df$SalePrice

mean(abs(final.2.error))
mean(final.2.error^2)

###what if using a simpler model? As a part of the final reflection

r12 <-lm(SalePrice ~ QualityIndex+TotalSqftCalc, data = train.clean1)
summary (r12)

layout(matrix(c(1,2,3,4),2,2)) 
plot(r12)
outlierTest(r12)

train.clean1$pred_final<- predict(r12)
train.clean1$residuals<- residuals(r12)

hist(train.clean1$residuals)

mean(abs(r12$residuals))
mean(r12$residuals^2)


r12.test = predict(r12, newdata = test.df)
r12.error = r12.test - test.df$SalePrice

mean(abs(r12.error))
mean(r12.error^2)


r12.testPCT <- abs(test.df$SalePrice-r12.test)/test.df$SalePrice;


# Assign Prediction Grades;
r12.testPredictionGrade <- ifelse(r12.testPCT<=0.10,'Grade 1: [0.0.10]',
                                    ifelse(r12.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                           ifelse(r12.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                  'Grade 4: (0.25+]')
                                    )					
)
r12.testTable <-table(r12.testPredictionGrade)
r12.testTable/sum(r12.testTable)