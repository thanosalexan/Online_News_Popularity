
dataset_21<-read.csv("alldata_onlinenews_21.csv",header=TRUE,dec=",",sep=";")

# QUESTION 1

str(dataset_21)
names(dataset_21)
View(dataset_21)

#removing non-useful columns
dataset_21 = subset(dataset_21, select = -c(X,url,timedelta,is_weekend))

# Checking for null/NaN values
dataset_21[dataset_21==""]<-NA

for (i in 1:ncol(dataset_21))
{
  na <- is.na(dataset_21[,i])
  inf <- is.infinite(dataset_21[,i])
  nan <- is.nan(dataset_21[,i])
}
any(na)
any(nan)
any(inf)

# Factor variables
dataset_21$weekday_is_monday<-factor(dataset_21$weekday_is_monday)
dataset_21$weekday_is_tuesday<-factor(dataset_21$weekday_is_tuesday)
dataset_21$weekday_is_wednesday<-factor(dataset_21$weekday_is_wednesday)
dataset_21$weekday_is_thursday<-factor(dataset_21$weekday_is_thursday)
dataset_21$weekday_is_friday<-factor(dataset_21$weekday_is_friday)
dataset_21$weekday_is_saturday<-factor(dataset_21$weekday_is_saturday)
dataset_21$weekday_is_sunday<-factor(dataset_21$weekday_is_sunday)


dataset_21$data_channel_is_bus<-factor(dataset_21$data_channel_is_bus)
dataset_21$data_channel_is_entertainment<-factor(dataset_21$data_channel_is_entertainment)
dataset_21$data_channel_is_lifestyle<-factor(dataset_21$data_channel_is_lifestyle)
dataset_21$data_channel_is_socmed<-factor(dataset_21$data_channel_is_socmed)
dataset_21$data_channel_is_tech<-factor(dataset_21$data_channel_is_tech)
dataset_21$data_channel_is_world<-factor(dataset_21$data_channel_is_world)

#Numeric variables
for (i in 1:(ncol(dataset_21)))
{if(class(dataset_21[,i])=='integer'){
  dataset_21[,i]<-as.numeric(dataset_21[,i])
}
}


# Create numeric variable dataset
library(psych)
index <- sapply(dataset_21, class) =='numeric'
numeric21 <- dataset_21[,index] 
round(t(describe(numeric21 <- dataset_21[,index])),2) 
n <- nrow(numeric21)

str(numeric21)


# Create factor variable dataset
factors21 <- dataset_21[,!index]

# Visualization numeric variables
par(mfrow=c(2,5))
for (i in 1:10)
{
  h1 <- hist(numeric21[,i], main=names(numeric21)[i], col='pink')
}
par(mfrow=c(2,5))
for (i in 11:20)
{
  h2 <- hist(numeric21[,i], main=names(numeric21)[i], col='pink')
}
par(mfrow=c(2,5))
for (i in 21:30)
{
  h3 <- hist(numeric21[,i], main=names(numeric21)[i], col='pink')
}
par(mfrow=c(2,5))
for (i in 31:40)
{
  h4 <- hist(numeric21[,i], main=names(numeric21)[i], col='pink')
}
par(mfrow=c(2,5))
for (i in 41:45)
{
  h5 <- hist(numeric21[,i], main=names(numeric21)[i], col='pink')
} 
  
# Visualization of factor variables
par(mfrow=c(1,1))
barplot(sapply(factors21[,c(1:6)],table)/n, names.arg = c("Lifestyle", "Entertainment", "Bus", "Socmed", "Tech", "World"), horiz=T, las=1, col=2:3, ylim=c(0,11), cex.names=0.7)
legend('topleft', fil=2:3, legend=c('No','Yes'), ncol=2,cex=0.5)
barplot(sapply(factors21[,c(7:13)],table)/n, names.arg = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), horiz=T, las=1, col=2:3, ylim=c(0,11), cex.names=0.8)
legend('topleft', fil=2:3, legend=c('No','Yes'), ncol=2, cex=0.5)

#oultiers_check
outliers_shares <- boxplot.stats(df_Numeric[,45])$out 
length(outliers_shares)
boxplot(df_Numeric[,45], main=names(df_Numeric)[45], boxwex=0.1)

dataset_21$shares <- log(dataset_21$shares)
numeric21$shares<- log(numeric21$shares)

# Pairs of shares and other numerical variables
pairs(df_Numeric[,c(45,1,2,3,4,5)])
pairs(df_Numeric[,c(45,6,7,8,9,10)])
pairs(df_Numeric[,c(45,11,12,13,14,15,16,17)])
pairs(df_Numeric[,c(45,18,19,20,21,22,23)])
pairs(df_Numeric[,c(45,24,25,26,27,28)])
pairs(df_Numeric[,c(45,29,30,31,32,33,34)])
pairs(df_Numeric[,c(45,35,36,37,38,39,40)])
pairs(df_Numeric[,c(45,41,42,43,44)])

require(corrplot)

corrplot(cor(df_Numeric[,c(45,1,2,3,4,5)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.37, tl.cex=0.37)
corrplot(cor(df_Numeric[,c(45,6,7,8,9,10)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.37, tl.cex=0.37)
corrplot(cor(df_Numeric[,c(45,11,12,13,14,15,16,17)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.55, tl.cex=0.55)
corrplot(cor(df_Numeric[,c(45,18,19,20,21,22,23)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.4, tl.cex=0.4)
corrplot(cor(df_Numeric[,c(45,24,25,26,27,28)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.6, tl.cex=0.6)
corrplot(cor(df_Numeric[,c(45,29,30,31,32,33,34)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.4, tl.cex=0.4)
corrplot(cor(df_Numeric[,c(45,35,36,37,38,39,40)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.4, tl.cex=0.4)
corrplot(cor(df_Numeric[,c(45,41,42,43,44)]),method='number',type = "upper", number.cex = .5, tl.offset = 0.5, tl.cex=0.5)


# Shares on each factor variable 
par(mfrow=c(2,3))
for(j in 1:6){
  boxplot(dataset_21[,58]~factors21[,j], xlab=names(factors21)[j], ylab='Shares',cex.lab=1.5) 
  abline(lm(dataset_21[,1]~factors21[,j]),col=2)
} 
par(mfrow=c(2,4))
for(j in 7:13){
  boxplot(dataset_21[,58]~factors21[,j], xlab=names(factors21)[j], ylab='Shares',cex.lab=1.5)
  abline(lm(dataset_21[,58]~factors21[,j]),col=2) 
}


###selecting attributes########

library(corrplot)
correlations <- cor(numeric21, use="pairwise.complete.obs")
#sort on decreasing correlations with SalePrice variable
cor_sorted <- as.matrix(sort(correlations[,"shares"], decreasing = TRUE))
#select only high correlations
high_correlations <- names(which(apply(cor_sorted, 1, function(x) (abs(x) < -0.04 || abs(x)> 0.04))))
correlations2 <- correlations[high_correlations, high_correlations]
corrplot(correlations2,method='number',type = "upper", number.cex = .28, tl.offset = 0.41, tl.cex=0.41)
high_correlations
#variable shares has stronger corellations with the variables included in matrix high_correlations


require(glmnet)
library(glmnet)
fullmodel21 <- lm(shares ~kw_avg_avg+num_hrefs+num_imgs+LDA_03+self_reference_max_shares+self_reference_avg_sharess+kw_min_avg+
                    self_reference_min_shares+global_sentiment_polarity+kw_max_avg+kw_avg_max+global_subjectivity+title_sentiment_polarity+num_keywords+num_videos+
                    n_tokens_content+abs_title_sentiment_polarity+title_subjectivity+num_self_hrefs+global_rate_positive_words+LDA_00+max_positive_polarity+
                    min_positive_polarity+n_unique_tokens+n_non_stop_unique_tokens+rate_negative_words+LDA_02+
                    weekday_is_monday+weekday_is_tuesday+weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+weekday_is_saturday+
                    weekday_is_sunday+data_channel_is_bus+data_channel_is_entertainment+data_channel_is_lifestyle+data_channel_is_socmed+
                  data_channel_is_tech+data_channel_is_world, data = dataset_21)

summary(fullmodel21)

X_21 <- model.matrix(fullmodel21)[,-1]
lasso <- glmnet(X_21, numeric21$shares, standardize = TRUE,alpha = 1)
library(plotmo) 
plot_glmnet(lasso,label=15)
lasso2 <- cv.glmnet(X_21,numeric21$shares, alpha = 1)
plot(lasso2)
#1se0.06905 min#0.0051
lassoModel1 <- coef(lasso2, s = lasso2$lambda.1se)
lassoModel


#Return coefficients with values
rownames(coef(lasso2, s = 'lambda.1se'))[coef(lasso2, s = 'lambda.1se')[,1]!= 0]

#Stepwise with coefficients of 'lambda.1se'

#AIC step
new_model <- lm(shares ~kw_avg_avg+num_hrefs+num_imgs+self_reference_max_shares+self_reference_avg_sharess+
                LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = dataset_21)
summary(new_model)

AIC<-step(new_model, direction='both') 
summary(AIC)

anova(AIC)

after_AIC_model <- lm(shares ~kw_avg_avg+num_hrefs+num_imgs+self_reference_avg_sharess+
                    LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = dataset_21) 
summary(after_AIC_model)

after_AIC_model_no_intercept<-lm(shares ~kw_avg_avg+num_hrefs+num_imgs+self_reference_avg_sharess+
                                   LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed-1, data = dataset_21) 
summary(after_AIC_model_no_intercept)

true.r2 <- 1-sum(after_AIC_model_no_intercept$residuals^2)/((n-1)*var(dataset_21$shares))

#VIF
install.packages('psych')
require(car)
vif(after_AIC_model)#all below 10


#Normality of the residuals

plot(after_AIC_model, which=2)
res <- after_AIC_model$residuals

library(nortest)
lillie.test(res)
shapiro.test(res)

#constant variance
Stud.residuals<-rstudent(after_AIC_model)
yhat <- fitted(after_AIC_model)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

library(car)
ncvTest(after_AIC_model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(after_AIC_model)~yhat.quantiles)

#non-linearity#
library(car)

par(mfrow=c(1,1))
residualPlot(after_AIC_model, type='rstudent')
residualPlots(after_AIC_model, plot=F, type = "rstudent")

#Independence of errors


library(car)
durbinWatsonTest(after_AIC_model)

plot(rstudent(after_AIC_model), type='l')

##Use polynomials###########################################################

model2<-lm(shares ~kw_avg_avg+I(kw_avg_avg^2)+I(kw_avg_avg^3)+num_hrefs+num_imgs+self_reference_avg_sharess+
             LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = dataset_21) 


residualPlots(model2,plot=F)
summary(model2)

#Normality of the residuals

plot(model2, which=2)
res2 <- model2$residuals

library(nortest)

lillie.test(res2)
shapiro.test(res2)

#constant variance
Stud.residuals2<-rstudent(model2)
yhat2 <- fitted(model2)
par(mfrow=c(1,2))
plot(yhat2, Stud.residuals2)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat2, Stud.residuals2^2)
abline(h=4, col=2, lty=2)

library(car)
ncvTest(model2)
yhat.quantiles2<-cut(yhat2, breaks=quantile(yhat2, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles2)
leveneTest(rstudent(model2)~yhat.quantiles2)

#non-linearity#
library(car)

par(mfrow=c(1,1))
residualPlot(model2, type='rstudent')
residualPlots(model2, plot=F, type = "rstudent")

#Independence of errors


library(car)
durbinWatsonTest(model2)

plot(rstudent(model2), type='l')



#### Prediction - test dataset #########################################

test_dataset<-read.csv("OnlineNewsPopularity_test.csv",header=TRUE,dec=",",sep=";")
View(test_dataset)

test_dataset = subset(test_dataset, select = -c(X,url,timedelta,is_weekend))

test_dataset$weekday_is_monday<-factor(test_dataset$weekday_is_monday)
test_dataset$weekday_is_tuesday<-factor(test_dataset$weekday_is_tuesday)
test_dataset$weekday_is_wednesday<-factor(test_dataset$weekday_is_wednesday)
test_dataset$weekday_is_thursday<-factor(test_dataset$weekday_is_thursday)
test_dataset$weekday_is_friday<-factor(test_dataset$weekday_is_friday)
test_dataset$weekday_is_saturday<-factor(test_dataset$weekday_is_saturday)
test_dataset$weekday_is_sunday<-factor(test_dataset$weekday_is_sunday)

test_dataset$data_channel_is_bus<-factor(test_dataset$data_channel_is_bus)
test_dataset$data_channel_is_entertainment<-factor(test_dataset$data_channel_is_entertainment)
test_dataset$data_channel_is_lifestyle<-factor(test_dataset$data_channel_is_lifestyle)
test_dataset$data_channel_is_socmed<-factor(test_dataset$data_channel_is_socmed)
test_dataset$data_channel_is_tech<-factor(test_dataset$data_channel_is_tech)
test_dataset$data_channel_is_world<-factor(test_dataset$data_channel_is_world)



test_dataset$shares <- log(test_dataset$shares)


#1- - fold cross validation  Choose min RMSE 
#First Model
install.packages('caret')
library(caret)
set.seed(1)
train_control_CV_10 <- trainControl(method = "CV", number = 10)
model_cv_10_1 <- train(shares ~kw_avg_avg+num_hrefs+num_imgs+self_reference_avg_sharess+
                      LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = test_dataset,
                 trControl=train_control_CV_10,
                 method="lm")
model_cv_10_1

# Leave one out cross validation
train_control_LOOCV <- trainControl(method = "LOOCV")
model_LOOCV_1<- train(shares ~kw_avg_avg+num_hrefs+num_imgs+self_reference_avg_sharess+
                                          LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = test_dataset,
                       trControl=train_control_LOOCV, method="lm")
model_LOOCV_1

#1- - fold cross validation  Choose min RMSE 
#Second model
model_cv_10_2 <- train(shares ~kw_avg_avg+I(kw_avg_avg^2)+I(kw_avg_avg^3)+num_hrefs+num_imgs+self_reference_avg_sharess+
                         LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = test_dataset,
                       trControl=train_control_CV_10,
                       method="lm")
model_cv_10_2

# Leave one out cross validation
model_LOOCV_2<- train(shares ~kw_avg_avg+I(kw_avg_avg^2)+I(kw_avg_avg^3)+num_hrefs+num_imgs+self_reference_avg_sharess+
                                          LDA_02+weekday_is_saturday+weekday_is_sunday+data_channel_is_entertainment+data_channel_is_socmed, data = test_dataset,
                                        trControl=train_control_LOOCV, method="lm")
model_LOOCV_2

results<- rbind(model_cv_10_1$results[1:4],model_LOOCV_1$results,
                      model_cv_10_2$results[1:4],model_LOOCV_2$results)
rownames(results)<- c("model_cv_10_1","model_LOOCV_1","model_cv_10_2","model_LOOCV_2")



