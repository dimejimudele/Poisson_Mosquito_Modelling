#install.packages("randomForest")
library(randomForest)
library(ggplot2)
library(MASS)
library(ggthemes)
theme_set(theme_bw())

############################################################################
training = "./training.csv" #Define location of training csv data set on your local drive
test = "./testcsv"     #Define location of test csv data set on your local drive

############################################################################

# Roote mean square error canculator
rmse <- function(x,x_hat)
{
  error<-x-x_hat
  sqrt(mean(error^2))
}

# Mean squared error calculator
mse <- function(x,x_hat)
{
  error<-x-x_hat
  mean(error^2)
}

# Mean absolute percentual error - does not work with zero values
mape <- function(x,x_hat)
{
  error<-abs((x-x_hat)/x)
  100*mean(error)
}

#Mean Absolute Error calculator
mae <- function(x,x_hat)
{
  error<-abs(x-x_hat)
  mean(error)
}


#Read Training Data
data_training_input <-read.csv(training)

#Read Validation Data
data_validation_input <-read.csv(test)

head(data_training_input)
head(data_validation_input)

#Remove rows with nan values
data_training <- as.data.frame(na.omit(data_training_input)) 
data_validation<- as.data.frame(na.omit(data_validation_input)) 


f = as.formula(
  y~ NDVI_U +	NDVI_R +	NDWI_U +	NDWI_R +	TempD_U +	TempD_R +	
    TempN_U +	TempN_R+	Prec_U +	Prec_R+	NDVI_U1+	NDVI_R1+	NDWI_U1+	NDWI_R1+	TempD_U1+	TempD_R1+	
    TempN_U1+	TempN_R1+	Prec_U1 +	Prec_R1+	NDVI_U2+	NDVI_R2+	NDWI_U2+	NDWI_R2+	TempD_U2+	TempD_R2+	
    TempN_U2+	TempN_R2+	Prec_U2 +	Prec_R2
)

yt = data_training[,2] # column 9 contain the mean of mosquitos
yv = data_validation[,2] # column 9 contain the mean of mosquitos
week = data_validation[,1]

#Standardizing the data (z-score)
data_training_ <- as.data.frame(scale(data_training))
data_validation_ <- as.data.frame(scale(data_validation))

#Obtaining the mean and standard deviation so as the return the prediction back to the scale of the data
mt = mean(yt)
st = sd(yt)

mv = mean(yv)
sv = sd(yv)

###############################################################################################
# RANDOM FOREST FULL FEATURES
###############################################################################################
rf <- randomForest(f, data=data_training_, ntree=500)

pt = predict(rf, data_training_)*st+mt
pv = predict(rf, data_validation_)*sv+mv

training_quality_rf= mae(yt,pt)
validation_quality_rf= mae(yv,pv)

training_quality_rf
validation_quality_rf

df <- data.frame(week, yv, pv)

ggplot(df, aes(week, y = value , color = Variable)) + 
  geom_point(aes(y = yv, col = "Observed"), size=4) + 
  geom_line(aes(y = pv, col = "Fitted to RF"), size=1.5) +
  labs( y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.5, 0.82))+
  theme(legend.title=element_blank())

pt_full_rf = pt
pv_full_rf = pv
description_rf = c(min(pv_full_rf),quantile(pv_full_rf,0.25),median(pv_full_rf),mean(pv_full_rf),quantile(pv_full_rf,0.75),max(pv_full_rf)) 
description_rf

###############################################################################################
# RANDOM FOREST IMPORTANT FEATURES
###############################################################################################
important=c()
for(i in 1:50)
{
  rf_temp <- randomForest(f, data=data_training_)
  important = cbind(important,importance(rf_temp) )
}
m_imp=apply(important,1,mean)
sort(m_imp)
plot(sort(m_imp))


imp = matrix(m_imp,ncol=1)
colnames(imp)="MDI" # colnames(rf$importance)
vnames = c("NDVI-U",   "NDVI-R"  , "NDWI-U"  , "NDWI-R"  , "TempD-U" , "TempD-R" , "TempN-U",  "TempN-R",  "Prec-U",   "Prec-R",  
           "NDVI-U1",  "NDVI-R1"  ,"NDWI-U1"  ,"NDWI-R1"  ,"TempD-U1" ,"TempD-R1", "TempN-U1", "TempN-R1", "Prec-U1",  "Prec-R1", 
           "NDVI-U2",  "NDVI-R2"  ,"NDWI-U2"  ,"NDWI-R2"  ,"TempD-U2" ,"TempD-R2", "TempN-U2", "TempN-R2", "Prec-U2",  "Prec-R2" )
rownames(imp)= vnames #  rownames(rf$importance)
imp
varImpPlot(rf_temp,main=" ") 


# this is the plot part, be sure to use reorder with the correct measure name
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp)
rownames(imp) <- NULL
#imp$var_categ <- rep(1:2, 5)
imp
ggplot(imp, aes(x=reorder(varnames, MDI), y=MDI)) + 
  geom_point( ) +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=MDI)) +
  scale_color_discrete(name="Variable Group") +
  ylab("MDI") +
  xlab("Climatic  variable") +
  coord_flip()+
  theme(text = element_text(size=15))
ggsave('plots/RF_importance_rank.pdf')

f_rf_important = as.formula(
  y~ TempN_R1 + TempN_R + TempD_R1 + TempD_R + NDWI_R2 + TempN_U + NDWI_R1 + NDWI_U1 + TempN_R2 + TempD_R2)

rf_filtered = randomForest(f_rf_important, data=data_training_, ntree=500)

pt = predict(rf_filtered, data_training_)*st+mt
pv = predict(rf_filtered, data_validation_)*sv+mv

description_rf_filtered = c(min(pv),quantile(pv,0.25),median(pv),mean(pv),quantile(pv,0.75),max(pv)) 
description_rf_filtered  
training_quality_rf= mae(yt,pt)
validation_quality_rf= mae(yv,pv)

training_quality_rf
validation_quality_rf

df <- data.frame(week, yv, pv)

ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=4) + 
  geom_line(aes(y = pv_full_rf, col = "RF"), size=1.5) +
  geom_line(aes(y = pv, col = "RF*"), size=1.5) +
  labs( y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.5, 0.82))+
  theme(legend.title=element_blank())
ggsave('plots/rf_subset.pdf')
pt_subset_rf = pt
pv_subset_rf = pv

###################################################################################
# SUPPORT VECTOR MACHINES
###################################################################################

#install.packages("e1071")
library(e1071) #Library for Support Vector Machines

modelsvm = svm(f,data=data_training_)
modelsvm

# Prediction
pt = predict(modelsvm, newdata=data_training_)*st+mt
pv = predict(modelsvm, newdata=data_validation_)*sv+mv
training_quality_rf= mae(yt,pt)
validation_quality_rf= mae(yv,pv)

training_quality_rf
validation_quality_rf
cor(yv,pv)
df <- data.frame(week, yv, pv)

ggplot(df, aes(week, y = value , color = Variable)) + 
  geom_point(aes(y = yv, col = "Observed"), size=4) + 
  geom_line(aes(y = pv, col = "SVM fitted"), size=1.5) +
  labs( y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+
  theme(legend.title=element_blank())

pt_svm = pt
pv_svm = pv
description_svm = c(min(pv_svm),quantile(pv_svm,0.25),median(pv_svm),mean(pv_svm),quantile(pv_svm,0.75),max(pv_svm)) 
description_svm

#######################################################################################################
# WEIGHTED GLM FOR COMPARISON
#######################################################################################################

# FIRST A LOG LINK FUNCTION FLM IS FIT WITHOUT WEIGHTS
poisson_lm <- glm(f, family = poisson(link = "log"), data = data_training)
pt_unweighted = predict(poisson_lm, newdata=data_training, type = "response") 
pv_unweighted = predict(poisson_lm, newdata=data_validation, type = "response")
summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt_unweighted),"RMSE:", rmse(yt,pt_unweighted), "MSE:",mse(yt,pt_unweighted),"MAE:",mae(yt,pt_unweighted), "MAPE:",mape(yt,pt_unweighted))
validation_quality_lm= c("Correlation:",cor(yv,pv_unweighted), "RMSE:",rmse(yv,pv_unweighted), "MSE:",mse(yv,pv_unweighted),"MAE:",mae(yv,pv_unweighted),"MAPE:",mape(yv,pv_unweighted))

AIC(poisson_lm)
training_quality_lm
validation_quality_lm


residuals = abs(yv - pv_unweighted) #Obtain residuals
weight = 1 / residuals   #inverse of the residuals

#WEIGHTED GLM 

poisson_lm <- glm(f, family = poisson(link = "log"), data = data_training, weights = weight)

pt = predict(poisson_lm, newdata=data_training, type = "response")
pv = predict(poisson_lm, newdata=data_validation, type = "response")
summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt),"RMSE:", rmse(yt,pt), "MSE:",mse(yt,pt),"MAE:",mae(yt,pt), "MAPE:",mape(yt,pt))
validation_quality_lm= c("Correlation:",cor(yv,pv), "RMSE:",rmse(yv,pv), "MSE:",mse(yv,pv),"MAE:",mae(yv,pv),"MAPE:",mape(yv,pv))
AIC(poisson_lm)
training_quality_lm 
validation_quality_lm

#STEPWISE REGRESSION FOR THE WEIGHTED GLM
step_model <- stepAIC(poisson_lm, direction = "both", trace = FALSE)

#step_model <- glmulti(poisson_lm, # use the model with built as a starting point
#                                    level = 2,  #  just look at main effects
#                                   crit="aicc")



pt_step = predict(step_model, newdata=data_training, type = "response")
pv_step = predict(step_model, newdata=data_validation, type = "response")

training_quality_lm_step= c("Correlation:",cor(yt,pt_step),"RMSE:", rmse(yt,pt_step), "MSE:",mse(yt,pt_step),"MAE:",mae(yt,pt_step), "MAPE:",mape(yt,pt_step))
validation_quality_lm_step= c("Correlation:",cor(yv,pv_step), "RMSE:",rmse(yv,pv_step), "MSE:",mse(yv,pv_step),"MAE:",mae(yv,pv_step),"MAPE:",mape(yv,pv_step))

"FULL MODEL RESULTS"
training_quality_lm
validation_quality_lm
AIC(poisson_lm)



"SUBSET MODEL RESULTS"
training_quality_lm_step
validation_quality_lm_step
AIC(step_model)
summary(step_model)
pv_step_link = predict(step_model, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_step_link, fit + (2 * se.fit))
lwr <- with(pv_step_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(step_model)$linkinv
## map these on to the scale of response
fit <- with(pv_step_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)
summary(step_model)

df <- data.frame(week, yv, pv_subset_rf, pv_svm, pv_step,upr, lwr )
# PLOT of "Weighted poisson Regression with log Root link"
ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv_subset_rf, col = "RF*"), size=1.5) +
  geom_line(aes(y = pv_svm, col = "SVM"), size=1.5) +
  geom_line(aes(y = pv_step, col = "GLM-W*"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") + 
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.80))+
  theme(legend.title=element_blank())
ggsave('plots/ml_and_statmodel_compare_lines.pdf')

ggplot(df, aes(yv, y = value , color = Legend)) + 
  geom_point(aes(y = pv_subset_rf, col = "RF*"), size=3) + 
  geom_point(aes(y = pv_svm, col = "SVM"), size=3) + 
  geom_point(aes(y = pv_step, col = "GLM-W*"), size=3) +
  labs( y="Fitted", x= "Observed") + 
  theme(text = element_text(size=22)) + theme(legend.position = c(0.75, 0.15))+
  theme(legend.title=element_blank())
ggsave('plots/ml_and_statmodel_compare_scatterplot.pdf')
