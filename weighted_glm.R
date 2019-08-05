rm(list=ls(all=TRUE)) 
library(ggplot2)
library(MASS)
library(ggthemes)
library(devtools)
#library(ggthemr)
#ggthemr("dust")
#devtools::install_github('cttobin/ggthemr')
theme_set(theme_bw())

# Roote mean square error canculator


############################################################################
training = "./training.csv" #Define location of training csv data set on your local drive
test = "./test.csv"     #Define location of test csv data set on your local drive

############################################################################
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

adj_residual <- function(x, x_hat)
{   
  error <- x - x_hat
  error/sd(error)
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
  y~ndvi_u+	ndvi_r+	ndwi_u+	ndwi_r+	tempd_u+	tempd_r+	
    tempn_u+	tempn_r+	prec_u+	prec_r+		
    ndvi_u1+	ndvi_r1+	ndwi_u1+	ndwi_r1+	tempd_u1+	tempd_r1+	
    tempn_u1+	tempn_r1+	prec_u1+	prec_r1+		
    ndvi_u2+  ndvi_r2+	ndwi_u2+	ndwi_r2+	tempd_u2+	
    tempd_r2+	tempn_u2+	tempn_r2+	prec_u2+	prec_r2
)

yt = data_training[,2] # column 9 contain the mean of mosquitos
yv = data_validation[,2] # column 9 contain the mean of mosquitos
description = c(min(yv),quantile(yv,0.25),median(yv),mean(yv),quantile(yv,0.75),max(yv)) 
description 
week = data_validation[,1]

# FIRST A LOG LINK FUNCTION GLM IS FIT WITHOUT WEIGHTS

poisson_lm <- glm(f, family = poisson(link = "log"), data = data_training)
pt_unweighted = predict(poisson_lm, newdata=data_training, type = "response") 

pv_unweighted = predict(poisson_lm, newdata=data_validation, type = "response")

summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt_unweighted),"RMSE:", rmse(yt,pt_unweighted), "MSE:",mse(yt,pt_unweighted),"MAE:",mae(yt,pt_unweighted), "MAPE:",mape(yt,pt_unweighted))
validation_quality_lm= c("Correlation:",cor(yv,pv_unweighted), "RMSE:",rmse(yv,pv_unweighted), "MSE:",mse(yv,pv_unweighted),"MAE:",mae(yv,pv_unweighted),"MAPE:",mape(yv,pv_unweighted))

AIC(poisson_lm)
training_quality_lm
validation_quality_lm

#Confidence interval
pv_unweighted_link = predict(poisson_lm, newdata=data_validation, type = 'link', se.fit = TRUE)
upr_unweighted <- with(pv_unweighted_link, fit + (2 * se.fit))
lwr_unweighted <- with(pv_unweighted_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(poisson_lm)$linkinv
## map these on to the scale of response
fit <- with(pv_unweighted_link, invLink(fit))
upr_unweighted <- invLink(upr_unweighted)
lwr_unweighted <- invLink(lwr_unweighted)
summary(poisson_lm)

#Obtaining the weights for the weighted regression 

residuals = abs(yv - pv_unweighted) #Obtain residuals
weight = 1 / residuals   #inverse of the residuals


adj_res <- adj_residual(yv, pv_unweighted)


#WEIGHTED GLM 

poisson_lm <- glm(f, family = poisson(link = "log"), data = data_training, weights = weight)

pt = predict(poisson_lm, newdata=data_training, type = 'response')

pv = predict(poisson_lm, newdata=data_validation, type = 'response')

summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt),"RMSE:", rmse(yt,pt), "MSE:",mse(yt,pt),"MAE:",mae(yt,pt), "MAPE:",mape(yt,pt))
validation_quality_lm= c("Correlation:",cor(yv,pv), "RMSE:",rmse(yv,pv), "MSE:",mse(yv,pv),"MAE:",mae(yv,pv),"MAPE:",mape(yv,pv))
AIC(poisson_lm)
training_quality_lm 
validation_quality_lm
description = c(min(pv),quantile(pv,0.25),median(pv),mean(pv),quantile(pv,0.75),max(pv)) 
description
#CONFIDENCE INTERVAL
pv_link = predict(poisson_lm, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_link, fit + (2 * se.fit))
lwr <- with(pv_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(poisson_lm)$linkinv
## map these on to the scale of response
fit <- with(pv_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)
summary(poisson_lm)


df <- data.frame(week, yt, pv, pv_unweighted, upr, lwr, upr_unweighted, lwr_unweighted)

# PLOT of "weighted Poisson Regression with log link" 

ggplot(df, aes(week, y = value , color = Legend )) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "GLM-W"), size=1.5) +
  geom_line(aes(y = pv_unweighted, col = "GLM"), size=1.5) +
  labs(y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.82))+
  theme(legend.title=element_blank())
ggsave("plots/weighted_glm_log_linked_poisson.pdf")

ggplot(df, aes(week, y = value , color = variable )) + 
  geom_point(aes(y = yv, col = "Observed"), size=4) + 
  geom_line(aes(y = pv_unweighted, col = "GLM"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr_unweighted, ymax=upr_unweighted), alpha=0.2, inherit.aes = FALSE) +
  labs(y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.5, 0.82))+
  theme(legend.title=element_blank())

ggplot(df, aes(week, y = value , color = Legend )) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "GLM-W"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs(y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.82)) +
  theme(legend.title=element_blank())
ggsave("plots/weighted_glm_only_log_linked_poisson.pdf")

ggplot(df, aes(yv, y = value , color = Legend)) + 
  geom_point(aes(y = pv, col = "GLM-W"), size=3)+ 
  geom_point(aes(y = pv_unweighted, col = "GLM"), size=3) +
  labs( y="Fitted", x= "Observed") + 
  theme(text = element_text(size=22)) + theme(legend.position = c(0.7, 0.2)) +
  theme(legend.title=element_blank())
ggsave("plots/weighted_glm_log_linked_poisson_scatterplot.pdf")


#STEPWISE REGRESSION FOR THE WEIGHTED GLM
step_model <- stepAIC(poisson_lm, direction = "both", trace = FALSE)

#step_model <- glmulti(poisson_lm, # use the model with built as a starting point
#                                    level = 2,  #  just look at main effects
#                                   crit="aicc")

summary(step_model)
pt_step = predict(step_model, newdata=data_training, type = "response")
pv_step = predict(step_model, newdata=data_validation, type = "response")

training_quality_lm_step= c("Correlation:",cor(yt,pt_step),"RMSE:", rmse(yt,pt_step), "MSE:",mse(yt,pt_step),"MAE:",mae(yt,pt_step), "MAPE:",mape(yt,pt_step))
validation_quality_lm_step= c("Correlation:",cor(yv,pv_step), "RMSE:",rmse(yv,pv_step), "MSE:",mse(yv,pv_step),"MAE:",mae(yv,pv_step),"MAPE:",mape(yv,pv_step))
description = c(min(pv_step),quantile(pv_step,0.25),median(pv_step),mean(pv_step),quantile(pv_step,0.75),max(pv_step)) 
description
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


df <- data.frame(week, yv, pv, pv_step, upr, lwr)
# PLOT of "Weighted poisson Regression with log Root link"
ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "GLM-W"), size=1.5) +
  geom_line(aes(y = pv_step, col = "GLM-W*"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") + 
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.82))+
  theme(legend.title=element_blank())
ggsave("plots/stepwise_model_selection_weighted_glm.pdf")

ggplot(df, aes(week, y = value , color = Legend )) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv_step, col = "10 features - GLM-W"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs(y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.82))+
  theme(legend.title=element_blank())
ggsave("plots/stepwise_model_selection_weighted_glm_only.pdf")

ggplot(df, aes(yv, y = value , color = Legend)) + 
  geom_point(aes(y = pv, col = "GLM-W"), size=3) + 
  geom_point(aes(y = pv_step, col = "GLM-W*"), size=3) +
  labs( y="Fitted", x= "Observed") + 
  theme(text = element_text(size=22)) + theme(legend.position = c(0.8, 0.2))+
  theme(legend.title=element_blank())
ggsave("plots/weighted_glm_log_linked_poisson_stepwise_scatterplot.pdf")
adj_res <- adj_residual(yv, pv_step)



df <- data.frame(yv, pv_step, adj_res)

ggplot(df, aes(yv, y = value , color = Legend )) + 
  geom_point(aes(y = adj_res, col = "Residuals"), size=3) +
  labs(y="Residuals", x= "Observed data") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.8, 0.2))+ theme(legend.position = "none")+
  theme(legend.title=element_blank())

ggplot(df, aes(pv_step, y = value , color = Legend )) + 
  geom_point(aes(y = adj_res, col = "Residuals"), size=3) +
  labs(y="Residuals", x= "Fitted data") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.82)) + theme(legend.position = "none")+
  theme(legend.title=element_blank())
ggsave("plots/weighted_glm_residuals_fitted_weighted_stepwise.pdf")


#P value variable selection 5% threshold
f_p <- as.formula(y ~  tempd_u + tempn_u + prec_u + prec_r + ndwi_r1 + tempd_u2)

poisson_lm_p <- glm(f_p, family = poisson(link = "log"), data = data_training, weights = weight)

pt_pvalue = predict(poisson_lm_p, newdata=data_training, type = 'response')

pv_pvalue = predict(poisson_lm_p, newdata=data_validation, type = 'response')

summary(poisson_lm_p)

training_quality_lm = c("Correlation:",cor(yt,pt_pvalue),"RMSE:", rmse(yt,pt_pvalue), "MSE:",mse(yt,pt_pvalue),"MAE:",mae(yt,pt_pvalue), "MAPE:",mape(yt,pt_pvalue))
validation_quality_lm= c("Correlation:",cor(yv,pv_pvalue), "RMSE:",rmse(yv,pv_pvalue), "MSE:",mse(yv,pv_pvalue),"MAE:",mae(yv,pv_pvalue),"MAPE:",mape(yv,pv_pvalue))
AIC(poisson_lm_p)
training_quality_lm 
validation_quality_lm

#CONFIDENCE INTERVAL
pv_link = predict(poisson_lm_p, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_link, fit + (2 * se.fit))
lwr <- with(pv_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(poisson_lm_p)$linkinv
## map these on to the scale of response
fit <- with(pv_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)
summary(poisson_lm_p)


df <- data.frame(week, yt, pv_pvalue, pv_step, upr, lwr)

ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "observed"), size=3) + 
  geom_line(aes(y = pv_pvalue, col = "6 features"), size=1.5) +
  geom_line(aes(y = pv_step, col = "10 features"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") + 
  theme(text = element_text(size=22))+ theme(legend.position = c(0.2, 0.82))+
  theme(legend.title=element_blank())
ggsave("plots/stepwise_model_selection_AIC_pvalue_weighted_glm.pdf")

ggplot(df, aes(yv, y = value , color = Legend)) + 
  geom_point(aes(y = pv_pvalue, col = "AIC stepwise + Pvalue"), size=3) + 
  geom_point(aes(y = pv_step, col = "AIC stepwise"), size=3) + 
  labs( y="Fitted", x= "Observed") + 
  theme(text = element_text(size=22)) + theme(legend.position = c(0.75, 0.20))+
  theme(legend.title=element_blank())
ggsave('plots/stepwise_model_selection_AIC_pvalue_weighted_glm_scatterplot.pdf')

#Residual analysis


##########################################################################################################################################
##########################################################################################################################################

#FORWARD REGRESSION FOR THE WEIGHTED GLM
forward_model <- stepAIC(poisson_lm, direction = "forward", trace = FALSE)

#step_model <- glmulti(poisson_lm, # use the model with built as a starting point
#                                    level = 2,  #  just look at main effects
#                                   crit="aicc")

summary(forward_model)
pt_forward = predict(forward_model, newdata=data_training, type = "response")
pv_forward = predict(forward_model, newdata=data_validation, type = "response")

training_quality_lm_forward= c("Correlation:",cor(yt,pt_forward),"RMSE:", rmse(yt,pt_forward), "MSE:",mse(yt,pt_forward),"MAE:",mae(yt,pt_forward), "MAPE:",mape(yt,pt_forward))
validation_quality_lm_forward = c("Correlation:",cor(yv,pv_forward), "RMSE:",rmse(yv,pv_forward), "MSE:",mse(yv,pv_forward),"MAE:",mae(yv,pv_forward),"MAPE:",mape(yv,pv_forward))

"FULL MODEL RESULTS"
training_quality_lm
validation_quality_lm
AIC(poisson_lm)



"SUBSET MODEL RESULTS"
training_quality_lm_forward
validation_quality_lm_forward
AIC(forward_model)
summary(forward_model)

#BACKWARD SELECTION

backward_model <- stepAIC(poisson_lm, direction = "backward", trace = FALSE)

#step_model <- glmulti(poisson_lm, # use the model with built as a starting point
#                                    level = 2,  #  just look at main effects
#                                   crit="aicc")

summary(backward_model)
pt_backward= predict(backward_model, newdata=data_training, type = "response")
pv_backward = predict(backward_model, newdata=data_validation, type = "response")

training_quality_lm_backward= c("Correlation:",cor(yt,pt_backward),"RMSE:", rmse(yt,pt_backward), "MSE:",mse(yt,pt_backward),"MAE:",mae(yt,pt_backward), "MAPE:",mape(yt,pt_backward))
validation_quality_lm_backward = c("Correlation:",cor(yv,pv_backward), "RMSE:",rmse(yv,pv_backward), "MSE:",mse(yv,pv_backward),"MAE:",mae(yv,pv_backward),"MAPE:",mape(yv,pv_backward))

"FULL MODEL RESULTS"
training_quality_lm
validation_quality_lm
AIC(poisson_lm)



"SUBSET MODEL RESULTS"
training_quality_lm_backward
validation_quality_lm_backward
AIC(backward_model)
summary(backward_model)


