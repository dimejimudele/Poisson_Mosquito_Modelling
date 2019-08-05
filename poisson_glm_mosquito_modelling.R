
rm(list=ls(all=TRUE)) 
library(MASS)

library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
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
data_training_input <-read.csv("training_descreteTarget_forPoisson.csv")

#Read Validation Data
data_validation_input <-read.csv("testing_descreteTarget_forPoisson.csv")

input_nonsplit <-read.csv("Non_split_data_2017_poisson.csv")
y_nonsplit = input_nonsplit[,2]

description_ns = c(mean(y_nonsplit),median(y_nonsplit),sd(y_nonsplit), min(y_nonsplit),max(y_nonsplit))
description_ns

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
week = data_validation[,1]

#LOG LINK FUNCTION
poisson_lm <- glm(f, family = poisson(link = "log"), data = data_training)
pt = predict(poisson_lm, newdata=data_training, type = "response") 
pv = predict(poisson_lm, newdata=data_validation, type = "response")
summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt),"RMSE:", rmse(yt,pt), "MSE:",mse(yt,pt),"MAE:",mae(yt,pt), "MAPE:",mape(yt,pt))
validation_quality_lm= c("Correlation:",cor(yv,pv), "RMSE:",rmse(yv,pv), "MSE:",mse(yv,pv),"MAE:",mae(yv,pv),"MAPE:",mape(yv,pv))

description = c(min(pv),quantile(pv,0.25),median(pv),mean(pv),quantile(pv,0.75),max(pv)) 
description
step_model <- stepAIC(poisson_lm, direction = "both", trace = FALSE)

summary(step_model)
pt_step = predict(step_model, newdata=data_training, type = "response")
pv_step = predict(step_model, newdata=data_validation, type = "response")

training_quality_gamma_lm_step= c("Correlation:",cor(yt,pt_step),"RMSE:", rmse(yt,pt_step), "MSE:",mse(yt,pt_step),"MAE:",mae(yt,pt_step), "MAPE:",mape(yt,pt_step))
validation_quality_gamma_lm_step= c("Correlation:",cor(yv,pv_step), "RMSE:",rmse(yv,pv_step), "MSE:",mse(yv,pv_step),"MAE:",mae(yv,pv_step),"MAPE:",mape(yv,pv_step))

"FULL MODEL RESULTS"
training_quality_lm
validation_quality_lm
AIC(poisson_lm)



"SUBSET MODEL RESULTS"
training_quality_gamma_lm_step
validation_quality_gamma_lm_step
summary(step_model)
AIC(step_model)

#COnfidence interval - log

pv_link = predict(step_model, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_link, fit + (2 * se.fit))
lwr <- with(pv_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(step_model)$linkinv
invLink
## map these on to the scale of response
fit <- with(pv_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)

pv_log = pv
pv_step_log = pv_step
df <- data.frame(week, yv, pv, pv_step, lwr, upr)

ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "30 features"), size=1.5) +
  geom_line(aes(y = pv_step, col = "21 features"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") +
  theme(text = element_text(size=22))+ theme(legend.position = c(0.18, 0.87))+
  theme(legend.title=element_blank())
ggsave("plots/log_linked_poisson.pdf")


#IDENTITY LINK FUNCTION
poisson_lm <- glm(f, family = poisson(link = "identity"), data = data_training)
pt = predict(poisson_lm, newdata=data_training, type = "response")
pv = predict(poisson_lm, newdata=data_validation, type = "response")
summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt),"RMSE:", rmse(yt,pt), "MSE:",mse(yt,pt),"MAE:",mae(yt,pt), "MAPE:",mape(yt,pt))
validation_quality_lm= c("Correlation:",cor(yv,pv), "RMSE:",rmse(yv,pv), "MSE:",mse(yv,pv),"MAE:",mae(yv,pv),"MAPE:",mape(yv,pv))


step_model <- stepAIC(poisson_lm, direction = "both", trace = FALSE)

#step_model <- glmulti(poisson_lm, # use the model with built as a starting point
#                                    level = 2,  #  just look at main effects
#                                   crit="aicc")

summary(step_model)
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

#COnfidence interval - identity

pv_link = predict(step_model, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_link, fit + (2 * se.fit))
lwr <- with(pv_link, fit - (2 * se.fit))
## inverse link fun
## inverse link funinvLink <- family(step_model)$linkinv
invLink <- family(step_model)$linkinv
invLink
## map these on to the scale of response
fit <- with(pv_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)


df <- data.frame(week, yv, pv, pv_step, lwr, upr)

pv_idn = pv
pv_step_idn = pv_step
ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "30 features"), size=1.5) +
  geom_line(aes(y = pv_step, col = "26 features"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax= upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") + 
  theme(text = element_text(size=22))+ theme(legend.position = c(0.18, 0.87))+
  theme(legend.title=element_blank())
ggsave("plots/identity_linked_poisson.pdf")

#SQUARE ROOT LINK FUNCTION
poisson_lm <- glm(f, family = poisson(link = "sqrt"), data = data_training)
pt = predict(poisson_lm, newdata=data_training, type = "response")
pv = predict(poisson_lm, newdata=data_validation, type = "response")
summary(poisson_lm)

training_quality_lm = c("Correlation:",cor(yt,pt),"RMSE:", rmse(yt,pt), "MSE:",mse(yt,pt),"MAE:",mae(yt,pt), "MAPE:",mape(yt,pt))
validation_quality_lm= c("Correlation:",cor(yv,pv), "RMSE:",rmse(yv,pv), "MSE:",mse(yv,pv),"MAE:",mae(yv,pv),"MAPE:",mape(yv,pv))


step_model <- stepAIC(poisson_lm, direction = "both", trace = FALSE)

summary(step_model)
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

pv_link = predict(step_model, newdata=data_validation, type = 'link', se.fit = TRUE)
upr <- with(pv_link, fit + (2 * se.fit))
lwr <- with(pv_link, fit - (2 * se.fit))
## inverse link fun
invLink <- family(step_model)$linkinv
invLink
## map these on to the scale of response
fit <- with(pv_link, invLink(fit))
upr <- invLink(upr)
lwr <- invLink(lwr)


df <- data.frame(week, yv, pv, pv_step, lwr, upr)


pv_sqrt = pv
pv_step_sqrt = pv_step

ggplot(df, aes(week, y = value , color = Legend)) + 
  geom_point(aes(y = yv, col = "Observed"), size=3) + 
  geom_line(aes(y = pv, col = "30 features"), size=1.5) +
  geom_line(aes(y = pv_step, col = "24 features"), size=1.5) +
  geom_ribbon(x = week, aes(ymin=lwr, ymax=upr), alpha=0.25, inherit.aes = FALSE) +
  labs( y="Number of Mosquitoes", x= "Week") + 
  theme(text = element_text(size=23))+ theme(legend.position = c(0.18, 0.87))+
  theme(legend.title=element_blank())
ggsave("plots/sqrt_linked_poisson.pdf")




# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

