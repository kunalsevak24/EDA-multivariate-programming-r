library(ggplot2)
library(tidyverse)
library(ISLR2)
library(caret)
library(stargazer)
library(lmtest)
library(leaps)
library(readr)

RiceData <- read_csv("C:/Users/kunal/OneDrive/Desktop/Conestoga College Docs/Multivariate Statistics/Final Data Set/Production of Rice in Indonesia.csv") 
summary(RiceData)
summary(RiceData$noutput)
summary(RiceData$price)
RiceData %>% ggplot(aes(noutput)) + geom_histogram()
RiceData %>% ggplot(aes(price)) + geom_histogram()
hist(RiceData$noutput)
hist(RiceData$price)

# We will start with 2 Response Variables - Price and Net Output 

# Correlation of all major variables with Price and Net Output
# Correlation Value will always be between -1 and 1 
# 0 Indicates no Correlation between the 2 variables 
# 1 being the best linear relationship between two variables 
# -1 being inversely related if one variable increases other decreases in a linear way 
# -1 to 0 indicate that both the variables are associated negatively in a linear way 
# 0 to 1 indicate that both the variables are associated positively in a linear way 


cor(RiceData$price,RiceData$noutput)
# Positive Linear Relation between Price of Rice and Net Output of Rice = 0.09852095
cor(RiceData$price,RiceData$size)
# Negative Linear Relationship between price of Rice and Size of field = -0.01223299
cor(RiceData$price,RiceData$seed)
# -0.02642857
cor(RiceData$price,RiceData$urea)
# 0.07534964
cor(RiceData$price,RiceData$phosphate)
# 0.1899533
cor(RiceData$price,RiceData$pesticide)
#0.1061548
cor(RiceData$price,RiceData$pseed)
#0.6689168
cor(RiceData$price,RiceData$pphosph)
#0.687863
cor(RiceData$price,RiceData$purea)
#0.6849733
cor(RiceData$price,RiceData$hiredlabor)
#-0.003335649
cor(RiceData$price,RiceData$famlabor)
# 0.109077
cor(RiceData$price,RiceData$totlabor)
#0.03012847
cor(RiceData$price,RiceData$wage)
#0.8593039
cor(RiceData$price,RiceData$goutput)
#0.09119443
cor(RiceData$price,RiceData$noutput)
#0.09852095

# Correlations with Net Out Put and Other Variables 

cor(RiceData$noutput,RiceData$size)
#0.8915277
cor(RiceData$noutput,RiceData$seed)
#0.5475009
cor(RiceData$noutput,RiceData$urea)
#0.8134663
cor(RiceData$noutput,RiceData$phosphate)
#0.7370739
cor(RiceData$noutput,RiceData$pesticide)
# 0.3951422
cor(RiceData$noutput,RiceData$pseed)
# 0.1532252
cor(RiceData$noutput,RiceData$pphosph)
# 0.0217065
cor(RiceData$noutput,RiceData$purea)
# 0.02984448
cor(RiceData$noutput,RiceData$hiredlabor)
# 0.8511969
cor(RiceData$noutput,RiceData$famlabor)
# 0.4115718
cor(RiceData$noutput,RiceData$totlabor)
# 0.8681
cor(RiceData$noutput,RiceData$wage)
# 0.17174056
cor(RiceData$noutput,RiceData$goutput)
# 0.9988217
cor(RiceData$noutput,RiceData$price)
# 0.09852095

# Factoring Attempt for Region wise

RiceData$region <- factor(x=RiceData$region, labels = c(1,2,3,4,5,6))
RiceData$region <- as.numeric(RiceData$region)
# 6 = wargabinangun
# 3 = langan 
# 2 = gunungwang
# 4 = malausma
# 5 = sukaambit
# 1 = ciwangi

# Finding Correlation based on Region 
cor(RiceData$noutput,RiceData$region)
cor(RiceData$price,RiceData$region)


# Price Linear Regression with 2 Best Cor Value Variables 

# No. 1 - Linear Regresion - Price and Wage Relationship 

price_regression_wage <- lm(price ~ wage, data=RiceData)
coef(price_regression_wage)

# x = 29.5421133 + Constant*0.7637004

# Plotting between Price and Wage

RiceData %>% ggplot(aes(x = price, y = wage)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Value
Resids_P_W <- price_regression_wage$residuals
Fitted_P_W <- price_regression_wage$fitted.values

hist(Resids_P_W)
qqnorm(Resids_P_W)
plot(Fitted_P_W, Resids_P_W)


# Summary & Stargazer 

summary(price_regression_wage)
stargazer(price_regression_wage, type ="text")

# No.2 - Linear Regression - Price and Pphos

price_regression_pphos <- lm(price ~ pphosph, data = RiceData)
coef(price_regression_pphos)
# x = -130.370791 + Constant*2.781683
# Plotting between Price and PPhosph
RiceData %>% ggplot(aes(x = price, y = pphosph)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Values 
Resids_P_P <- price_regression_pphos$residuals
Fitted_P_P <- price_regression_pphos$fitted.values

hist(Resids_P_P)
qqnorm(Resids_P_P)
plot(Fitted_P_P, Resids_P_P)

# Summary & Stargazer 

summary(price_regression_pphos)
stargazer(price_regression_pphos, type ="text")

# No.  3 - Linear Regression - Price with Wage and Pphoph

price_regression_pphos_Wage <- lm(price ~ wage + pphosph, data = RiceData)
coef(price_regression_pphos_Wage)
RiceData %>% ggplot(aes(x = price, y = pphosph + wage)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Values

Resids_P_PW <- price_regression_pphos_Wage$residuals
Fitted_P_PW <- price_regression_pphos_Wage$fitted.values

hist(Resids_P_PW)
qqnorm(Resids_P_PW)
plot(Fitted_P_PW, Resids_P_PW)

# Summary & Stargazer 

summary(price_regression_pphos_Wage)
stargazer(price_regression_pphos_Wage, type ="text")

# No. 4 - Linear Regression - Price with Full Model

price_regression_fullModel <- lm(price ~ ., data = RiceData)
coef(price_regression_fullModel)
#RiceData %>% ggplot(aes(x = price, y = .)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Values

Resids_P_FM <- price_regression_fullModel$residuals
Fitted_P_FM <- price_regression_fullModel$fitted.values

hist(Resids_P_FM)
qqnorm(Resids_P_FM)
plot(Fitted_P_FM, Resids_P_FM)

# Summary & Stargazer 

summary(price_regression_fullModel)
stargazer(price_regression_fullModel, type ="text")


# Response Variable Net Out Put with relevant Variables 

# Netoutput with Size

Netoutput_regression_size <- lm(noutput ~ size, data=RiceData)
coef(Netoutput_regression_size)
RiceData %>% ggplot(aes(x = noutput, y = size)) + geom_point() + geom_smooth(method="lm", se = FALSE)
# Equation --> x = 87.53125 + Constant*2672.37165

# Residuals and Fitted Values

Resids_N_S <- Netoutput_regression_size$residuals
Fitted_N_S <- Netoutput_regression_size$fitted.values

hist(Resids_N_S)
qqnorm(Resids_N_S)
plot(Fitted_N_S, Resids_N_S)

# Summary & Stargazer 

summary(Netoutput_regression_size)
stargazer(Netoutput_regression_size, type ="text")

# Netoutput with Gross Output 

Netoutput_regression_Grossoutput <- lm(noutput ~ goutput , data= RiceData)
coef(Netoutput_regression_Grossoutput)
RiceData %>% ggplot(aes(x = noutput, y = goutput)) + geom_point() + geom_smooth(method="lm", se = FALSE)
# One of the most amazing Linear Relationship between 2 variables 
# Equation --> x = 43.9262983 + Constant*0.8518518

# Residuals and Fitted Values

Resids_N_G <- Netoutput_regression_Grossoutput$residuals
Fitted_N_G <- Netoutput_regression_Grossoutput$fitted.values

hist(Resids_N_G)
qqnorm(Resids_N_G)
plot(Fitted_N_G, Resids_N_G)

# Summary & Stargazer 

summary(Netoutput_regression_Grossoutput)
stargazer(Netoutput_regression_Grossoutput, type ="text")


# Netoutput with Hired Labor 

Netoutput_regression_HiredLabor <- lm(noutput ~ hiredlabor , data= RiceData)
coef(Netoutput_regression_HiredLabor)
# Equation --> 457.771704 + Constant * 3.304097
RiceData %>% ggplot(aes(x = noutput, y = hiredlabor)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Values

Resids_N_HL <- Netoutput_regression_HiredLabor$residuals
Fitted_N_HL <- Netoutput_regression_HiredLabor$fitted.values

hist(Resids_N_HL)
qqnorm(Resids_N_HL)
plot(Fitted_N_HL, Resids_N_HL)

# Summary & Stargazer 

summary(Netoutput_regression_HiredLabor)
stargazer(Netoutput_regression_HiredLabor, type ="text")

# Net Output with Gross Output + Hired Labor + Size 

Netoutput_regression_SGH <- lm(noutput ~ goutput + size + hiredlabor, data=RiceData)
coef(Netoutput_regression_SGH)
# Equation --> x = 49.95850143 + Constant G Output* 0.85924618 + 
RiceData %>% ggplot(aes(x = noutput, y = hiredlabor + size + goutput)) + geom_point() + geom_smooth(method="lm", se = FALSE)

# Residuals and Fitted Values

Resids_N_S_HL_G <- Netoutput_regression_SGH$residuals
Fitted_N_S_HL_G <- Netoutput_regression_SGH$fitted.values

hist(Resids_N_S_HL_G)
qqnorm(Resids_N_S_HL_G)
plot(Fitted_N_S_HL_G, Resids_N_S_HL_G)

# Summary & Stargazer 

summary(Netoutput_regression_SGH)
stargazer(Netoutput_regression_SGH, type ="text")

# Net Output with all Variables 

NetOutput_regression_fullModel <- lm(noutput ~ ., data = RiceData)
coef(NetOutput_regression_fullModel)

# Residuals and Fitted Values

Resids_N_FM <- NetOutput_regression_fullModel$residuals
Fitted_N_FM <- NetOutput_regression_fullModel$fitted.values

hist(Resids_N_FM)
qqnorm(Resids_N_FM)
plot(Fitted_N_FM, Resids_N_FM)


# Summary & Stargazer 

summary(NetOutput_regression_fullModel)
stargazer(NetOutput_regression_fullModel, type ="text")

###################################################################

# PREDICTIONS 








