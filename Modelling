#MODELLING 

# Linear Regression
model1 <- lm(Covid_Deaths ~ Deprivation)

# add regression line to scatter plot #figure 8
plot(Deprivation, Covid_Deaths, main = "Scatterplot",
     xlab = "Deprivation", ylab = "Covid_Deaths")
abline(model1, col = "red")

#BOX 9
summary(model1)

hist(model1$residuals)
rug(model1$residuals)
# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))

#MULTIPLE LINEAR REGRESSION

# Multiple Regression
#BOX 9 Model 2 Summary and VIF
# model with all variables
model2 <- lm(Covid_Deaths ~ Deprivation + Social_Grade_AB + 
               +Social_Grade_DE + No_deprivation + Hourly_Pay + 
               Urban + Health_and_Disabilities)
summary(model2)
# calculate variance inflation factor
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high

# model with four variables
model3 <- lm(Covid_Deaths ~ No_deprivation + 
               Hourly_Pay + Urban + Health_and_Disabilities)
summary(model3) 

# Box 10 
# Calculate variance inflation factor FOUR INDEPDENT VARIABLES
vif(model3)
sqrt(vif(model3)) > 2  # if > 2 vif too high 

#calculate partial correlation
library(ppcor)
pcor.test(Covid_Deaths, No_deprivation, Health_and_Disability, method="spearman")
pcor.test(Covid_Deaths, Health_and_Disability, No_deprivation, method="spearman")
cor(Covid_Deaths, No_deprivation, method="spearman")
cor(Covid_Deaths, Health_and_Disability, method="spearman")

#THIS SHOWS URBANITY HAS NO IMPACT ON HEALTH AND 
#DISABILTIES RElATIONSHIP WITH COVID DEATHS
cor(Covid_Deaths, Urban_Households)
cor(Covid_Deaths, Health_and_Disability)
pcor.test(Covid_Deaths, Health_and_Disability, Urban_Households)
pcor.test(Covid_Deaths, Urban_Households, Health_and_Disability)

# model with three variables 
#Box 11
model4 <- lm(Covid_Deaths ~ Hourly_Pay + Urban + Health_and_Disabilities)
summary(model4) 

#Box 12
anova(model2, model4, test = "F")

#Box 13
calc.relimp(model4, type = c("lmg"), rela = TRUE)
