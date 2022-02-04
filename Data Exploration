#DATA EXPLORATION
# Create boxplots of variables 
#Fig 1
boxplot(Covid_Deaths, main="Covid deaths per thousand by district", ylab="Covid deaths per thousand",col = "Bisque")
identify(rep(1, length(Covid_Deaths)), Covid_Deaths, labels = seq_along(LA_name))

#East Riding of Yorkshire 
COVID[89,]

#Fig 2
boxplot(pMarch_2020, pApril_2020, pMay_2020, pJune_2020, pJuly_2020,
        pAugust_2020, pSeptember_2020, pOctober_2020, pNovember_2020,
        pDecember_2020, pJanuary_2021, pFebruary_2021,pMarch_2021, pApril_2021,
        ylab="Covid deaths per thousand", xlab="Month", main="Monthly Covid deaths per thousand", names=c("Mar_20", "Apr_20", "pMay_20", "Jun_20", "Jul_20",
        "August_2020", "Sep_20", "Oct_20", "Nov_20", "Dec_20", "January_21", "Feb_21", "March_21", "Apr_21"),col = "Bisque")
identify(rep(1, length(Covid_Deaths)), Covid_Deaths, labels = seq_along(LA_name))

#standardize new variables
COVID <- within (COVID, Social_Grade_AB1 <- (Social_Grade_AB*100))
COVID <- within (COVID, No_deprivation1 <- (No_deprivation*100))
COVID <- within (COVID, Deprivation_11 <- (Deprivation_1*100))
COVID <- within (COVID, Deprivation_21 <- (Deprivation_2*100))
COVID <- within (COVID, Deprivation_31 <- (Deprivation_3*100))
COVID <- within (COVID, Deprivation_41 <- (Deprivation_4*100))

#Fig 3
# inspect outliers High Social Grade
boxplot(Social_Grade_AB1, ylab="Percentage %", main="Social Grade AB", col = "Bisque")
identify(rep(1, length(Social_Grade_AB1)), Social_Grade_AB1, labels = seq_along(Social_Grade_AB1))

# Fig 4 boxplot of all six percentage variables of deprivation
boxplot(No_deprivation1, Deprivation_11, 
        Deprivation_21, Deprivation_31,
        Deprivation_41,
        names=c("Any", "1 Dimension", "2 Dimension", "3 Dimension", "4 Dimension"),
        xlab="Deprivation Dimensions",  main="Household Deprivation by District",ylab="Percentage %", col = "Bisque")

#test normality
shapiro.test(Social_Grade_AB)
shapiro.test(Social_Grade_C1)
shapiro.test(Social_Grade_C2)
shapiro.test(Social_Grade_DE)
ks.test(No_deprivation, "pnorm", mean(No_deprivation), sd(No_deprivation))
ks.test(Deprivation_1, "pnorm", mean(Deprivation_1), sd(Deprivation_1))
ks.test(Deprivation, "pnorm", mean(Deprivation), sd(Deprivation))
ks.test(GDHI, "pnorm", mean(GDHI), sd(GDHI))
ks.test(Hourly_Pay, "pnorm", mean(Hourly_Pay), sd(Hourly_Pay))
ks.test(Urban, "pnorm", mean(Urban), sd(Urban))


# test correlation of dependent variable with all independent variables
cor.test(Covid_Deaths, Social_Grade_AB, method = "spearman")
cor.test(Covid_Deaths, Social_Grade_C1, method = "spearman")
cor.test(Covid_Deaths, Social_Grade_C2, method = "spearman")
cor.test(Covid_Deaths, Social_Grade_DE, method = "spearman")
cor.test(Covid_Deaths, No_deprivation, method = "spearman")
cor.test(Covid_Deaths, Deprivation_1, method = "spearman")
cor.test(Covid_Deaths, Deprivation, method = "spearman")
cor.test(Covid_Deaths, GDHI, method = "spearman")
cor.test(Covid_Deaths, Hourly_Pay, method = "spearman")
cor.test(Covid_Deaths, Urban, method = "spearman")
cor.test(Covid_Deaths, Health_and_Disabilities, method = "spearman")

# looking at internal correlations between three variables
cor.test(pDaytodayactivitiesLimitedalot, pApproximatedsocialgradeAB, method = "spearman")
cor.test(pApproximatedsocialgradeDE, pDaytodayactivitiesLimitedalot, method = "spearman")
cor.test(pApproximatedsocialgradeDE, pTOTALHouseholdisdeprivedin3dimensions, method = "spearman")
cor.test(pApproximatedsocialgradeDE, pTOTALHouseholdisdeprivedin2dimensions, method = "spearman")
cor.test(pApproximatedsocialgradeDE, pTOTALHouseholdisnotdeprivedinanydimension, method = "spearman")
cor.test(pApproximatedsocialgradeAB, pTOTALHouseholdisdeprivedin4dimensions, method = "spearman")
cor.test(pApproximatedsocialgradeAB, pTOTALHouseholdisdeprivedin3dimensions, method = "spearman")
cor.test(pApproximatedsocialgradeAB, pTOTALHouseholdisdeprivedin2dimensions, method = "spearman")
cor.test(pApproximatedsocialgradeAB, pTOTALHouseholdisnotdeprivedinanydimension, method = "spearman")

#multivariate of independent variables READY TO BE DELETED
pairs(~ Social_Grade_AB + GDHI2011 + FTHourlyGrossPayMean +
              No_deprivation,  data = COVID,
      main = "multivariate scatterplot matrix")


#spearman correlation matrix

#MATRIX FOR DEPENDENT AND INDEPENDENT VARIABLES
CovidMatrix <- data.frame(Covid_Deaths, Social_Grade_AB, 
                          Social_Grade_DE, No_deprivation,
                          Deprivation, GDHI2011, Hourly_Pay,
                          Urban, Health_and_Disabilities)
#FIG 5
pairs.panels(CovidMatrix, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman Correlation Matrix")

#FIG 6
IntCorrCovidMatrix <- data.frame(Social_Grade_AB, Social_Grade_DE,
                                 No_deprivation, Deprivation, GDHI, 
                                 Hourly_Pay, Urban, Health_and_Disabilities, 
                                 main = "Indepedent Variable Correlation Matrix")

corrgram(IntCorrCovidMatrix,  main = "Indepedent Variable Correlation Matrix", 
         order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt)

#partial correlation - select the two independent variables which correlate 
#most strongly with the dependent variables - Deprivation, 

#1 test for partial correlation
cor.test(Covid_Deaths, Deprivation, method="spearman")
cor.test(Covid_Deaths, Urban, method="spearman")
pcor.test(Covid_Deaths, Deprivation, Urban, method="spearman")
pcor.test(Covid_Deaths, Urban, Deprivation, method="spearman")

#2 test for partial correlation
cor.test(Covid_Deaths, Social_Grade_DE, method="spearman")
cor.test(Covid_Deaths, Urban, method="spearman")
pcor.test(Covid_Deaths, Social_Grade_DE, Urban, method="spearman")
pcor.test(Covid_Deaths, Urban, Social_Grade_DE, method="spearman")

#3 test for partial correlation
cor.test(Covid_Deaths, Social_Grade_DE, method="spearman")
cor.test(Covid_Deaths, Health_and_Disabilities, method="spearman")
pcor.test(Covid_Deaths, Social_Grade_DE, Health_and_Disabilities, method="spearman")
pcor.test(Covid_Deaths, Health_and_Disabilities, Social_Grade_DE, method="spearman")

#4 test for partial correlation
cor.test(Covid_Deaths, Deprivation, method="spearman")
cor.test(Covid_Deaths, Health_and_Disabilities, method="spearman")
pcor.test(Covid_Deaths, Deprivation, Health_and_Disabilities , method="spearman")
pcor.test(Covid_Deaths, Health_and_Disabilities, Deprivation, method="spearman")

#4 test for partial correlation
cor.test(Covid_Deaths, Social_Grade_AB, method="spearman")
cor.test(Covid_Deaths, Health_and_Disabilities, method="spearman")
pcor.test(Covid_Deaths, Social_Grade_AB, Health_and_Disabilities , method="spearman")
pcor.test(Covid_Deaths, Health_and_Disabilities, Social_Grade_AB, method="spearman")
