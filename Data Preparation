#DATA PREPARATION

# read in data from csv file
COVID <- read.csv("COVID v6.csv", stringsAsFactors = FALSE)
head(COVID)    # Inspect top rows of the data
str(COVID)
summary(COVID)

#standardize all COVID variables
COVID <- within (COVID, pMarch_2020 <- (March_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pApril_2020 <- (April_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pMay_2020 <- (May_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pJune_2020 <- (June_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pJuly_2020 <- (July_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pAugust_2020 <- (August_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pSeptember_2020 <- (September_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pOctober_2020 <- (October_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pNovember_2020 <- (November_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pDecember_2020 <- (December_2020 / TotalPopulation)*1000)
COVID <- within (COVID, pJanuary_2021 <- (January_2021 / TotalPopulation)*1000)
COVID <- within (COVID, pFebruary_2021 <- (February_2021 / TotalPopulation)*1000)
COVID <- within (COVID, pMarch_2021 <- (March_2021 / TotalPopulation)*1000)
COVID <- within (COVID, pApril_2021 <- (April_2021 / TotalPopulation)*1000)
COVID <- within (COVID, Covid_Deaths <- (Total / TotalPopulation)*1000)

#standardize all Social Grade variables
COVID <- within (COVID, Social_Grade_AB <- (ApproximatedsocialgradeAB / Approximatedsocialgrade))
COVID <- within (COVID, Social_Grade_C1 <- (ApproximatedsocialgradeC1 / Approximatedsocialgrade))
COVID <- within (COVID, Social_Grade_C2 <- (ApproximatedsocialgradeC2 / Approximatedsocialgrade))
COVID <- within (COVID, Social_Grade_DE <- (ApproximatedsocialgradeDE / Approximatedsocialgrade))


#standardize all Deprivation variables
COVID <- within (COVID, No_deprivation <- (TOTALHouseholdisnotdeprivedinanydimension / TOTALAllcategoriesClassificationofhouseholddeprivation))
COVID <- within (COVID, Deprivation_1 <- (TOTALHouseholdisdeprivedin1dimension / TOTALAllcategoriesClassificationofhouseholddeprivation))
COVID <- within (COVID, Deprivation_2 <- (TOTALHouseholdisdeprivedin2dimensions / TOTALAllcategoriesClassificationofhouseholddeprivation))
COVID <- within (COVID, Deprivation_3 <- (TOTALHouseholdisdeprivedin3dimensions / TOTALAllcategoriesClassificationofhouseholddeprivation))
COVID <- within (COVID, Deprivation_4 <- (TOTALHouseholdisdeprivedin4dimensions / TOTALAllcategoriesClassificationofhouseholddeprivation))
COVID <- within (COVID, Deprivation <- (Deprivation_4+Deprivation_3+Deprivation_2))

#standardize other variables
COVID <- within (COVID, Urban <- (UrbanPopulation / TotalPopulation))
COVID <- within (COVID, Health_and_Disabilities_little <- (Daytodayactivitieslimitedalittle/ TotalPopulation))
COVID <- within (COVID, Health_and_Disabilities <- (Daytodayactivitieslimitedalot / TotalPopulation))
COVID <- within (COVID, No_Health_and_Disabilities <- (Daytodayactivitiesnotlimited / TotalPopulation))
COVID <- within (COVID, GDHI <- (GDHI2011))
COVID <- within (COVID, Hourly_Pay <- (FTHourlyGrossPayMean))

#observe summaries
summary(COVID$Covid_Deaths)
par("mar")
par(mar=c(3,3,3,3))

attach(COVID)

