source("Meteo_Analysis.R")

ConsolidatedData <-MergeAndCleanDataSets()

ConsolidatedData <- CoerceCharacterToDate(ConsolidatedData)


# Univariate Analysis of pluie

univariate_without_log(ConsolidatedData$pluie, "Values of Pluie", "Count", "Distribution of Pluie")

univariate_with_log(ConsolidatedData$pluie, "Values of Pluie", "Count", "Distribution of Pluie with Log10 Transformation")


# Univariate Analysis of Temperature

univariate_without_log(ConsolidatedData$temperature_en_degre_c, "Values of Temperature", "Count", "Distribution of Temperature")


# Bivariate Analysis of pluie

bivariate_line(ConsolidatedData$date, ConsolidatedData$pluie, "Date", "Pluie", "Distribution of Pluie across dates")


# Bivaraiate Analysis of temperature

bivariate_line(ConsolidatedData$date, ConsolidatedData$temperature_en_degre_c, "Date", "Temperature", "Distribution of Temperature across dates")

correlation_plot(ConsolidatedData)


# Bivariate Analysis with moving average

bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$pluie, "Date", "Pluie", "Mean value of Pluie across dates")

bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$temperature_en_degre_c, "Date", "Temperature", "Mean value of Temperature across dates")



# Bivariate Barplot


ConsolidatedData <- na.omit(ConsolidatedData)

Data <- ConsolidatedData[!ConsolidatedData$id==47,]



bivariate_bar(Data$id, Data$pluie, "Station ID", "Pluie", "Distribution of Pluie across stations")

bivariate_bar(Data$id, Data$temperature_en_degre_c, "Station ID", "Temperature", "Distribution of Temperature across stations")


bivariate_bar(Data$type_de_station, Data$pluie, "Station ID", "Pluie", "Distribution of Pluie across stations")


# Idendifying top significant parameters
coordinates <- linear_model(ConsolidatedData)


# Getting Correlation Matrix

corr_matrix <- correlation(ConsolidatedData)

corr_matrix

ld <- linear_model(ConsolidatedData)
ld
