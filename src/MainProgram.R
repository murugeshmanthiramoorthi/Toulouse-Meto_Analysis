source("Meteo_Analysis.R") # Include the Source from another R
Input_files <- read.csv("../Input/Input_file.csv",header=F) # List of station to fetch
# Step 1 - Download Data
Download_Dataset(Input_files,dest_location="../Data/Stations")

# Step 2 - Merge the downloaded data sets of 43 Stations.
# Here we merge only the columns common between 43 stations

# write.csv(MergeAndCleanDataSets(),"../Data/Consolidated/ConsolidatedStationData.csv", row.names = F)

Consolidated_station <- MergeAndCleanDataSets()

# Datatype conversion

Consolidated_station$Date <- as.Date(Consolidated_station$heure_utc)
Consolidated_station$Time <- as.ts(Consolidated_station$heure_utc)


summary(Consolidated_station)



