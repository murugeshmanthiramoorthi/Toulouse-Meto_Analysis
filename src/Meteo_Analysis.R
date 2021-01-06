#Download a file from an URL

library(RCurl)
library(dplyr)
library(chron)
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(GGally)
##download.file("https://toulouse-metropole.opendatasoft.com/explore/dataset/00-station-meteo-toulouse-valade/download/?format=csv",
#              destfile = "../Data/00-station-meteo-toulouse-valade.csv",method="libcurl")

#Reading an Input file which contains stations list
##Input_files <- read.csv("../Input/Input_file_test.csv",header=F)




#Download multiple files

Download_Dataset <- function(x,base_url="https://toulouse-metropole.opendatasoft.com/explore/dataset/", 
                             download_parameter="/download/?format=csv",dest_location="../Data/",file_extension=".csv")
{
  sapply(x$V1,function(i)
    {
    download.file(paste0(base_url,i,download_parameter),destfile = paste0(dest_location,i,file_extension),method="libcurl") # paste0 uses NULL seperation factor
  })

}



MergeAndCleanDataSets <- function(sourcePath = "../Data/Stations", extension = "*.csv")
{
  list_files <- paste0(sourcePath,"/",list.files(path=sourcePath,pattern = extension))
  all_station_data <- lapply(list_files,read.csv,sep=";",header=T ) # Read all files, and store in list of dataframes
  # Do a col rename if it has wrong col name which does not match with other station dataset
  all_station_data <- lapply(all_station_data,function(x){
    if(!("heure_utc" %in% colnames(x)))
    {
      x <- rename(x,heure_utc=mm_dd_yy_hh_mm)
    }
    x
  })
  # Extract the common columns from all the datasets
  common_cols <- Reduce(intersect, lapply(all_station_data, names)) # To Find common columns in all the files
  #Merge All Dataframe and their common columns
  Consolidated_station<- do.call(rbind,lapply(all_station_data, "[", common_cols))
  Consolidated_station
}

CoerceCharacterToDate <- function(Dataset, columnName="heure_utc")
{
  Dataset[columnName][Dataset[columnName] == ""] <- NA
  dtparts = t(as.data.frame(strsplit(sub("\\+00:00","",Dataset[,columnName]),'T')))
  row.names(dtparts) = NULL
  Dataset["CoercedUTC"] <- as.POSIXct(chron(dates=dtparts[,1],times=dtparts[,2],format=c('y-m-d','h:m:s')), tz="utc")
  Dataset["date"] <- as.Date(Dataset$CoercedUTC)
  Dataset
}



cleanNA <- function(vector)
{
  vector <- vector[!is.na(vector)] 
}


createHistogram <- function(vector,breaks)
{
  hist(vector,breaks)
}

createHistogramWithoutNA <- function(vector,breaks)
{
  vector <- vector[!is.na(vector)]
  hist(vector,breaks)
}

createHistogramWithoutNAandZero <- function(vector,breaks)
{
  vector <- cleanNA(vector)
  vector <- vector[vector!=0]
  hist(vector,breaks)
}

# Univariate Analysis
# Withput log transformation
univariate_without_log <- function(vector, xlab, ylab, title){
  
  data_frame(val = vector) %>% ggplot(.,aes(x=vector)) + 
    geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
    labs(x = xlab, y = ylab,
         title = title,
         subtitle = "Univariate Analysis",
         caption = "Data: Toulouse Metropole") + 
    theme(axis.title.x = element_text(color = "Orangered", size = 15, vjust = -0.35),
          axis.title.y = element_text(color = "orangered", size = 15, vjust = 0.35),
          plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
          axis.line = element_line(arrow = arrow()),
          panel.background = element_rect(fill = "white")) 
  
}

# With log transformation for viewing skewed data
univariate_with_log <- function(vector,xlab, ylab, title){
  
  data_frame(val = vector) %>% ggplot(.,aes(x=vector)) + 
    geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
    scale_y_continuous(trans='log10') +
    labs(x = xlab, y = ylab,
         title = title,
         subtitle = "Univariate Analysis",
         caption = "Data: Toulouse Metropole") + 
    theme(axis.title.x = element_text(color = "Orangered", size = 15, vjust = -0.35),
          axis.title.y = element_text(color = "orangered", size = 15, vjust = 0.35),
          plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
          axis.line = element_line(arrow = arrow()),
          panel.background = element_rect(fill = "white")) 
  
}

# Bivariate Analysis
bivariate_line <- function(vector1,vector2, xlab, ylab, title){
  
  data_frame(vector1=vector1,vector2=vector2) %>% ggplot(.,aes(x=vector1, y=vector2)) + 
    geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2) + 
    scale_x_date(date_labels = "%m-%Y", date_minor_breaks = "1 month") + 
    labs(x = xlab, y = ylab,
         title = title,
         subtitle = "Bivariate Analysis",
         caption = "Data: Toulouse Metropole") + 
    theme(axis.title.x = element_text(color = "Orangered", size = 15, vjust = -0.35),
          axis.title.y = element_text(color = "orangered", size = 15, vjust = 0.35),
          plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
          axis.line = element_line(arrow = arrow()),
          panel.background = element_rect(fill = "white")) 
  
}

# Bivariate Analysis with mean values
bivariate_line_mean <- function(vector1,vector2, xlab, ylab, title){
  
  data_frame(vector1=vector1,vector2=vector2) %>% ggplot(.,aes(x=vector1, y=vector2)) + 
    geom_line(color="#69b3a2", size=2, alpha=0.9, linetype=2,stat = "summary", fun.y = "mean") + 
    scale_x_date(date_labels = "%m-%Y", date_minor_breaks = "1 month") + 
    labs(x = xlab, y = ylab,
         title = title,
         subtitle = "Bivariate Analysis",
         caption = "Data: Toulouse Metropole") + 
    theme(axis.title.x = element_text(color = "Orangered", size = 15, vjust = -0.35),
          axis.title.y = element_text(color = "orangered", size = 15, vjust = 0.35),
          plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
          axis.line = element_line(arrow = arrow()),
          panel.background = element_rect(fill = "white")) 
  
}


# Bivariate Barplot between any two variables



bivariate_bar <- function(vector1,vector2, xlab, ylab, title){
  
  data_frame(vector1=vector1,vector2=vector2) %>% ggplot(.,aes(x=vector1, y=vector2)) + 
    geom_bar(color="#69b3a2", alpha=0.9,stat = "summary", fun.y = mean) + 
    geom_hline(yintercept = mean(vector2), color="blue") + 
    labs(x = xlab, y = ylab,
         title = title,
         subtitle = "Bivariate Analysis",
         caption = "Data: Toulouse Metropole") + 
    theme(axis.title.x = element_text(color = "Orangered", size = 15, vjust = -0.35),
          axis.title.y = element_text(color = "orangered", size = 15, vjust = 0.35),
          plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
          axis.line = element_line(arrow = arrow()),
          panel.background = element_rect(fill = "white")) 
  
}




linear_model <- function(ConsolidatedData){
  
  model <- summary(lm(temperature_en_degre_c~humidite+direction_du_vecteur_de_vent_max+pression+direction_du_vecteur_vent_moyen+type_de_station+
                pluie+direction_du_vecteur_de_vent_max_en_degres+force_moyenne_du_vecteur_vent+force_rafale_max+temperature_en_degre_c+
                CoercedUTC+date, data=ConsolidatedData))
  
}


correlation <- function(ConsolidatedData){
  
  corr_data <- subset(ConsolidatedData, select = c(humidite,direction_du_vecteur_de_vent_max,pression,direction_du_vecteur_vent_moyen,pluie,
                                                   direction_du_vecteur_de_vent_max_en_degres,force_moyenne_du_vecteur_vent,force_rafale_max,
                                                   temperature_en_degre_c))
  
  #corr_data <- corr_data[!(is.na(corr_data$humidite)) | !(is.na(corr_data$direction_du_vecteur_de_vent_max)) | 
   #                        !(is.na(corr_data$pression)) | !(is.na(corr_data$direction_du_vecteur_vent_moyen)) |
    #                       !(is.na(corr_data$pluie)) | !(is.na(corr_data$direction_du_vecteur_de_vent_max_en_degres)) |
     #                      !(is.na(corr_data$force_moyenne_du_vecteur_vent)) | !(is.na(corr_data$force_rafale_max)) |
      #                     !(is.na(corr_data$temperature_en_degre_c))]
  
  df1_complete <- na.omit(corr_data)
  
  matrix <- cor(df1_complete)
  
}

correlation_plot <- function(ConsolidatedData){
  
  corr_data <- subset(ConsolidatedData, select = c(humidite,direction_du_vecteur_de_vent_max,pression,direction_du_vecteur_vent_moyen,pluie,
                                                   direction_du_vecteur_de_vent_max_en_degres,force_moyenne_du_vecteur_vent,force_rafale_max,
                                                   temperature_en_degre_c))
  
  df1_complete <- na.omit(corr_data)
  
  ggpairs(df1_complete, title="correlogram with ggpairs()") 
  
}





