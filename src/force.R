
source("Meteo_Analysis.R")
Data <- MergeAndCleanDataSets()

#Univariate Analysis of Wind force
univariate_without_log(Data$force_moyenne_du_vecteur_vent,"Wind_Force","Count","Average force of wind vector") + xlim(0,20)

#Univariate Analysis of Burst Force
univariate_without_log(Data$force_rafale_max,"Burst_Force","Count","Max Burst Force") + xlim(0,40)

Data <- na.omit(Data)

Data <-Data[!Data$id==47,]

#Bivariate Analysis of Wind Force2
bivariate_bar(Data$id,Data$force_moyenne_du_vecteur_vent,"Stations","Wind_Force","Wind Force in different Stations")


bivariate_bar(Data$id,Data$force_rafale_max,"Stations","Burst_Force","Max Burst Force in different Stations")

bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$force_rafale_max, "Date", "force_rafale_max", "force_rafale_max across dates")


# Removing null values 
Data <- na.omit(Data)

# Removing irrelevant values
Data <- ConsolidatedData[!Data$id==47,]

da <- summarise_at(group_by(Data,id),vars(force_rafale_max),funs(mean(.,na.rm=TRUE)))

da1 <- da[order(da$force_rafale_max, decreasing = T),]

# Lollipop chart
ggplot(da1, aes(x=id, y=force_rafale_max)) +
  geom_segment( aes(x=id, xend=id, y=0, yend=force_rafale_max ), color=ifelse(da1$id %in% c("1","50"), "orange", "grey"), size=ifelse(da1$id %in% c("1","50"), 1.3, 0.7) ) +
  geom_point( color=ifelse(da1$id %in% c("1", "50"), "orange", "grey"), size=ifelse(da1$id %in% c("1","50"), 5, 2) ) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position="none") +
  xlab("Station ID") +
  ylab("Burst Force") +
  ggtitle("Max and Min Burst Force Stations") +
  annotate("text", x=4, y=8, 
             label="Toulouse Météopole weather station", 
             color="orange", size=4 , angle=0, fontface="bold", hjust=0) +
  annotate("text", x=50, y=1.5, 
           label="Blagnac Quinze Sols weather station", 
           color="orange", size=4 , angle=0, fontface="bold", hjust=0) 








