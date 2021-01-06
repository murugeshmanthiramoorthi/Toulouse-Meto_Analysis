


bivariate_bar(Data$id,Data$pression,"Stations","Pression","Distribution of Pression across stations")
bivariate_bar(Data$id,Data$force_rafale_max,"Stations","Burst_Force","Distribution of Max Burst Force across stations")



bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$pression, "Date", "Pluie", "Mean value of Pluie across dates")



da <- summarise_at(group_by(Data,id),vars(force_rafale_max),funs(mean(.,na.rm=TRUE)))

da1 <- da[order(da$force_rafale_max, decreasing = T),]


d1 <- ConsolidatedData[ConsolidatedData$id==50,]             
summary(d1)   


univariate_without_log(ConsolidatedData$force_rafale_max, "Burst_Force ", "Count", "Max Burst Force") +
  xlim(0, 40) +
  ylim(0,150000)


univariate_without_log(ConsolidatedData$force_moyenne_du_vecteur_vent, "Wind_Force ", "Count", "Average Wind Force") +
  xlim(0, 20) +
  ylim(0,250000)


bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$force_rafale_max, "Date", "force_rafale_max", "force_rafale_max across dates")


bivariate_line_mean(ConsolidatedData$date, ConsolidatedData$force_moyenne_du_vecteur_vent, "Date", "force_rafale_max", "Mean value of force_rafale_max across dates")
