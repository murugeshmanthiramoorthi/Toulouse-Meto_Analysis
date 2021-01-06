library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Create data
set.seed(1000)
data <- data.frame(
  x=LETTERS[1:26], 
  y=abs(rnorm(26))
)

# Reorder the data
data <- data %>%
  arrange(y) %>%
  mutate(x=factor(x,x))

# Plot
p <- ggplot(da1, aes(x=id, y=temperature_en_degre_c)) +
  geom_segment( aes(x=id, xend=id, y=0, yend=temperature_en_degre_c ), color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 5, 2) ) +
  theme_ipsum() +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("Value of Y") +
  ggtitle("How did groups A and D perform?")

# Add annotation
p + annotate("text", x=grep("D", data$x), y=data$y[which(data$x=="D")]*1.2, 
             label="Group D is very impressive", 
             color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
  
  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, 
           label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" ) , 
           color="orange", size=4 , angle=0, fontface="bold", hjust=0)


ConsolidatedData %>%
  arrange(temperature_en_degre_c) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  ggplot( aes(x=id, y=temperature_en_degre_c)) +
  geom_segment( aes(xend=id, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")


p <- ggplot(da1, aes(x=id, y=force_rafale_max)) +
  geom_segment( aes(x=id, xend=id, y=0, yend=force_rafale_max ), color=ifelse(da1$id %in% c("1","50"), "orange", "grey"), size=ifelse(da1$id %in% c("1","50"), 1.3, 0.7) ) +
  geom_point( color=ifelse(da1$id %in% c("1", "50"), "orange", "grey"), size=ifelse(da1$id %in% c("1","50"), 5, 2) ) +
  theme_ipsum() +
  coord_flip() +
  theme(legend.position="none") +
  xlab("Station ID") +
  ylab("Burst Force") +
  ggtitle("Max and Min Burst Force Stations")


p

p + annotate("text", x=4, y=8, 
             label="Toulouse Météopole weather station", 
             color="orange", size=4 , angle=0, fontface="bold", hjust=0) +
  annotate("text", x=50, y=1.5, 
           label="Blagnac Quinze Sols weather station", 
           color="orange", size=4 , angle=0, fontface="bold", hjust=0) 

lolipop_chart <- function(data, vector1,vector2){
  
  data %>% ggplot(., aes(x=vector1, y=vector2)) +
    geom_segment( aes(x=vector1, xend=vector1, y=0, yend=vector2 ), color=ifelse(data$vector1 %in% c("1","50"), "orange", "grey"), size=ifelse(data$vector1 %in% c("1","50"), 1.3, 0.7) ) +
    geom_point( color=ifelse(data$vector1 %in% c("1", "50"), "orange", "grey"), size=ifelse(data$vector1 %in% c("1","50"), 5, 2) ) +
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
  
}


lolipop_chart(ConsolidatedData, "id", "force_rafale_max")


