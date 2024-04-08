install.packages("flexdashboard")
library(pacman)
p_load(tidyverse,lubridate,showtest)
library(showtext)
showtext_auto()
font_add_google("Bebas Neue","Bebas Neue")

netflix= read.csv(file.choose())
View(netflix)
#data cleaning
head(netflix)
mean(netflix$Title) #if missing value show warning message with NA
mean(netflix$duration,na.rm=TRUE)#Handling missig values in R
mean(netflix$Genre)
mean(netflix$Premiere)
mean(netflix$Runtime)
mean(netflix$IMDB.Score)
mean(netflix$duration)#do not have any missing value
mean(netflix$Language) 
is.na(netflix$IMDB.Score)

#Data cleaning operations
summary(netflix)
head(netflix)
library(dplyr)
#boxplot(netflix$IMDB.Score)
as_tibble(sapply(netflix,class))
netflix=netflix %>% mutate(Released = mdy(Premiere))
#library(dplyr)
#netflix = netflix %>% mutate(Year = year(Released))  %>% mutate(Month = month(Released, label=abc)) %>% mutate(Date = day(Released)) %>% mutate(Day = wday(Released, label=xyz,abbr=False))
#rlang::last_trace()  
library(dplyr)
library(ggplot2)
library(dplyr)
library(lubridate)
  n <- netflix %>%
  group_by(Released) %>%
  summarise(total = n())
  n_graph <- ggplot(data = n) +
  geom_col(mapping = aes(x = Released,
                         y = total,
                         fill = ifelse(total == max(total), "red", "grey"))) +
  labs(title = "Netflix Movies released each year") +
  theme_minimal() +
  scale_fill_manual(values = c("black", "red")) +
  theme(legend.position = "none",
        plot.title = element_text(family = "Bebas Neue",
                                  size = 25,
                                  color = "red"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Displaying the plot
print(n_graph)
  
 
#most popular Genres
library(dplyr)
n4 <- netflix %>% group_by(Genre) %>% 
  summarise(Movies=n()) %>% 
  arrange(desc(Movies)) %>% 
  head(5)
library(ggplot2)
n4_graph <-
  ggplot(data=n4)+
  geom_col(mapping = aes(
    x=reorder(Genre, -Movies),
    y=Movies,
    fill=ifelse(Movies == max(Movies),"red","blue")))+
  labs(title="Most Popular Genres")+
  theme_minimal()+
  scale_fill_manual(values = c("blue","red"))+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="black"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size=20)
  )

n4_graph


#most popular languages
library(dplyr)
library(ggplot)  
n5 <- netflix %>% 
  group_by(Language) %>% 
  summarise(Movies=n()) %>% 
  arrange(desc(Movies)) %>% 
  head(5)

n5_graph <- 
  ggplot(data=n5)+
  geom_col(mapping=aes(
    x=reorder(Language, -Movies),
    y=Movies,
    fill=ifelse(Movies == max(Movies),"yellow","orange")))+
  labs(title="Most Popular Languages")+
  theme_minimal()+
  scale_fill_manual(values=c("orange","yellow"))+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="black"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor = element_blank(),
    title=element_text(size=20)
  )

n5_graph



#IMDB Score Distribution
library(ggplot2)
library(dplyr)
n6_graph <- ggplot(netflix)+
  geom_dotplot(mapping=aes(x=`IMDB.Score`),
               binwidth=0.3,fill="darkblue",color="lightblue")+
  labs(title="IMDB Score Distribution")+
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="black"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n6_graph


#Highest rated Movies
library(ggplot2)
library(dplyr)
n7 <- netflix %>% arrange(desc(`IMDB.Score`)) %>% head(5)

n7_graph <- ggplot(data=n7)+
  geom_col(mapping=aes(
    x=reorder(`Title`,`IMDB.Score`),
    y=`IMDB.Score`,
    fill=ifelse(
      `IMDB.Score`==max(`IMDB.Score`),
      "red","black")))+
  labs(title="Highest Rated Movies")+
  theme_minimal()+
  scale_fill_manual(values = c("black","red"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="red"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n7_graph



#Lowest rated Movies
library(dplyr)
library(ggplot2)
n8 <- netflix %>% arrange(desc(-`IMDB.Score`)) %>% head(5)

n8_graph <- ggplot(data=n8)+
  geom_col(mapping=aes(
    x=reorder(`Title`, -`IMDB.Score`),
    y=`IMDB.Score`,
    fill=ifelse(
      `IMDB.Score`==min(`IMDB.Score`),
      "red","black")))+
  labs(title="Lowest Rated Movies")+
  theme_minimal()+
  scale_fill_manual(values = c("#2d2d2d","#E50914"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="red"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n8_graph



#Movie Runtime

n9_graph <- ggplot(data=netflix)+
  geom_dotplot(
    mapping=aes(x=Runtime),
    binwidth=2.25,
    fill="#2d2d2d",
    color="purple")+
  labs(title="Movie Runtime")+
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="red"),
    axis.title.x = element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n9_graph


#Longest Movies
n10 <- netflix %>% arrange(desc(Runtime)) %>% head(5)

n10_graph <- ggplot(data=n10)+
  geom_col(mapping=aes(
    x=reorder(`Title`,`Runtime`),
    y=`Runtime`,
    fill=ifelse(Runtime==max(`Runtime`),"purple","pink")))+
  labs(title="Longest Movies")+
  theme_minimal()+
  scale_fill_manual(values=c("pink","purple"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="black"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n10_graph


#Shortest Movies
n11 <- netflix %>% arrange(desc(-Runtime)) %>% head(5)

n11_graph <- ggplot(data=n11)+
  geom_col(mapping=aes(
    x = reorder(`Title`,`Runtime`),
    y = `Runtime`,
    fill = ifelse(Runtime==min(`Runtime`),"red","seagreen")))+
  labs(title="Shortest Movies")+
  theme_minimal()+
  scale_fill_manual(values = c("seagreen","#E50914"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="seagreen"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank(),
    text=element_text(size=20)
  )

n11_graph

#Runtime vs IMDB Rating

n12_graph <- ggplot(data=netflix,aes(x = `IMDB.Score`, y = Runtime))+
  geom_point()+
  geom_smooth(method = "lm", color="blue")+
  labs(title="Runtime vs IMDB Rating")+
  theme_minimal()+
  scale_fill_manual(values=c("lightgreen","lightblue"))+
  theme(
    legend.position = "none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="black"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n12_graph
