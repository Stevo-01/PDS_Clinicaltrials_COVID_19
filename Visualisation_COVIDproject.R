library('readr')
library('dplyr')
library('tidyr')
library('rlist')
library('Hmisc')
library('stringr')
library('treemap')
library('ggplot2')
library('plotly')
library('tidyverse')
library(lubridate, warn.conflicts = FALSE)
library(gapminder)

#setting the working directory
setwd("/Users/steve/Documents/COVID")
#Loading data
df = read.csv('MyCleanData.csv')
#view the data
view(df)


# Showing countries with enrollment greater than 200,000. We try to higher the number of enrollment to make out visualization more neat
df %>% select(MapCountry, Status, Enrollment) %>%
  filter(Enrollment >= 200000) %>%
  group_by(Status == 'Recruiting') %>%
  ggplot(mapping = aes(x=MapCountry, y=log(Enrollment)))+geom_violin()+
  geom_boxplot(width=0.1)+ggtitle('Countries by Enrollment Greater Than 200,000')+ xlab('MapCountry')+
  ylab('Status per Enrollment(Log)')

  
# Visualizing based on over all gender enrolled
  df%>%
    group_by(Gender)%>%
    count(Gender)%>%
    ggplot(aes(Gender, n,fill=Gender))+
    geom_col()+
    geom_text(aes(label=n))+
    labs(x="Gender" , "Number of candidates" , title = "Number of Over all Study per Gender")

  # Number of completed study as of now based on gender
  df%>%
    filter(Status=="Completed")%>%
    group_by(Gender)%>%
    count(Gender)%>%
    ggplot(aes(Gender, n,fill=Gender))+
    geom_col()+
    geom_text(aes(label=n))+
    labs(x="Gender" , "Number of candidates" , title = "Number of Completed Study per Gender")  

  
  # Recruitment based on gender
  df%>%
    filter(Status=="Recruiting")%>%
    group_by(Gender)%>%
    count(Gender)%>%
    ggplot(aes(Gender, n,fill=Gender))+
    geom_col()+
    geom_text(aes(label=n))+
    labs(x="Gender" , "Number of candidates" , title = "Number of Still Recruiting Study per Gender") 
  
  # Suspended cases based on gender
  df%>%
    filter(Status=="Suspended")%>%
    group_by(Gender)%>%
    count(Gender)%>%
    ggplot(aes(Gender, n,fill=Gender))+
    geom_col()+
    geom_text(aes(label=n))+
    labs(x="Gender" , "Number of candidates" , title = "Number of Suspended Study per Gender") 
  
  # Number of completed Study per country
  df%>%
    group_by(MapCountry)%>%
    filter(Status=="Completed")%>%
    count(MapCountry)%>%
    ungroup()%>%
    mutate(percentage=round((n/sum(n))*100,1))%>%
    arrange(desc(n))%>%
    ggplot(aes(reorder(MapCountry,n),n, fill=MapCountry,label = percentage))+
    geom_col(show.legend = F)+
    geom_text(size=4,color="black")+
    coord_flip()+
    scale_y_continuous(breaks = seq(0,150,10))+
    labs(x="List Of Countries" , y= "Study" , title ="Number of Completed Study per Country")
  
  
# 6 countries with highest number of Studies with Completion years set per country for each Study.
  df %>%
    filter(MapCountry %in% c("Italy", "Spain", "France", "China","United States", "United Kingdom")) %>%
    ggplot(aes(x=MapCountry,y=Completion.Year, fill = MapCountry)) +
    geom_boxplot() + 
    facet_wrap(~Month, ncol= 4) + theme(
      axis.text.x = element_text(angle=90, size=5 ),
      axis.title.y = element_text(color="cadetblue" , vjust=0.35)   
    )

##################################################################################################################################################################################################################################################################################################################s
  
              