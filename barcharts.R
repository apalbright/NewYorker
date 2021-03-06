#use libs
library(stringr);library(ggplot2);library(plyr);library(reshape2);#library(reshape);
library(grid);library(scales);library(RColorBrewer);library(ggthemes)

#load data
city=read.csv("citiescount.csv")

#add brooklyn & staten island to ny,ny and take these out
city$count[city$place == "New York, N.Y."] <- city$count[city$place == "New York, N.Y."] + city$count[city$place == "Brooklyn, N.Y."] + city$count[city$place == "Staten Island, N.Y."]

city<-city[!(city$place=="Brooklyn, N.Y."),]
city<-city[!(city$place=="Staten Island, N.Y."),]

#look at top cities
city$percent <- (city$count/466)*100
city1 <- city[ which(city$count >= 5), ]

ggplot(city1, aes(x=place, y=count))+
geom_bar(stat = "identity")+
theme_fivethirtyeight()+
labs(title= "", x="", y="")+
ggtitle(expression(atop(bold("Caption Contest Wins: Top 8 Cities"), atop(italic("Number of winning captions (limited to cities that have won 5 or more contests)"),""))))+
theme(plot.title = element_text(size = 20, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

#look at top states
st=read.csv("statesnew.csv")

#top 10
st$percent <- (st$count/466)*100
st1 <- st[ which(st$percent >= 3), ]

b<-ggplot(st1, aes(x=state, y=count))+
geom_bar(stat = "identity")+
theme_fivethirtyeight()+
labs(title= "", x="", y="")+
ggtitle(expression(atop(bold("Caption Contest Wins: Top 10 States"), atop(italic("Number of New Yorker caption contests won"),""))))+
theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = .5, hjust=0.5)) 
b+ scale_x_discrete(labels=c("CA","CT", "IL", "MD", "MA","NJ", "NY", "PA", "VA", "WA"))

#look at per population numbers--using the 2010 census numbers
st$pop <- c(4779736, 710231, 6392017, 2915918, 37253956, 5029196, 3574097, 897934, 18801310, 9687653, 1360301, 1567582, 12830632, 6483802, 3046355, 2853118, 4339367, 4533372, 1328361, 5773552, 6547629, 9883640, 5303925, 2967297, 5988927, 989415, 1826341, 2700551, 1316470, 8791894, 2059179, 19378102, 9535483, 672591, 11536504, 3751351, 3831074, 12702379, 1052567, 4625364, 814180, 6346105, 25145561, 2763885, 625741, 8001024, 6724540, 1852994, 5686986, 563626)
#deleted dc pop cause it's not a state

#make var captions per million residents
st$capp1000000 <- ((st$count)/(st$pop))*1000000
#same thing: st$ratep1000000 <- st$count/(st$pop/1000000)

#set cut-off to get top 10 states
st1 <- st[ which(st$capp1000000 >= 2.4), ]

a<-ggplot(st1, aes(x=state, y=capp1000000))+
geom_bar(stat = "identity")+
theme_fivethirtyeight()+
labs(title= "", x="", y="")+
ggtitle(expression(atop(bold("Caption Contest Wins Per Million: Top 10 States"), atop(italic("Number of New Yorker caption contests won per one million state residents"),""))))+
theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = .5, hjust=0.5)) 

a+ scale_x_discrete(labels=c("AK", "CT", "MD", "MA","NH", "NY", "RI", "VT", "VA", "WA"))