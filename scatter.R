#use libs
library(stringr);library(ggplot2);library(plyr);library(reshape2);#library(reshape);
library(grid);library(scales);library(RColorBrewer)

#load data
st=read.csv("states3.csv")

#Create a function for the look of my charts
#Used minimaxir's code as base R code to work off of
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=20, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

#create scatter for % lib
ggplot(st, aes(x=lib, y=cap))+
my_theme()+
geom_point(shape=1) +
geom_smooth(method=lm)+
labs(title= "", x="Percentage of state residents who describe their political views as liberal", y="Caption Contest Wins Per Million Residents")+
ggtitle(expression(atop(bold("Damn Liberal States and Their Caption Contests"), atop(italic("Association between liberal % and caption contest wins per million"),""))))+
geom_text(aes(label=ab), vjust=-1, hjust=0.5, size=2)+
theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

#create scatter for % nonreligious
ggplot(st, aes(x=noreg, y=cap))+
my_theme()+
geom_point(shape=1) +
geom_smooth(method=lm)+
labs(title= "", x="Percentage of state residents who describe themselves as nonreligious", y="Caption Contest Wins Per Million Residents")+
geom_text(aes(label=ab), vjust=-1, hjust=0.5, size=2)+
ggtitle(expression(atop(bold("Damn Nonreligious States and Their Caption Contests"), atop(italic("Association between nonreligious % and caption contest wins per million"),""))))+
theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5))