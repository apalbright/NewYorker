#use libs
library(choroplethr); #library(choroplethrMaps)
library(stringr);library(ggplot2);library(plyr);library(reshape2);#library(reshape);
library(grid);library(scales);library(RColorBrewer);library(ggthemes)

states=read.csv("states1.csv")

#define theme for looks
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="white", color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color="white",size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill="white")) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) +
  theme(legend.title = element_text(size=11,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=20, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=0,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=0,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=0,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=0,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, 0, 0, 0), "cm"))
}

#create 1st map
state_choropleth(states, title="", legend="Wins", num_colors=5) + 
my_theme()+
ggtitle(expression(atop(bold("Geography of Humor: The New Yorker Caption Contest"), atop(italic("Total Caption Contest Wins"),""))))+
theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

#create 2nd map
states1=read.csv("states2.csv")

state_choropleth(states1, title="", legend="Wins/Million", num_colors=5) + #can do 9 as well for num_colors
my_theme()+
ggtitle(expression(atop(bold("Geography of Humor: The New Yorker Caption Contest"), atop(italic("Caption Contest Wins Per Million Residents"),""))))+
theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 
