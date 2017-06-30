# Install packages required to run this script
list.of.packages <- c("dplyr", "stringr", "ggplot2", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load Libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

drawBarA <- function(inputs, graphTitle, yLabel) {

  print(graphTitle)
  print(inputs)
  
  df_molten <- melt(inputs, "Tools")
  print(df_molten)
  yLimit <- max(df_molten$value)+5
  
  df_molten$Tools <- factor(df_molten$Tools, levels=df_molten[order(-df_molten$value), "Tools"])
  
  ggplot(df_molten, aes(Tools, value, fill = variable, label=value)) + 
    geom_bar(stat="identity", position = "dodge", width=0.3, fill="royalblue1") + 
    ggtitle(graphTitle) + 
    ylab(yLabel) +
    scale_y_continuous(expand = c(0.01,0), limits = c(0, yLimit)) + 
    scale_x_discrete(expand = c(0.01,0)) 
}

drawHorizontalBarA <- function(inputs, graphTitle, yLabel) {
  
  plot <- drawBarA(inputs, graphTitle, yLabel)
  plot <- plot + coord_flip() + geom_text(aes(y=(value + 1.0)))
  
  plot <- plot + theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, family="Times", face="bold", color = "cornflowerblue"), 
    axis.text = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 10, face = "bold", family="Times", colour = "orange"), 
    panel.background = element_rect(fill="white"), 
    legend.position = "bottom",
    axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) 
  
  print(plot)
}

main <- function () {
  dataset <- read.table("/home/weapon-x/Documents/R/latestreport.txt")

  print(dataset)
  
  drawHorizontalBarA(dataset, "Project Management Tool", "Product Teams")
}

## Starting point of the script
main()

