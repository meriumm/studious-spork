#MARYAM SAEED   NOOR FATIMA
#Data Visualization Coding Challenge

#Reading the excel sheet in R
library(readr)
library(ggplot2)

Mycotoxin_Data <- read.csv("MycotoxinData.csv", na = "na")
View(Mycotoxin_Data)

## Q.No. 2 Making a box plot
ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, color = Cultivar))+
  geom_boxplot()+
  xlab("")+
  ylab("DON (ppm)")

## Q.No.3 Converting data into Bar Chart
ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
  stat_summary(fun = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data =  mean_se, geom = "errorbar", position = "dodge")+
  xlab("")+
  ylab("DON (ppm)")

## Q.No.4a Adding points to the box plot

ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
  geom_boxplot()+
  geom_point(shape = 21, col ="black",
             position = position_jitterdodge(dodge.width = 0.97))+
  xlab("")+
  ylab("DON (ppm)")

## Q.No.4b Adding points to the bar chart

ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
  stat_summary(fun = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data =  mean_se, geom = "errorbar", position = "dodge")+
  geom_point(shape = 21, col = "black", position = position_jitterdodge(dodge.width = 0.97))+
  xlab("")+
  ylab("DON (ppm)")

##Q.No. 5a Change the color of points the box plot

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
  geom_boxplot()+
  geom_point(shape = 21, col ="black",
             position = position_jitterdodge(dodge.width = 0.97))+
  scale_fill_manual(values = cbbPalette)
  xlab("")+
  ylab("DON (ppm)")
  
##Q.No. 5bchange the pallet color of the bar chart
  
  ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
    stat_summary(fun = mean, geom = "bar", position = "dodge")+
    stat_summary(fun.data =  mean_se, geom = "errorbar", position = "dodge")+
    geom_point(shape = 21, col = "black", position = position_jitterdodge(dodge.width = 0.97))+
    scale_fill_manual(values = cbbPalette)
    xlab("")+
    ylab("DON (ppm)")
    

##Q.No.6a Add a facet to the plots based on cultivar
    #box plot
    
    ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
      geom_boxplot()+
      geom_point(shape = 21, col ="black",
                 position = position_jitterdodge(dodge.width = 0.97))+
      scale_fill_manual(values = cbbPalette)+
      facet_wrap(~Cultivar)+
    xlab("")+
      ylab("DON (ppm)")
## Q.No.6b facet wrap on bar chart
    ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
      stat_summary(fun = mean, geom = "bar", position = "dodge")+
      stat_summary(fun.data =  mean_se, geom = "errorbar", position = "dodge")+
      geom_point(shape = 21, col = "black", position = position_jitterdodge(dodge.width = 0.97))+
      scale_fill_manual(values = cbbPalette)+
      facet_wrap(~Cultivar)+
    xlab("")+
      ylab("DON (ppm)")
##Q.No. 7a Adding transparency to the points of boxplot
    ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
      geom_boxplot()+
      geom_point(shape = 21, col ="black",alpha = 0.5,
                 position = position_jitterdodge(dodge.width = 0.97))+
      scale_fill_manual(values = cbbPalette)+
      facet_wrap(~Cultivar)+
      xlab("")+
      ylab("DON (ppm)")
##Q.No.7b Adding transparency to the points of the bar chart
    ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
      stat_summary(fun = mean, geom = "bar", position = "dodge")+
      stat_summary(fun.data =  mean_se, geom = "errorbar", position = "dodge")+
      geom_point(shape = 21, col = "black", alpha = 0.5, position = position_jitterdodge(dodge.width = 0.97))+
      scale_fill_manual(values = cbbPalette)+
      facet_wrap(~Cultivar)+
      xlab("")+
      ylab("DON (ppm)")
##Q.No.8 Plotting data using violin plots and Jitterpoints
    
      ggplot(data = Mycotoxin_Data, aes( x = Treatment, y = DON, fill = Cultivar))+
      geom_violin(trim = FALSE, alpha = 0.5)+
      geom_point(shape = 21, col = "black", alpha = 0.5, position = position_jitterdodge(dodge.width = 0.97))+
      scale_fill_manual(values = cbbPalette)+
      facet_wrap(~Cultivar)+
      xlab("")+
      ylab("DON (ppm)")
      #Answer: I choosed violin plot because it show summary statistics and 
      #distribution of data points in the plots.
    
    
    
    
















