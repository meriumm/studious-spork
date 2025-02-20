#Maryam Saeed Noor Fatima
#Coding Challenge 2

library(readr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggrepel) 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#importing the excel file
MycotoxinData <- read.csv("MycotoxinData.csv",na = "na")

#Q.No.1
#creating a ggplot box plot
plot.1<- ggplot(data = MycotoxinData, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(color = "black") +  # Black outline for boxplots
  geom_point(aes(fill = Cultivar), shape = 21, color = "black", alpha = 0.6, #b. Changing the transparency to 0.6
             position = position_jitterdodge(dodge.width = 0.9)) +  #a. Jitter points on the boxplot
  scale_fill_manual(values = c(cbbPalette[6], cbbPalette[4])) +  # Fill colors for Cultivar
  xlab("") +
  ylab("DON(ppm)") + #c. Changing the y axis
  theme_classic() + #d. classic theme
  facet_wrap(~Cultivar, scale = "free") #e. faceting by Cultivar
plot.1


#Q.No.2
#Changing the factor levels
MycotoxinData$Treatment<- factor(MycotoxinData$Treatment, levels = c("NTC","Fg","Fg + 37","Fg + 40","Fg + 70")

 #Q.No.3
 #a. using 15ADON as Y variable
 
plot.2<- ggplot(data = MycotoxinData, aes(x = Treatment, y = X15ADON , fill = Cultivar)) +
   geom_boxplot(color = "black") +  # Black outline for boxplots
   geom_point(aes(fill = Cultivar), shape = 21, color = "black", alpha = 0.6, #b. Changing the transparency to 0.6
              position = position_jitterdodge(dodge.width = 0.9)) +  #a. Jitter points on the boxplot
   scale_fill_manual(values = c(cbbPalette[6], cbbPalette[4])) +  # Fill colors for Cultivar
   xlab("") +
   ylab("15ADON") + #c. Changing the y axis
   theme_classic() + #d. classic theme
   facet_wrap(~Cultivar, scale = "free") #e. faceting by Cultivar
 plot.2

 #b. using MassperSeed_mg as Y variable
 plot.3<- ggplot(data = MycotoxinData, aes(x = Treatment, y = MassperSeed_mg , fill = Cultivar)) +
   geom_boxplot(color = "black") +  # Black outline for boxplots
   geom_point(aes(fill = Cultivar), shape = 21, color = "black", alpha = 0.6, #b. Changing the transparency to 0.6
              position = position_jitterdodge(dodge.width = 0.9)) +  #a. Jitter points on the boxplot
   scale_fill_manual(values = c(cbbPalette[6], cbbPalette[4])) +  # Fill colors for Cultivar
   xlab("") +
   ylab("MassperSeed_mg") + #c. Changing the y axis
   theme_classic() + #d. classic theme
   facet_wrap(~Cultivar, scale = "free") #e. faceting by Cultivar
 plot.3
 
 #Q.No.4
 #using ggarrange 
 
plot.4<- ggarrange(plot.1, plot.2, plot.3,
          nrow =3,
          ncol= 1,
          labels = "auto",
          common.legend = T) #The common legend function give only one legend for all the 3 plot and T is for 
                            #placing the legend at the top.
plot.4







