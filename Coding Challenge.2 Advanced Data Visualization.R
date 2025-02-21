#Maryam Saeed Noor Fatima
#Code Challenge Advanced Data Visualization

# Q.No.1

# importing excel sheet
library(readr)
library(ggplot2)
library(readr)
library(ggplot2)
 
Mycotoxin_Data <- read.csv("MycotoxinData.csv", na.strings = "na")
str(Mycotoxin_Data) # structure of the dataframe
#importing the color blind pallet
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plotting boxplot


ggplot(Mycotoxin_Data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("DON (ppm)") + #changing the name of the y variable
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) + #making the points jitter
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) + #adding cbb pallet color manually
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() + #changing theme of the graph to classic
  facet_wrap(~Cultivar, scale = "free") #faceting the grap according to the cultivors



#Q.No.2

# Change the factor order level so that the treatment “NTC” is first, followed by 
# “Fg”, “Fg + 37”, “Fg + 40”, and “Fg + 70. 

Mycotoxin_Data$Treatment <- factor(Mycotoxin_Data$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

Plot.DON <-ggplot(Mycotoxin_Data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("DON (ppm)") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar)
Plot.DON

#Q.No.3

# Change the y-variable to plot X15ADON and MassperSeed_mg.
#plot with X15ADON


Plot.X15ADON <- ggplot(Mycotoxin_Data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("15ADON") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar)

Plot.X15ADON

# Changing the y variable to Seed Mass Plot

plot.seed.mass <- ggplot(Mycotoxin_Data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("Seed Mass (mg)") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar)

plot.seed.mass


# Q.No.4 Arranging all the plots in 1 picture

library(ggpubr)

Combine.plot <- ggarrange(
  Plot.DON,
  Plot.X15ADON, 
  plot.seed.mass, 
  labels = "auto",
  nrow = 1, #number of row is 1
  ncol = 3, # number of columns is 3
  common.legend = TRUE) #assigning one common legend to all

Combine.plot

### Answer: If we use common ligend function  
##then we don't need to assign the same legend for every plot.
### There will be only one common legend for all plots. 




## Q.No.5

# Use geom_pwc() to add t.test pairwise comparisons to the three plots made above. 
# Save each plot as a new R object, and combine them again with ggarange as you did in question 4. 

# a)
DONplot.pwc <- ggplot(Mycotoxin_Data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("DON (ppm)") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar) +
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(c("NTC", "Fg"), 
                                        c("Fg", "Fg + 37"), 
                                        c("Fg + 37", "Fg + 40"), 
                                        c("Fg + 40", "Fg + 70")))

DONplot.pwc


# b)
X15ADONplot.pwc <- ggplot(Mycotoxin_Data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("15ADON") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar) +
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(c("NTC", "Fg"), 
                                        c("Fg", "Fg + 37"), 
                                        c("Fg + 37", "Fg + 40"), 
                                        c("Fg + 40", "Fg + 70")))

X15ADONplot.pwc

# c) 
Seed.massplot.pwc <- ggplot(Mycotoxin_Data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(outliers = F) + 
  xlab("") + 
  ylab("Seed Mass (mg)") + 
  geom_point(alpha = 0.6, pch = 21, color = "black", position = position_jitterdodge()) +
  scale_color_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  scale_fill_manual(values = c(cbbPalette[3], cbbPalette[4])) +
  theme_classic() +
  facet_wrap(~Cultivar) +
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(c("NTC", "Fg"), 
                                        c("Fg", "Fg + 37"), 
                                        c("Fg + 37", "Fg + 40"), 
                                        c("Fg + 40", "Fg + 70")))

Seed.massplot.pwc

#combining all the graphs after runing the t.test on it
Combineplot.pwc <- ggarrange(
  DONplot.pwc,
  X15ADONplot.pwc, 
  Seed.massplot.pwc, 
  labels = "auto",
  nrow = 1,
  ncol = 3,
  common.legend = TRUE,
  legend = "right" )

Combineplot.pwc

