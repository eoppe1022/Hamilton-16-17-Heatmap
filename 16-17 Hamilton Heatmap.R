# Code for Heatmap for Hamilton Hockey players 16-17

library(ggplot2)
library(RColorBrewer)
library(reshape2)

ham <- read.csv("Hamilton 16-17 Stats.csv", header = TRUE, stringsAsFactors = FALSE)

# Delete players with not enough GP
ham2 <- ham[-c(4:7),]

# Pick columns for use in heatmap
ham3 <- ham2[,c(1, 6, 20, 33:35, 38, 45, 46)]

# Make DIV/0 into 0
ham3[ham3 == "#DIV/0!"] <- 0

# Make all variables numeric
ham3$PIM.........60. <- as.numeric(ham3$PIM.........60.)
ham3$X5v5.POINTS..60. <- as.numeric(ham3$X5v5.POINTS..60.)
ham3$CORSI.......60 <- as.numeric(ham3$CORSI.......60)
ham3$CORSI......60. <- as.numeric(ham3$CORSI......60.)
ham3$CORSI......60..1 <- as.numeric(ham3$CORSI......60..1)
ham3$iCF...60. <- as.numeric(ham3$iCF...60.)
ham3$FO. <- as.numeric(ham3$FO.)
ham3$GAME.SCORE <- as.numeric(ham3$GAME.SCORE)

# Sort players by highest average Game Score
ham3 <- ham3[order(ham3$GAME.SCORE),]

# Give row names
rownames(ham3) <- ham3$Name
ham3$Name <- NULL

# Change Column Names
colnames(ham3) <- c("PIM +/- (60)", 
                    "5v5 Points (60)", 
                    "Corsi +/- (60)", 
                    "Corsi + (60)", 
                    "Corsi - (60)", 
                    "Individual Corsi (60)", 
                    "FO %", 
                    "Game Score")

# Reverse Corsi Against so that best players are blue and worst are red
ham3$`Corsi - (60)` <- -1 * ham3$`Corsi - (60)`

# Standardize values
ham4 <- scale(ham3[,c(1:8)])

hammatrix <- data.matrix(ham4)
meltedmatrix <- melt(hammatrix[,1:7])

# Picks color palette (will be Red to White to Blue)
hm.palette <- colorRampPalette(brewer.pal(11, "RdBu"), space = "Lab")

# Creates the heatmap
heatmap <- ggplot(meltedmatrix, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() + 
  coord_equal(expand = TRUE) + 
  labs(title = "Hamilton 2016-17 Heatmap", subtitle = "Sorted by Highest Average Game Score") + 
  scale_fill_gradientn(colors = hm.palette(100), breaks = c(1.5, -1.5), labels = paste(c("Better", "Worse"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold", family = "mono"), 
        axis.text.x = element_text(size = 12, angle = 50, hjust = 1, face = "bold", family = "mono", margin = margin(6,0,3,0)),
        axis.text.y = element_text(size = 12, face = "bold", family = "mono"), 
        axis.ticks.y = element_blank(),
        axis.title = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold", family = "mono", margin = margin(0, 40, 0, 0)),
        legend.margin = margin(0, 0, 0, 30),
        plot.margin = unit(c(5, 0, 8, 0), "mm"), 
        plot.subtitle = element_text(size = 13, face = "bold", family = "mono", hjust = 0.5, margin = margin(0,0,30,0)))

# Plots the heatmap
heatmap

