# Code for Heatmap for Hamilton Hockey players 16-17
# Using base heatmap() function


ham <- read.csv("Hamilton 16-17 Stats.csv",
    header = TRUE, stringsAsFactors = FALSE)

# Delete players with not enough GP
ham2 <- ham[-c(5:7, 15),]

# Pick columns for use in heatmap
ham3 <- ham2[,c(1, 6, 14, 20, 33:35, 38, 45, 46)]

# Make DIV/0 into 0
ham3[ham3 == "#DIV/0!"] <- 0

# Make all variables numeric
ham3$PIM.........60. <- as.numeric(ham3$PIM.........60.)
ham3$ALL.SIT..PRIMARY.POINTS..60. <- as.numeric(ham3$ALL.SIT..PRIMARY.POINTS..60.)
ham3$X5v5.POINTS..60. <- as.numeric(ham3$X5v5.POINTS..60.)
ham3$CORSI.......60 <- as.numeric(ham3$CORSI.......60)
ham3$CORSI......60. <- as.numeric(ham3$CORSI......60.)
ham3$CORSI......60..1 <- as.numeric(ham3$CORSI......60..1)
ham3$iCF...60. <- as.numeric(ham3$iCF...60.)
ham3$FO. <- as.numeric(ham3$FO.)
ham3$GAME.SCORE <- as.numeric(ham3$GAME.SCORE)

# Order heatmap by average game score
ham3 <- ham3[order(ham3$GAME.SCORE),]

# Give row names
rownames(ham3) <- ham3$Name
ham3$Name <- NULL

# Change Column Names
colnames(ham3) <- c("PIM +/- (60)", "All Sit. Primary Points (60)", 
    "5v5 Points (60)", "Corsi +/- (60)", "Corsi + (60)", "Corsi - (60)", 
    "Individual Corsi (60)", "FO %", "Game Score")

# Reverse Corsi Against so that best players are blue and worse are yellow
ham3$`Corsi - (60)` <- -1 * ham3$`Corsi - (60)`

ham4 <- scale(ham3[,c(1:9)])
hammatrix <- data.matrix(ham4)

# Define color palette
my_palette <- colorRampPalette(c("khaki", "blue3"))(n = 299)

# Make the heatmap
heatmap <- heatmap(hammatrix[,1:8], Rowv=NA, Colv=NA,
    scale="column", main="Hamilton 16-17 Heatmap", col=my_palette,
    margins=c(15,8))
