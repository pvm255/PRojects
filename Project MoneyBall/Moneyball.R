#Project MoneyBall
library(ggplot2)
library(dplyr)
bat <- read.csv("C:/Users/priyanandana/Desktop/R/Project MoneyBall/Batting.csv")
head(bat)
str(bat)
head(bat$AB)
head(bat$X2B)

#Calculating Batting Average
bat$BA <- bat$H/bat$AB
tail(bat$BA,5)

#Calculating On-Base-Percentage
bat$OBP <- (bat$H + bat$BB + bat$HBP)/(bat$AB + bat$BB + bat$HBP + bat$SF ) 
tail(bat$OBP)

#Calculating Slugging Percentage
bat$X1B <- bat$H - bat$X2B - bat$X3B - bat$HR
tail(bat$X1B)
bat$SLG <- (bat$X1B + (2*bat$X2B) + (3*bat$X3B) + (4*bat$HR))/bat$AB
tail(bat$SLG)
str(bat)

#Grabing data after 1984 to match salaries Data Frame
summary(bat)
bat1 <- subset(bat, yearID >1984)
summary(bat1)

#Loading Salary Data Frame 
sal <- read.csv("C:/Users/priyanandana/Desktop/R/Project MoneyBall/Salaries.csv")
combo <- merge(bat1,sal,by = c('playerID','yearID'))
summary(combo)

#Separating Lost Players and extracting information only from 2001
lost.Players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost.Players <- subset(lost.Players,yearID == 2001)
print(lost.Players) 
head(lost.Players)

lost.Players <- lost.Players[c("playerID","H","X2B","X3B","HR","OBP","SLG","BA","AB")]
print(lost.Players)

#exploratory plot
pl <- ggplot(combo,aes(x=OBP,y=salary)) + geom_point(size=2)
print(pl)

#Replacing Lost players
combo <- subset(combo,salary < 8000000 && OBP >0)
combo <- subset(combo,AB >= 450)
str(combo)
head(combo)

#Goals mean of AB > 450,Salary for three replacement players == 15million and average OBP of 0.364
player.options <- arrange(combo,desc(OBP))
print(player.options)
player.options[,c('playerID','AB','salary','OBP')]
                    
#Chosen players
player.options[c(7,5,6),c('playerID','AB','salary','OBP')]

