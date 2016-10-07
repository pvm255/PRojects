library(ggplot2)
library(plotly)
library(dplyr)
 bike <- read.csv("C:/Users/priyanandana/Desktop/R/bikeshare.csv")
head(bike)

#Exploratory Data Analysis count vs temp
pl <- ggplot(data=bike, aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.3) + theme_bw()
print(pl)

#Plotting count vs datetime
class(bike$datetime)
bike$datetime <- as.POSIXct(bike$datetime)
class(bike$datetime)

pl2 <- ggplot(data = bike,aes(x=datetime,y=count)) +
       geom_point(aes(color=temp),alpha=0.5) + theme_bw()
print(pl2)

#correlation between tempreture and count
cor(bike$temp,bike$count)

#Exploring season data
pl3 <- ggplot(data = bike,aes(x=factor(season),y=count)) +
      geom_boxplot(aes(color=factor(season)))
print(pl3)

#Plot time and count 
bike$date <- as.Date(bike$datetime)
bike$time <- sapply(bike$datetime,function(x){format(x,"%H")})

pl4 <- ggplot(filter(bike,workingday==1),aes(x=time,y=count))+
       geom_point(aes(color=temp),alpha=0.3, position=position_jitter(w=1, h=0)) + theme_classic() + 
      scale_color_gradientn(colors=terrain.colors(30))
print(pl4)

#Creating a plot for time and count for non working days
head(bike)
summary(bike)

pl5 <- ggplot(filter(bike,workingday==0),aes(x=time,y=count))+
       geom_point(aes(color=temp),alpha=0.3, position=position_jitter(w=1, h=0)) + theme_classic() + 
  scale_color_gradientn(colors=terrain.colors(30))
print(pl5)

#linear model for count based on tempreture
model <- lm(formula = count~ temp,data=bike)
summary(model)

#Predicition for bike rentals if tempreture was 25 degrees 
temp.p <- data.frame(temp=c(25))
bikerentals <- predict(model,temp.p)
print(bikerentals)

#Converting Hour to numeric and predicting with more variables
time <- sapply(bike,as.numeric)
class(time)
mode(time)

#Building a model with more variables
main.model <- lm(count~.-casual -registered -datetime -atemp -date -time,bike)
summary(main.model)

#Conclusion This is a seasonal data and the linear regression doesnt work well with this
