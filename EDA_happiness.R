#Load Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Amelia)
library(corrplot)
library(plot3D)

#import data
data15 <- read.csv("2015.csv")
data15 %>% head()
region <- levels(data15$Region)
new_regions <- c("Austrailia","Europe","Asia","South America","Africa","North America","Asia","Asia","Africa","Europe") 
data15$Continent <- data15$Region
levels(data15$Continent) <- new_regions
str(data15)

#Check if any missing values
missmap(data15)

# %% [code]
#lets see a plot between score and rank 
ggplot(data15,aes(x=Happiness.Rank,y=Happiness.Score,color=Continent)) + geom_point() + ggtitle("Rank VS Score")

# %% [code]
#Now lets see the corelation between coloumns
temp <-sapply(data15,is.numeric)
cor_data <- cor(data15[,temp])
corrplot(cor_data,method="number")

# %% [code]
#Lets look the continents with their averages
ggplot(data15,aes(x=Economy..GDP.per.Capita.,y=Trust..Government.Corruption.,color=Region)) + geom_point() +facet_wrap(Continent~.)

# %% [code]
ggplot(data15,aes(x=Continent,y=Happiness.Score,fill=Continent)) + geom_boxplot()

# %% [code]
ggplot(data15,aes(x=Health..Life.Expectancy.,y=Happiness.Score)) + geom_point(aes(color=Region),size=2,alpha=0.85) +geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T) + facet_wrap(Continent~.) + ggtitle("Regression of Life Expentancy Vs Happiness Score")

ggplot()
# %% [code]
scatter3D(data15$Freedom,
          data15$Health..Life.Expectancy,
          data15$Happiness.Score,
          phi=0,bty="g",pch=20,cex=2,ticktype="detailed",
          main="Happiness ",xlab="Freedom",ylab="Life Expectancy",zlab="Score")
