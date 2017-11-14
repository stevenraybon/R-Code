#Use data from NLSY dataset to plot wages and then use Oaxaca-Blinder decomposition
#to analyze mean gender difference

library(ggplot2)
library(plyr)
setwd("C:/Users/Steven/SkyDrive/R Code/Data Sets/NLSY97")

data <- read_csv('NLSY_Panel_forR.csv')
cols <- c('inid','year','female', 'married', 'log_wage')
df <- data[cols]
df <- na.omit(df)

#Want to turn female variable into factor variabe; right now it's integer
df$female <- as.factor(df$female)
#check it
levels(df$female)


attach(df)
datf <- df[df$female==1,] 
datm <- df[df$female==0,]


#Typical Histogram
hist(datf$log_wage, col=rgb(0,0,1,.5),xlim=c(-7,7), ylim=c(0,15000), main="Overlapping Histogram", xlab="Variable")
hist(datm$log_wage, col=rgb(0,1,0,.5), add=T)
box()

#ggplot histogram
#(1)
qplot(df$log_wage,data=df,fill=female)

#(2)
lnwage <- df$log_wage
Female <- df$female
ggplot(df, aes(lnwage, fill=female)) + geom_histogram(alpha=0.4)

#(3)
g1 <- ggplot(df, aes(lnwage, fill=female)) + geom_histogram(alpha=0.4) + facet_grid(female ~.) + 
  ggtitle("Log Wages for Men and Women",subtitle = "Female = 1")
plot(g1)


