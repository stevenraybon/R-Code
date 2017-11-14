#Use data from NLSY dataset to plot wages and then use Oaxaca-Blinder decomposition
#to analyze mean gender difference

library(ggplot2)

#Read data into dataframe
#Use only a subset of columns
#drop NaN values
data <- read_csv('NLSY_Panel_forR.csv')
cols <- c('year','female', 'married','age', 'tot_child', 
          'urban', 'White','Black','Asian','Am_Indian','edulev','log_wage')
df <- data[cols]
df <- na.omit(df)

#Variables I want as factors as integers. Change into factors here
df$female    <- as.factor(df$female)
df$married   <- as.factor(df$married)
df$urban     <- as.factor(df$urban)
df$White     <- as.factor(df$White)
df$Black     <- as.factor(df$Black)
df$Asian     <- as.factor(df$Asian)
df$Am_Indian <- as.factor(df$Am_Indian)
df$edulev    <- as.factor(df$edulev)

#check it
levels(df$female)

#These dataframes are used in the graphs below
datf <- df[df$female==1,] 
datm <- df[df$female==0,]
lnwage <- df$log_wage


#Typical Histogram: no frills
hist(datf$log_wage, col=rgb(0,0,1,.5),xlim=c(-7,7), ylim=c(0,15000), main="Overlapping Histogram", xlab="Variable")
hist(datm$log_wage, col=rgb(0,1,0,.5), add=T)
box()

#ggplot histogram: looks better
g1 <- ggplot(df, aes(lnwage,fill=female)) + 
  geom_density(alpha=0.7) +
  scale_fill_manual(values=c("#0066CC", "#FF99CC")) +
  facet_grid(female ~.) + 
  ggtitle("Log Wages for Men and Women",subtitle = "Female = 1")
plot(g1)

#Boxplots
g2 <- ggplot(df, aes(x=female, y=lnwage)) + 
  geom_boxplot(fill="#CC9966", alpha=.7) +
  ggtitle("Log Wage Boxplots by Gender", subtitle = "Female = 1") + 
  scale_y_continuous(name = "Log Wages")
plot(g2)


#################
#Run Regressions#
#################
#Run two regressions for men and women
#regress log wages on covariates and then apply decomp formula


##Regression for men
df_m <- subset(df,df$female==0)
fit_m <- lm(df_m$log_wage ~ df_m$married + df_m$urban + 
              df_m$White + df_m$Black + df_m$Asian + df_m$Am_Indian + df_m$edulev, data=df_m)

#Put coefficients in a vector
#Will use this in the decomposition below
coef_m <- coef(summary(fit_m))[,1]

#Get means of all the covariates and then put them in a vector
avg_married_m <- mean(as.integer(df_m$married))-1
avg_urban_m   <- mean(as.integer(df_m$urban))-1
avg_White_m   <- mean(as.integer(df_m$White))-1
avg_Black_m   <- mean(as.integer(df_m$Black))-1
avg_Asian_m   <- mean(as.integer(df_m$Asian))-1
avg_Indian_m   <- mean(as.integer(df_m$Am_Indian))-1

#Education levels are multi-factored and if I use means in the 
#decomposition below, I need to get the means for the variables
#used in the actual regression. So I get the mean for the levels
#that show up in the regression output: level2 and level3

#level 2 education
edulev2_m <- as.integer(df_m$edulev)
edulev2_m[edulev2_m==1] <- 0
edulev2_m[edulev2_m==2] <- 1
edulev2_m[edulev2_m==3] <- 0
avg_edulev2_m <- mean(edulev2_m)

#level 3 education
edulev3_m <- as.integer(df_m$edulev)
edulev3_m[edulev3_m==1] <- 0
edulev3_m[edulev3_m==2] <- 0
edulev3_m[edulev3_m==3] <- 1
avg_edulev3_m <- mean(edulev3_m)

#put means in a vector and add a 1 to get the interpect when I 
#multiply below
meanX_m <- cbind(1, avg_married_m, avg_urban_m, avg_White_m,
                 avg_Black_m, avg_Asian_m, avg_Indian_m, avg_edulev2_m, avg_edulev3_m )

##Regression for women
df_f <- subset(df,df$female==1)
fit_f <- lm(df_f$log_wage ~ df_f$married + df_f$urban + 
              df_f$White + df_f$Black + df_f$Asian + df_f$Am_Indian + df_f$edulev, data=df_f)

#Put coefficients in a vector
coef_f <- coef(summary(fit_f))[,1]

avg_married_f <- mean(as.integer(df_f$married))-1
avg_urban_f   <- mean(as.integer(df_f$urban))-1
avg_White_f   <- mean(as.integer(df_f$White))-1
avg_Black_f   <- mean(as.integer(df_f$Black))-1 
avg_Asian_f   <- mean(as.integer(df_f$Asian)) -1 
avg_Indian_f   <- mean(as.integer(df_f$Am_Indian))-1 


#level 2 education
edulev2_f <- as.integer(df_f$edulev)
edulev2_f[edulev2_f==1] <- 0
edulev2_f[edulev2_f==2] <- 1
edulev2_f[edulev2_f==3] <- 0
avg_edulev2_f <- mean(edulev2_f)

#level 3 education
edulev3_f <- as.integer(df_f$edulev)
edulev3_f[edulev3_f==1] <- 0
edulev3_f[edulev3_f==2] <- 0
edulev3_f[edulev3_f==3] <- 1
avg_edulev3_f <- mean(edulev3_f)

meanX_f <- cbind(1, avg_married_f, avg_urban_f, avg_White_f,
                 avg_Black_f, avg_Asian_f, avg_Indian_f, avg_edulev2_f, avg_edulev3_f )

#########################
#Calculate Decomposition#
#########################

endow <- (meanX_m - meanX_f)%*%coef_f
coeff <- meanX_f%*%(coef_m - coef_f)
inter <- (meanX_m - meanX_f)%*%(coef_m - coef_f)



