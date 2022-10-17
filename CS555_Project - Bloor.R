#Final Project CS555

#load Libs
library(plotly)

#set wd
setwd("C:\\Users\\James Bloor\\Desktop\\BU\\CS555\\Project")

#read in file
goal_data <- read.csv("project_555_data.csv",header=T)

#look at a histogram of the data to spot outliers
plot_ly(goal_data, x = ~Goals, type="box", name = 'Goals Scored 2019')

#we can see there are 4 outliers so we will exact those few
goal_data_filtered <- filter(goal_data, Goals < 28)


#2. The test we will use is:
#F =  MS Reg/ MS Res
#92 Degrees of Freedom

#3. State the decision rule
qf(.95, df1=1, df2 = 92)
#Decision Rule: Reject H0 if F>=3.945
#Otherwise, do not Reject H0

#4.Compute the test statistic
#Simple Linear Regression
attach ( goal_data_filtered )
m <- lm( Goals ~ Mins )
summary (m)
anova(m)

#F value shows as 19.034

#5.Conclusion
#Reject H0 



#Calculate the correlation coefficient.  
cor(goal_data_filtered$Mins ,goal_data_filtered$Goals )



#To get a sense of the data, generate a scatterplot..  
attach ( goal_data_filtered )
plot ( Mins , Goals , main ="Top Goal Scores
       in Soccer 2019", xlab ="Minutes Played", ylab =" Total Goals ",  col="blue", cex.lab =1.5)

#Add the regression line to the scatterplot
abline (m , lty =3 , col =" red ")





