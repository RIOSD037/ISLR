# 2020 April 11

# load packages

if(!require(ISLR)){
  install.packages("ISLR")
  library(ISLR)
}

if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}
  

# Chapter 2 -- Applied Excerises

# 8 a) Call the loaded data College. Make sure that you have
# the directory set to the correct location for the data

data("College")

# look at the data
head(College)

# collect rownames in a variable and remove rownames

rownames(College)
fix(College)
rownames <- rownames(College)

# remove rownames
rownames(College) <- c()

# check
head(College)

# 8 c) Use the summary() function to produce a numerical summary
# of the variables in the data set.

summary(College)

# 8 c ii) Use the pairs function to produce a scatterplot matrix of the first 
# ten columns or variables of the data

pairs(College[,1:10])

# 8 c iii) Plot side by side boxplots of OutState versus Private

f <- list(
  family = "Century Gothic",
  size = 18, 
  color = "#7f7f7f"
)


f2 <- list(
  family = "Century Gothic",
  size = 13, 
  color = "#7f7f7f"
)

x <- list(title = "Private College", titlefont = f)
y <- list(title = "Out of State", titlefont = f)


plot_ly(x = ~College$Private, y = ~College$Outstate, type = "box") %>%
  layout(title = "Out of State Tuition vs Private Schools", font = f2,
         xaxis = x, yaxis = y)

# c iv) Create a new qualitative variable, called Elit, by binning the 
# Top10perc variable. We are going to divide universities into two groups
# based on whether or not the proportion of students coming from the top 10%
# of their high school classes exceeds 50%

Elite <- rep("No",nrow(College))
Elite[College$Top10perc>50] <- "Yes"

Elite <- as.factor(Elite)
College <- data.frame(College, Elite)

head(College)
?College

f <- list(
  family = "Century Gothic",
  size = 18, 
  color = "#7f7f7f"
)


f2 <- list(
  family = "Century Gothic",
  size = 13, 
  color = "#7f7f7f"
)

x <- list(title = "Elite College", titlefont = f)
y <- list(title = "Out of State", titlefont = f)


plot_ly(x = ~College$Elite, y = ~College$Outstate, type = "box") %>%
  layout(title = "Out of State Tuition vs Elite Schools", font = f2,
         xaxis = x, yaxis = y)

# 8 v) Use the hist() function to produce some histograms with numbers of bins for 
# a few quantitive variables

par(mfrow = c(2,2))
hist(College$Books, col = 2, xlab = "Books", ylab = "Count")
hist(College$PhD, col = 3, xlab = "PhD", ylab = "Count")
hist(College$Grad.Rate, col = 4, xlab = "Grad Rate", ylab = "Count")
hist(College$perc.alumni, col = 6, xlab = "% Alumni", ylab = "Count")

summary(College$PhD)

College[College$PhD == 103,]
rownames[as.numeric(rownames(College[College$PhD == 103,]))]


# 9 Auto data set

data(Auto)
head(Auto)
Auto <- na.omit(Auto)
str(Auto)

head(Auto)

# qualitative predictors: name & origin
# quantitative predictors: mpg, cylindes, displacement, horsepower, weight, 
# acceleration, year

names(Auto)
install.packages("sqldf")
library(sqldf)

sqldf("SELECT DISTINCT origin FROM Auto")

# What is the range of each quantitative predictor? 

summary(Auto[,-c(7,9)])

# What is the mean and standard deviation of each quantitative predictor?

sapply(Auto[,-c(7,9)], mean)
sapply(Auto[,-c(7,9)], sd)

# Remove the 10th through 85th observation. What is the range, mean, and sd
# of each predictor in the subset of the data that remains?

subset <- Auto[-c(10:85),-c(7,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)

# Using the full data set, investigate the predictors graphically, using
# the scatterplots of other tools of your choice.


pairs(Auto)

cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$horsepower, Auto$displacement)


# 10  Boston housing data

library(MASS)

Boston$chas <- as.factor(Boston$chas)
nrow(Boston)

ncol(Boston)
pairs(Boston)
