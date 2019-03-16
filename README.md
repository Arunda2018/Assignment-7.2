# Assignment-7.2
Visualize the correlation between all variables in a meaningful and clear way of representing. Find out
top 3 reasons for having more crime in a city.
b. What is the difference between co-variance and correlation? Take an example from this dataset and
show the differences if any?

head(diamonds)

#Graphical Analysis
#Scatter plots can help visualize any linear relationships between the dependent (response) variable and independent (predictor) variables
scatter.smooth(x=diamonds$carat, y=diamonds$price, main="Price~Carat")  # scatterplot

#Boxplot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(diamonds$carat, main="Carat", 
        sub=paste("Outlier rows: ", 
        boxplot.stats(diamonds$carat)$out))  
boxplot(diamonds$price, main="Price", 
        sub=paste("Outlier rows: ", 
        boxplot.stats(diamonds$price)$out)) 

fit <-lm(price~carat,data=diamonds)
print(fit)

summary(fit)

plot(fit)

#assuming all variables continuous
cor(mtcars)
pairs(mtcars)

cor.test(mtcars$mpg,mtcars$wt)

#using correlation plot
library(corrplot)
#create a correlations matrix
mat <- cor(mtcars)
#represent correlations
corrplot(cor(mtcars),type = "full","circle")

corrplot(cor(mtcars),type = "full","square")

corrplot(cor(mtcars),type = "full","ellipse")

corrplot(cor(mtcars),type = "full","number")

corrplot(cor(mtcars),type = "full","shade")

corrplot(cor(mtcars),type = "full","color")

corrplot(cor(mtcars),type = "full","pie")

corrplot(cor(mtcars),type = "lower","ellipse")

corrplot(cor(mtcars),type = "upper","ellipse")

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'original')

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'AOE')

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'FPC')

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'hclust')

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'alphabet',diag = TRUE)

corrplot(cor(mtcars),type = "full","ellipse",
         order = 'hclust',sig.level = 0.05,
         insig = c("pch", "p-value", "blank", "n"))


###Rank Correlation coefficient calculation
rank1 <- sample(1:10,20,replace = T)
rank2 <- sample(1:10,20,replace = T)

cbind(rank1,rank2)

plot(rank1,rank2)

cor(rank1,rank2,method = 'spearman')

cor.test(rank1,rank2,method = 'spearman')

###Kendall Correlation coefficient calculation
rank1 <- sample(1:0,20,replace = T)
rank2 <- sample(1:0,20,replace = T)

cbind(rank1,rank2)

cor(rank1,rank2,method = 'kendall')

cor.test(rank1,rank2,method = 'kendall')

