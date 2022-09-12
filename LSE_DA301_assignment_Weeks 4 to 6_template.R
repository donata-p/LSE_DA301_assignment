


################################################################################
# DONATA POLLOCK's GITHUB REPOSITORY:
# https://github.com/donata-p/LSE_DA301_assignment
###############################################################################


# LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

install.packages("tidyr")
library(tidyr)

# Determine the working directory.
getwd() 

# Import a CSV file. turtle_sales.csv 
# data <- read.csv('turtle_sales.csv', header=T)
data <- read.csv(file.choose(), header=T)

# Print the data frame.
data

# View the data frame.
View(data)
head(data)

# Sense-check the data set
# Return the structure of the data frame.
str(data)

# Check the type of the data frame.
typeof(data)

# Check the class of the data frame.
class(data)

# Check the dimensions of the data frame
dim(data)

# Convert data frame to a tibble.
as_tibble(data)
glimpse(data)

# descriptive statistics
summary(data)


# Data cleaning
# 1. Product must be a categorical variable not int.
# Convert 'Product' to factor (categorical variable).
data2 <- mutate(data,
                Product=as.factor(Product))
# 2. 
summary(data2)
as_tibble(data2)

# 3. Year has 2 NA's
filter(data2, is.na(Year))
which(is.na(data2$Year))
dim(data2)
summary(data2$Year)

# or could insert any Year we want
# data2$Year[which(is.na(data2$Year))] <- 2006

# 4. filter out 2 NA's from Year column
data3 <- filter(data2,
                 !is.na(Year))
dim(data3)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns.
data4 <- select(data3, -Ranking, -Year, -Genre, -Publisher)

# identifying if any NA's
summary(data4$Product)      
summary(data4$Platform)           
summary(data4$NA_Sales)          
summary(data4$EU_Sales)       
summary(data4$Global_Sales)

# View the data frame.
head(data4)
str(data4)
as_tibble(data4)
glimpse(data4)

# View the descriptive statistics.
summary(data4)

View(data4)
################################################################################
install.packages('ggplot2')
browseVignettes('ggplot2')
library(ggplot2)
# 5. Review plots to determine insights into the data set.

## 5a) Scatterplots
# Create scatterplots.
qplot(Platform, NA_Sales, data=data4, geom=c('point', 'smooth'))
qplot(Platform, EU_Sales, data=data4, geom=c('point', 'smooth'))
qplot(Platform, Global_Sales, data=data4, geom=c('point', 'smooth'))

# if only y variable was supplied
qplot(y=NA_Sales, data=data4)
qplot(y=EU_Sales, data=data4)
qplot(y=Global_Sales, data=data4)

## 5b) Histograms
# Create histograms.
qplot(NA_Sales, data=data4)
qplot(EU_Sales, data=data4)
qplot(Global_Sales, data=data4)

qplot(NA_Sales, bins=10, data=data4)
qplot(EU_Sales, bins=10, data=data4)
qplot(Global_Sales, bins=10, data=data4)

# sales perfomance per Platform
qplot(Platform, data=data4, colour=I('green'))

## 5c) Boxplots
# Create boxplots.
qplot(Platform, NA_Sales, data=data4, colour=I('green'), geom='boxplot')
qplot(Platform, EU_Sales, data=data4, colour=I('blue'), geom='boxplot')
qplot(Platform, Global_Sales, data=data4, colour=I('red'), geom='boxplot')

qplot(Platform, NA_Sales, colour=NA_Sales, data=data4, geom=c('point', 'jitter'))
qplot(Platform, EU_Sales, colour=NA_Sales, data=data4, geom=c('point', 'jitter'))
qplot(Platform, Global_Sales, colour=NA_Sales, data=data4, geom=c('point', 'jitter'))
###############################################################################
###############################################################################

# 4. Observations and insights

## Your observations and insights here ......
# Sales data contains 352 games, 10 platforms and 12 genres. 
# Global sales are 5.349 million, while the EU and NA mean sales are 1.6507 million and 2.519 million respectively.

###############################################################################
###############################################################################

# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(data4)
View(data4)
dim(data4)

# Check output: Determine the min, max, and mean values.
# Call the function to calculate the mean.
mean(data4$NA_Sales) 
mean(data4$EU_Sales) 
mean(data4$Global_Sales) 

# Call the function to calculate the median.
median(data4$NA_Sales) 
median(data4$EU_Sales) 
median(data4$Global_Sales) 

# Measure the variability in values
# Determine the minimum and maximum value.
min(data4$NA_Sales) 
min(data4$EU_Sales) 
min(data4$Global_Sales) 

max(data4$NA_Sales) 
max(data4$EU_Sales) 
max(data4$Global_Sales) 

# Range = Max - Min.
max(data4$NA_Sales) - min(data4$NA_Sales) 
max(data4$EU_Sales) - min(data4$EU_Sales) 
max(data4$Global_Sales) - min(data4$Global_Sales) 

# Calculate Q1 and Q3.
quantile(data4$NA_Sales, 0.25) 
quantile(data4$EU_Sales, 0.25) 
quantile(data4$Global_Sales, 0.25) 

quantile(data4$NA_Sales, 0.75)
quantile(data4$EU_Sales, 0.75) 
quantile(data4$Global_Sales, 0.75)

# Calculate IQR.
IQR(data4$NA_Sales)  
IQR(data4$EU_Sales)  
IQR(data4$Global_Sales)  

# Determine the variance.
var(data4$NA_Sales)  
var(data4$EU_Sales)  
var(data4$Global_Sales)  

# Return the standard deviation.
sd(data4$NA_Sales)  
sd(data4$EU_Sales)  
sd(data4$Global_Sales)    

# summary
summary(data4)

###############################################################################
# 3. Determine the impact on sales per product_id.

# 3.a) Group data based on Product and determine the sum per Product.
table(data4$Product)

# search for missing values in a data set.
sum(is.na(data4))
dim(data4)

# group_by and sum
data5 <- data4 %>% group_by(Product) %>%
  summarise(sum_NA_Sales=sum(NA_Sales),
            sum_EU_Sales=sum(EU_Sales),
            sum_Global_Sales=sum(Global_Sales),
            .groups='drop')

# View the results after group_by and sum
View(data5)
head(data5)
dim(data5)

# identifying if any NA's
summary(data5$Product)      
summary(data5$sum_NA_Sales)          
summary(data5$sum_EU_Sales)       
summary(data5$sum_Global_Sales)

# product with max sales 
apply(data5,  2, max, na.rm=TRUE)

# product with min sales. If we did not add na.rm=TRUE, then our output would be NA.
apply(data5, 2, min, na.rm=TRUE)

## aggregate functions.
data6 <- data5 %>% group_by(Product) %>%
  summarise(sd_NA_Sales=sd(sum_NA_Sales),
            sd_EU_Sales=sd(sum_EU_Sales),
            sd_Global_Sales=sd(sum_Global_Sales),
            
            var_NA_Sales=var(sum_NA_Sales),
            var_EU_Sales=var(sum_EU_Sales),
            var_Global_Sales=var(sum_Global_Sales),
            
            mean_NA_Sales=mean(sum_NA_Sales),
            mean_EU_Sales=mean(sum_EU_Sales),
            mean_Global_Sales=mean(sum_Global_Sales),
            
            min_NA_Sales=min(sum_NA_Sales),
            min_EU_Sales=min(sum_EU_Sales),
            min_Global_Sales=min(sum_Global_Sales),
            
            max_NA_Sales=max(sum_NA_Sales),
            max_EU_Sales=max(sum_EU_Sales),
            max_Global_Sales=max(sum_Global_Sales),
            .groups='drop')
# View the data frame from aggregate functions.
head(data6)

summary(data5)


# #  4 Create plots to review and determine insights into the data set
# Create scatterplots.
qplot(sum_NA_Sales, sum_EU_Sales, data=data5)
qplot(sum_NA_Sales, sum_Global_Sales, data=data5) 
qplot(sum_Global_Sales, sum_EU_Sales, data=data5)

# Specify histogram function.
hist(data5$sum_NA_Sales)  
qplot(sum_NA_Sales, data=data5, bins=20)

hist(data5$sum_EU_Sales)  
qplot(sum_EU_Sales, data=data5, bins=20)

hist(data5$sum_Global_Sales)   
qplot(sum_Global_Sales, data=data5, bins=20)

# Specify boxplot function.
boxplot(data5$sum_NA_Sales)
qplot(sum_NA_Sales, data=data5, geom='boxplot')

boxplot(data5$sum_EU_Sales) 
qplot(sum_EU_Sales, data=data5, geom='boxplot')

boxplot(data5$sum_Global_Sales)
qplot(sum_Global_Sales, data=data5, geom='boxplot')



# Determine the unique values 
unique(data5$Product)

# produce a data profile report
DataExplorer::create_report(data5)


## 5. Determine the normality of the data set.
# Determine the unique values 
unique(data5$Product)

# produce a data profile report
DataExplorer::create_report(data4)

# rid of all the rows with missing values
na.omit(data5)

# check for NAs, dataframe
sum(is.na(data5)) 
dim(data5)
head(data5)

# Determine the unique values 
unique(data5$Product)
unique(data5$Platform)
unique(data5$sum_NA_Sales)
unique(data5$sum_EU_Sales)
unique(data5$sum_Global_Sales)

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify qqnorm function (draw a qqplot).
qqnorm(data5$sum_NA_Sales)
qqnorm(data5$sum_EU_Sales)
qqnorm(data5$sum_Global_Sales)

# Specify qqline function.
qqline(data5$sum_NA_Sales)
qqline(data5$sum_EU_Sales)
qqline(data5$sum_Global_Sales)


## 5b) Perform Shapiro-Wilk test
shapiro.test(data5$sum_NA_Sales)
# The output here indicates that the p-value < 2.2e-16,
# so there is little evidence that NA_Sales is normally distributed
shapiro.test(data5$sum_EU_Sales)
# The output here indicates that the p-value = 3.11e-16,
# so there is little evidence that EU_Sales is normally distributed
shapiro.test(data5$sum_Global_Sales)
# The output here indicates that the p-value < 2.2e-16,
# so there is little evidence that sum_Global_Sales is normally distributed.

# NB.If the p-value is small – less than 5%, say – we would conclude that the assumption 
# of normality is a poor fit for the data.


## 5c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Install the moments package and load the library.
install.packages('moments') 
library(moments)


# 5d) Specify the skewness
skewness(data5$sum_NA_Sales)
#  skewness 3.046616 greater than 0. This is positive skewness, which suggests that 
# the distribution is very right-skewed and biased towards higher values
skewness(data5$sum_EU_Sales)
#  skewness 2.894049 greater than 0. This is positive skewness, which suggests that 
# the distribution is very right-skewed and biased towards higher values
skewness(data5$sum_Global_Sales)
#  skewness 3.054206 greater than 0. This is positive skewness, which suggests that 
# the distribution is very right-skewed and biased towards higher values

#  A data distribution that is not symmetric is said to be skewed. 
#  Negative skewness generally means that the mean is less than the median and the data is left-leaning. 
# Positive skewness means the opposite, where the mean is larger than the median and data leans to the right
#  skewness is less than 1 but greater than 0. This is positive skewness, which suggests that 
# the distribution is slightly right-skewed and biased towards higher values


# kurtosis functions
kurtosis(data5$sum_NA_Sales)
#  15.60427 higher than 3 indicates a leptokurtic (or heavy-tailed) distribution, 
#  that is, one with more extreme outliers than the normal distribution. 
kurtosis(data5$sum_EU_Sales)
#  16.31649 higher than 3 indicates a leptokurtic (or heavy-tailed) distribution, 
#  that is, one with more extreme outliers than the normal distribution. 
kurtosis(data5$sum_Global_Sales)
#  17.75875 higher than 3 indicates a leptokurtic (or heavy-tailed) distribution, 
#  that is, one with more extreme outliers than the normal distribution. 

#  Kurtosis measures the tailedness of the distribution, 
#  in other words, whether the distribution tails are light or heavy. 
#  Our point of reference is the normal distribution, which has a coefficient of kurtosis equal to 3. 
#  A value higher than 3 indicates a leptokurtic (or heavy-tailed) distribution, 
#  that is, one with more extreme outliers than the normal distribution. 
#  By contrast, a value less than 3 indicates a platykurtic (or light-tailed) distribution.
#  This suggests that the data is platykurtic and will produce less extreme outliers 
# than the normal distribution.


## 4d) Determine correlation
# Determine correlation.
# Specify the cor function.
# Set the first and second variables
cor(data5$sum_NA_Sales, data5$sum_Global_Sales)
# A positive correlation coefficient suggests that the two variables vary in the same direction. 
# That means as the one increases, so does the other; and if one decreases, the other does too. 
# Again the coefficient is 0.9168662 so closer 1, meaning there is a strong positive correlation.
cor(data5$sum_EU_Sales, data5$sum_Global_Sales) 
# Again the coefficient is 0.8487806 so closer to 1, meaning there is a strongish positive correlation.
cor(data5$sum_EU_Sales, data5$sum_NA_Sales) 
# Again the coefficient is 0.622516 slightly closer to 1, meaning there is a positive correlation.


# Correlation
# check column data type
as_tibble(data5)

# Remove unnecessary columns.
data7 <- select(data5, -Product)

# Determine the correlation for the whole data frame.
cor(data7)
#                       NA_Sales   EU_Sales          Global_Sales
# sum_NA_Sales        1.0000000    0.6225160        0.9168662
# sum_EU_Sales        0.6225160    1.0000000        0.8487806
# sum_Global_Sales    0.9168662    0.8487806        1.0000000

# rounded to 2 decimal places
round (cor(data7),
       digits=2)
#                         NA_Sales      EU_Sales        Global_Sales
# sum_NA_Sales             1.00         0.62             0.92
# sum_EU_Sales             0.62         1.00             0.85
# sum_Global_Sales         0.92         0.85             1.00

###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.
# 3a) Histogram
# Start with a simple histogram.
ggplot(data7, aes(x=sum_NA_Sales)) + 
  geom_histogram()

ggplot(data7, aes(x=sum_EU_Sales)) + 
  geom_histogram()

ggplot(data7, aes(x=sum_Global_Sales)) + 
  geom_histogram()

# Histogram with 15 bins.
ggplot(data7, aes(sum_NA_Sales)) + 
  geom_histogram(bins = 15)

ggplot(data7, aes(sum_EU_Sales)) + 
  geom_histogram(bins = 15)

ggplot(data7, aes(sum_Global_Sales)) + 
  geom_histogram(bins = 15)

# 3b) Densityplot
# Smoothed density plot.
ggplot(data7, aes(x=sum_NA_Sales)) + 
  geom_density()

ggplot(data7, aes(x=sum_EU_Sales)) + 
  geom_density()

ggplot(data7, aes(x=sum_Global_Sales)) + 
  geom_density()

# 3c) Scatterplot
# Start with a simple scatterplot.
ggplot(data7, aes(x=sum_EU_Sales, y=sum_Global_Sales)) + 
  geom_point()

ggplot(data7, aes(x=sum_NA_Sales, y=sum_Global_Sales)) + 
  geom_point()

ggplot(data7, aes(x=sum_NA_Sales, y=sum_EU_Sales)) + 
  geom_point()


# Scatterplot with no method in geom_smooth() (spline).
ggplot(data7, aes(x=sum_EU_Sales, y=sum_Global_Sales)) + 
  geom_point() + 
  geom_smooth()
  labs(title ='Correlation between EU Sales and Global Sales',
     subtitle = 'Scatter Plot for EU Sales vs Global Sales',
     x = 'EU Sales',
     y = 'Global Sales') +
    theme_classic()

ggplot(data7, aes(x=sum_NA_Sales, y=sum_Global_Sales)) + 
    geom_point() + 
    geom_smooth()
    labs(title ='Correlation between North America Sales and Global Sales',
     subtitle = 'Scatter Plot for North America Sales vs Global Sales',
     x = 'North America Sales',
     y = 'Global Sales') +
      theme_classic() 


ggplot(data7, aes(x=sum_NA_Sales, y=sum_EU_Sales)) + 
    geom_point() + 
    geom_smooth()
    labs(title ='Correlation between North America Sales and EU Sales',
     subtitle = 'Scatter Plot for North America Sales vs EU Sales',
     x = 'North America Sales',
     y = 'EU Sales') +
      theme_classic()
    
###############################################################################

# 4. Observations and insights
# Your observations and insights here...
    # data in all our sales figures is not normally distributed. With 
    # Shapiro-Wilk showing p values of 2.2e-16 on all sales data. We can also see
    # that the data is leptokurtic with a very high positive skewness. This means 
    # data is not normally distributed and is very skewed. 
    # The correlation between the sales data there is a positive
    # correlation between all the sales data. This means that as one sales figure increases the
    # other sales figures will also increase. The highest correclation is between NA_Sales and
    # Global_Sales of 0.93. 

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
head(data7)
as_tibble(data7)

# Determine a summary of the data frame.
summary(data7)

###############################################################################

# 2. Create a simple linear regression model

## 2a) Determine the correlation between columns
head(data4)
cor(select(data4, -Product, -Platform))
#               NA_Sales  EU_Sales Global_Sales
# NA_Sales     1.0000000 0.7064574    0.9351596
# EU_Sales     0.7064574 1.0000000    0.8778032
# Global_Sales 0.9351596 0.8778032    1.0000000


# Create a linear regression model on the original data.
  # 1.model_NAEU_Sales linear regression model
model_NAEU_Sales <- lm(EU_Sales~NA_Sales, data=data4)
# View the model
model_NAEU_Sales
# View more outputs for the model - the full regression table.
summary(model_NAEU_Sales)

plot(data4$EU_Sales, data4$NA_Sales)
plot(model_NAEU_Sales, 1)
# need horizontal line around 0. Our is not due to outliers.
# Add line-of-best-fit.adds line to exciting plots and shows best fit.
abline(coefficients(model_NAEU_Sales))



  # 2.GlobalEU_Saleslinear regression model
model_GlobalEU_Sales <- lm(Global_Sales~EU_Sales, data=data4)
# View the model.
model_GlobalEU_Sales
# View more outputs for the model - the full regression table.
summary(model_GlobalEU_Sales)

plot(data4$Global_Sales, data4$EU_Sales)
plot(model_GlobalEU_Sales, 1)
# need horizontal line around 0. Our is not due to outliers.
# Add line-of-best-fit.adds line to exciting plots and shows best fit.
abline(coefficients(model_GlobalEU_Sales))


  # 3.model_GlobalNA_Sales linear regression model
model_GlobalNA_Sales <- lm(Global_Sales~NA_Sales, data=data4)
# View the model.
model_GlobalNA_Sales
# View more outputs for the model - the full regression table.
summary(model_GlobalNA_Sales)

plot(data4$Global_Sales, data4$NA_Sales)
plot(model_GlobalNA_Sales, 1)
# need horizontal line around 0. Our is not due to outliers.
# Add line-of-best-fit.adds line to exciting plots and shows best fit.
abline(coefficients(model_GlobalNA_Sales))


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(data4$EU_Sales, data4$NA_Sales)
plot(data4$Global_Sales, data4$EU_Sales)
plot(data4$Global_Sales, data4$NA_Sales)

# Add line-of-best-fit.adds line to exciting plots and shows best fit.
abline(coefficients(model_NAEU_Sales))
abline(coefficients(model_GlobalEU_Sales))
abline(coefficients(model_GlobalNA_Sales))

# Plot the residuals which are estimates of error terms. we want no patern here, but get high pattern here!
plot(model_NAEU_Sales$residuals)
plot(model_GlobalEU_Sales$residuals)
plot(model_GlobalNA_Sales$residuals)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
data5 <- select(data4, -Product, -Platform)
head(data5)

# Multiple linear regression model 
model_all_Sales = lm(Global_Sales ~ EU_Sales + NA_Sales,
                  data=data5)
# View the result.
summary(model_all_Sales)
# Multiple R-squared:  0.9687,	Adjusted R-squared:  0.9685 
# Very high R-squared value. Suggest model will be a good fit to predict global sales.

# Checking linear relationship
plot(model_all_Sales,1) 
# not horizontal line around 0. 

# Calculater the sum of squares error (SSE) to determine strength.
SSE_model_all_Sales = sum(model_all_Sales$residuals^2)

# View the result = 431.205
SSE_model_all_Sales 
# high SSE is not good fit.

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

str(data4)

head(data4)
tail(data4)

Product  <- c(107, 107,107,107,107)
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, .052)

dataPredict <- data.frame(Product, NA_Sales, EU_Sales)

head(dataPredict)

prediction <- predict(model_all_Sales, newdata = dataPredict,
                      interval='confidence')

# Print prediction
prediction
#         fit       lwr       upr
# 1 71.460784 70.150426 72.771143
# 2  6.858034  6.719488  6.996579
# 3  4.250722  4.103385  4.398059
# 4  4.136789  4.010304  4.263274
# 5 25.809927 24.757751 26.862103

#         1         2         3         4         5 
# 71.468572  6.856083  4.248367  4.134744 25.803524 
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Very high 431.205 = SSE_model_all_Sales is a poor fit.
# The closer the SSE is to 0, the better the fit.

# The p-value is the same for the three models (< 0.05).

# High R squared in multiple linear regression model suggests a good fit.

# Looking at correlation we can see that the highest correlation 
# in our data is between North America Sales and Global Sales 
# 93% followed by EU and Global Sales of 87% with the least being EU and 
# North American Sales of 70%.

# predicted values are very close to actual values. Model is working well.
###############################################################################
###############################################################################




