#####################
#Creating scatterplots
#####################

install.packages("Hmisc")
library(Hmisc)
install.packages("markdown")
library(markdown)
getOptions('respos')

# Read data from a URL into a dataframe called PE (physical endurance)
PE <- read.table('http://assets.datacamp.com/course/Conway/Lab_Data/Stats1.13.Lab.04.txt',header=1)

# Summary statistics
describe(PE)

??describe

# Scatter plots
plot(PE$age~PE$activeyears)
plot(PE$endurance~PE$activeyears)
plot(PE$endurance~PE$age)

##################
#Correlation matrix
##################

# PE is already loaded in

# Correlation Analysis
round(cor(PE[2:4]),2)

# Do some correlation tests. If the null hypothesis of no correlation can be rejected on a significance level of 5%, then the relationship between variables is  significantly different from zero at the 95% confidence level
cor.test(PE$age, PE$activeyears)
cor.test(PE$age, PE$endurance)
cor.test(PE$activeyears, PE$endurance)

##############################
#Non-representative data samples
##############################

# The impact dataset is already loaded in

impact <- read_csv("~/R Scripts/impact.csv")


# Summary statistics entire dataset
describe(impact)

# Calculate correlation coefficient
entirecorr <- round(cor(impact$vismem2,impact$vermem2),2)

# Summary statistics subsets
describeBy(impact,impact$condition)

# Create 2 subsets: control and concussed
control <- subset(impact,impact[,2]=="control")
concussed <- subset(impact,impact[,2]=="concussed")

# Calculate correlation coefficients for each subset
controlcorr <- round(cor(control$vismem2,control$vermem2),2)
concussedcorr <- round(cor(concussed$vismem2,concussed$vermem2),2)

# Display all values at the same time
correlations <- cbind(entirecorr, controlcorr, concussedcorr)
correlations

#################
#Impact experiment
#################

install.packages("corrplot")
library(corrplot)

# The dataset `impact` is already loaded

# Look at the dataset. Note that the variables we are interested in are on the 9th to 14th columns
impact

# Create a correlation matrix for the dataset
correlations <- cor(impact[9:14])

# Create the scatterplot matrix for the dataset
corrplot(correlations, method="shade")

################################################
#Manual computation of a simple linear regression
################################################

# The dataset `impact` is already loaded.

# Calculate the required means, standard deviations and correlation coefficient
mean_sym2 <- mean(impact$sym2)
mean_ic2 <- mean(impact$ic2)
sd_sym2 <- sd(impact$sym2)
sd_ic2 <- sd(impact$ic2)
r <- cor(impact$ic2,impact$sym2)

# Calculate the slope
B_1 <- r * ( sd_sym2 )/( sd_ic2 )

# Calculate the intercept
B_0 <- mean(impact$sym2) - mean(impact$ic2) * B_1

# Plot of ic2 against sym2
plot(impact$ic2, impact$sym2, main = "Scatterplot", ylab ="Symptoms" , xlab = "Impulse Control")

# Add the regression line
abline (B_0, B_1, col = "red")

###########################################
#Executing a simple linear regression using R
###########################################

# The dataset impact is still loaded

# Construct the regression model
model_1 <- lm(impact$sym2 ~ impact$ic2)

# Look at the results of the regression by using the summary function
summary(model_1)

# Create a scatter plot of Impulse Control against Symptom Score
plot(impact$sym2 ~ impact$ic2, main = "Scatterplot", ylab = "Symptoms", xlab = "Impulse Control")

# Add a regression line
abline(model_1, col = "red")

####################################
#Executing a multiple regression in R
####################################

# The impact dataset is already loaded in

# Multiple Regression
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Examine the results of the regression
summary(model_2)

# Extract the predicted values
predicted <- fitted(model_2)

# Plotting predicted scores against observed scores
plot(predicted ~ impact$sym2, main = "Scatterplot", xlab = "Observed Scores", ylab = "Predicted Scores")
abline(lm(predicted ~ impact$sym2), col = "green")

########################################
#Calculating the sum of squared residuals
########################################

# The impact dataset is already loaded the workspace.

# Create a linear regression with `ic2` and `vismem2` as regressors
model_1 <- lm(impact$sym2 ~ impact$ic2 + impact$vismem2)

# Extract the predicted values
predicted_1 <- fitted(model_1)

# Calculate the squared deviation of the predicted values from the observed values
deviation_1 <- (predicted_1-impact$sym2)^2

# Sum the squared deviations
SSR_1 <- sum(deviation_1)
SSR_1


# Create a linear regression with `ic2` and `vermem2` as regressors
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Extract the predicted values
predicted_2 <- fitted(model_2)

# Calculate the squared deviation of the predicted values from the observed values
deviation_2 <- (predicted_2-impact$sym2)^2

# Sum the squared deviations
SSR_2 <- sum(deviation_2)
SSR_2

##############################
#Standardized linear regression
##############################

# The dataset `impact` is already loaded

# Create a standardized simple linear regression
model_1_z <- lm(scale(impact$sym2) ~ scale(impact$ic2))

#Look at the output of this regression model
summary(model_1_z)

# Extract the R-Squared value for this regression
r_square_1 <- summary(model_1_z)$r.squared

#Calculate the correlation coefficient
corr_coef_1 <- sqrt(r_square_1)


# Create a standardized multiple linear regression
model_2_z <- lm(scale(impact$sym2) ~ scale(impact$ic2) + scale(impact$vismem2))

# Look at the output of this regression model
summary(model_2_z)

# Extract the R-Squared value for this regression
r_square_2 <- summary(model_2_z)$r.squared

# Calculate the correlation coefficient
corr_coef_2 <- sqrt(r_square_2)

##################
#Plotting residuals
##################

# Extract the residuals from the model
residual <- resid(model_2)

# Draw a histogram of the residuals
hist(residual)

# Extract the predicted symptom scores from the model
predicted <- fitted(model_2)

# Plot the residuals against the predicted symptom scores
plot(residual ~ predicted, main = "Scatterplot", xlab="Model 2 Predicted Scores" , ylab="Model 2 Residuals")
abline(lm(residual ~ predicted), col = "red")

