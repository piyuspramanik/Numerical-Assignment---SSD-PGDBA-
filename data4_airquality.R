library(factoextra)
library(ggcorrplot)
library(psych)
library(MASS)
library(pheatmap)
library(rattle)
library(dplyr)
data = airquality
#----------------------------------------------------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS
#----------------------------------------------------------------------------------------------------------------------------
# 1. Loading the data set airquality
View(data)

# Imputation of null values with mean
data$Solar.R[is.na(data$Solar.R)] <- mean(data$Solar.R, na.rm = TRUE)
data$Wind[is.na(data$Wind)] <- mean(data$Wind, na.rm = TRUE)
data$Ozone[is.na(data$Ozone)] <- mean(data$Ozone, na.rm = TRUE)
data$Temp[is.na(data$Temp)] <- mean(data$Temp, na.rm = TRUE)
View(data)

# Structure of the data : 153 obs. of  6 variables
str(data)
cat("Number of rows and columns of the dataset =", dim(data), "\n")
cat("Number of rows  =", nrow(data), "\n")
cat("Number of columns  =", ncol(data), "\n")
cat("Column names :", colnames(data), "\n")

#----------------------------------------------------------------------------------------------------------------------------
# 2. Summary Statistics
# Choose a numerical variable.
# Calculate and interpret the mean, median, standard
# deviation, minimum, and maximum of this variable.

View(describe(data))      # Describing the statistics of each variable

#----------------------------------------------------------------------------------------------------------
# 3. Distribution Visualization
# Create a histogram and a boxplot for the chosen variable. 
# Describe the shape of the distribution and any potential `outliers’.
# Defining the numerical columns----

num_col = c('Ozone','Solar.R','Wind','Temp')
data_description_num = describe(data[num_col])

# Convert to a data frame (if required) and view
data_description_num = as.data.frame(data_description_num)
View(data_description_num)

# Plotting and outlier detection
par(mfrow=c(1,2))
for (i in num_col){
  hist(data[[i]],col='green',main=paste("Histogram of",i,""),
       xlab=i, ylab="Frequency")
  boxplot(data[[i]],col='gold',outpch=19,outcol='red',
          main=paste("Boxplot of",i,""), xlab=i, ylab="Frequency")
}
par(mfrow=c(1,1))

# As per the box whisker plot there are 3 outliers

# Chosen numerical variable = Wind
cat("Mean = ", mean(data$Wind), "\n")
cat("Median = ", median(data$Wind), "\n")
cat("Standard Deviation = ", sd(data$Wind), "\n")   
cat("Min = ", min(data$Wind), "\n")
cat("Max = ", max(data$Wind), "\n")

cat("Median = 9.96  and Mean = 9.70  
since Mean > median , we can infer that the distribution is POSITIVELY SKEWED")

#----------------------------------------------------------------------------------------------------------------------------
# 4. Categorical Variable Analysis
# Choose a categorical variable (say, `Month`). 
# Create a bar plot to visualize the distribution of this variable.
# What insights can you gather from the plot? 

# Catagorical variable = Month
categorical_columns = c('Month', 'Day')
for (j in categorical_columns){
  col_counts = table(data[[j]])
  barplot(col_counts, xlab=j, ylab="frequency", main = j, col='gold')
}

# Frequency table
table(data$Month)

# Proportion table
prop.table(table(data$Month))

# Bar plot
barplot(table(data$Month), main = "Bar plot of Month", col = "magenta", xlab = "Reading in months", ylab = "Frequency")

#----------------------------------------------------------------------------------------------------------------------------
# MULTIVARIATE ANALYSIS
#----------------------------------------------------------------------------------------------------------------------------

# 5. Correlation Analysis
# Select two numerical variables (say, `Wind` and `Temp`). Calculate the Pearson 
# correlation coefficient. What does this value indicate about the relationship 
# between these two variables?

#Finding the correlation matrix for the numerical variable
corr_data = data[,num_col]
corr_matrix = cor(corr_data)
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 10,fontsize_col=10,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=10,
         main="Pearson's correlation cofficient")

#  Considered variables : Temp and Solar.R

# Calculating Pearson Correlation Coefficient between Wind and Temp of airquality table
correlation_1 <- cor(data$Temp, data$Solar.R , method = "pearson")
correlation_1

correlation_2 <- cor(data$Temp, data$Ozone, method = "pearson")
correlation_2

#----------------------------------------------------------------------------------------------------------------------------
# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between the two selected variables.
# Add a trend line. Discuss the relationship you observe.

# Scatter plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
plot(airquality$Temp, airquality$Ozone, 
     main = "Scatter Plot of Temp vs Ozone", 
     xlab = "Ozone", 
     ylab = "Temp", 
     col = "blue", 
     pch = 19)
# Add a linear trend line
model <- lm(airquality$Temp ~ airquality$Ozone)  # Fit a linear model
abline(model, col = "red", lwd = 2)  # Add the trend line

#----------------------------------------------------------------------------------------------------------------------------
# 7. Multiple Regression
# Fit a linear regression model predicting `Temp` using `Solar.R` and `Wind` as predictors 
# (here, for the `mtcars` data).Display the summary of the model and interpret the 
# coefficients.

# Fit the linear regression model
model <- lm(Temp ~ Solar.R + Ozone, data = airquality)

# View summary
summary(model)

# Extract coefficients
coefficients <- coef(model)
print(coefficients)

#----------------------------------------------------------------------------------------------------------------------------
# 8. Model Diagnostics
# Plot the residuals of your regression model. Check for homoscedasticity and normality of 
# residuals. What do the diagnostic plots tell you about the model fit?
# Plot diagnostic graphs
par(mfrow = c(2, 2))
plot(model)

# Residual vs Fitted plot :
# Homoscedasticity: If the plot shows random scatter around zero, 
# it indicates homoscedasticity. Any clear patterns or fanning out may indicate 
# heteroscedasticity (i.e., non-constant variance).

# Q-Q Residuals
# Normality: If the residuals are normally distributed, the points will lie roughly along 
# a straight line. Significant deviations from the line indicate that the residuals are not 
# normally distributed.

# Predict for new values
new_data <- data.frame(Wind = c(25, 27), Solar.R = c(340, 350))
predictions <- predict(model, newdata = new_data)
print(predictions)

#----------------------------------------------------------------------------------------------------------------------------
# ADVANCE ANALYSIS
#----------------------------------------------------------------------------------------------------------------------------
# Perform PCA on the numerical variables in this dataset. Plot the explained variance. 
# How many components would you choose based on the scree plot, and why?

data(airquality)
airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)
airquality$Wind[is.na(airquality$Wind)] <- mean(airquality$Wind, na.rm = TRUE)
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Temp[is.na(airquality$Temp)] <- mean(airquality$Temp, na.rm = TRUE)

df  <- airquality[,1:4]

df_scaled <- scale(df)

pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)  # Importance of components
pca_result$rotation  # Principal axes (loadings)

fviz_eig(pca_result, addlabels = TRUE) # Scree plot for the pca

# From the summary of the PCA (Cumulative Proportion  0.5455 0.7838 0.9078 1.00000) 
# , it is observed  that the 90.78 %  of variance is restored if 1st 3 PCs are  chosen and 

# From the scree plot it is clear that the explained variance starts to level off from point 3.
# thus we can retain the first 3 PCs, with 92.32% of variance  restored.

pca_result$x         # Principal component scores

#----------------------------------------------------------------------------------------------------------------------------
# Visualize the PCA results (say, using a biplot). 
# Discuss the loadings of the first two principal components. 
# What patterns or groupings do you observe in the data?
fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
