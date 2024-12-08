# Considering the datasets 
# 1. mtcars

install.packages("ggcorrplot")
install.packages("factoextra")
library(factoextra)
library(psych)
library(pheatmap)
library(ggcorrplot)

data = mtcars
#-----------------------------------------------------------------------------------------------------------------------------

# UNIVARIATE ANALYSIS

# 1. Loading the data set data
View(data)

# Structure of the data : 32 obs. of  11 variables
str(data)
cat("Number of rows and columns of the dataset =", dim(data), "\n")
cat("Number of rows  =", nrow(data), "\n")
cat("Number of columns  =", ncol(data), "\n")
cat("Column names :", colnames(data), "\n")

#-----------------------------------------------------------------------------------------------------------------------------
# 2. Summary Statistics
# Choose a numerical variable (say, `mpg`, `hp`, `wt`). 
# Calculate and interpret the mean, median, standard
# deviation, minimum, and maximum of this variable.

View(describe(data))      # Describing the statistics of each variable

# Defining the numerical columns----
num_col = c('mpg', 'hp', 'wt')
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

# Chosen numerical variable = mpg
cat("Mean = ", mean(data$mpg), "\n")
cat("Median = ", median(data$mpg), "\n")
cat("Standard Deviation = ", sd(data$mpg), "\n")   
cat("Min = ", min(data$mpg), "\n")
cat("Max = ", max(data$mpg), "\n")

#-----------------------------------------------------------------------------------------------------------------------------
# 3. Distribution Visualization
# Create a histogram and a boxplot for the chosen variable. 
# Describe the shape of the distribution and any potential `outliers’.

# According to the box plot there is no outliers

cat("Median = 19.2  and Mean = 20.09  
since Mean > median , we can infer that the distribution is POSITIVELY SKEWED")

#-----------------------------------------------------------------------------------------------------------------------------
# 4. Categorical Variable Analysis
# Choose a categorical variable (say, `cyl`, `gear`). 
# Create a bar plot to visualize the distribution of this variable.
# What insights can you gather from the plot? 

categorical_columns = c('cyl','gear','carb')
for (j in categorical_columns){
  col_counts = table(data[[j]])
  barplot(col_counts, xlab=j, ylab="frequency", main=j, col='gold')
}

# Chosen catagorical variable = cyl
# Frequency table
table(data$cyl)

# Proportion table
prop.table(table(data$cyl))

# Bar plot
barplot(table(data$cyl), main = "Bar plot of cylinder (cyl)", col = "magenta", xlab = "No of Cylinders", ylab = "Frequency")

#-----------------------------------------------------------------------------------------------------------------------------
# MULTIVARIATE ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------

# 5. Correlation Analysis
# Select two numerical variables (say, `mpg` and `hp`). Calculate the Pearson 
# correlation coefficient. What does this value indicate about the relationship 
# between these two variables?

#Finding the correlation matrix for the numerical variable
corr_data = data[,num_col]
corr_matrix = cor(corr_data)
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 10,fontsize_col=10,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=10,
         main="Pearson's correlation cofficient")

#  Considered variables : mpg and hp
# Calculating Pearson Correlation Coefficient between mpg and hp of data table
correlation_1 <- cor(data$mpg, data$hp, method = "pearson")
correlation_1

# Correlation of -0.7761684 => a strong negative linear relationship between the two variables
# The negative sign (**) indicates that as one variable increases, the other variable tends to decrease

correlation_2 <- cor(data$wt, data$mpg, method = "pearson")
correlation_2

# The corr(mpg, disp) is more negative than corr(mpg,hp) it inmplies the variance of mpg wrt disp
# is more as compared to that wrt hp.

#-----------------------------------------------------------------------------------------------------------------------------
# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between the two selected variables.
# Add a trend line. Discuss the relationship you observe.

# Scatter plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
plot(data$mpg, data$hp, 
     main = "Scatter Plot of mpg vs hp", 
     xlab = "mpg", 
     ylab = "hp", 
     col = "blue", 
     pch = 19)
# Add a linear trend line
model <- lm(data$hp ~ data$mpg)  # Fit a linear model
abline(model, col = "red", lwd = 2)  # Add the trend line

#-----------------------------------------------------------------------------------------------------------------------------
# 7. Multiple Regression
# Fit a linear regression model predicting `mpg` using `hp` and `wt` as predictors 
# (here, for the `data` data).Display the summary of the model and interpret the 
# coefficients.

# Fit the linear regression model
model <- lm(mpg ~ hp + wt, data = data)

# View summary
summary(model)

# Extract coefficients
coefficients <- coef(model)
print(coefficients)

#-----------------------------------------------------------------------------------------------------------------------------
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
new_data <- data.frame(hp = c(120, 160), wt = c(2.8, 3.2))
predictions <- predict(model, newdata = new_data)
print(predictions)

#-----------------------------------------------------------------------------------------------------------------------------
# ADVANCE ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------
# Perform PCA on the numerical variables in this dataset. Plot the explained variance. 
# How many components would you choose based on the scree plot, and why?

data(data)
df  <- data[,1:11]

df_scaled <- scale(df)

pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)  # Importance of components
pca_result$rotation  # Principal axes (loadings)

fviz_eig(pca_result, addlabels = TRUE) # Scree plot for the pca
  
# From the summary of the PCA (Cumulative Proportion : 0.6008 0.8417 0.89873 0.92324 0.94356) 
# , it is observed  that the 92.32 %  of variance is restored if 1st 4 PCs are  chosen and 
# 94.35 % of variance is restored of 1st 5 PCs are considered.

# From the scree plot it is clear that the explained variance starts to level off from point 4.
# thus we can retain the first 4 PCs, with 92.32% of variance  restored.

pca_result$x         # Principal component scores

#-----------------------------------------------------------------------------------------------------------------------------
# Visualize the PCA results (say, using a biplot). 
# Discuss the loadings of the first two principal components. 
# What patterns or groupings do you observe in the data?

fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
biplot(pca_result, main = "PCA Biplot")
