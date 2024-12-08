# Considering the datasets 
# 4. Wine
install.packages("ggcorrplot")
install.packages("factoextra")
install.packages("rattle")
library(factoextra)
library(ggcorrplot)
library(psych)
library(MASS)
library(pheatmap)
library(rattle)
data = wine
#-----------------------------------------------------------------------------------------------------------------------------

# UNIVARIATE ANALYSIS

# 1. Loading the data set data
View(data)

# Structure of the data : 178 obs. of  14 variables
str(data)
cat("Number of rows and columns of the dataset =", dim(data), "\n")
cat("Number of rows  =", nrow(data), "\n")
cat("Number of columns  =", ncol(data), "\n")
cat("Column names :", colnames(data), "\n")

#-----------------------------------------------------------------------------------------------------------------------------
# 2. Summary Statistics
# Choose a numerical variable (say, `ptratio`, `hp`, `wt`). 
# Calculate and interpret the mean, median, standard
# deviation, minimum, and maximum of this variable.

View(describe(data))      # Describing the statistics of each variable

#-----------------------------------------------------------------------------------------------------------------------------
# 3. Distribution Visualization
# Create a histogram and a boxplot for the chosen variable. 
# Describe the shape of the distribution and any potential `outliers’.

# Defining the numerical columns----
num_col = c('Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Phenols', 'Flavanoids', 'Nonflavanoids', 'Proanthocyanins', 'Color','Hue', 'Dilution')
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

# According to the box plot there are no outliers

# Chosen numerical variable = Alcohol
cat("Mean = ", mean(data$Alcohol), "\n")
cat("Median = ", median(data$Alcohol), "\n")
cat("Standard Deviation = ", sd(data$Alcohol), "\n")   
cat("Min = ", min(data$Alcohol), "\n")
cat("Max = ", max(data$Alcohol), "\n")


cat("Median = 13.05  and Mean = 13.00062   
since Mean nearly equals to median , we can infer that the distribution is NOT SKEWED")

#-----------------------------------------------------------------------------------------------------------------------------
# 4. Categorical Variable Analysis
# Choose a categorical variable (say, 'Type'). 
# Create a bar plot to visualize the distribution of this variable.
# What insights can you gather from the plot? 

categorical_columns = c('Type')
for (j in categorical_columns){
  col_counts = table(data[[j]])
  barplot(col_counts, xlab=j, ylab="frequency", main = j, col='gold')
}

# Catagorical variable = Type
# Frequency table
table(data$Type)

# Proportion table
prop.table(table(data$rad))

#-----------------------------------------------------------------------------------------------------------------------------
# MULTIVARIATE ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------

# 5. Correlation Analysis
# Select two numerical variables. Calculate the Pearson 
# correlation coefficient. What does this value indicate about the relationship 
# between these two variables?

#Finding the correlation matrix for the numerical variable
corr_data = data[,num_col]
corr_matrix = cor(corr_data)
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 6,fontsize_col=8,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=8,
         main="Pearson's correlation cofficient")

#  Considered variables : Dilution and Flavanoids
# Calculating Pearson Correlation Coefficient between Dilution and Flavanoids of data table
correlation_1 <- cor(data$Dilution, data$Flavanoids, method = "pearson")
correlation_1

correlation_2 <- cor(data$Alcohol, data$Color, method = "pearson")
correlation_2

#-----------------------------------------------------------------------------------------------------------------------------
# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between the two selected variables.
# Add a trend line. Discuss the relationship you observe.

# Scatter plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
plot(data$Color, data$Dilution, 
     main = "Scatter Plot of Color vs Dilution", 
     xlab = "Dilution", 
     ylab = "Color", 
     col = "blue", 
     pch = 19)
# Add a linear trend line
model <- lm(data$Color ~ data$Dilution)  # Fit a linear model
abline(model, col = "red", lwd = 2)  # Add the trend line

#-----------------------------------------------------------------------------------------------------------------------------
# 7. Multiple Regression
# Fit a linear regression model predicting `Color` using `Flavanoids` and `Dilution` as predictors 
# (here, for the `data` data).Display the summary of the model and interpret the 
# coefficients.

# Fit the linear regression model
model <- lm(Color ~ Alcohol + Dilution, data = data)

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
new_data <- data.frame(Dilution = c(3,4), Flavanoids = c(4, 5))
predictions <- predict(model, newdata = new_data)
print(predictions)

#-----------------------------------------------------------------------------------------------------------------------------
# ADVANCE ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------
# Perform PCA on the numerical variables in this dataset. Plot the explained variance. 
# How many components would you choose based on the scree plot, and why?

data(data)
df  <- data[,2:13]

df_scaled <- scale(df)

pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)  # Importance of components
pca_result$rotation  # Principal axes (loading)

fviz_eig(pca_result, addlabels = TRUE) # Scree plot for the pca

# From the summary of the PCA (Cumulative Proportion : 0.3663 0.5476 0.6659 0.73989 0.80788 0.86054 0.90620) 
# ,it is observed  that the 90.62 %  of variance is restored if 1st 7 PCs are  chosen and 

# From the scree plot it is clear that the explained variance starts to level off from point 7 
# thus we can retain the first 7 PCs, with 90.62% of variance restored.

pca_result$x         # Principal component scores

#-----------------------------------------------------------------------------------------------------------------------------
# Visualize the PCA results (say, using a biplot). 
# Discuss the loadings of the first two principal components. 
# What patterns or groupings do you observe in the data?

fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

##############################################################################################################################