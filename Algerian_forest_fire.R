### COSC 757 Assignment 1 ###

# Ann Pham # 0790792
# Using the Algerian Forest Fires Dataset: https://archive.ics.uci.edu/dataset/547/algerian+forest+fires+dataset

# clear workspace
rm(list = ls()) 

## Load Libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gbm)

# Load the dataset
data <- read.csv("Merge_Algerian_forest_fires.csv")
head(data)
summary(data)




# 1. Clean data
  ## Remove extra whitespaces from variables name row
  names(data) <- str_trim(names(data))
      
  ## Remove extra space from each value in the 'Classes' variable
  data$Classes <- str_trim(data$Classes)
      
  ## Convert variables to appropriate data types
  data$DC <- as.numeric(data$DC)
  data$FWI <- as.numeric(data$FWI)
  
  data$Classes <- as.factor(data$Classes)
  data$Region <- as.factor(data$Region)
  
  ## Recheck type    
  head(data)
  summary(data)
  
  ## Check for missing values
  na_count <- colSums(is.na(data))
  print(na_count) # 1 in DC 1 in FWI
      
  ## Remove rows with NA values in any column
  data <- na.omit(data)
      
  ## Recheck for missing values
  na_count <- colSums(is.na(data))
  print(na_count) # no N/A





# 2. EDA
  head(data)
  summary(data)
      
  # Distribution of attributes
  # Loop through each attribute and create individual plots
  for (col in names(data)) {
    # Check if the column contains numeric data
    if (is.numeric(data[[col]])) {
      # For numeric columns, create a histogram
      hist(data[[col]], main = paste("Distribution of", col), xlab = col, col = "lightblue", border = "black")
    } else {
      # For categorical columns, create a bar plot
      barplot(table(data[[col]]), main = paste("Distribution of", col), xlab = col, col = "lightblue", border = "black")
    }
  }
  
  
  # Distribution of month vs other numeric attributes
  plots <- list()
  
  numeric_vars <- c('Temperature', 'RH', 'Ws', 'Rain', 'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI')
  
  # Loop through each numeric variable and create scatter plots
  for (var in numeric_vars) {
    scatter_plot <- ggplot(data, aes(x = factor(month), y = !!sym(var))) +
      geom_point(aes(color = Classes), alpha = 0.5) +
      labs(title = paste("Relationship Between Month and", var),
           x = "Month", y = var) +
      theme_minimal() +
      theme(legend.position = "top")
    
    scatter_plots[[var]] <- scatter_plot
  }
  
  # Arrange and display the scatter plots in a grid
  grid.arrange(grobs = scatter_plots, ncol = 2)
  
  # Month vs classes
  ggplot(data, aes(x = factor(month), fill = Classes)) +
    geom_bar(position = 'dodge') +
    labs(title = 'Month vs Classes', x = 'Month', y = 'Count') +
    theme_minimal()
  
  
  
  # Distribution of FWI vs other attributes
  attributes <- c("Temperature", "RH", "Ws", "Rain", "FFMC", "DMC", "DC", "ISI", "BUI")
  
  # Store scatter plots in a list
  scatter_plots <- list()
  
  # Loop through each numeric variable and create scatter plots
  for (var in attributes) {
    scatter_plot <- ggplot(data, aes(x = FWI, y = !!sym(var))) +
      geom_point(alpha = 0.5) +
      labs(title = paste("Relationship Between FWI and", var),
           x = "FWI", y = var) +
      theme_minimal()
    
    scatter_plots[[var]] <- scatter_plot
  }
  
  # Arrange and display the scatter plots in a grid
  grid.arrange(grobs = scatter_plots, ncol = 2)
  
  
  

# 3. Data Pre-processing
# 3.1 Normalization
cols_to_normalize <- c('Temperature', 'RH', 'Ws', 'Rain', 'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI')

  # Min-max normalization
      par(mfrow=c(5, 4), mar=c(4, 4, 2, 2), oma=c(0, 0, 2, 0))
      
      for (col in cols_to_normalize) {
    
      # Plot before normalization
      hist(data[[col]], main = paste("Before\n", col), xlab = col, col = "lightblue", border = "black")
      
      # Plot after Min-Max Normalization
      hist(data[[col]]/(max(data[[col]]) - min(data[[col]])), main = paste("After\n", col), xlab = col, col = "lightgreen", border = "black")
    }
    
      # Reset the plotting layout to the default
      par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
    
      
  # Z-score normalization
      par(mfrow=c(5, 4), mar=c(4, 4, 2, 2), oma=c(0, 0, 2, 0))
      
      for (col in cols_to_normalize) {
      
      # Plot before normalization
      hist(data[[col]], main = paste("Before\n", col), xlab = col, col = "lightblue", border = "black")
      
      # Plot after Z-score Normalization
      hist(scale(data[[col]]), main = paste("After\n", col), xlab = col, col = "lightgreen", border = "black")
      }
      
      # Reset the plotting layout to the default
      par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
    
    
  # Decimal scaling normalization
      par(mfrow=c(5, 4), mar=c(4, 4, 2, 2), oma=c(0, 0, 2, 0))
      
      for (col in cols_to_normalize) {
        
      # Plot before normalization
        hist(data[[col]], main = paste("Before\n", col), xlab = col, col = "lightblue", border = "black")
        
      # Calculate the scaling factor
        scaling_factor <- 10^(ceiling(log10(max(data[[col]]))))
        
      # Plot after Decimal Scaling Normalization
        hist(data[[col]] / scaling_factor, main = paste("After\n", col), xlab = col, col = "lightgreen", border = "black")
      }
      
      # Reset the plotting layout to the default
      par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
      
  


# 3.2 Binning
      # Select the 'Temperature' variable
      temperature <- data$Temperature
      
      # Equal width binning
      num_bins_equal_width <- 5 # Define the number of bins
      equal_width_bins <- cut(temperature, breaks = num_bins_equal_width, labels = FALSE)
      hist(temperature, breaks = num_bins_equal_width, col = "lightblue", xlab = "Temperature", main = "Equal Width Binning")  # Plot histogram
      
      # Equal frequency binning
      num_bins_equal_freq <- 5 # Define the number of bins
      equal_freq_bins <- cut(temperature, breaks = quantile(temperature, probs = seq(0, 1, 1/num_bins_equal_freq), na.rm = TRUE), labels = FALSE)
      hist(temperature, breaks = quantile(temperature, probs = seq(0, 1, 1/num_bins_equal_freq), na.rm = TRUE), col = "lightgreen", xlab = "Temperature", main = "Equal Frequency Binning")   # Plot histogram
      

      
# 3.3 Transformation
  # Select the 'Rain' variable
  rain <- data$Rain
      
  # Original distribution
  hist(rain, main = "Original Distribution of rain", xlab = "rain", col = "lightblue")
      
  # Transform using natural log
  rain_log <- log(rain)
  hist(rain_log, main = "Distribution of rain (Natural Log Transformation)", xlab = "rain (log)", col = "lightgreen")
      
  # Transform using square root
  rain_sqrt <- sqrt(rain)
  hist(rain_sqrt, main = "Distribution of rain (Square Root Transformation)", xlab = "rain (sqrt)", col = "lightpink")
      
  # Transform using inverse square root
  rain_inv_sqrt <- 1/sqrt(rain)
  hist(rain_inv_sqrt, main = "Distribution of rain (Inverse Square Root Transformation)", xlab = "rain (1/sqrt)", col = "lightyellow")
   
  
  
     
# 4. Regression Analysis
## Question: Can we predict the Fire Weather Index (FWI) based on weather-related variables such as Temperature, Relative Humidity (RH), Wind Speed (Ws), Rainfall (Rain)? 
  # Null Hypothesis: There is no significant relationship between the weather-related variables and FWI.
  # Alternative Hypothesis: There is a significant relationship between the weather-related variables and FWI.
  
  # Split data into training and testing sets (80% training, 20% testing)
  set.seed(123) # for reproducibility
  sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[sample_index, ]
  test_data <- data[-sample_index, ]
  
  # Linear Regression
  linear_model <- lm(FWI ~ Temperature + RH + Ws + Rain, data = train_data)
  summary(linear_model)
 
  # Gradient Boosting Regression
  gbm_model <- gbm(FWI ~ Temperature + RH + Ws + Rain, data = train_data, n.trees = 100, interaction.depth = 4, shrinkage = 0.1, distribution = "gaussian")
  summary(gbm_model)
  
  # Make predictions on the test set
  linear_pred <- predict(linear_model, newdata = test_data)
  gbm_pred <- predict(gbm_model, test_data, n.trees = 100)
  
  ## Model Comparison and Evaluation
  # Calculate RMSE for each model
  rmse_linear <- sqrt(mean((test_data$FWI - linear_pred)^2))
  rmse_gbm <- sqrt(mean((test_data$FWI - gbm_pred)^2))
  
  # Print RMSE for each model
  print(paste("Linear Regression RMSE: ", rmse_linear))
  print(paste("Gradient Boosting RMSE: ", rmse_gbm))
  
  ## Visualization
  # Create a data frame with actual and predicted FWI values
  results <- data.frame(Actual = test_data$FWI, Linear_Predicted = linear_pred, GBM_Predicted = gbm_pred)
  
  # Plotting the scatter plot
  plot(results$Actual, results$Linear_Predicted, col = "blue", xlab = "Actual FWI", ylab = "Predicted FWI", main = "Actual vs Predicted FWI")
  points(results$Actual, results$GBM_Predicted, col = "green")
  legend("topright", legend = c("Linear Regression", "Gradient Boosting"), col = c("blue", "green"), pch = 1)
  
  # Add a 45-degree line to indicate perfect predictions
  abline(a = 0, b = 1, col = "black", lty = 2)

  





