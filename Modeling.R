#Modeling
library(dplyr)
setwd('/Users/oliverreidmiller/Desktop/Applied Econ Data/Project/Lax Pro Data/Womens Data ')
library(caret)
library(kernlab)
library(ROCR)

df <- read.csv('finalWLAXdf.csv')

df <- df[-c(1)]

library(stargazer)
stargazer(df, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
library(corrplot)

correlation_matrix <- cor(df)

# Plot correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")


#accuracy =0.744654
library(e1071)
df$Win <- as.factor(df$Win)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train <- df[sample, ]
test <- df[!sample, ]

library(ROCR)

# Train the SVM model using the training data
svm_model <- svm(Win ~ ., data = train, cost = 4, kernel = 'linear')

# Predict probabilities on the test set
svm_probs <- predict(svm_model, newdata = test)

# Create a prediction object
svm_pred <- prediction(svm_probs[,2], test$Win,probability = TRUE)

# Create a performance object
svm_perf <- performance(svm_pred, "tpr", "fpr")

# Plot the ROC curve
plot(svm_perf, main = "ROC Curve for SVM Model", col = "blue")




svm_model <- svm(Win ~ ., data = train, cost = 4, kernel = 'linear')
summary(svm_model)
accuracy=0

library(ggplot2)

# Initialize an empty data frame to store iteration and accuracy
plot_data <- data.frame(iteration = 1:20, accuracy = numeric(20))

# Loop through 20 iterations
for (i in 1:20) {
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
  train <- df[sample, ]
  test <- df[!sample, ]
  
  # Train the SVM model (you need to define `svm_model` before this loop)
  
  # Predictions on the testing data
  predicted_classes <- predict(svm_model, test)
  
  # Calculate accuracy and store it
  plot_data$accuracy[i] <- mean(predicted_classes == test$Win)
}

ggplot(plot_data, aes(x = factor(iteration), y = accuracy)) +
  geom_bar(stat = "identity", fill = "firebrick") +  # Set stat to "identity" to use values directly
  labs(x = "Iteration", y = "Accuracy", title = "20 Fold Resampled Test Set Accuracy",
       subtitle = 'Average Test Set Accuracy = 74%')  +  # Set y-axis limits
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    plot.title = element_text(face = "italic") # Make axis titles italic
  )+ coord_cartesian(ylim = c(.7, .8)) +
  geom_hline(yintercept = 0.7395129, color = "black", type = '')

print(mean(plot_data$accuracy))

svm_model <- svm(Win ~ ., data = df, cost = 4, kernel = 'linear')


perf_val <- performance(predicted_classes,"auc")
perf_val

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)



# Assuming your target variable is in the last column
target <- df[, ncol(df)]
features <- df[, -ncol(df)]
feature_names <- colnames(features)  # Get variable names

# Train linear SVM model

# Extract feature importance (coefficients)
coefficients <- t(svm_model$coefs) %*% svm_model$SV

# Get the absolute value of coefficients for feature importance
importance <- abs(coefficients)

# Sort the importance scores
sorted_importance <- sort(importance, decreasing = TRUE)
sorted_feature_names <- feature_names[order(importance, decreasing = TRUE)]  # Sort variable names accordingly


# Reformatting feature names
reformatted_feature_names <- gsub("_", " ", sorted_feature_names)  # Replace underscores with spaces
reformatted_feature_names <- gsub("(\\b[a-z])", "\\U\\1", reformatted_feature_names, perl = TRUE)  # Capitalize first letter of each word
reformatted_feature_names <- gsub("(\\b[A-Z])", " \\1", reformatted_feature_names)  # Add space before capital letters


# Load required libraries
library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(
  feature = reformatted_feature_names,
  importance = sorted_importance
)
plot_data<- plot_data[1:10,]

ggplot(plot_data, aes(x = importance, y = reorder(feature, -importance))) +
  geom_bar(stat = "identity", fill = "firebrick") +
  labs(title = "Top 10 Feature Importance Parameters of Linear SVM",
       x = "Absolute Coefficient Value",
       y = "Features\n",
       caption = "Feature importance in linear SVMs is determined by coefficients assigned to each feature, indicating their impact on class separation. 
 Higher coefficients denote greater influence, while lower coefficients imply less contribution to the decision boundary.") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, family = "Times New Roman"),  # Increase y-axis text size
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, color = "black", family = "Times New Roman"),  # Increase x-axis text size
        axis.title.y = element_text(size = 14,  family = "Times New Roman"),  # Increase y-axis title size
        axis.title.x = element_text(size = 14, family = "Times New Roman"),  # Increase x-axis title size
        plot.title = element_text(size = 18, family = "Times New Roman", face = 'italic'),  # Increase title size
        plot.caption = element_text(size = 12, family = "Times New Roman")) +  # Increase caption size and set font
  coord_flip()  # Flip coordinates for horizontal bar plot

bivar_svm_model <- svm(Win ~ record+opp_record, data = df, cost = 4, kernel = 'linear')


# Assuming 'df' is your dataframe
# Fit the SVM model
svm_model <- svm(Win ~ ., data = train, cost = 4, kernel = 'linear')

summary(svm_model)

# Predictions on the training data (just for example, usually you'd use a separate test set)


# Evaluate the accuracy
print(paste("Accuracy:", accuracy))

# Print the summary of the model
#tune_out = tune(svm, 
#                Win~., 
#                data = train, 
#                kernel = "linear", 
 #               ranges = list(cost = c(3.5,3.75,4,4.25,4.5)))
#bestmod = tune_out$best.model
#summary(bestmod)


# Predictions on the training data (just for example, usually you'd use a separate test set)


bivar_svm_model <- svm(Win ~ record+opp_record, data = df, cost = 4, kernel = 'linear')

library(ggplot2)

# Train the SVM
bivar_svm_model <- svm(Win ~ record + opp_record, data = df, cost = 4, kernel = 'linear')

# Generate data for plotting decision boundary
x_range <- range(df$record)
y_range <- range(df$opp_record)
x <- seq(from = x_range[1], to = x_range[2], length.out = 100)
y <- seq(from = y_range[1], to = y_range[2], length.out = 100)
xy_grid <- expand.grid(record = x, opp_record = y)
predictions <- predict(bivar_svm_model, xy_grid)

# Create a data frame for plotting
plot_data <- cbind(xy_grid, predicted_class = as.numeric(predictions))

# Plot decision boundary
ggplot(plot_data, aes(x = record, y = opp_record)) +
  geom_contour(aes(z = predicted_class), color = "black") +
  geom_point(data = df, aes(color = Win),size = 2) +
  scale_color_manual(values = c("firebrick1", "dodgerblue")) +
  labs(x = "Record", y = "Opponent Record", title = "Bivariate Predictor Linear SVM Model using Record", 
       subtitle = '70% Test Set Accuracy',
       caption = 'Black line represents linear hyperplane') +
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(face = "italic", size = 30), 
  )


sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train <- df[sample, ]
test <- df[!sample, ]

# Train the SVM model (you need to define `svm_model` before this loop)
#bivar_svm_model <- svm(Win ~ avgFaceOffPct + opp_avgFaceOffPct, data = train, cost = 4, kernel = 'linear')

# Predictions on the testing data
predicted_classes <- predict(bivar_svm_model, test)

# Calculate accuracy and store it
print(mean(predicted_classes == test$Win))


