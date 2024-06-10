# Load necessary libraries
library(rpart)
library(caret)
library(rpart.plot)
library(RMySQL)
library(dplyr)
library(tidyverse)
library(caTools)
library(reshape2)
library(pROC)
library(glmnet)
library(factoextra)
library(ggplot2)
library(pheatmap)
library(class)

# Database Connection
connection <- dbConnect(MySQL(), user='root', password='Alaji@007', host='localhost', dbname='customer_churn', port=3306)

# Data Retrieval
query <- "SELECT * FROM customer_churn.customers"
telco_data <- dbGetQuery(connection, query)
dbDisconnect(connection)

# Renaming the columns
names(telco_data) <- c("customer_id", "college", "annual_income", "monthly_overcharge", "leftover_minutes_percent", 
                       "house_value", "phone_cost", "long_calls_per_month", "avg_call_duration", "satisfaction_level", 
                       "usage_level", "considering_plan_change", "churn_status")


# Data Preprocessing
telco_data <- telco_data %>%
  mutate(college = ifelse(college == 'zero', 0, 1),
         churn_status = ifelse(churn_status == 'STAY', 0, 1),
         satisfaction_level = factor(satisfaction_level),
         usage_level = factor(usage_level),
         considering_plan_change = factor(considering_plan_change))

#I am converting several character columns to factors because for some models like those for decision trees, need categorical data to be explicitly declared as factors. 
#So, I am converting these columns to ensure my model understands these are categorical variables.

# Checking nulls and duplicates
print(colSums(is.na(telco_data)))
print(any(duplicated(telco_data)))

##there are no nulls and no duplicate in the dataset

# Correlation Matrix and Heatmap
numeric_data <- telco_data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1), axis.text.y = element_text(size = 12)) +
  coord_fixed()

# Pie Chart for Churn Status
ggplot(telco_data, aes(x = "", fill = factor(churn_status))) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), position = position_stack(vjust = 0.5), stat = "count") +
  theme_void() +
  labs(fill = "Churn Status", title = "Pie Chart of Churn Status Proportions")



# TASK 1 DECISION TREE

data_tree <- telco_data %>% 
  select(-customer_id)

# setting the seed and Splitting the dataset using cat library
set.seed(123)
sample_split <- sample.split(data_tree$churn_status, SplitRatio = 0.70)
train_data <- subset(data_tree, sample_split == TRUE)
test_data <- subset(data_tree, sample_split == FALSE)

# Building the decision tree model
decision_model <- rpart(churn_status ~ ., data = train_data, method = "class", 
                        minbucket = 5, maxdepth = 6, cp = 0.001)

# Predicting and creating a confusion matrix
predictions <- predict(decision_model, test_data, type = "class")
conf_matrix <- table(predictions, test_data$churn_status)

# Display the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

# Extracting True Positives, False Positives, True Negatives, False Negatives
TP <- conf_matrix[2, 2]
FP <- conf_matrix[2, 1]
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]

# Calculating accuracy, precision, recall, and F1 score
accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Printing the evaluation metrics
cat("Model Evaluation Metrics:\n")
cat(sprintf("Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision: %.4f\n", precision))
cat(sprintf("Recall: %.4f\n", recall))
cat(sprintf("F1 Score: %.4f\n", f1_score))

# Visualizing the decision tree
rpart.plot(decision_model, main="Decision Tree", extra=104, box.palette="RdBu", 
           shadow.col="gray", fallen.leaves=TRUE, cex=0.6, type=4)

#TASK 2 Logistic Regression with glmnet

lr_data <- data_tree


# Setting the seed for reproducibility and splitting the dataset
set.seed(123)
sample_split <- sample.split(lr_data$churn_status, SplitRatio = 0.70)
train_data <- subset(lr_data, sample_split == TRUE)
test_data <- subset(lr_data, sample_split == FALSE)

# Fit logistic regression model using glmnet

# note that cv.glmnet uses cross-validation to select the best lambda automatically
x_train <- model.matrix(churn_status ~ . - 1, data = train_data)
y_train <- train_data$churn_status
cv_fit <- cv.glmnet(x_train, y_train, family = "binomial")

# Make predictions on the test set
# Convert probabilities to binary outcomes using a 0.5 threshold
predictions <- predict(cv_fit, newx = model.matrix(churn_status ~ . - 1, data = test_data), s = "lambda.min", type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create confusion matrix and calculate metrics
conf_matrix <- table(predicted_classes, test_data$churn_status)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * precision * recall / (precision + recall)

# Output these metrics
cat("Model Evaluation Metrics:\n")
cat(sprintf("Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision: %.4f\n", precision))
cat(sprintf("Recall: %.4f\n", recall))
cat(sprintf("F1 Score: %.4f\n", f1_score))

# Calculate the ROC curve and AUC
roc_curve <- roc(test_data$churn_status, predictions)
plot(roc_curve, col="#1c61b6", lwd=2, main="ROC Curve for Logistic Regression Model")
abline(a=0, b=1, col="gray", lty=2)
legend("bottomright", legend=c("Logistic Regression Model", "Chance"), col=c("#1c61b6", "gray"), lwd=2, lty=c(1, 2))
auc_value <- auc(roc_curve)
cat(sprintf("\nArea Under the ROC Curve (AUC): %.4f\n", auc_value))

# Extract coefficients at the best lambda
coef_at_min_lambda <- coef(cv_fit, s = "lambda.min")
cat("\nCoefficients at Minimum Lambda:\n")
print(coef_at_min_lambda)


# Justification for Using glmnet in Logistic Regression Model:IN the report



# kNN Model and Cross-validation 
#To Build the best kNN model to predict the probability a given customer will leave.

k_data <- data_tree

# Normalizing numeric features 2 to 8 for better model performance
k_data[, 2:8] <- scale(k_data[, 2:8])

# Splitting the data set into training and test sets and Setting a seed for reproducibility
set.seed(123)
sample_split <- sample.split(k_data$churn_status, SplitRatio = 0.70)
train_data <- subset(k_data, sample_split == TRUE)
test_data <- subset(k_data, sample_split == FALSE)

# Convert all features to numeric
train_data <- as.data.frame(lapply(train_data, as.numeric))
test_data <- as.data.frame(lapply(test_data, as.numeric))

# Preparing training and test sets for kNN
x_train_knn <- subset(train_data, select = -churn_status)
y_train_knn <- train_data$churn_status
x_test_knn <- subset(test_data, select = -churn_status)
y_test_knn <- test_data$churn_status

# Train the kNN model
knn_model <- knn(train = x_train_knn, test = x_test_knn, cl = y_train_knn, k = 5)

# Evaluate the model using confusion matrix and calculate metrics
conf_matrix_knn <- table(Predictions = knn_model, TrueStatus = y_test_knn)
accuracy_knn <- sum(diag(conf_matrix_knn)) / sum(conf_matrix_knn)
precision_knn <- conf_matrix_knn[2, 2] / sum(conf_matrix_knn[2, ])
recall_knn <- conf_matrix_knn[2, 2] / sum(conf_matrix_knn[, 2])
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

# Output initial model metrics

cat("Initial kNN Model Metrics:\n")
cat(sprintf("Accuracy: %.4f\n", accuracy_knn))
cat(sprintf("Precision: %.4f\n", precision_knn))
cat(sprintf("Recall: %.4f\n", recall_knn))
cat(sprintf("F1 Score: %.4f\n", f1_score_knn))

# Cross-validation to find the optimal k value which is key for this algorithm
control <- trainControl(method = "cv", number = 10)
y_train_knn_factor <- as.factor(y_train_knn) # Convert to factor for classification

grid <- expand.grid(k = 1:20) # Specifying the range of k to tune

set.seed(123) # Seed for reproducibility

knn_cv_model <- train(x = x_train_knn, y = y_train_knn_factor, method = "knn", trControl = control, tuneGrid = grid)

# Plotting model performance across different k values
results <- knn_cv_model$results
ggplot(results, aes(x = k, y = Accuracy)) + geom_line() + geom_point(shape = 1) +
  ggtitle("kNN Model Performance Across Different k Values") + xlab("Number of Neighbors (k)") +
  ylab("Accuracy") + theme_minimal()

# Final model prediction using optimal k via cross validation
optimal_k <- knn_cv_model$bestTune$k
final_predictions <- knn(train = x_train_knn, test = x_test_knn, cl = y_train_knn, k = optimal_k)

# Calculate and output final model metrics
final_conf_matrix <- table(Predicted = final_predictions, Actual = y_test_knn)
final_accuracy <- sum(diag(final_conf_matrix)) / sum(final_conf_matrix)
final_precision <- final_conf_matrix[2, 2] / sum(final_conf_matrix[2, ])
final_recall <- final_conf_matrix[2, 2] / sum(final_conf_matrix[, 2])
final_f1_score <- 2 * (final_precision * final_recall) / (final_precision + final_recall)

cat("\nFinal kNN Model Metrics with k =", optimal_k, ":\n")
cat(sprintf("Accuracy: %.4f\n", final_accuracy))
cat(sprintf("Precision: %.4f\n", final_precision))
cat(sprintf("Recall: %.4f\n", final_recall))
cat(sprintf("F1 Score: %.4f\n", final_f1_score))



# Enhanced kNN Model with Cross-Validation:

# The initial kNN model served as a foundational step in understanding customer churn data. However to achieve a more reliable model, implementing cross-validation iscrucial. 
# This method enabled testing the model's stability and performance across diverse data subset rather than depending solely on a single split. 
# Employing 10-fold cross-validation helped average out the performance variability caused by the randomness of the data split and led to a more dependable model assessment.
# Through cross-validation, the optimal number of neighbors (k) was identified as 20. This adjustment increased the model's accuracy from an initial 65.07% to a more substantial 67%. 
# The improvement in accuracy, precision and F1 score showsthe use of cross-validation in refining the kNN model. 
# It ensures that the model is not excessively tailored to a particular data subset and can effectively generalize to new and unseen data.


# Task 4: Clustering with K-Means

cluster_data <- data_tree

# scaling the continous variables
cluster_data[,2:8] <- scale(cluster_data[,2:8])

# Convert categorical variables to dummy variables because columns 9 to 11 are categorical
dummy_vars <- dummyVars("~ .", data = cluster_data[,9:11])

#merge both scaled and dummy to give the final data
final_cluster_data <- cbind(cluster_data[,1:8], predict(dummy_vars, newdata = cluster_data[,9:11]))
head(final_cluster_data)


#Step 1: Determine the Optimal Number of Clusters (Elbow Method)

set.seed(123)
wss <- sapply(1:15, function(k) sum(kmeans(final_cluster_data, centers = k, nstart = 25)$withinss))


# Plot the curve for thee Elbow Method 
plot(1:15, wss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within-Cluster Sum of Squares", 
     main = "Elbow Method for Determining Optimal Number of Clusters")

#To determine the ideal number of clusters for k-means clustering, 
#I employed the Elbow Method by manually calculating the 
#within-cluster sum of squares (WSS) for a range of potential cluster counts. 
#This method involves plotting WSS against varying numbers of clusters to visually identify the elbow point. 
#The elbow point represents the number of clusters after which there is a diminishing rate of decrease in WSS, 
#indicating diminishing returns in terms of cluster compactness. Upon examination of the plot, 
#the elbow point appeared to be between 4 and 6 clusters. Consequently, I selected 5 clusters as the optimal number. 
#This choice represents a balance between achieving lower WSS (indicative of tighter clusters) and maintaining a practical number of clusters for subsequent analysis. 
#This balance is crucial for effective clustering that captures meaningful patterns in the data without over fitting.


# Perform K-Means Clustering with 5 clusters (chosen based on Elbow Method)
set.seed(123)
kmeans_result <- kmeans(final_cluster_data, centers = 5, nstart = 25)
final_cluster_data$cluster <- kmeans_result$cluster

# Determine and print the means for each cluster
cluster_means <- aggregate(final_cluster_data[, -ncol(final_cluster_data)], 
                           by = list(cluster = final_cluster_data$cluster), FUN = mean)
print(cluster_means)
print(table(final_cluster_data$cluster))

# PCA and Heatmap Visualization


# Perform PCA analysis on the scaled data without the cluster column
pca_result <- prcomp(final_cluster_data[, -ncol(final_cluster_data)], scale. = TRUE)

# Data frame with the PCA results and cluster assignments
pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], cluster = final_cluster_data$cluster)

# Plot the first two principal components colored by cluster
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point() +
  scale_color_manual(values = rainbow(5)) +
  theme_minimal() +
  labs(title = "PCA Plot of Clusters", x = "Principal Component 1", y = "Principal Component 2", color = "Cluster")

# Prepare the data for the heatmap
cluster_means_no_cluster <- cluster_means[,-1]
rownames(cluster_means_no_cluster) <- cluster_means$cluster

# Draw the heatmap
pheatmap(cluster_means_no_cluster, scale = "row", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "complete", 
         color = colorRampPalette(c("blue", "white", "red"))(50))


The k-means clustering results are illustrated through the PCA plot and a heatmap. 
The PCA plot effectively displays the distribution of data points across the first two principal components categorized by their clusters. 
The  table also shows the size of each cluster provides a clear quantitative perspective on the segmentation within the customer base. 
### Cluster 2 Characteristics

* High annual income
* High monthly overcharge
* Moderate leftover minutes percent
* Slightly below-average house value
* Very high phone cost
* High number of long calls per month
* Slightly above-average call duration
* Satisfaction levels are relatively distributed with a slight lean towards higher satisfaction
* Moderate usage levels with a tendency towards higher usage
* Moderate consideration of plan changes

### Explanation in Business Terms based on the Task:

#Cluster 2 shows a segment of customers who have high annual incomes and are likely to incur high monthly overcharges. 
#This suggests they are heavy users of the service but may not be on the most cost-efficient plans. 
#Their house values are slightly below average in the dataset, maybe they are pragmatic about their spending outside of telecommunications services. 
#The very high phone costs and number of long calls per month may mean that they use their phones very often; maybe for business purposes. 
#This is also supported by their longer average call durations.

#This group also shows a varied level of satisfaction, meaning there is room for improving customer experience. 
#Their moderate consideration for plan changes indicates a level of contentment 
#but also presents an opportunity for proposing more suitable plans that cater to their heavy usage patterns.


# save models

# Save the decision tree model to disk
saveRDS(decision_model, "decision_model.rds")

# Save the logistic regression model to disk
saveRDS(cv_fit, "logistic_regression_model.rds")

# Save the training data and labels for kNN to disk
saveRDS(x_train_knn, "x_train_knn.rds")
saveRDS(y_train_knn, "y_train_knn.rds")



decision_model <- readRDS("C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/decision_model.rds")
logistic_regression_model <- readRDS("C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/logistic_regression_model.rds")
x_train_knn <- readRDS("C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/x_train_knn.rds")
y_train_knn <- readRDS("C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/y_train_knn.rds")

##data export
write.csv(data_tree, file = "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/data_tree.csv", row.names = FALSE)
write.csv(k_data, file = "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/k_data.csv", row.names = FALSE)
write.csv(lr_data, file = "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/lr_data.csv", row.names = FALSE)
# End of my scripts





## FINAL TASK : PREDICTIVE DASHBOARD


```{r decision-dashboard-img, echo=FALSE, out.width='100%', fig.align='center'}
library(png)
library(grid)

decision_dashboard_path <- "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/Decision_dashboard.PNG"
decision_dashboard_img <- readPNG(decision_dashboard_path, native = TRUE)
grid.raster(decision_dashboard_img)
```


```{r logistic-dashboard-img, echo=FALSE, out.width='100%', fig.align='center'}
logistic_dashboard_path <- "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/Logistic_dashboard.PNG"
logistic_dashboard_img <- readPNG(logistic_dashboard_path, native = TRUE)
grid.raster(logistic_dashboard_img)
```


```{r knn-dashboard-img, echo=FALSE, out.width='100%', fig.align='center'}
knn_dashboard_path <- "C:/Users/ACER SPIN3/Downloads/ASSIGNMENT DATA/DATA SCIENCE/knn_dashboard.PNG"
knn_dashboard_img <- readPNG(knn_dashboard_path, native = TRUE)
grid.raster(knn_dashboard_img)
```
