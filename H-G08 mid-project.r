library(dplyr)
library(ggplot2)
library(GGally)

# Define a custom function for mode (this must be run first!)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

                     
# 1. Load the dataset into R, explicitly handling 'NAN' strings found in the CSV
df <- read.csv("https://drive.google.com/file/d/1vOgWbAvg0Z1rtpnJzNMTgr0c1Lf_fKub/view?usp=sharing", header = TRUE, na.strings = c("NAN", "         NAN"))
# ... other code including the get_mode function...

# Handle Missing Values: Create a new dataframe removing rows with NA (na.omit)
df_clean <- na.omit(df)


# load the data set
df <- read.csv("https://drive.google.com/file/d/1vOgWbAvg0Z1rtpnJzNMTgr0c1Lf_fKub/view?usp=sharing")

# 2. Display the first few rows of the dataset
print("First few rows of the data:")
head(df) 

# 3. Show shape (rows x columns)
cat("\nShape of the dataset (Rows, Columns):")
print(dim(df))

# 4. Display data types of each column (Structure of the data frame)
cat("\nStructure of the dataset:")
str(df) 

# 5. Create a summary table for Quantity
quantity_stats <- df_clean %>%
  summarise(
    Count = n(),
    Mean = mean(Quantity),
    Median = median(Quantity),
    Mode = get_mode(Quantity),
    Std_Dev = sd(Quantity),
    Min = min(Quantity),
    Max = max(Quantity)
  )
cat("\nDescriptive Statistics for Quantity:\n")
print(quantity_stats)


#Identify categorical and numerical features.
# Identify Numerical Features (Columns that are numeric or integer)
numerical_features <- names(df)[sapply(df, is.numeric)]

# Identify Categorical Features (Columns that are character or factor)
# Note: Initial load often reads categorical data as 'character'
categorical_features <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]

print("--- Identified Features ---")
print(paste("Numerical Features:", paste(numerical_features, collapse = ", ")))
print(paste("Categorical Features:", paste(categorical_features, collapse = ", ")))



# --- 1. Univariate Analysis ---

# **Distribution Plots for Numerical Variables: Rating**
# Histogram for Rating 
ggplot(df, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Customer Ratings", x = "Rating")

# Boxplot for Unit Price (useful for detecting outliers) 
ggplot(df, aes(y = unit_price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Unit Price", y = "Unit Price")

# Bar chart for Branch (using ggplot2)
ggplot(df, aes(x = Branch, fill = Branch)) +
  geom_bar() +
  labs(title = "Sales Frequency by Branch", y = "Count")

# **Frequency of Categorical Variables: Branch**
cat("\nFrequency of Sales by Branch:")
branch_freq <- df %>%
  group_by(Branch) %>%
  summarise(Count = n()) 
print(branch_freq)

cat("\nFrequency of Sales by Gender:")
gender_freq <- df %>%
  group_by(Gender) %>%
  summarise(Count = n()) 
print(gender_freq)

cat("\nFrequency of Sales by Payment:")
payment_freq <- df %>%
  group_by(Payment) %>%
  summarise(Count = n()) 
print(payment_freq)


# --- 2. Bivariate Analysis ---

# **Correlation Matrix (Heatmap) for Numerical Features** 
# Select numerical columns (excluding Date)
numerical_df <- df %>%
  select(unit_price, Rating)
# Use ggcorr for correlation heatmap 
ggcorr(numerical_df, label = TRUE)

#  Scatter Plots for Numeric Pairs ---

# **Relationship between Unit Price and Quantity**
# Check if higher price leads to lower quantity sold
ggplot(df_clean, aes(x = unit_price, y = Quantity)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear trend line
  labs(title = "Scatter Plot: Unit Price vs. Quantity", x = "Unit Price", y = "Quantity")

# **Relationship between Unit Price and Rating**
# Check if expensive items receive better ratings
ggplot(df_clean, aes(x = unit_price, y = Rating)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Scatter Plot: Unit Price vs. Rating", x = "Unit Price", y = "Customer Rating")

#  Boxplots: Categorical vs. Numerical Features ---

# **Payment Method vs. Customer Type**
# Compare the distribution of sales values across different payment methods
ggplot(df_clean, aes(x = Customer_type, y = Payment, fill = Customer_type)) +
  geom_boxplot() +
  labs(title = "Boxplot: Payment Method system by Customer Type", x = "Payment Method", y = "Total Sales") +
  theme(legend.position = "none") # Remove legend since it's redundant

# **Gender vs. Customer Rating**
# Compare the distribution of ratings given by male vs. female customers
ggplot(df_clean, aes(x = Gender, y = Rating, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot: Customer Rating Distribution by Gender", x = "Gender", y = "Customer Rating") +
  theme(legend.position = "none")


# 3. --Identify Patterns/Relationships (Using Scatter Plot for Unit Price vs. Total Sales) ---

  # Patterns are visible as trends or clusters in the points.
  ggplot(df_clean, aes(x = unit_price, y = Rating)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Unit Price vs. Rating (Check Patterns)", x = "Unit Price", y = "Rating")
  
#  Identify Skewness (Using Histogram for Unit Price) ---
# Skewness is visible when the distribution is not symmetrical.
ggplot(df_clean, aes(x = unit_price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Unit Price (Check Skewness)", x = "Unit Price")  

#  Identify Possible Outliers (Using Boxplot for Quantity) ---
# Outliers appear as individual points (dots) outside the whiskers.
ggplot(df_clean, aes(y = Quantity)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Quantity (Check Outliers)", y = "Quantity")


# Data Processing

# --- 1. Handling Missing Values ---

# Check how many NA values (original 'NAN' strings) are in each column [cite: 761]
cat("\nMissing Values (NA Count) per Column:")
print(colSums(is.na(df))) 

# For simplicity, we remove rows with NA values (as demonstrated in data science.docx)
# For a real project, consider imputation (e.g., replacing with mean/median)
df_clean <- na.omit(df) 
cat("\nTotal NA values after cleaning:")
print(sum(is.na(df_clean))) 

# --- Outlier Handling (Using Interquartile Range - IQR Method) ---

# We will handle outliers in the 'Quantity' column

df_clean <- df_clean[!duplicated(df_clean), ]
# Convert the core numerical columns to the correct numeric type.
df_clean$Quantity <- as.numeric(df_clean$Quantity)
df_clean$unit_price <- as.numeric(df_clean$unit_price)
df_clean$Rating <- as.numeric(df_clean$Rating)

# Calculate the total sales value
df_clean <- df_clean %>%
  mutate(Total_Sales = unit_price * Quantity)

# 1. Calculate IQR and bounds for Quantity
Q1_q <- quantile(df_clean$Quantity, 0.25, na.rm = TRUE)
Q3_q <- quantile(df_clean$Quantity, 0.75, na.rm = TRUE)
IQR_q <- Q3_q - Q1_q

lower_bound_q <- Q1_q - 1.5 * IQR_q
upper_bound_q <- Q3_q + 1.5 * IQR_q

cat("\nOutlier bounds for Quantity (Lower, Upper): ", lower_bound_q, ", ", upper_bound_q, "\n")

# 2. Filter out rows where Quantity falls outside the IQR range
df_final <- df_clean %>%
  filter(Quantity >= lower_bound_q & Quantity <= upper_bound_q)

# Check the reduction in rows after removing outliers
cat("\nOriginal Rows (Cleaned Data):", nrow(df_clean))
cat("\nRows after Outlier Removal:", nrow(df_final), "\n")



# --- 3. Data Conversion (Label Encoding) ---

# Convert R factor types (created in previous steps) into integer codes.
# This results in Label Encoding (1, 2, 3...) for each category.

df_converted <- df_final %>%
  mutate(
    Branch_Encoded = as.numeric(Branch),
    Customer_type_Encoded = as.numeric(Customer_type),
    Gender_Encoded = as.numeric(Gender),
    Payment_Encoded = as.numeric(Payment),
    `Product.line_Encoded` = as.numeric(Product_line)
  )

# Clean up the data frame by removing the original categorical text columns
# and non-essential features (like the original date).
df_converted <- df_converted %>%
  select(
    -Branch, -Customer_type, -Gender, -Payment, -Product_line # Removing the original Date column
    
  )

cat("\nStructure after Label Encoding:\n")
str(df_converted)


# --- 4. Data Transformation (Normalization and Log Transformation) ---

# Function for Min-Max Normalization (scales data to range 0 to 1)
min_max_normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply Min-Max Scaling to numerical features (Unit Price, Quantity, Rating)
df_transformed <- df_converted %>%
  mutate(
    Unit_price_Scaled = min_max_normalize(unit_price),
    Quantity_Scaled = min_max_normalize(Quantity),
    Rating_Scaled = min_max_normalize(Rating),
    Total_Sales_Scaled = min_max_normalize(Total_Sales)
    
  )

# Apply Log Transformation to fix right-skewness (using log(x + 1) for safety)
# This is typically needed for count data or variables like sales/price.
df_transformed <- df_transformed %>%
  mutate(Total_Sales_Log = log(Total_Sales + 1))

# Optional: Drop the original unscaled/untransformed numeric features
df_transformed <- df_transformed %>%
  select(-Unit_price, -Quantity, -Rating, -Total_Sales)

cat("\nStructure after Transformation/Scaling:\n")
str(df_transformed)





# --- 5. Feature Selection ---

# 1. Feature Selection based on Correlation Analysis
# We check the correlation of the scaled features to identify redundant variables.

# Select only the features that will be used for modeling (scaled/encoded features)
df_selected <- df_transformed %>%
  select(
    ends_with("_Encoded"), # All encoded categorical features
    ends_with("_Scaled"), # All Min-Max scaled features
    Total_Sales_Log # Log transformed target
  )

# Calculate and display the final correlation matrix
cor_matrix_final <- cor(df_selected)
print("\nFinal Correlation Matrix (for Feature Selection):")
print(round(cor_matrix_final, 2))

# DECISION BASED ON CORRELATION (EXAMPLE):
# If two features are highly correlated (e.g., cor > 0.9), you typically drop one.
# Since Total_Sales is derived from Unit_price and Quantity, we might choose to drop
# Quantity_Scaled to reduce multicollinearity, but we keep Total_Sales_Scaled as a key metric.

df_final_project <- df_selected %>%
  select(-Quantity_Scaled) # Example decision: Quantity_Scaled is redundant/highly correlated

# 2. Variance Thresholding (Remove features with near-zero variance)
# Features with very little variance do not contribute much to predictive models.
variance_check <- sapply(df_final_project, var)
low_variance_features <- names(variance_check)[variance_check < 0.01]

cat("\nFeatures with Variance < 0.01 (Low Variance Threshold, these should be dropped):\n")
print(low_variance_features)

# Final clean-up: Remove low variance features if any were found
df_final_project <- df_final_project %>%
  select(-one_of(low_variance_features))

cat("\nStructure of the FINAL Project Data Frame (Ready for Modeling):\n")
str(df_final_project)
head(df_final_project)

# --- 4. Model Evaluation ---

set.seed(123)
target_col <- "Payment_Encoded" 

# Prepare Data: Convert target to factor BEFORE splitting
model_df <- df_final_project
model_df[[target_col]] <- as.factor(model_df[[target_col]])

# Train/Test Split
trainIndex <- createDataPartition(model_df[[target_col]], p = 0.8, list = FALSE)
trainData <- model_df[trainIndex, ]
testData  <- model_df[-trainIndex, ]

# Decision Tree Model
form <- as.formula(paste(target_col, "~ ."))
model_dt <- rpart(form, data = trainData, method = "class")
rpart.plot(model_dt, main = "Decision Tree Visualization")

# Predictions
pred_dt <- predict(model_dt, newdata = testData, type = "class")

# FIX: Force prediction levels to match test data (Prevents "1:numclass" error)
pred_dt <- factor(pred_dt, levels = levels(testData[[target_col]]))

# Confusion Matrix
conf_mat <- confusionMatrix(pred_dt, testData[[target_col]])
print(conf_mat)

# Extract Metrics
accuracy <- conf_mat$overall["Accuracy"]

byClass <- conf_mat$byClass
if (is.null(dim(byClass))) {
  # Binary case
  precision <- byClass["Pos Pred Value"]
  recall    <- byClass["Sensitivity"]
  f1        <- byClass["F1"]
} else {
  # Multiclass case (Macro Average)
  precision <- mean(byClass[, "Pos Pred Value"], na.rm = TRUE)
  recall    <- mean(byClass[, "Sensitivity"], na.rm = TRUE)
  f1        <- mean(byClass[, "F1"], na.rm = TRUE)
}

print(paste("Accuracy: ", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:   ", round(recall, 4)))
print(paste("F1 Score: ", round(f1, 4)))

# Visualize Confusion Matrix
cm_df <- as.data.frame(conf_mat$table)

ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, color = "black") +
  scale_fill_gradient(low = "#e7f0fa", high = "#2c7bb6") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap",
    x = "Actual Class",
    y = "Predicted Class",
    fill = "Frequency"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )