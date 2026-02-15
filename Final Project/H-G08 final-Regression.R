library(dplyr)
library(ggplot2)
library(GGally)
library(janitor)
library(caret)
library(rpart)
library(rpart.plot)

# Define a custom function for mode (this must be run first!)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 1. Load the dataset into R, explicitly handling 'NAN' strings found in the CSV
# NOTE: The /view link is NOT a direct CSV file. Use direct download link:
file_id <- "1vOgWbAvg0Z1rtpnJzNMTgr0c1Lf_fKub"
url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

df <- read.csv(
  url,
  header = TRUE,
  na.strings = c("NAN", "         NAN", "   NAN"),
  stringsAsFactors = FALSE
)

# Standardize names so your existing code (unit_price, Rating, Quantity, etc.) works
df <- df %>%
  clean_names() %>%
  rename(
    Branch = branch,
    Customer_type = customer_type,
    Gender = gender,
    Payment = payment,
    Product_line = product_line,
    Quantity = quantity,
    Rating = rating
    # unit_price stays unit_price after clean_names()
  )

# Handle Missing Values: Create a new dataframe removing rows with NA (na.omit)
df_clean <- na.omit(df)

# 2. Display the first few rows of the dataset
print("First few rows of the data:")
head(df_clean)

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
    Mean = mean(Quantity, na.rm = TRUE),
    Median = median(Quantity, na.rm = TRUE),
    Mode = get_mode(Quantity),
    Std_Dev = sd(Quantity, na.rm = TRUE),
    Min = min(Quantity, na.rm = TRUE),
    Max = max(Quantity, na.rm = TRUE)
  )
cat("\nDescriptive Statistics for Quantity:\n")
print(quantity_stats)

cat("\nSummary statistics for ALL variables:\n")
print(summary(df))

cat("\nSummary statistics for NUMERIC variables:\n")
print(df %>% select(where(is.numeric)) %>% summary())

#Identify categorical and numerical features.
numerical_features <- names(df)[sapply(df, is.numeric)]
categorical_features <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]

print("--- Identified Features ---")
print(paste("Numerical Features:", paste(numerical_features, collapse = ", ")))
print(paste("Categorical Features:", paste(categorical_features, collapse = ", ")))

# --- 1. Univariate Analysis ---

ggplot(df, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Customer Ratings", x = "Rating")

ggplot(df, aes(y = unit_price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Unit Price", y = "Unit Price")

ggplot(df, aes(x = Branch, fill = Branch)) +
  geom_bar() +
  labs(title = "Sales Frequency by Branch", y = "Count")

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

numerical_df <- df %>%
  select(where(is.numeric))

ggcorr(numerical_df, label = TRUE)

ggplot(df_clean, aes(x = unit_price, y = Quantity)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Unit Price vs. Quantity", x = "Unit Price", y = "Quantity")

ggplot(df_clean, aes(x = unit_price, y = Rating)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Scatter Plot: Unit Price vs. Rating", x = "Unit Price", y = "Customer Rating")

# FIX: Boxplot needs numeric y, so create Total_Sales and use it
df_clean <- df_clean %>%
  mutate(Total_Sales = unit_price * Quantity)

ggplot(df_clean, aes(x = Customer_type, y = Total_Sales, fill = Customer_type)) +
  geom_boxplot() +
  labs(title = "Boxplot: Total Sales by Customer Type", x = "Customer Type", y = "Total Sales") +
  theme(legend.position = "none")

ggplot(df_clean, aes(x = Gender, y = Rating, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot: Customer Rating Distribution by Gender", x = "Gender", y = "Customer Rating") +
  theme(legend.position = "none")

ggplot(df_clean, aes(x = unit_price, y = Rating)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Unit Price vs. Rating (Check Patterns)", x = "Unit Price", y = "Rating")

ggplot(df_clean, aes(x = unit_price)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Unit Price (Check Skewness)", x = "Unit Price")

ggplot(df_clean, aes(y = Quantity)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Quantity (Check Outliers)", y = "Quantity")

# Data Processing

cat("\nMissing Values (NA Count) per Column:")
print(colSums(is.na(df)))

df_clean <- na.omit(df)
cat("\nTotal NA values after cleaning:")
print(sum(is.na(df_clean)))

# --- Outlier Handling (IQR Method) ---

df_clean <- df_clean[!duplicated(df_clean), ]

df_clean$Quantity <- as.numeric(df_clean$Quantity)
df_clean$unit_price <- as.numeric(df_clean$unit_price)
df_clean$Rating <- as.numeric(df_clean$Rating)

df_clean <- df_clean %>%
  mutate(Total_Sales = unit_price * Quantity)

Q1_q <- quantile(df_clean$Quantity, 0.25, na.rm = TRUE)
Q3_q <- quantile(df_clean$Quantity, 0.75, na.rm = TRUE)
IQR_q <- Q3_q - Q1_q

lower_bound_q <- Q1_q - 1.5 * IQR_q
upper_bound_q <- Q3_q + 1.5 * IQR_q

cat("\nOutlier bounds for Quantity (Lower, Upper): ", lower_bound_q, ", ", upper_bound_q, "\n")

df_final <- df_clean %>%
  filter(Quantity >= lower_bound_q & Quantity <= upper_bound_q)

cat("\nOriginal Rows (Cleaned Data):", nrow(df_clean))
cat("\nRows after Outlier Removal:", nrow(df_final), "\n")

# --- 3. Data Conversion (Label Encoding) ---

# FIX: encode factors properly (as.numeric(as.factor(...)))
df_converted <- df_final %>%
  mutate(
    Branch_Encoded = as.numeric(as.factor(Branch)),
    Customer_type_Encoded = as.numeric(as.factor(Customer_type)),
    Gender_Encoded = as.numeric(as.factor(Gender)),
    Payment_Encoded = as.numeric(as.factor(Payment)),
    `Product.line_Encoded` = as.numeric(as.factor(Product_line))
  )

df_converted <- df_converted %>%
  select(-Branch, -Customer_type, -Gender, -Payment, -Product_line)

cat("\nStructure after Label Encoding:\n")
str(df_converted)

# --- 4. Data Transformation (Normalization and Log Transformation) ---

min_max_normalize <- function(x) {
  x <- as.numeric(x)
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (isTRUE(all.equal(mx, mn))) return(rep(0, length(x)))
  (x - mn) / (mx - mn)
}

df_transformed <- df_converted %>%
  mutate(
    Unit_price_Scaled = min_max_normalize(unit_price),
    Quantity_Scaled = min_max_normalize(Quantity),
    Rating_Scaled = min_max_normalize(Rating),
    Total_Sales_Scaled = min_max_normalize(Total_Sales)
  )

df_transformed <- df_transformed %>%
  mutate(Total_Sales_Log = log(Total_Sales + 1))

# FIX: correct column names to drop (unit_price not Unit_price)
df_transformed <- df_transformed %>%
  select(-unit_price, -Quantity, -Rating, -Total_Sales)

cat("\nStructure after Transformation/Scaling:\n")
str(df_transformed)

# --- 5. Feature Selection ---

df_selected <- df_transformed %>%
  select(
    ends_with("_Encoded"),
    ends_with("_Scaled"),
    Total_Sales_Log
  )

cor_matrix_final <- cor(df_selected, use = "complete.obs")
print("\nFinal Correlation Matrix (for Feature Selection):")
print(round(cor_matrix_final, 2))

df_final_project <- df_selected %>%
  select(-Quantity_Scaled)

variance_check <- sapply(df_final_project, var, na.rm = TRUE)
low_variance_features <- names(variance_check)[variance_check < 0.01]

cat("\nFeatures with Variance < 0.01 (Low Variance Threshold, these should be dropped):\n")
print(low_variance_features)

df_final_project <- df_final_project %>%
  select(-one_of(low_variance_features))

cat("\nStructure of the FINAL Project Data Frame (Ready for Modeling):\n")
str(df_final_project)
head(df_final_project)

# --- 6. Linear Regression Modeling (Continuous Output) ---

set.seed(123)

# Split Data into Training and Testing
trainIndex <- createDataPartition(df_final_project$Total_Sales_Log, p = 0.8, list = FALSE)
trainData <- df_final_project[trainIndex, ]
testData  <- df_final_project[-trainIndex, ]

# Build a Regression Model (Linear Regression)
model_lm <- lm(Total_Sales_Log ~ ., data = trainData)
summary(model_lm)

# Predictions
pred_lm <- predict(model_lm, newdata = testData)
head(pred_lm)

# Model Evaluation (RMSE, MAE, R2)
rmse <- sqrt(mean((pred_lm - testData$Total_Sales_Log)^2))
mae  <- mean(abs(pred_lm - testData$Total_Sales_Log))
r2   <- 1 - sum((pred_lm - testData$Total_Sales_Log)^2) /
  sum((testData$Total_Sales_Log - mean(testData$Total_Sales_Log))^2)

metrics <- c(RMSE = rmse, MAE = mae, R2 = r2)
metrics

# --- 7. Linear Regression Visualization -- ---

# Actual vs Predicted (Test Set)
test_viz <- testData %>%
  mutate(
    Predicted_Total_Sales_Log = pred_lm,
    Residuals = Total_Sales_Log - Predicted_Total_Sales_Log
  )

ggplot(test_viz, aes(x = Predicted_Total_Sales_Log, y = Total_Sales_Log)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Linear Regression: Actual vs Predicted (Test Set)",
    x = "Predicted Total_Sales_Log",
    y = "Actual Total_Sales_Log"
  )