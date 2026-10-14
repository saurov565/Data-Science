# =====================================================================
#  NETFLIX CUSTOMER CHURN & ENGAGEMENT ANALYTICS
#  Load -> Overview -> Univariate -> Bivariate -> Data Processing
#  -> Feature Selection -> Model Evaluation (75% TRAIN / 25% TEST)
#  Models: Decision Tree + Random Forest (no scaling needed)
# =====================================================================

# ---- Packages (installed automatically if missing) ----
required_pkgs <- c("dplyr", "tidyr", "ggplot2", "GGally", "caret", "e1071",
                   "rpart", "rpart.plot", "randomForest", "pROC")
new_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(caret)
library(rpart)
library(rpart.plot)

# ---- CONFIG (the only part you may need to edit) ----
DRIVE_FILE_ID  <- "1P_MVt2TYvUtw6gVw-q27XG7Af9FOw0zN"   # only the ID, not the full link
LOCAL_FALLBACK <- "netflix_customer_churn.csv"           # backup file (optional)
NA_STRINGS     <- c("NAN", "NaN", "NA", "")

TARGET_COL   <- "churned"   # churn column name (lower-case, spaces -> underscores)
CHURN_VALUES <- c("yes", "y", "true", "1", "churned", "churn", "left")  # values meaning "churned"
ID_COLS      <- c("customer_id", "user_id", "id", "subscriber_id")      # dropped if present

TRAIN_PROP       <- 0.75   # 75% training, 25% testing
SEED             <- 123
OUTLIER_IQR_MULT <- 3      # 1.5 = strict, 3 = extreme outliers only
VAR_THRESHOLD    <- 0.001  # features with lower variance are dropped (raw scale)
CORR_CUTOFF      <- 0.90   # one of two features correlated above this is dropped

# Define a custom function for mode (this must be run first!)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Simple skewness helper (used in the skewness check)
skewness <- function(x) {
  s <- sd(x)
  if (is.na(s) || s == 0) return(0)
  mean(((x - mean(x)) / s)^3)
}


# 1. Load the dataset into R from Google Drive
#    (file must be shared as "Anyone with the link - Viewer")
load_dataset <- function() {
  url <- paste0("https://drive.google.com/uc?export=download&confirm=t&id=", DRIVE_FILE_ID)
  tmp <- tempfile(fileext = ".csv")
  
  try(download.file(url, tmp, mode = "wb", quiet = TRUE), silent = TRUE)
  
  # If Drive gave a web page instead of a CSV, use the local file
  is_html <- !file.exists(tmp) || file.size(tmp) == 0 ||
    grepl("<html|<!DOCTYPE", readLines(tmp, n = 1, warn = FALSE), ignore.case = TRUE)
  path <- if (!is_html) tmp else LOCAL_FALLBACK
  
  if (!file.exists(path)) stop("Download failed. Share the Drive file as 'Anyone with the link - Viewer'.")
  read.csv(path, na.strings = NA_STRINGS, strip.white = TRUE)
}

df <- load_dataset()

# Standardise column names: "Watch Hours" / "Watch.Hours" -> "watch_hours"
names(df) <- gsub("^_|_$", "", tolower(gsub("[^A-Za-z0-9]+", "_", names(df))))
cat("\nColumns found in your file:\n")
print(names(df))
if (!TARGET_COL %in% names(df)) {
  stop("TARGET_COL '", TARGET_COL, "' not found. Choose one of: ", paste(names(df), collapse = ", "))
}

# Prepare the target: 2-level factor with "Churned" FIRST (= positive class)
df <- df[!is.na(df[[TARGET_COL]]), ]
df$churn <- factor(ifelse(tolower(trimws(as.character(df[[TARGET_COL]]))) %in% CHURN_VALUES,
                          "Churned", "Retained"),
                   levels = c("Churned", "Retained"))

# IMPORTANT: always drop the ORIGINAL target column, otherwise it leaks into the
# predictors and the models get a perfect (fake) score.
if (TARGET_COL != "churn") df[[TARGET_COL]] <- NULL

# Drop ID columns and constant columns
df <- df[, !(names(df) %in% ID_COLS), drop = FALSE]
const_cols <- names(df)[sapply(df, function(x) length(unique(na.omit(x))) <= 1)]
if (length(const_cols) > 0) {
  cat("Dropping constant columns:", const_cols, "\n")
  df <- df[, !(names(df) %in% const_cols), drop = FALSE]
}

# Text columns -> factors (needed for label encoding later)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df[sapply(df, is.logical)]   <- lapply(df[sapply(df, is.logical)],   as.factor)

cat("\nTarget distribution (CHECK that this looks right!):\n")
print(table(df$churn))
print(round(prop.table(table(df$churn)), 3))

# Handle Missing Values: Create a new dataframe removing rows with NA (na.omit)
df_clean <- na.omit(df)


# 2. Display the first few rows of the dataset
print("First few rows of the data:")
head(df)

# 3. Show shape (rows x columns)
cat("\nShape of the dataset (Rows, Columns):")
print(dim(df))

# 4. Display data types of each column (Structure of the data frame)
cat("\nStructure of the dataset:")
str(df)


# Identify categorical and numerical features.
numerical_features <- names(df)[sapply(df, is.numeric)]
categorical_features <- setdiff(names(df)[sapply(df, is.character) | sapply(df, is.factor)], "churn")

print("--- Identified Features ---")
print(paste("Numerical Features:", paste(numerical_features, collapse = ", ")))
print(paste("Categorical Features:", paste(categorical_features, collapse = ", ")))


# 5. Create a summary table for every numerical feature
num_stats <- data.frame(
  Feature = numerical_features,
  Count   = sapply(df_clean[numerical_features], length),
  Mean    = sapply(df_clean[numerical_features], mean),
  Median  = sapply(df_clean[numerical_features], median),
  Mode    = sapply(df_clean[numerical_features], get_mode),
  Std_Dev = sapply(df_clean[numerical_features], sd),
  Min     = sapply(df_clean[numerical_features], min),
  Max     = sapply(df_clean[numerical_features], max),
  row.names = NULL
)
cat("\nDescriptive Statistics for numerical features:\n")
print(num_stats %>% mutate(across(where(is.numeric), ~ round(.x, 3))))


# --- 1. Univariate Analysis ---

# **Churn vs Retained (target distribution)**
churn_dist <- df_clean %>% count(churn) %>% mutate(pct = n / sum(n))
ggplot(churn_dist, aes(x = churn, y = n, fill = churn)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", round(100 * pct, 1), "%)")), vjust = -0.5) +
  scale_fill_manual(values = c("Churned" = "#E50914", "Retained" = "#4C566A")) +
  labs(title = "Churn vs Retained Customers", x = NULL, y = "Count")

# **Distribution Plots for Numerical Variables**
num_long <- df_clean %>%
  select(all_of(numerical_features)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value")

ggplot(num_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Distribution of Numerical Features", x = NULL, y = "Count")

# Boxplots (useful for detecting outliers)
ggplot(num_long, aes(y = value)) +
  geom_boxplot(fill = "tomato") +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Boxplots of Numerical Features (Check Outliers)", y = NULL)

# **Frequency of Categorical Variables**
for (col in categorical_features) {
  cat("\nFrequency of Customers by", col, ":\n")
  print(df_clean %>% group_by(.data[[col]]) %>% summarise(Count = n(), .groups = "drop"))
}

# Bar charts for categorical features (skips features with more than 12 levels)
plot_cats <- categorical_features[sapply(df_clean[categorical_features], nlevels) <= 12]
if (length(plot_cats) > 0) {
  cat_long <- df_clean %>%
    select(all_of(plot_cats)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "level")
  
  ggplot(cat_long, aes(x = level, fill = level)) +
    geom_bar(show.legend = FALSE) +
    facet_wrap(~feature, scales = "free_x") +
    labs(title = "Frequency of Categorical Features", x = NULL, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# --- 2. Bivariate Analysis ---

# **Correlation Matrix (Heatmap) for Numerical Features (churn added as 0/1)**
numerical_df <- df_clean %>%
  mutate(churn_flag = as.integer(churn == "Churned")) %>%
  select(all_of(numerical_features), churn_flag)
ggcorr(numerical_df, label = TRUE, label_round = 2)

cat("\nCorrelation of each numerical feature with churn:\n")
cor_with_churn <- cor(numerical_df)[, "churn_flag"]
print(round(sort(cor_with_churn[names(cor_with_churn) != "churn_flag"], decreasing = TRUE), 3))

# Scatter Plot for the two numeric features most correlated with churn ---
top2 <- names(sort(abs(cor_with_churn[numerical_features]), decreasing = TRUE))[1:2]
ggplot(df_clean, aes(x = .data[[top2[1]]], y = .data[[top2[2]]], color = churn)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Churned" = "#E50914", "Retained" = "#4C566A")) +
  labs(title = paste("Scatter Plot:", top2[1], "vs.", top2[2]), x = top2[1], y = top2[2])

# Boxplots: Churn Status vs. Numerical Features ---
box_long <- df_clean %>%
  select(churn, all_of(numerical_features)) %>%
  pivot_longer(-churn, names_to = "feature", values_to = "value")

ggplot(box_long, aes(x = churn, y = value, fill = churn)) +
  geom_boxplot() +
  facet_wrap(~feature, scales = "free_y") +
  scale_fill_manual(values = c("Churned" = "#E50914", "Retained" = "#4C566A")) +
  labs(title = "Boxplot: Numerical Features by Churn Status", x = NULL, y = NULL) +
  theme(legend.position = "none")

# Churn RATE by category (bars above the dashed line churn MORE than average) ---
if (length(plot_cats) > 0) {
  overall_rate <- mean(df_clean$churn == "Churned")
  cat_rate <- df_clean %>%
    select(churn, all_of(plot_cats)) %>%
    mutate(across(-churn, as.character)) %>%
    pivot_longer(-churn, names_to = "feature", values_to = "level") %>%
    group_by(feature, level) %>%
    summarise(Count = n(), churn_rate = mean(churn == "Churned"), .groups = "drop")
  
  ggplot(cat_rate, aes(x = level, y = churn_rate)) +
    geom_col(fill = "#E50914", alpha = 0.85) +
    geom_hline(yintercept = overall_rate, linetype = "dashed") +
    facet_wrap(~feature, scales = "free_x") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Churn Rate by Customer Segment",
         subtitle = paste0("Dashed line = overall churn rate (", round(100 * overall_rate, 1), "%)"),
         x = NULL, y = "Churn rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# 3. --Identify Skewness (Histogram of the most skewed numerical feature) ---
skew_values <- sapply(df_clean[numerical_features], skewness)
cat("\nSkewness of numerical features:\n")
print(round(sort(skew_values, decreasing = TRUE), 2))

most_skewed <- names(which.max(abs(skew_values)))
ggplot(df_clean, aes(x = .data[[most_skewed]])) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = paste("Distribution of", most_skewed, "(Check Skewness)"), x = most_skewed)


# Data Processing

# --- 1. Handling Missing Values ---
cat("\nMissing Values (NA Count) per Column:")
print(colSums(is.na(df)))

# For simplicity, we remove rows with NA values
df_clean <- na.omit(df)
cat("\nTotal NA values after cleaning:")
print(sum(is.na(df_clean)))

# --- 2. Outlier Handling (Using Interquartile Range - IQR Method) ---

df_clean <- df_clean[!duplicated(df_clean), ]
for (col in numerical_features) df_clean[[col]] <- as.numeric(df_clean[[col]])

keep_rows <- rep(TRUE, nrow(df_clean))
for (col in numerical_features) {
  Q1_q  <- quantile(df_clean[[col]], 0.25, na.rm = TRUE)
  Q3_q  <- quantile(df_clean[[col]], 0.75, na.rm = TRUE)
  IQR_q <- Q3_q - Q1_q
  if (IQR_q == 0) next                       # skip flags / zero-inflated columns
  
  lower_bound_q <- Q1_q - OUTLIER_IQR_MULT * IQR_q
  upper_bound_q <- Q3_q + OUTLIER_IQR_MULT * IQR_q
  cat("\nOutlier bounds for", col, "(Lower, Upper): ", lower_bound_q, ", ", upper_bound_q)
  keep_rows <- keep_rows & df_clean[[col]] >= lower_bound_q & df_clean[[col]] <= upper_bound_q
}

df_final <- df_clean[keep_rows, ]

cat("\n\nOriginal Rows (Cleaned Data):", nrow(df_clean))
cat("\nRows after Outlier Removal:", nrow(df_final), "\n")
print(table(df_final$churn))   # make sure churners were not removed disproportionately


# --- 3. Data Conversion (Label Encoding) ---

# Convert R factor types into integer codes (1, 2, 3...) for each categorical feature,
# and encode the target: Churned = 1, Retained = 0
cat_cols <- setdiff(names(df_final)[sapply(df_final, is.factor)], "churn")

df_converted <- df_final
for (col in cat_cols) {
  df_converted[[paste0(col, "_Encoded")]] <- as.numeric(df_converted[[col]])
}
df_converted$churn_Encoded <- ifelse(df_converted$churn == "Churned", 1, 0)

# Remove the original categorical text columns
df_converted <- df_converted[, !(names(df_converted) %in% c(cat_cols, "churn")), drop = FALSE]

cat("\nStructure after Label Encoding:\n")
str(df_converted)


# --- 4. Data Transformation ---
# Decision Trees and Random Forests split on thresholds, so they are not affected
# by feature scale or skew. The features are used on their original scale, which
# also keeps the tree splits and importance results easy to read.
df_transformed <- df_converted


# --- 5. Feature Selection ---

# 1. Feature Selection based on Correlation Analysis
# All columns are numeric at this point (raw numeric features + encoded categoricals)
df_selected <- df_transformed %>% select(where(is.numeric))

# Remove constant columns (they make correlation undefined)
df_selected <- df_selected[, sapply(df_selected, function(x) var(x) > 0), drop = FALSE]

# Calculate and display the final correlation matrix
cor_matrix_final <- cor(df_selected)
print("Final Correlation Matrix (for Feature Selection):")
print(round(cor_matrix_final, 2))

# DECISION BASED ON CORRELATION:
# If two features are highly correlated (cor > CORR_CUTOFF), drop one of them.
predictor_names <- setdiff(names(df_selected), "churn_Encoded")
high_cor_features <- findCorrelation(cor(df_selected[predictor_names]),
                                     cutoff = CORR_CUTOFF, names = TRUE)
cat("\nHighly correlated features dropped (cor >", CORR_CUTOFF, "):\n")
print(high_cor_features)

df_final_project <- df_selected %>%
  select(-any_of(high_cor_features))

# 2. Variance Thresholding (Remove features with near-zero variance)
variance_check <- sapply(df_final_project, var)
low_variance_features <- setdiff(names(variance_check)[variance_check < VAR_THRESHOLD], "churn_Encoded")

cat("\nFeatures with Variance <", VAR_THRESHOLD, "(Low Variance Threshold, these should be dropped):\n")
print(low_variance_features)

df_final_project <- df_final_project %>%
  select(-any_of(low_variance_features))

cat("\nStructure of the FINAL Project Data Frame (Ready for Modeling):\n")
str(df_final_project)
head(df_final_project)


# --- 6. Model Evaluation ---   (75% TRAIN / 25% TEST)

set.seed(SEED)
target_col <- "churn_Encoded"

# Prepare Data: Convert target to factor BEFORE splitting (Churned = positive class)
model_df <- df_final_project
model_df[[target_col]] <- factor(model_df[[target_col]], levels = c(1, 0),
                                 labels = c("Churned", "Retained"))

# Train/Test Split (75% training, 25% testing, stratified by churn)
trainIndex <- createDataPartition(model_df[[target_col]], p = TRAIN_PROP, list = FALSE)
trainData  <- model_df[trainIndex, ]
testData   <- model_df[-trainIndex, ]

cat("\n--- Data split ---\n")
cat("Training rows:", nrow(trainData), sprintf("(%.0f%%)", 100 * nrow(trainData) / nrow(model_df)), "\n")
cat("Testing rows :", nrow(testData),  sprintf("(%.0f%%)", 100 * nrow(testData)  / nrow(model_df)), "\n")

form <- as.formula(paste(target_col, "~ ."))

# Helper: Confusion Matrix, Accuracy, Precision, Recall, F1, ROC-AUC + heatmap for ONE model
evaluate_model <- function(model_name, pred_class, pred_prob) {
  
  pred_class <- factor(pred_class, levels = levels(testData[[target_col]]))
  
  conf_mat <- confusionMatrix(pred_class, testData[[target_col]], positive = "Churned")
  cat("\n=====", model_name, "=====\n")
  print(conf_mat)
  
  accuracy  <- conf_mat$overall["Accuracy"]
  precision <- conf_mat$byClass["Pos Pred Value"]
  recall    <- conf_mat$byClass["Sensitivity"]
  f1        <- conf_mat$byClass["F1"]
  
  roc_obj <- pROC::roc(testData[[target_col]], pred_prob,
                       levels = c("Retained", "Churned"), direction = "<", quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  print(paste("Accuracy: ", round(accuracy, 4)))
  print(paste("Precision:", round(precision, 4)))
  print(paste("Recall:   ", round(recall, 4)))
  print(paste("F1 Score: ", round(f1, 4)))
  print(paste("ROC-AUC:  ", round(auc_val, 4)))
  
  cm_df <- as.data.frame(conf_mat$table)
  print(
    ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Freq), size = 6, color = "black") +
      scale_fill_gradient(low = "#e7f0fa", high = "#2c7bb6") +
      theme_minimal() +
      labs(
        title = paste("Confusion Matrix Heatmap -", model_name),
        x = "Actual Class",
        y = "Predicted Class",
        fill = "Frequency"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  )
  
  list(
    row = data.frame(Model = model_name, Accuracy = unname(accuracy), Precision = unname(precision),
                     Recall = unname(recall), F1_Score = unname(f1), ROC_AUC = auc_val),
    roc = roc_obj
  )
}

# ---- Model 1: Decision Tree ----
model_dt <- rpart(form, data = trainData, method = "class")

cat("\n===== DECISION TREE MODEL =====\n")
print(model_dt)                                   # the tree rules as text
cat("\nVariable importance (Decision Tree):\n")
print(round(sort(model_dt$variable.importance, decreasing = TRUE), 2))

rpart.plot(model_dt, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree Visualization")

pred_dt <- predict(model_dt, newdata = testData, type = "class")
prob_dt <- predict(model_dt, newdata = testData, type = "prob")[, "Churned"]
res_dt  <- evaluate_model("Decision Tree", pred_dt, prob_dt)

# ---- Model 2: Random Forest ----
set.seed(SEED)
model_rf <- randomForest::randomForest(form, data = trainData, ntree = 500, importance = TRUE)

cat("\n===== RANDOM FOREST MODEL =====\n")
print(model_rf)                                   # OOB error + training confusion matrix
cat("\nVariable importance (Random Forest):\n")
print(round(randomForest::importance(model_rf)[order(-randomForest::importance(model_rf)[, "MeanDecreaseGini"]), ], 3))

randomForest::varImpPlot(model_rf, main = "Random Forest - Variable Importance")
plot(model_rf, main = "Random Forest - Error vs Number of Trees")
legend("topright", legend = colnames(model_rf$err.rate), col = 1:3, lty = 1:3, cex = 0.8)

pred_rf <- predict(model_rf, newdata = testData, type = "class")
prob_rf <- predict(model_rf, newdata = testData, type = "prob")[, "Churned"]
res_rf  <- evaluate_model("Random Forest", pred_rf, prob_rf)

# ---- Model Comparison (test set, 25% hold-out) ----
results <- bind_rows(res_dt$row, res_rf$row) %>% arrange(desc(ROC_AUC))
cat("\n--- TEST-SET PERFORMANCE (positive class = Churned) ---\n")
print(results %>% mutate(across(where(is.numeric), ~ round(.x, 4))))

# ROC curves (both models on one chart)
roc_list <- list(res_dt$roc, res_rf$roc)
names(roc_list) <- paste0(c("Decision Tree", "Random Forest"),
                          " (AUC = ", round(c(res_dt$row$ROC_AUC, res_rf$row$ROC_AUC), 3), ")")

pROC::ggroc(roc_list, legacy.axes = TRUE, linewidth = 1) +
  geom_abline(linetype = "dashed", color = "grey50") +
  coord_equal() +
  theme_minimal() +
  labs(title = "ROC Curves on the Test Set",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Recall)", color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "bottom", legend.direction = "vertical")

cat("\nBest model:", results$Model[1], "| ROC-AUC =", round(results$ROC_AUC[1], 4), "\n")