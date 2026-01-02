# Libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(janitor)
library(caret)
library(rpart)
library(rpart.plot)
library(factoextra)
library(cluster)

# Custom mode function
get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 1) Load dataset from Google Drive (direct download)
file_id <- "1VVjomrEr8Siw9MmkUJSoXy13ZxappKoa"
url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

df <- read.csv(
  url,
  header = TRUE,
  na.strings = c("NAN", "         NAN", "   NAN", "", "NA", "NaN"),
  stringsAsFactors = FALSE
)

# Standardize names (match your previous style)
df <- df %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ trimws(.))) %>%
  rename(
    ID = id,
    Gender = gender,
    Age = age,
    Hypertension = hypertension,
    Heart_disease = heart_disease,
    Ever_married = ever_married,
    Work_type = work_type,
    Residence_type = residence_type,
    Avg_glucose_level = avg_glucose_level,
    BMI = bmi,
    Smoking_status = smoking_status,
    Stroke = stroke
  )

# 2) Basic checks
cat("\nFirst few rows:\n")
print(head(df))

cat("\nShape (Rows, Columns):\n")
print(dim(df))

cat("\nStructure:\n")
str(df)

cat("\nSummary (all variables):\n")
print(summary(df))

cat("\nMissing Values (NA Count) per Column:\n")
print(colSums(is.na(df)))

# 3) Handle missing values (BMI has NA in this dataset) -> median imputation
bmi_median <- median(df$BMI, na.rm = TRUE)
df_clean <- df %>%
  mutate(BMI = ifelse(is.na(BMI), bmi_median, BMI))

cat("\nTotal NA values after BMI imputation:\n")
print(sum(is.na(df_clean)))

# 4) Identify feature types
numerical_features <- names(df_clean)[sapply(df_clean, is.numeric)]
categorical_features <- names(df_clean)[sapply(df_clean, is.character) | sapply(df_clean, is.factor)]

cat("\n--- Identified Features ---\n")
print(paste("Numerical Features:", paste(numerical_features, collapse = ", ")))
print(paste("Categorical Features:", paste(categorical_features, collapse = ", ")))

# 5) Descriptive stats (Age + BMI + Avg_glucose_level)
desc_stats <- df_clean %>%
  summarise(
    n = n(),
    age_mean = mean(Age, na.rm = TRUE),
    age_median = median(Age, na.rm = TRUE),
    age_sd = sd(Age, na.rm = TRUE),
    bmi_mean = mean(BMI, na.rm = TRUE),
    bmi_median = median(BMI, na.rm = TRUE),
    bmi_sd = sd(BMI, na.rm = TRUE),
    glucose_mean = mean(Avg_glucose_level, na.rm = TRUE),
    glucose_median = median(Avg_glucose_level, na.rm = TRUE),
    glucose_sd = sd(Avg_glucose_level, na.rm = TRUE)
  )

cat("\nDescriptive Statistics:\n")
print(desc_stats)

# 6) Univariate Analysis (plots)
ggplot(df_clean, aes(x = factor(Stroke))) +
  geom_bar(fill="skyblue", color="black") +
  labs(title = "Stroke Class Distribution", x = "Stroke (0/1)", y = "Count")

ggplot(df_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill="skyblue", color="black") +
  labs(title = "Distribution of Age", x = "Age", y = "Count")

ggplot(df_clean, aes(x = Avg_glucose_level)) +
  geom_histogram(binwidth = 10, fill="skyblue", color="black") +
  labs(title = "Distribution of Avg Glucose Level", x = "Avg Glucose Level", y = "Count")

ggplot(df_clean, aes(y = BMI)) +
  geom_boxplot() +
  labs(title = "Boxplot of BMI", y = "BMI")

ggplot(df_clean, aes(x = Smoking_status)) +
  geom_bar(fill="red",color="black") +
  labs(title = "Smoking Status Counts", x = "Smoking Status", y = "Count") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# 7) Bivariate Analysis
numeric_df <- df_clean %>%
  select(where(is.numeric)) %>%
  select(-ID)

ggcorr(numeric_df, label = TRUE)

ggplot(df_clean, aes(x = factor(Stroke), y = Age)) +
  geom_boxplot(fill="skyblue",color="black") +
  labs(title = "Age by Stroke", x = "Stroke (0/1)", y = "Age")

ggplot(df_clean, aes(x = factor(Stroke), y = Avg_glucose_level)) +
  geom_boxplot(fill="skyblue",color="black") +
  labs(title = "Avg Glucose Level by Stroke", x = "Stroke (0/1)", y = "Avg Glucose Level")

ggplot(df_clean, aes(x = factor(Stroke), y = BMI)) +
  geom_boxplot() +
  labs(title = "BMI by Stroke", x = "Stroke (0/1)", y = "BMI")

# 8) Outlier capping (IQR) instead of dropping rows
# Make sure df_clean has the expected column names
if ("BMI" %in% names(df_clean) && !("bmi" %in% names(df_clean))) {
  df_clean <- df_clean %>% rename(bmi = BMI)
}
if ("Avg_glucose_level" %in% names(df_clean) && !("avg_glucose_level" %in% names(df_clean))) {
  df_clean <- df_clean %>% rename(avg_glucose_level = Avg_glucose_level)
}
# Now cap outliers safely
df_clean <- df_clean %>%
  mutate(
    bmi = cap_iqr(bmi),
    avg_glucose_level = cap_iqr(avg_glucose_level)
  )

# 9) Prepare data for K-means clustering
#IMPORTANT: drop id + stroke, one-hot encode categorical vars
df_kmeans <- df_clean %>%
  select(-id, -stroke) %>%
  mutate(across(where(is.character), as.factor))

# One-hot encode
x_mat <- model.matrix(~ . - 1, data = df_kmeans)

# Remove near-zero variance features (helps clustering)
nzv <- nearZeroVar(x_mat)
if (length(nzv) > 0) {
  x_mat <- x_mat[, -nzv, drop = FALSE]
}

# Scale (recommended for K-means)
x_scaled <- scale(x_mat)

cat("\nMatrix for clustering:\n")
print(dim(x_scaled))

# 10) K-means + Evaluation

set.seed(123)
k <- 3
km_res <- kmeans(x_scaled, centers = k, nstart = 25)
print(km_res)

# Cluster assignment merged back to cleaned df
df_clustered <- df_clean %>%
  mutate(Cluster = as.factor(km_res$cluster))

cat("\nHead with Cluster:\n")
print(head(df_clustered))

# Silhouette score (use sample if dataset is large to avoid memory issues)
set.seed(123)
n <- nrow(x_scaled)
idx <- if (n > 2500) sample(1:n, 2500) else 1:n

sil <- silhouette(km_res$cluster[idx], dist(x_scaled[idx, ]))
sil_score <- mean(sil[, 3])
cat("\nSilhouette Score (mean):", sil_score, "\n")

# Visualization (PCA-based)
fviz_cluster(km_res, data = x_scaled, ggtheme = theme_minimal())
