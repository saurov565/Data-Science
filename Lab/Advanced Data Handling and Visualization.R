#Dataset Description
data("mtcars")
head(mtcars)

#Advanced Visualization with ggplot2
#Trend Analysis: Horsepower vs. Miles per Gallon
library(ggplot2) 
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "HP vs MPG", x = "Horsepower", y = "Miles per Gallon")

# Detecting Outliers with Boxplot
ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Miles per Gallon", y = "MPG")

#Correlation Heatmap
install.packages("GGally")
library(GGally)
ggcorr(mtcars, label = TRUE)

#Data Cleaning
#Remove Missing Values (if any)

# --- Handling Missing Values ---
# Check how many NA values are in each column
colSums(is.na(mtcars))

# Create a new dataframe with rows containing NA values removed
mtcars_clean <- na.omit(mtcars)
# Verify that there are no more NA values
cat("Total NA values after cleaning:", sum(is.na(mtcars_clean)), "\n")

#Remove Duplicate Rows
mtcars_clean <- mtcars_clean[!duplicated(mtcars_clean), ]

#Data Wrangling with dplyr
#Filter cars with mpg > 20
install.packages("dplyr")      
library(dplyr)
mtcars_filtered <- mtcars_clean %>% filter(mpg > 20)

#Select specific columns
mtcars_selected <- mtcars_filtered %>% select(mpg, hp, wt)

#Create a new variable: Power-to-Weight Ratio
mtcars_mutated <- mtcars_selected %>%
  mutate(power_to_weight = hp / wt)

#Normalization and Scaling
#Normalize mpg, hp, wt
mtcars_scaled <- mtcars_selected %>%
  mutate(across(c(mpg, hp, wt), ~ scale(.)[,1]))
head(mtcars_scaled)

