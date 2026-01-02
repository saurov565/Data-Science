#Get the data and clean NAs
data(penguins)
penguins <- na.omit(penguins)
head(penguins)

#Split Data into Training and Testing
library(caret)
set.seed(123)
trainIndex <- createDataPartition(penguins$species, p = 0.8, list = FALSE)
trainData <- penguins[trainIndex, ]
testData  <- penguins[-trainIndex, ]

#Build a Classification Model (Decision Tree)
library(rpart)
model_dt <- rpart(species ~ ., data = trainData, method = "class")
print(model_dt)

#Predictions
pred_dt <- predict(model_dt, newdata = testData, type = "class")
head(pred_dt)

#Visualization of decision tree model
library(rpart.plot)
rpart.plot(model_dt)

#Model Evaluation
#Confusion Matrix
library(caret)
conf_mat <- confusionMatrix(pred_dt, testData$species)
conf_mat

#Accuracy
accuracy <- conf_mat$overall['Accuracy']
accuracy

#Logistic Regression ( Binary Classification )
penguins_bin <- penguins
penguins_bin$IsAdelie <- ifelse(penguins_bin$species == "Adelie", "Yes", "No")
penguins_bin$IsAdelie <- as.factor(penguins_bin$IsAdelie)

#Training and Testing
set.seed(123)
trainIndex2 <- createDataPartition(penguins_bin$IsAdelie, p = 0.8, list = FALSE)
trainData2  <- penguins_bin[trainIndex2, ]
testData2   <- penguins_bin[-trainIndex2, ]

model_log <- glm(
  IsAdelie ~ bill_length_mm + bill_depth_mm,
  data   = trainData2,
  family = binomial
)

summary(model_log)

#Prediction and Evaluation
prob <- predict(model_log, newdata = testData2, type = "response")
pred_log <- ifelse(prob > 0.5, "Yes", "No")
pred_log <- as.factor(pred_log)

confusionMatrix(pred_log, testData2$IsAdelie)
