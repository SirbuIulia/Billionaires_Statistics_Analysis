install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("rpart.plot")
install.packages("rsample")
library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(rsample) 
library(rpart) 
library(rpart.plot)
library(caret)
library(randomForest)
library(tree) 
library(ipred)
library(pROC)

data <- read_csv("C:/Users/IULIA SÎRBU/Downloads/Output_ProiectBD.csv")
head(data)

ggplot(data, aes(x = age, y = finalWorth)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Final Worth vs Age", x = "Age", y = "Final Worth")

ggplot(data, aes(x = industries, y = finalWorth)) + 
  geom_point() + 
  labs(title = "Final Worth vs Industries", x = "Industries", y = "Final Worth") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggplot(data, aes(x = gross_tertiary_education_enrollment, y = finalWorth)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Final Worth vs Tertiary Education Enrollment", x = "Gross Tertiary Education Enrollment", y = "Final Worth")

ggplot(data, aes(x = gross_primary_education_enrollment_country, y = finalWorth)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Final Worth vs Primary Education Enrollment", x = "Gross Primary Education Enrollment Country", y = "Final Worth")

ggplot(data, aes(x = gender, y = finalWorth)) + 
  geom_point() + 
  labs(title = "Final Worth vs Gender", x = "Gender", y = "Final Worth")

data <- data %>% mutate_if(is.character, as.factor)
data$finalWorth <- as.numeric(data$finalWorth)
data$finalWorth_binary <- ifelse(data$finalWorth > median(data$finalWorth, na.rm = TRUE), 1, 0)

mod_age <- glm(finalWorth_binary ~ age, data, family = binomial)
summary(mod_age)

mod_gender <- glm(finalWorth_binary ~ gender, data, family = binomial)
summary(mod_gender)

mod_industries <- glm(finalWorth_binary ~ industries, data, family = binomial)
summary(mod_industries)

mod_gender_industries <- glm(finalWorth_binary ~ gender + industries, data, family = binomial)
summary(mod_gender_industries)

set.seed(123) 
split <- initial_split(data, prop = 0.7, strata = "finalWorth")

train <- training(split) 
test <- testing(split) 

table(train$ finalWorth)
table(test$ finalWorth) 

model <- glm(finalWorth_binary ~ gender + industries + age + gross_tertiary_education_enrollment, 
             data = train_data, family = binomial)

pred_test <- predict(model, newdata = test, type = "response")

pred_test_binary <- ifelse(pred_test > 0.5, 1, 0)
confusion_matrix <- table(Predicted = pred_test_binary, Actual = test$finalWorth_binary)
print(confusion_matrix)

train$finalWorth_binary <- as.factor(train$finalWorth_binary)
test$finalWorth_binary <- as.factor(test$finalWorth_binary)

#Cross validation
train_control <- trainControl(method = "cv", number = 10)

model_cv <- train(finalWorth_binary ~ gender + industries + age + gross_tertiary_education_enrollment,
                  data = train,
                  method = "glm",
                  family = binomial,
                  trControl = train_control)

print(model_cv)

pred_test_cv <- predict(model_cv, newdata = test)

conf_matrix <- confusionMatrix(pred_test_cv, test$finalWorth_binary)
print(conf_matrix)

pred_probs <- predict(model_cv, newdata = test, type = "prob")[,2]

roc_curve <- roc(test$finalWorth_binary, pred_probs)
auc_value <- auc(roc_curve)

plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "red")

print(paste("AUC:", auc_value))

