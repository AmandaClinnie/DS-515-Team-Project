# ----------------------------------
# Decision Tree
# ----------------------------------

# quick inspection of data
aug_train %>% head(10)
names(aug_train)
summary(aug_train$target)

# load libraries
library("rpart")
library("rpart.plot")
library("caret")

# set seed for reproducable results
set.seed(515)

# Tree #1 ----------------------------------
# train classification tree model
# predict based on all attributes (excluding target)
tree <- rpart(target ~ city_development_index + gender + relevent_experience + enrolled_university + education_level + major_discipline + experience + company_size + company_type + last_new_job + training_hours,
        method="class",
        data = aug_train,
        parms=list(split='information'))

# get summary of model
summary(tree)

# plot tree
rpart.plot(tree)

# get confusion matrix
pred = predict(tree, type="class")
table(pred)

confusionMatrix(pred, aug_train$target)

# Tree #2 ----------------------------------
# train classification tree model
# predict based on all attributes except city_development_index 
# (excluding target)
tree2 <- rpart(target ~ gender + relevent_experience + enrolled_university + education_level + major_discipline + experience + company_size + company_type + last_new_job + training_hours,
        method="class",
        data = aug_train,
        parms=list(split='information'))

# get summary of model
summary(tree2)

# plot tree
rpart.plot(tree2)

# get confusion matrix
pred2 = predict(tree2, type="class")
table(pred2)

confusionMatrix(pred2, aug_train$target)

# Tree #3 ----------------------------------
# train classification tree model
# predict based on all attributes except city_development_index & company_size
# (excluding target)
tree3 <- rpart(target ~ gender + relevent_experience + enrolled_university + education_level + major_discipline + experience + company_type + last_new_job + training_hours,
        method="class",
        data = aug_train,
        parms=list(split='information'))

# get summary of model
summary(tree3)

# plot tree
rpart.plot(tree3)

# get confusion matrix
pred3 = predict(tree3, type="class")
table(pred3)

confusionMatrix(pred3, aug_train$target)

# Tree #4 ----------------------------------
# train classification tree model
# predict based on only city_development_index attribute (excluding target)
tree4 <- rpart(target ~ city_development_index,
        method="class",
        data = aug_train,
        parms=list(split='information'))

# get summary of model
summary(tree4)

# plot tree
rpart.plot(tree4)

# get confusion matrix
pred4 = predict(tree4, type="class")
table(pred4)

confusionMatrix(pred4, aug_train$target)

str(aug_train)
str(aug_test)

# use decision tree model to predict outcome of test data
predictTest = predict(tree4, newdata=aug_test, type="class")
table(predictTest)

# add target feature to test data dataframe
aug_test$target = predictTest
print(head(aug_test))

# get summary of city_development_index values for both churn and remain
summary(aug_test$city_development_index[aug_test$target == "Will Remain"])
summary(aug_test$city_development_index[aug_test$target == "Will Churn"])

# inspect some feature values for those rows/employees that remain vs churn
table(aug_test$relevent_experience[aug_test$target == "Will Remain"])
table(aug_test$relevent_experience[aug_test$target == "Will Churn"])

table(aug_test$major_discipline[aug_test$target == "Will Remain"])
table(aug_test$major_discipline[aug_test$target == "Will Churn"])

summary(aug_test$training_hours[aug_test$target == "Will Remain"])
summary(aug_test$training_hours[aug_test$target == "Will Churn"])


# Tree #5 ----------------------------------
# train classification tree model
# predict based on only company_size attribute (excluding target)
tree5 <- rpart(target ~ company_size,
        method="class",
        data = aug_train,
        parms=list(split='information'))

# get summary of model
summary(tree5)

# plot tree
rpart.plot(tree5)

# get confusion matrix
pred5 = predict(tree5, type="class")
table(pred5)

confusionMatrix(pred5, aug_train$target)