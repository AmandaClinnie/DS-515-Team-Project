# ----------------------------------
# Decision Tree
# ----------------------------------

# quick inspection of data
aug_train %>% head(10)
names(aug_train)
summary(aug_train)

# load libraries
library("rpart")
library("rpart.plot")

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