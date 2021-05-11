library(Amelia)
library(ggplot2)
library(tidyverse)
library(plyr)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

# PHASE 2: Data Preparation

# Extract Data
aug_train_complete <- read.csv('aug_train.csv', na.strings=c("", "NA"))

# Learning about the Data
str(aug_train_complete)
summary(aug_train_complete)
head(aug_train_complete)

# remove unneccessary variables: employee_id and city 
aug_train = subset(aug_train_complete, select = -c(1,2))

head(aug_train)
summary(aug_train)
str(aug_train)

# Check continuous variables
summary(aug_train$city_development_index)
summary(aug_train$training_hours)

# Data Conditioning

# remove duplicates
unique(aug_train$company_size)  
# Transform company_size 
small_cap = c('<10', '10/49','50-99')
medium_cap = c('100-500','500-999')
large_cap = c('1000-4999','5000-9999','10000+')
all_size = c(small_cap,medium_cap,large_cap)

# Assign elements in company_size to correct cap
for (i in all_size){
        if (i %in% small_cap){
                aug_train$company_size[aug_train$company_size == i] = "Small Cap"}
        else if (i %in% medium_cap){
                aug_train$company_size[aug_train$company_size == i] = "Medium Cap"}
        else if (i %in% large_cap){
                aug_train$company_size[aug_train$company_size == i] = "Large Cap"}}

# Plot transformed variable
ggplot(aug_train, aes(company_size)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)


# remove duplicates
unique(aug_train$experience)
# Transform experience variable
entry_level = c('<1','1','2','3')  # year of experience <1 - 3
mid_level = c('4','5','6','7','8','9') # year of experience 4-9
senior_level = c('10','11','12','13','14','15','16','17','18','19','20','>20') # year of experience 10 to >20
all_level = c(entry_level,mid_level,senior_level) # combine all new variables 
# Assign elements in experience in correct level
for (i in all_level){
    if (i %in% entry_level){
        aug_train$experience[aug_train$experience == i] = "Entry Level"}
    else if (i %in% mid_level){
        aug_train$experience[aug_train$experience == i] = "Mid Level"}
    else if (i %in% senior_level){
        aug_train$experience[aug_train$experience == i] = "Senior Level"}}
# Plot transformed variable
ggplot(aug_train, aes(experience)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)


# remove duplicates
unique(aug_train$major_discipline)

# Transform major_discipline and Assign elements in major_discipline to other_major and label as "Other"
other_major = c('Arts','Business Degree','Humanities','No Major')
for (i in other_major){
        if (i %in% other_major){
                aug_train$major_discipline[aug_train$major_discipline == i] = "Other"}}
# Plot transformed variable
ggplot(aug_train, aes(major_discipline)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)


# Remove duplicates
unique(aug_train$company_type)
# Transform company_type by Assigning elements to other_company_type then label as Other
other_company_type = c("Early Stage Startup","Funded Startup","NGO","Public Sector")
for (i in other_company_type){
        aug_train$company_type[aug_train$company_type == i] = "Other"}
# PLot transformed company_type
ggplot(aug_train, aes(company_type)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)


# Remove duplicates and print out unique values
unique(aug_train$target)
# Provide appropriate name for target labels: 1 as positive label
aug_train$target[aug_train$target == "1"] = "Will Churn"
aug_train$target[aug_train$target == "0"] = "Will Remain"
# Show new unique target labels: "Will Churn" as positive label
unique(aug_train$target)

# Convert character to factor
aug_train$target = as.factor(aug_train$target)


# PHASE 3: Model Planning
# EDA and Data Visualization

# plot distribution of target variable: Higher number of "Will Remain"
ggplot(aug_train, aes(target)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)


# plot missing data: 9% are missing data
missing = is.na(aug_train)
sum(missing)
missmap(aug_train, main = 'Missing Map', col = c('yellow', 'purple'))

# Fill in missing data for gender with "Other"
aug_train$gender = as.character(aug_train$gender)
aug_train$gender = replace_na(aug_train$gender,'Other')

# Check for NAs
sum(is.na(aug_train$gender))
unique(aug_train$gender)

# Fill in missing data for enrolled_university with "Unknown"
aug_train$enrolled_university = replace_na(aug_train$enrolled_university,'Unknown')

# Check for NAs
sum(is.na(aug_train$enrolled_university))
unique(aug_train$enrolled_university)

# Fill in missing data for education_level with "Unknown"
aug_train$education_level = replace_na(aug_train$education_level,'Unknown')
sum(is.na(aug_train$education_level))
unique(aug_train$education_level)

# Fill in missing data for company_size with "Unknown"
aug_train$company_size = replace_na(aug_train$company_size,'Unknown')

# Check for NAs
sum(is.na(aug_train$company_size))
unique(aug_train$company_size)

# Fill in missing data for major_discipline with "Other"
aug_train$major_discipline  = replace_na(aug_train$major_discipline ,'Other')

# Check for NAs
sum(is.na(aug_train$major_discipline ))
unique(aug_train$major_discipline )

# Fill in missing data for company_type with "Other"
aug_train$company_type  = replace_na(aug_train$company_type ,'Other')

# Check for NAs
sum(is.na(aug_train$company_type ))
unique(aug_train$company_type )

# Drop missing values in experience and last_new_job 

aug_train = aug_train[is.na(aug_train$experience) != TRUE, ]
aug_train = aug_train[is.na(aug_train$last_new_job) != TRUE, ]

# Check missing data again : 0 missing data
missmap(aug_train, main = 'Missing Map', col = c('yellow', 'purple'))

# Plot each variables
# plot gender
aug_train$gender[aug_train$target == "1"] = "Will Churn"
# Shows Counts
ggplot(aug_train, aes(gender)) + geom_bar(aes(fill = target)) +
        labs(y= "Count", x= 'Gender')+ 
        geom_text( stat='count', aes(label=..count..), vjust=-1)
# Shows Percentage: "Other" is highest followed by the Female population
ggplot(aug_train, aes(gender)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Gender')


# plot relevant experience
# Shows Counts
ggplot(aug_train, aes(relevent_experience)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)
# SHows Percentage: Higher Churn percentage with population with No Relevant Experience
ggplot(aug_train, aes(relevent_experience)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Relevent Experience')


# plot experience
# Shows COunt
ggplot(aug_train, aes(experience)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)
# Shows Percentage: Higher Churn in Entry Level 
ggplot(aug_train, aes(experience)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Experience')

# plot education and percentage
# Show Counts
ggplot(aug_train, aes(education_level)) + geom_bar(aes(fill = target)) +
        labs(y= "Count", x= 'Education Level')+ geom_text( stat='count', aes(label=..count..), vjust=-1)
# Show percentage: Highest Churn among those with Graduate level
ggplot(aug_train, aes(education_level)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Education Level')

# plot major
# Show Count
ggplot(aug_train, aes(major_discipline)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)
# Show Percentage: "STEM" has higher CHURN percentage
ggplot(aug_train, aes(major_discipline)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Major')


# plot company_type
# Show Count
ggplot(aug_train, aes(company_type)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)
# Show Percentage: "Other" has higher CHURN percentage
ggplot(aug_train, aes(company_type)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Company Type')


# plot city_development_index: index around 0.9 and 0.62 have highest Churn while index at 0.9 will Remain
# Not indicative of CHURN 
ggplot(aug_train, aes(city_development_index, fill = target))+ geom_density(alpha = 0.5)

# plot training_hours: More or less the same ratio. Not very indicative of CHURN
ggplot(aug_train, aes(training_hours, fill = target))+ geom_density(alpha = 0.5)

