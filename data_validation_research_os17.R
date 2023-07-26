#Gender
surveydata = read.csv("/Users/aadharshhariharan/Desktop/Research Project/Documents and resources/survey_responses.csv")
data$gendernum <- ""
data$gendernum[data$GENDER == "Man"] <- 1
>ata$gendernum[data$GENDER == "Woman"] <- 2
data$gendernum[data$GENDER == "Non-binary or Other"] <- 3
a <- table(data$GENDER)
mysurvey = surveydata
mysurvey$gendernum[mysurvey$gender == "Male"] <- 1
mysurvey$gendernum[mysurvey$gender == "Female"] <- 2
mysurvey$gendernum[mysurvey$gender == "Other"] <- 3
library(tidyr)
mysurvey = mysurvey %>% drop_na(gender, gendernum)
F3 = c(sum(theirsurvey$gender==1), sum(theirsurvey$gender==2), sum(theirsurvey$gender==3))
library("stringr")
my = c(sum(str_count(surveydata$gender, "Male")),
         +        sum(str_count(surveydata$gender, "Female")),
         +        sum(str_count(surveydata$gender, "Other")))
gender.compare = as.data.frame(rbind(F3, my))
names(gender.compare) = c('Male', 'Female', 'Other')
gender.compare
    Male Female Other
F3 3387    125     0
my   75      4     7
chisq.test(gender.compare)

Pearsons Chi-squared test

data:  gender.compare
X-squared = 286.88, df = 2, p-value < 2.2e-16

#Education
a<- table(data$FORMAL.EDUCATION)
data$education <- ""
data$education[data$FORMAL.EDUCATION == "Less than secondary (high) school"] <- "P"
data$education[data$FORMAL.EDUCATION == "Secondary (high) school graduate or equivalent"] <- "H"
## Incomplete university as high school
data$education[data$FORMAL.EDUCATION == "Some college, no degree"] <- "H"
data$education[data$FORMAL.EDUCATION == "Vocational/trade program or apprenticeship"] <- "T"
## Merge three university levels
data$education[data$FORMAL.EDUCATION == "Bachelor's degree"] <- "U"
data$education[data$FORMAL.EDUCATION == "Master's degree"] <- "U"
data$education[data$FORMAL.EDUCATION == "Doctorate (Ph.D.) or other advanced degree (e.g. M.D., J.D.)"]
library("stringr")
FLOSS2013 = data
F3=c(sum(str_count(FLOSS2013$education, "P")),
       +      sum(str_count(FLOSS2013$education, "H")),
       +      sum(str_count(FLOSS2013$education, "T")),
       +      sum(str_count(FLOSS2013$education, "U")))
my=c(sum(str_count(surveydata$education, "P")),
       +      sum(str_count(surveydata$education, "H")),
       +      sum(str_count(surveydata$education, "T")),
       +      sum(str_count(surveydata$education, "U")))
education.compare = as.data.frame(rbind(F3, my))
names(education.compare) = c('Primary', 'High School', 'Trade School', 'University')
education.compare
Primary High School Trade School University
F3     126        1015          127       2173
my       0           5            8         73
chisq.test(education.compare)

Pearsons Chi-squared test

data:  education.compare
X-squared = 32.548, df = 3, p-value = 4.011e-07