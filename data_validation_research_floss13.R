source("survey_68745_R_syntax_file.R", encoding = "UTF-8")
surveydata = read.csv("/Users/aadharshhariharan/Desktop/Research Project/Documents and resources/survey_responses.csv")

#GENDER
FLOSS2013male = data[data[10] == "Male",]
FLOSS2013male$gendernum <- 1
FLOSS2013female = data[data[10] == "Female",]
FLOSS2013female$gendernum <- 2
FLOSS2013other = data[!is.na(data[11] == "TRUE"),]
FLOSS2013other$gendernum <- 3
FLOSS2013gender <- rbind(FLOSS2013male, FLOSS2013female, FLOSS2013other)
mysurvey = surveydata
mysurvey$gendernum[mysurvey$gender == "Male"] <- 1
mysurvey$gendernum[mysurvey$gender == "Female"] <- 2
mysurvey$gendernum[mysurvey$gender == "Other"] <- 3
library(tidyr)
mysurvey = mysurvey %>% drop_na(gender, gendernum)
F3 = c(nrow(FLOSS2013male), nrow(FLOSS2013female), nrow(FLOSS2013other))
library(stringr)
my = c(sum(str_count(surveydata$gender, "Male")),
         +        sum(str_count(surveydata$gender, "Female")),
         +        sum(str_count(surveydata$gender, "Other")))
gender.compare = as.data.frame(rbind(F3, my))
names(gender.compare) = c('Male', 'Female', 'Other')
gender.compare
  Male Female Other
F3 1957    407    30
my   75      4     7
chisq.test(gender.compare)

Pearsons Chi-squared test

data:  gender.compare
X-squared = 34.33, df = 2, p-value = 3.51e-08

#Education
mysurvey$gendernum[mysurvey$education == "Technical/trade"] <- "Trade School"
mysurvey$gendernum[mysurvey$education == "University/college"] <- "University"
data$education <- ""
data$education[data[18] == "Elementary School"] <- "P"
data$education[data[18] == "High School"] <- "H"
## Merge "A-level" in to high school
data$education[data[18] == "A-Level"] <- "H"
## Treat apprenticeship as trade school
data$education[data[18] == "Apprenticeship"] <- "T"
## Merge three university levels
data$education[data[18] == "University - Bachelors"] <- "U"
data$education[data[18] == "University - Masters"] <- "U"
data$education[data[18] == "University - PhD"] <- "U"
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
F3      24         360           66       1567
my       0           5            8         73
chisq.test(education.compare)

Pearsons Chi-squared test

data:  education.compare
X-squared = 16.977, df = 3, p-value = 0.0007145

#Contribution
mysurvey$gendernum[mysurvey$contribution == "Code, Programming"] <- "Code"
mysurvey$gendernum[mysurvey$contribution == "Other contributions (documentation, translations, tests, artwork...)"] <- "Other"
data$contrib[data[8] == "Code, programming"] <- "C"
data$contrib[data[8] == "Other contributions (documentation, translations, tests, artwork...)"] <- "O"
data$contrib[data[8] == "Both"] <- "B"
FLOSS2013 = data
FLOSS2013 = FLOSS2013 %>% drop_na(contrib)
F3 = c(sum(str_count(FLOSS2013$contrib, "C")),
         +        sum(str_count(FLOSS2013$contrib, "O")),
         +        sum(str_count(FLOSS2013$contrib, "B")))
my=c(sum(str_count(surveydata$contribution, "C")),
       +      sum(str_count(surveydata$contribution, "O")),
       +      sum(str_count(surveydata$contribution, "B")))
contrib.compare = as.data.frame(rbind(F3, my))
names(contrib.compare) = c('Code', 'Both', 'Other')
contrib.compare
   Code Both Other
F3 1048  577   530
my   36   22    30
chisq.test(contrib.compare)

Pearsons Chi-squared test

data:  contrib.compare
X-squared = 4.1972, df = 2, p-value = 0.1226

#check if the difference in education is connected with the type of contributor
names(surveydata)[names(surveydata) == 'contribution'] <- 'contrib'
education.P <- rbind(subset(data, education=='P', select=c(education,contrib)),
                       +                      subset(surveydata, education=='P', select=c(education,contrib)))
education.H <- rbind(subset(data, education=='H', select=c(education,contrib)),
                       +                      subset(surveydata, education=='H', select=c(education,contrib)))
education.T <- rbind(subset(data, education=='T', select=c(education,contrib)),
                       +                      subset(surveydata, education=='T', select=c(education,contrib)))
education.U <- rbind(subset(data, education=='U', select=c(education,contrib)),
                       +                      subset(surveydata, education=='U', select=c(education,contrib)))
education.contrib = as.data.frame(rbind(education.P, education.H, education.T, education.U))
chisq.test(education.contrib$education, education.contrib$contrib)

Pearsons Chi-squared test

data:  education.contrib$education and education.contrib$contrib
X-squared = 19.632, df = 6, p-value = 0.003219

