surveydata = read.csv("/Users/aadharshhariharan/Desktop/Research Project/Documents and resources/survey_responses.csv")
mysurvey=surveydata
mysurvey$contribution[mysurvey$contribution == "Code, Programming"] <- 1
mysurvey$contribution[mysurvey$contribution == "Other contributions (documentation, translations, tests, artwork...)"] <- 2
mysurvey$contribution[mysurvey$contribution == "Both"] <- 3
mysurvey$gender[mysurvey$gender == "Male"] <- 1
mysurvey$gender[mysurvey$gender == "Female"] <- 2
mysurvey$gender[mysurvey$gender == "Other"] <- 3
cola<- as.numeric(mysurvey$contribution)
colb<-as.numeric(mysurvey$gender)
cor.test(cola, colb, method = "spearman", use = "complete.obs")

Spearmans rank correlation rho

data:  cola and colb
S = 85835, p-value = 0.07941
alternative hypothesis: true rho is not equal to 0
sample estimates:
      rho 
0.1902019 

colb<-as.numeric(mysurvey$age)
cor.test(cola, colb, method = "spearman", use = "complete.obs")

Spearmans rank correlation rho

data:  cola and colb
S = 106887, p-value = 0.4574
alternative hypothesis: true rho is not equal to 0
sample estimates:
        rho 
-0.08218346 

colb<-as.numeric(mysurvey$hours)
> cor.test(cola, colb, method = "spearman", use = "complete.obs")

Spearmans rank correlation rho

data:  cola and colb
S = 116832, p-value = 0.7901
alternative hypothesis: true rho is not equal to 0
sample estimates:
        rho 
-0.02877994 

colb<- as.numeric(mysurvey$episodic)
> cor.test(cola, colb, method = "spearman", use = "complete.obs")

Spearmans rank correlation rho

data:  cola and colb
S = 108921, p-value = 0.7053
alternative hypothesis: true rho is not equal to 0
sample estimates:
       rho 
0.04088471 

colb<- as.numeric(mysurvey$regular)
> cor.test(cola, colb, method = "spearman", use = "complete.obs")

Spearmans rank correlation rho

data:  cola and colb
S = 123489, p-value = 0.4181
alternative hypothesis: true rho is not equal to 0
sample estimates:
       rho 
-0.0873932 