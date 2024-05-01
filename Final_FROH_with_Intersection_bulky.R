# Binary Trait : Bulky

library(dplyr)
library(ggplot2)

databulky <- read.csv("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/Final_phenotypes_values/Binary_Traits/Bulky.csv")

setwd("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab//722_Breeds/Interaction_Files/Intersection_466_without_outliers/")
AllIndividuals_Intersection_without_outliers <- read.table("AllIndividuals_Intersection_without_outliers.txt")

# Remove repeated values from the interaction files and save them 
AllIndividuals_Intersection_without_outliers <- AllIndividuals_Intersection_without_outliers  %>% distinct(V1, V2, V3, .keep_all = T)


result_dfB <- AllIndividuals_Intersection_without_outliers %>%
  semi_join(databulky, by = c("V1" = "id"))

result_dfB1 <- result_dfB %>% semi_join(databulky,by=c("V2" ="id"))
AllIndividuals_Intersection_without_outliers <- result_dfB1

Matrix_AllIndividual_Matrix_without_outliers <- as.matrix(ftable(xtabs(AllIndividuals_Intersection_without_outliers$V3 ~ AllIndividuals_Intersection_without_outliers$V1+AllIndividuals_Intersection_without_outliers$V2, AllIndividuals_Intersection_without_outliers)))

library(tidyverse)

# Only have rows and columns to dogs of interest. 
kinshipMat = Matrix_AllIndividual_Matrix_without_outliers %>% as.matrix()

library(GMMAT)  
all_data <- databulky 

#Largeears

model1 <- glmmkin(BulkyAdjusted ~ FROH, data = all_data
                  ,kins = kinshipMat, id= "id",family = binomial(link="logit"))

model1$theta
model1$coefficients
model1$cov

LogBeta1 = model1$coef[2]
oddsratio1 = exp(LogBeta1)
LogBetaStandardized1 = model1$coef[2]/sqrt(model1$cov[2,2])

LogisticPval1 = ifelse(model1$coef[2]/sqrt(model1$cov[2,2])<0, pnorm(model1$coef[2]/sqrt(model1$cov[2,2]),lower=TRUE)*2,pnorm(model1$coef[2]/sqrt(model1$cov[2,2]),lower=FALSE)*2)
confintUpper1 = model1$coefficients + 1.96*sqrt(diag(model1$cov))
confintLower1= model1$coefficients - 1.96*sqrt(diag(model1$cov))

LogBeta1 #2.496   
LogBetaStandardized1 #1.865712   
LogisticPval1 #0.06208162   
confintUpper1 # Intercept -2.180716         with FROH  5.118140    
confintLower1 # Intercept -4.0127776        with FROH-0.1261405   

confintUpper1[2] # 5.11814    
confintLower1[2] #-0.1261405 


