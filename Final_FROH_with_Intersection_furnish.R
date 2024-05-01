# Binary Trait : furnish

library(dplyr)
library(ggplot2)

datafurnish <- read.csv("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/Final_phenotypes_values/Binary_Traits/Furnish.csv")

setwd("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab//722_Breeds/Interaction_Files/Intersection_466_without_outliers/")
AllIndividuals_Intersection_without_outliers <- read.table("AllIndividuals_Intersection_without_outliers.txt")

# Remove repeated values from the interaction files and save them 
AllIndividuals_Intersection_without_outliers <- AllIndividuals_Intersection_without_outliers  %>% distinct(V1, V2, V3, .keep_all = T)


result_dfB <- AllIndividuals_Intersection_without_outliers %>%
  semi_join(datafurnish, by = c("V1" = "id"))

result_dfB1 <- result_dfB %>% semi_join(datafurnish,by=c("V2" ="id"))
AllIndividuals_Intersection_without_outliers <- result_dfB1

Matrix_AllIndividual_Matrix_without_outliers <- as.matrix(ftable(xtabs(AllIndividuals_Intersection_without_outliers$V3 ~ AllIndividuals_Intersection_without_outliers$V1+AllIndividuals_Intersection_without_outliers$V2, AllIndividuals_Intersection_without_outliers)))

library(tidyverse)

# Only have rows and columns to dogs of interest. 
kinshipMat = Matrix_AllIndividual_Matrix_without_outliers %>% as.matrix()

library(GMMAT)  
all_data <- datafurnish

#Furnish

model1 <- glmmkin(FurnishAdjusted ~ FROH, data = all_data
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

LogBeta1 #-2.500878     
LogBetaStandardized1 #-2.642212        
LogisticPval1 #0.008236657  
confintUpper1 # Intercept 0.4998046     with FROH -0.6457198         
confintLower1 # Intercept -0.6271021        with FROH -4.3560371        

confintUpper1[2] #-0.6457198         
confintLower1[2] #-4.356037      


