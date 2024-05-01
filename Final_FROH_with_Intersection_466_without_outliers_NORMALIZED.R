#Without two outliers - PER00747 and PER00393
#With Normalized data 

library(dplyr)
library(ggplot2)

# To load file
setwd("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/roh_summary_722breeds_files/Final_roh_summary_files/")
#setwd("~/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/roh_summary_722breeds_files/Final_roh_summary_files/")

summary_AncienctSpitzDogs <- read.table("roh_summary_AncientSpitzDogs_31.txt")
summary_AllScentHound <- read.table("roh_summary_AllScentHound_23.txt")
summary_AllSightHound <- read.table("roh_summary_AllSightHound_21.txt")
summary_Herding <- read.table("roh_summary_Herding_76.txt")
summary_MastiffLike <- read.table("roh_summary_MastiffLike_65.txt")
summary_Retrievers <- read.table("roh_summary_Retrievers_50.txt")
summary_Smallterriers <- read.table("roh_summary_Smallterriers_102.txt")
summary_WorkingDogs <- read.table("roh_summary_WorkingDogs_44.txt")
summary_Terriers <- read.table("roh_summary_Terriers_16.txt")
summary_ToyDogs <- read.table("roh_summary_ToyDogs_18.txt")
summary_Spaniels <- read.table("roh_summary_Spaniels_22.txt")

all_breeds <- rbind(summary_AncienctSpitzDogs, summary_AllScentHound, summary_AllSightHound, summary_Herding,
                    summary_MastiffLike, summary_Retrievers, 
                    summary_Smallterriers, summary_WorkingDogs, summary_Terriers,
                    summary_ToyDogs, summary_Spaniels)

# To add ids as a column
all_breeds$id <- rownames(all_breeds)


# sort alphabetically based on id 
all_breeds <- all_breeds[order(all_breeds$id), ]

# Load a table for ids and their breedgroup and then sort alphabetically based on id 

Breedgroup_information_ids <- read.csv("../../CSV_XLS_files/Only_ids_breedgroup_568individuals.csv")
Breedgroup_information_ids <- Breedgroup_information_ids[-c(469:558),]

#Add breed groups to Allspecied722 file
all_breeds<- merge( all_breeds,Breedgroup_information_ids, by="id")

# To rearrange order of columns
all_breeds <- all_breeds[,c(1,18,3:16)]
# To remove row names
rownames(all_breeds) <- NULL

#Upload Normalized data
setwd("C:/Users/Sweetalana//OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/Final_phenotypes_values/")
#setwd("~/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/Final_phenotypes_values/")


# Load ids with all breeds file - Normalized All Individuals - 466

ids_br <- read.csv ("Normalized_All_466_Quantitative_phenotypes.csv", sep = ",")
ids_br <-  ids_br[,-c(5) ]
ids_br <- ids_br[order(ids_br$id), ]

all_data <- merge( all_breeds,ids_br, by="id")

# Load ids with all breeds file - Normalized Small Individuals - 234

Small_Normalized_Individuals <- read.csv ("Normalized_small_234_Quantitative_phenotypes.csv", sep = ",")
Small_Normalized_Individuals <-  Small_Normalized_Individuals[,-c(5) ]
Small_Normalized_Individuals <- Small_Normalized_Individuals[order(Small_Normalized_Individuals$id), ]

all_data_small <- merge( all_breeds,Small_Normalized_Individuals, by="id")

# Load ids with all breeds file - Normalized Large Individuals - 232

Large_Normalized_Individuals <- read.csv ("Normalized_large_232_Quantitative_phenotypes.csv", sep = ",")
Large_Normalized_Individuals <-  Large_Normalized_Individuals[,-c(5) ]
Large_Normalized_Individuals <- Large_Normalized_Individuals[order(Large_Normalized_Individuals$id), ]

all_data_large <- merge( all_breeds,Large_Normalized_Individuals, by="id")


library(dplyr)
library(ggplot2)

Mbp = 1000000
all_data$TOTAL <- all_data$TOTAL/Mbp
all_data_small$TOTAL <- all_data_small$TOTAL/Mbp
all_data_large$TOTAL <- all_data_large$TOTAL/Mbp



#Remove two outliers - PER00747 and PER00393
all_data <- all_data[!(all_data$id=="PER00747" | all_data$id=="PER00393"),]

#Genome Size = 2203764842  from sum of all chromosomes from vcf file
#Subtract X,Y and unknown = 2203.764 mbp

genome_size = 2203.764
all_data[["FROH"]] <- all_data$TOTAL/genome_size
all_data_small[["FROH"]] <- all_data_small$TOTAL/genome_size
all_data_large[["FROH"]] <- all_data_large$TOTAL/genome_size

library(dplyr)
library(ggplot2)

setwd("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab//722_Breeds/Interaction_Files/Intersection_466_without_outliers/")
#setwd("~/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Interaction_Files/Intersection_466_without_outliers/")

AllIndividuals_Intersection_without_outliers <- read.table("AllIndividuals_Intersection_without_outliers.txt")

# Remove repeated values from the interaction files and save them 
AllIndividuals_Intersection_without_outliers <- AllIndividuals_Intersection_without_outliers  %>% distinct(V1, V2, V3, .keep_all = T)

# Creating a matrix
Matrix_AllIndividual_Matrix_without_outliers <- as.matrix(ftable(xtabs(AllIndividuals_Intersection_without_outliers$V3 ~ AllIndividuals_Intersection_without_outliers$V1+AllIndividuals_Intersection_without_outliers$V2, AllIndividuals_Intersection_without_outliers)))
library(tidyverse)

# Only have rows and columns to dogs of interest. 
kinshipMat = Matrix_AllIndividual_Matrix_without_outliers %>% as.matrix()

library(GMMAT)  


#Height - All 466 Individuals

model1_height <- glmmkin(Height ~ FROH, data = all_data
                         ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height$theta
model1_height$coefficients
model1_height$cov

LogBeta1_height = model1_height$coef[2]
oddsratio1_height = exp(LogBeta1_height)
LogBetaStandardized1_height = model1_height$coef[2]/sqrt(model1_height$cov[2,2])

LogisticPval1_height = ifelse(model1_height$coef[2]/sqrt(model1_height$cov[2,2])<0, pnorm(model1_height$coef[2]/sqrt(model1_height$cov[2,2]),lower=TRUE)*2,pnorm(model1_height$coef[2]/sqrt(model1_height$cov[2,2]),lower=FALSE)*2)
confintUpper1_height = model1_height$coefficients + 1.96*sqrt(diag(model1_height$cov))
confintLower1_height= model1_height$coefficients - 1.96*sqrt(diag(model1_height$cov))

LogBeta1_height #0.3076083 
LogBetaStandardized1_height #2.87396
LogisticPval1_height #0.004053605 
confintUpper1_height
confintLower1_height

confintUpper1_height[2] #0.5173928  
confintLower1_height[2]  #0.09782378 

#Height - Small dogs - 234 Individuals

model1_height_small <- glmmkin(Height ~ FROH, data = all_data_small
                         ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_small$theta
model1_height_small$coefficients
model1_height_small$cov

LogBeta1_height_small = model1_height_small$coef[2]
oddsratio1_height_small = exp(LogBeta1_height_small)
LogBetaStandardized1_height_small = model1_height_small$coef[2]/sqrt(model1_height_small$cov[2,2])

LogisticPval1_height_small = ifelse(model1_height_small$coef[2]/sqrt(model1_height_small$cov[2,2])<0, pnorm(model1_height_small$coef[2]/sqrt(model1_height_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_small$coef[2]/sqrt(model1_height_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_small = model1_height_small$coefficients + 1.96*sqrt(diag(model1_height_small$cov))
confintLower1_height_small = model1_height_small$coefficients - 1.96*sqrt(diag(model1_height_small$cov))

LogBeta1_height_small #0.1001136 
LogBetaStandardized1_height_small #0.7353851
LogisticPval1_height_small #0.462105 
confintUpper1_height_small
confintLower1_height_small

confintUpper1_height_small[2] #0.3669432  
confintLower1_height_small[2]  #-0.1667161 


#Height - Large dogs - 232 Individuals

model1_height_large <- glmmkin(Height ~ FROH, data = all_data_large
                               ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_large$theta
model1_height_large$coefficients
model1_height_large$cov

LogBeta1_height_large = model1_height_large$coef[2]
oddsratio1_height_large = exp(LogBeta1_height_large)
LogBetaStandardized1_height_large = model1_height_large$coef[2]/sqrt(model1_height_large$cov[2,2])

LogisticPval1_height_large = ifelse(model1_height_large$coef[2]/sqrt(model1_height_large$cov[2,2])<0, pnorm(model1_height_large$coef[2]/sqrt(model1_height_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_large$coef[2]/sqrt(model1_height_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_large = model1_height_large$coefficients + 1.96*sqrt(diag(model1_height_large$cov))
confintLower1_height_large = model1_height_large$coefficients - 1.96*sqrt(diag(model1_height_large$cov))

LogBeta1_height_large #-0.08537293 
LogBetaStandardized1_height_large #-0.8836499 

LogisticPval1_height_large #0.3768852  
confintUpper1_height_large
confintLower1_height_large

confintUpper1_height_large[2] #0.1039905   
confintLower1_height_large[2]  #-0.2747363 
 

#Height - All 466 Individuals Normalized with weight as covariate

model1_height_modelW <- glmmkin(Height ~ FROH + Weight, data = all_data
                         ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelW$theta
model1_height_modelW$coefficients
model1_height_modelW$cov

#######FOR FROH#######
LogBeta1_height_modelW  = model1_height_modelW$coef[2]
oddsratio1_height_modelW  = exp(LogBeta1_height_modelW )
LogBetaStandardized1_height_modelW  = model1_height_modelW$coef[2]/sqrt(model1_height_modelW$cov[2,2])

LogisticPval1_height_modelW  = ifelse(model1_height_modelW$coef[2]/sqrt(model1_height_modelW$cov[2,2])<0, pnorm(model1_height_modelW$coef[2]/sqrt(model1_height_modelW$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelW$coef[2]/sqrt(model1_height_modelW$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelW  = model1_height_modelW$coefficients + 1.96*sqrt(diag(model1_height_modelW$cov))
confintLower1_height_modelW = model1_height_modelW$coefficients - 1.96*sqrt(diag(model1_height_modelW$cov))

LogBeta1_height_modelW  #0.06859071 
LogBetaStandardized1_height_modelW  #1.383837 
LogisticPval1_height_modelW  #0.1664084  
confintUpper1_height_modelW 
confintLower1_height_modelW 

confintUpper1_height_modelW[2] #FROH = 0.1657393, Weight =    1.3140330 
confintLower1_height_modelW[2]  #FROH = -0.02855787 , Weight = 1.19576907 
 
#######FOR WEIGHT#######
LogBeta1_height_modelW_Weight  = model1_height_modelW$coef[3]
oddsratio1_height_modelW_Weight  = exp(LogBeta1_height_modelW_Weight )
LogBetaStandardized1_height_modelW_Weight  = model1_height_modelW$coef[3]/sqrt(model1_height_modelW$cov[3,3])

LogisticPval1_height_modelW_Weight  = ifelse(model1_height_modelW$coef[3]/sqrt(model1_height_modelW$cov[3,3])<0, pnorm(model1_height_modelW$coef[3]/sqrt(model1_height_modelW$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelW$coef[3]/sqrt(model1_height_modelW$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelW_Weight  = model1_height_modelW$coefficients + 1.96*sqrt(diag(model1_height_modelW$cov))
confintLower1_height_modelW_Weight = model1_height_modelW$coefficients - 1.96*sqrt(diag(model1_height_modelW$cov))

LogBeta1_height_modelW_Weight  #1.254901  
LogBetaStandardized1_height_modelW_Weight  #41.5952  
LogisticPval1_height_modelW_Weight  #0  
confintUpper1_height_modelW 
confintLower1_height_modelW 

confintUpper1_height_modelW[3] #FROH = 0.1657393, Weight =    1.3140330 
confintLower1_height_modelW[3]  #FROH = -0.02855787 , Weight = 1.19576907 

# 
#Height - All 234 Small Individuals Normalized with weight as covariate

model1_height_modelW_small <- glmmkin(Height ~ FROH + Weight, data = all_data_small
                                ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelW_small$theta
model1_height_modelW_small$coefficients
model1_height_modelW_small$cov

#######FOR FROH#######
LogBeta1_height_modelW_small  = model1_height_modelW_small$coef[2]
oddsratio1_height_modelW_small  = exp(LogBeta1_height_modelW_small )
LogBetaStandardized1_height_modelW_small  = model1_height_modelW_small$coef[2]/sqrt(model1_height_modelW_small$cov[2,2])

LogisticPval1_height_modelW_small  = ifelse(model1_height_modelW_small$coef[2]/sqrt(model1_height_modelW_small$cov[2,2])<0, pnorm(model1_height_modelW_small$coef[2]/sqrt(model1_height_modelW_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelW_small$coef[2]/sqrt(model1_height_modelW_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelW_small  = model1_height_modelW_small$coefficients + 1.96*sqrt(diag(model1_height_modelW_small$cov))
confintLower1_height_modelW_small = model1_height_modelW_small$coefficients - 1.96*sqrt(diag(model1_height_modelW_small$cov))

LogBeta1_height_modelW_small  #-0.03358523  
LogBetaStandardized1_height_modelW_small  #-0.6531626  
LogisticPval1_height_modelW_small  #0.5136515   
confintUpper1_height_modelW_small 
confintLower1_height_modelW_small 

confintUpper1_height_modelW_small[2] #FROH = 0.06719680  , Weight =    0.78129474  
confintLower1_height_modelW_small[2]  #FROH = -0.13436726     , Weight = 0.70359820  

#######FOR WEIGHT#######
LogBeta1_height_modelW_Weight_small  = model1_height_modelW_small$coef[3]
oddsratio1_height_modelW_Weight_small  = exp(LogBeta1_height_modelW_Weight_small )
LogBetaStandardized1_height_modelW_Weight_small  = model1_height_modelW_small$coef[3]/sqrt(model1_height_modelW_small$cov[3,3])

LogisticPval1_height_modelW_Weight_small  = ifelse(model1_height_modelW_small$coef[3]/sqrt(model1_height_modelW_small$cov[3,3])<0, pnorm(model1_height_modelW_small$coef[3]/sqrt(model1_height_modelW_small$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelW_small$coef[3]/sqrt(model1_height_modelW_small$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelW_Weight_small  = model1_height_modelW_small$coefficients + 1.96*sqrt(diag(model1_height_modelW_small$cov))
confintLower1_height_modelW_Weight_small = model1_height_modelW_small$coefficients - 1.96*sqrt(diag(model1_height_modelW_small$cov))

LogBeta1_height_modelW_Weight_small  #0.7424465   
LogBetaStandardized1_height_modelW_Weight_small  #37.45843   
LogisticPval1_height_modelW_Weight_small  #4.379484e-307  
confintUpper1_height_modelW_small
confintLower1_height_modelW_small 

confintUpper1_height_modelW_small[3] #FROH = 0.06719680  , Weight =    0.78129474  
confintLower1_height_modelW_small[3]  #FROH = -0.13436726   , Weight = 0.70359820  

#####################################################
#Height - All 232 large Individuals Normalized with weight as covariate

model1_height_modelW_large <- glmmkin(Height ~ FROH + Weight, data = all_data_large
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelW_large$theta
model1_height_modelW_large$coefficients
model1_height_modelW_large$cov

#######FOR FROH#######
LogBeta1_height_modelW_large  = model1_height_modelW_large$coef[2]
oddsratio1_height_modelW_large  = exp(LogBeta1_height_modelW_large )
LogBetaStandardized1_height_modelW_large  = model1_height_modelW_large$coef[2]/sqrt(model1_height_modelW_large$cov[2,2])

LogisticPval1_height_modelW_large  = ifelse(model1_height_modelW_large$coef[2]/sqrt(model1_height_modelW_large$cov[2,2])<0, pnorm(model1_height_modelW_large$coef[2]/sqrt(model1_height_modelW_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelW_large$coef[2]/sqrt(model1_height_modelW_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelW_large  = model1_height_modelW_large$coefficients + 1.96*sqrt(diag(model1_height_modelW_large$cov))
confintLower1_height_modelW_large = model1_height_modelW_large$coefficients - 1.96*sqrt(diag(model1_height_modelW_large$cov))

LogBeta1_height_modelW_large  #-0.01228976  
LogBetaStandardized1_height_modelW_large  #-0.1647254   
LogisticPval1_height_modelW_large  #0.8691602    
confintUpper1_height_modelW_large 
confintLower1_height_modelW_large 

confintUpper1_height_modelW_large[2] #FROH = 0.1339411 , Weight =    0.6576059   
confintLower1_height_modelW_large[2]  #FROH = -0.1585206       , Weight = 0.4806900   

#######FOR WEIGHT#######
LogBeta1_height_modelW_Weight_large  = model1_height_modelW_large$coef[3]
oddsratio1_height_modelW_Weight_large  = exp(LogBeta1_height_modelW_Weight_large )
LogBetaStandardized1_height_modelW_Weight_large  = model1_height_modelW_large$coef[3]/sqrt(model1_height_modelW_large$cov[3,3])

LogisticPval1_height_modelW_Weight_large  = ifelse(model1_height_modelW_large$coef[3]/sqrt(model1_height_modelW_large$cov[3,3])<0, pnorm(model1_height_modelW_large$coef[3]/sqrt(model1_height_modelW_large$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelW_large$coef[3]/sqrt(model1_height_modelW_large$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelW_Weight_large  = model1_height_modelW_large$coefficients + 1.96*sqrt(diag(model1_height_modelW_large$cov))
confintLower1_height_modelW_Weight_large = model1_height_modelW_large$coefficients - 1.96*sqrt(diag(model1_height_modelW_large$cov))

LogBeta1_height_modelW_Weight_large  #0.5691479    
LogBetaStandardized1_height_modelW_Weight_large  #12.61085    
LogisticPval1_height_modelW_Weight_large  #1.839966e-36 
 
confintUpper1_height_modelW_large
confintLower1_height_modelW_large 

confintUpper1_height_modelW_large[3] # Weight =    0.6576059   
confintLower1_height_modelW_large[3]  # Weight = 0.48069   

#####################################################
# With the interaction term as co variate - 466 All Individuals
model1_height_modelA <- glmmkin(Height ~ FROH + Weight + FROH*Weight, data = all_data
                                ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelA$theta
model1_height_modelA$coefficients
model1_height_modelA$cov

LogBeta1_height_modelA  = model1_height_modelA$coef[2]
oddsratio1_height_modelA  = exp(LogBeta1_height_modelA )
LogBetaStandardized1_height_modelA  = model1_height_modelA$coef[2]/sqrt(model1_height_modelA$cov[2,2])

LogisticPval1_height_modelA  = ifelse(model1_height_modelA$coef[2]/sqrt(model1_height_modelA$cov[2,2])<0, pnorm(model1_height_modelA$coef[2]/sqrt(model1_height_modelA$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelA$coef[2]/sqrt(model1_height_modelA$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelA  = model1_height_modelA$coefficients + 1.96*sqrt(diag(model1_height_modelA$cov))
confintLower1_height_modelA = model1_height_modelA$coefficients - 1.96*sqrt(diag(model1_height_modelA$cov))

LogBeta1_height_modelA  #0.2259482  
LogBetaStandardized1_height_modelA  #2.82585  
LogisticPval1_height_modelA  #0.004715528    
confintUpper1_height_modelA 
confintLower1_height_modelA 

confintUpper1_height_modelA[2] #FROH=0.3826650,  Weight=1.5821448,  FROH:Weight= -0.1336829  
confintLower1_height_modelA[2]  #FROH = 0.06923131  , Weight= 1.28107325 ,  FROH:Weight= -1.10535602 


#######FOR WEIGHT#######
LogBeta1_height_modelA_Weight  = model1_height_modelA$coef[3]
oddsratio1_height_modelA_Weight  = exp(LogBeta1_height_modelA_Weight )
LogBetaStandardized1_height_modelA_Weight  = model1_height_modelA$coef[3]/sqrt(model1_height_modelA$cov[3,3])

LogisticPval1_height_modelA_Weight  = ifelse(model1_height_modelA$coef[3]/sqrt(model1_height_modelA$cov[3,3])<0, pnorm(model1_height_modelA$coef[3]/sqrt(model1_height_modelA$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelA$coef[3]/sqrt(model1_height_modelA$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelA_Weight  = model1_height_modelA$coefficients + 1.96*sqrt(diag(model1_height_modelA$cov))
confintLower1_height_modelA_Weight = model1_height_modelA$coefficients - 1.96*sqrt(diag(model1_height_modelA$cov))

LogBeta1_height_modelA_Weight  #1.431609   
LogBetaStandardized1_height_modelA_Weight  #18.63978  
LogisticPval1_height_modelA_Weight  #1.528648e-77  
confintUpper1_height_modelA 
confintLower1_height_modelA 

confintUpper1_height_modelA[3]# Weight =    1.582145    
confintLower1_height_modelA[3]  # Weight = 1.281073   

#######FOR WEIGHT * FROH #######
LogBeta1_height_modelA_WeightFROH  = model1_height_modelA$coef[4]
oddsratio1_height_modelA_WeightFROH  = exp(LogBeta1_height_modelA_WeightFROH )
LogBetaStandardized1_height_modelA_WeightFROH  = model1_height_modelA$coef[4]/sqrt(model1_height_modelA$cov[4,4])

LogisticPval1_height_modelA_WeightFROH  = ifelse(model1_height_modelA$coef[4]/sqrt(model1_height_modelA$cov[4,4])<0, pnorm(model1_height_modelA$coef[4]/sqrt(model1_height_modelA$cov[4,4]),lower=TRUE)*2,pnorm(model1_height_modelA$coef[4]/sqrt(model1_height_modelA$cov[4,4]),lower=FALSE)*2)
confintUpper1_height_modelA_WeightFROH  = model1_height_modelA$coefficients + 1.96*sqrt(diag(model1_height_modelA$cov))
confintLower1_height_modelA_WeightFROH = model1_height_modelA$coefficients - 1.96*sqrt(diag(model1_height_modelA$cov))

LogBeta1_height_modelA_WeightFROH  #-0.6195195    
LogBetaStandardized1_height_modelA_WeightFROH  #-2.499314  
LogisticPval1_height_modelA_WeightFROH  #0.01244339   
confintUpper1_height_modelA 
confintLower1_height_modelA 

confintUpper1_height_modelA[4]# WeightCFROH =    -0.1336829   
confintLower1_height_modelA[4]  # WeightFROH = -1.105356   


# Small 234 individuals With the interaction term as co variate
model1_height_modelA_small <- glmmkin(Height ~ FROH + Weight + FROH*Weight, data = all_data_small
                                ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelA_small$theta
model1_height_modelA_small$coefficients
model1_height_modelA_small$cov

LogBeta1_height_modelA_small  = model1_height_modelA_small$coef[2]
oddsratio1_height_modelA_small  = exp(LogBeta1_height_modelA_small )
LogBetaStandardized1_height_modelA_small  = model1_height_modelA_small$coef[2]/sqrt(model1_height_modelA_small$cov[2,2])

LogisticPval1_height_modelA_small  = ifelse(model1_height_modelA_small$coef[2]/sqrt(model1_height_modelA_small$cov[2,2])<0, pnorm(model1_height_modelA_small$coef[2]/sqrt(model1_height_modelA_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelA_small$coef[2]/sqrt(model1_height_modelA_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelA_small  = model1_height_modelA_small$coefficients + 1.96*sqrt(diag(model1_height_modelA_small$cov))
confintLower1_height_modelA_small = model1_height_modelA_small$coefficients - 1.96*sqrt(diag(model1_height_modelA_small$cov))

LogBeta1_height_modelA_small  #0.261817   
LogBetaStandardized1_height_modelA_small  #2.782737   
LogisticPval1_height_modelA_small  #0.005390251     
confintUpper1_height_modelA_small 
confintLower1_height_modelA_small 

confintUpper1_height_modelA_small[2] #FROH=0.4462259 
confintLower1_height_modelA_small[2]  #FROH = 0.07740814   


#######FOR WEIGHT#######
LogBeta1_height_modelA_small_Weight  = model1_height_modelA_small$coef[3]
oddsratio1_height_modelA_small_Weight  = exp(LogBeta1_height_modelA_small_Weight )
LogBetaStandardized1_height_modelA_small_Weight  = model1_height_modelA_small$coef[3]/sqrt(model1_height_modelA_small$cov[3,3])

LogisticPval1_height_modelA_small_Weight  = ifelse(model1_height_modelA_small$coef[3]/sqrt(model1_height_modelA_small$cov[3,3])<0, pnorm(model1_height_modelA_small$coef[3]/sqrt(model1_height_modelA_small$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelA_small$coef[3]/sqrt(model1_height_modelA_small$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelA_small_Weight  = model1_height_modelA_small$coefficients + 1.96*sqrt(diag(model1_height_modelA_small$cov))
confintLower1_height_modelA_small_Weight = model1_height_modelA_small$coefficients - 1.96*sqrt(diag(model1_height_modelA_small$cov))

LogBeta1_height_modelA_small_Weight  #0.9169273    
LogBetaStandardized1_height_modelA_small_Weight  #18.02948   
LogisticPval1_height_modelA_small_Weight  #1.143721e-72  
confintUpper1_height_modelA_small 
confintLower1_height_modelA_small 

confintUpper1_height_modelA_small[3]# Weight =    1.016607     
confintLower1_height_modelA_small[3]  # Weight = 0.8172473    

#######FOR WEIGHT * FROH #######
LogBeta1_height_modelA_small_WeightFROH  = model1_height_modelA_small$coef[4]
oddsratio1_height_modelA_small_WeightFROH  = exp(LogBeta1_height_modelA_small_WeightFROH )
LogBetaStandardized1_height_modelA_small_WeightFROH  = model1_height_modelA_small$coef[4]/sqrt(model1_height_modelA_small$cov[4,4])

LogisticPval1_height_modelA_small_WeightFROH  = ifelse(model1_height_modelA_small$coef[4]/sqrt(model1_height_modelA_small$cov[4,4])<0, pnorm(model1_height_modelA_small$coef[4]/sqrt(model1_height_modelA_small$cov[4,4]),lower=TRUE)*2,pnorm(model1_height_modelA_small$coef[4]/sqrt(model1_height_modelA_small$cov[4,4]),lower=FALSE)*2)
confintUpper1_height_modelA_small_WeightFROH  = model1_height_modelA_small$coefficients + 1.96*sqrt(diag(model1_height_modelA_small$cov))
confintLower1_height_modelA_small_WeightFROH = model1_height_modelA_small$coefficients - 1.96*sqrt(diag(model1_height_modelA_small$cov))

LogBeta1_height_modelA_small_WeightFROH  #-0.7356957     
LogBetaStandardized1_height_modelA_small_WeightFROH  #-3.708044   
LogisticPval1_height_modelA_small_WeightFROH  #0.0002088667    
confintUpper1_height_modelA_small 
confintLower1_height_modelA_small 

confintUpper1_height_modelA_small[4]# WeightFROH =    -0.3468212    
confintLower1_height_modelA_small[4]  # WeightFROH =   -1.12457  


# large 232 individuals With the interaction term as co variate
model1_height_modelA_large <- glmmkin(Height ~ FROH + Weight + FROH*Weight, data = all_data_large
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_height_modelA_large$theta
model1_height_modelA_large$coefficients
model1_height_modelA_large$cov

LogBeta1_height_modelA_large  = model1_height_modelA_large$coef[2]
oddsratio1_height_modelA_large  = exp(LogBeta1_height_modelA_large )
LogBetaStandardized1_height_modelA_large  = model1_height_modelA_large$coef[2]/sqrt(model1_height_modelA_large$cov[2,2])

LogisticPval1_height_modelA_large  = ifelse(model1_height_modelA_large$coef[2]/sqrt(model1_height_modelA_large$cov[2,2])<0, pnorm(model1_height_modelA_large$coef[2]/sqrt(model1_height_modelA_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_height_modelA_large$coef[2]/sqrt(model1_height_modelA_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_height_modelA_large  = model1_height_modelA_large$coefficients + 1.96*sqrt(diag(model1_height_modelA_large$cov))
confintLower1_height_modelA_large = model1_height_modelA_large$coefficients - 1.96*sqrt(diag(model1_height_modelA_large$cov))

LogBeta1_height_modelA_large  #-0.2122139    
LogBetaStandardized1_height_modelA_large  #-1.915412 
LogisticPval1_height_modelA_large  #0.05543993      
confintUpper1_height_modelA_large 
confintLower1_height_modelA_large 

confintUpper1_height_modelA_large[2] #FROH=0.004939982  
confintLower1_height_modelA_large[2]  #FROH = -0.4293678    


#######FOR WEIGHT#######
LogBeta1_height_modelA_large_Weight  = model1_height_modelA_large$coef[3]
oddsratio1_height_modelA_large_Weight  = exp(LogBeta1_height_modelA_large_Weight )
LogBetaStandardized1_height_modelA_large_Weight  = model1_height_modelA_large$coef[3]/sqrt(model1_height_modelA_large$cov[3,3])

LogisticPval1_height_modelA_large_Weight  = ifelse(model1_height_modelA_large$coef[3]/sqrt(model1_height_modelA_large$cov[3,3])<0, pnorm(model1_height_modelA_large$coef[3]/sqrt(model1_height_modelA_large$cov[3,3]),lower=TRUE)*2,pnorm(model1_height_modelA_large$coef[3]/sqrt(model1_height_modelA_large$cov[3,3]),lower=FALSE)*2)
confintUpper1_height_modelA_large_Weight  = model1_height_modelA_large$coefficients + 1.96*sqrt(diag(model1_height_modelA_large$cov))
confintLower1_height_modelA_large_Weight = model1_height_modelA_large$coefficients - 1.96*sqrt(diag(model1_height_modelA_large$cov))

LogBeta1_height_modelA_large_Weight  #0.2816131     
LogBetaStandardized1_height_modelA_large_Weight  #2.218692    
LogisticPval1_height_modelA_large_Weight  #0.02650766  
confintUpper1_height_modelA_large 
confintLower1_height_modelA_large 

confintUpper1_height_modelA_large[3]# Weight =    0.5303911      
confintLower1_height_modelA_large[3]  # Weight = 0.03283519     

#######FOR WEIGHT * FROH #######
LogBeta1_height_modelA_large_WeightFROH  = model1_height_modelA_large$coef[4]
oddsratio1_height_modelA_large_WeightFROH  = exp(LogBeta1_height_modelA_large_WeightFROH )
LogBetaStandardized1_height_modelA_large_WeightFROH  = model1_height_modelA_large$coef[4]/sqrt(model1_height_modelA_large$cov[4,4])

LogisticPval1_height_modelA_large_WeightFROH  = ifelse(model1_height_modelA_large$coef[4]/sqrt(model1_height_modelA_large$cov[4,4])<0, pnorm(model1_height_modelA_large$coef[4]/sqrt(model1_height_modelA_large$cov[4,4]),lower=TRUE)*2,pnorm(model1_height_modelA_large$coef[4]/sqrt(model1_height_modelA_large$cov[4,4]),lower=FALSE)*2)
confintUpper1_height_modelA_large_WeightFROH  = model1_height_modelA_large$coefficients + 1.96*sqrt(diag(model1_height_modelA_large$cov))
confintLower1_height_modelA_large_WeightFROH = model1_height_modelA_large$coefficients - 1.96*sqrt(diag(model1_height_modelA_large$cov))

LogBeta1_height_modelA_large_WeightFROH  #0.8963547      
LogBetaStandardized1_height_modelA_large_WeightFROH  #2.420104    
LogisticPval1_height_modelA_large_WeightFROH  #0.01551607     
confintUpper1_height_modelA_large 
confintLower1_height_modelA_large 

confintUpper1_height_modelA_large[4]# WeightFROH =1.622297     
confintLower1_height_modelA_large[4]  # WeightFROH =   0.1704126   


#Weight - All 466 Individuals

model1_weight <- glmmkin(Weight ~ FROH, data = all_data
                         ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_weight$theta
model1_weight$coefficients
model1_weight$cov

LogBeta1_weight = model1_weight$coef[2]
oddsratio1_weight = exp(LogBeta1_weight)
LogBetaStandardized1_weight = model1_weight$coef[2]/sqrt(model1_weight$cov[2,2])

LogisticPval1_weight = ifelse(model1_weight$coef[2]/sqrt(model1_weight$cov[2,2])<0, pnorm(model1_weight$coef[2]/sqrt(model1_weight$cov[2,2]),lower=TRUE)*2,pnorm(model1_weight$coef[2]/sqrt(model1_weight$cov[2,2]),lower=FALSE)*2)
confintUpper1_weight = model1_weight$coefficients + 1.96*sqrt(diag(model1_weight$cov))
confintLower1_weight= model1_weight$coefficients - 1.96*sqrt(diag(model1_weight$cov))

LogBeta1_weight #0.1904673 
LogBetaStandardized1_weight #2.514227 
LogisticPval1_weight #0.01192936  
confintUpper1_weight
confintLower1_weight

confintUpper1_weight[2] #0.3389486   
confintLower1_weight[2]  #0.04198592  

#Weight - Small dogs - 234 Individuals

model1_weight_small <- glmmkin(Weight ~ FROH, data = all_data_small
                               ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_weight_small$theta
model1_weight_small$coefficients
model1_weight_small$cov

LogBeta1_weight_small = model1_weight_small$coef[2]
oddsratio1_weight_small = exp(LogBeta1_weight_small)
LogBetaStandardized1_weight_small = model1_weight_small$coef[2]/sqrt(model1_weight_small$cov[2,2])

LogisticPval1_weight_small = ifelse(model1_weight_small$coef[2]/sqrt(model1_weight_small$cov[2,2])<0, pnorm(model1_weight_small$coef[2]/sqrt(model1_weight_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_weight_small$coef[2]/sqrt(model1_weight_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_weight_small = model1_weight_small$coefficients + 1.96*sqrt(diag(model1_weight_small$cov))
confintLower1_weight_small = model1_weight_small$coefficients - 1.96*sqrt(diag(model1_weight_small$cov))

LogBeta1_weight_small #0.1800787  
LogBetaStandardized1_weight_small #1.059848 
LogisticPval1_weight_small #0.2892136 
confintUpper1_weight_small
confintLower1_weight_small

confintUpper1_weight_small[2] #0.513102   
confintLower1_weight_small[2]  #-0.1529447  


#Weight - Large dogs - 232 Individuals

model1_weight_large <- glmmkin(Weight ~ FROH, data = all_data_large
                               ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_weight_large$theta
model1_weight_large$coefficients
model1_weight_large$cov

LogBeta1_weight_large = model1_weight_large$coef[2]
oddsratio1_weight_large = exp(LogBeta1_weight_large)
LogBetaStandardized1_weight_large = model1_weight_large$coef[2]/sqrt(model1_weight_large$cov[2,2])

LogisticPval1_weight_large = ifelse(model1_weight_large$coef[2]/sqrt(model1_weight_large$cov[2,2])<0, pnorm(model1_weight_large$coef[2]/sqrt(model1_weight_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_weight_large$coef[2]/sqrt(model1_weight_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_weight_large = model1_weight_large$coefficients + 1.96*sqrt(diag(model1_weight_large$cov))
confintLower1_weight_large = model1_weight_large$coefficients - 1.96*sqrt(diag(model1_weight_large$cov))

LogBeta1_weight_large #-0.128408 
LogBetaStandardized1_weight_large #-1.181594 
 

LogisticPval1_weight_large #0.2373669   
confintUpper1_weight_large
confintLower1_weight_large

confintUpper1_weight_large[2] #0.08459218    
confintLower1_weight_large[2]  #-0.3414083 


#LifeSpan - All 466 Individuals

model1_LifeSpan <- glmmkin(LifeSpan ~ FROH, data = all_data
                         ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan$theta
model1_LifeSpan$coefficients
model1_LifeSpan$cov

LogBeta1_LifeSpan = model1_LifeSpan$coef[2]
oddsratio1_LifeSpan = exp(LogBeta1_LifeSpan)
LogBetaStandardized1_LifeSpan = model1_LifeSpan$coef[2]/sqrt(model1_LifeSpan$cov[2,2])

LogisticPval1_LifeSpan = ifelse(model1_LifeSpan$coef[2]/sqrt(model1_LifeSpan$cov[2,2])<0, pnorm(model1_LifeSpan$coef[2]/sqrt(model1_LifeSpan$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan$coef[2]/sqrt(model1_LifeSpan$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan = model1_LifeSpan$coefficients + 1.96*sqrt(diag(model1_LifeSpan$cov))
confintLower1_LifeSpan= model1_LifeSpan$coefficients - 1.96*sqrt(diag(model1_LifeSpan$cov))

LogBeta1_LifeSpan #-0.1354057 
LogBetaStandardized1_LifeSpan #-2.010364 
LogisticPval1_LifeSpan #0.04439272  
confintUpper1_LifeSpan
confintLower1_LifeSpan

confintUpper1_LifeSpan[2] #-0.003392183 
confintLower1_LifeSpan[2]  #-0.2674193 

#LifeSpan - Small dogs - 234 Individuals

model1_LifeSpan_small <- glmmkin(LifeSpan ~ FROH, data = all_data_small
                               ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_small$theta
model1_LifeSpan_small$coefficients
model1_LifeSpan_small$cov

LogBeta1_LifeSpan_small = model1_LifeSpan_small$coef[2]
oddsratio1_LifeSpan_small = exp(LogBeta1_LifeSpan_small)
LogBetaStandardized1_LifeSpan_small = model1_LifeSpan_small$coef[2]/sqrt(model1_LifeSpan_small$cov[2,2])

LogisticPval1_LifeSpan_small = ifelse(model1_LifeSpan_small$coef[2]/sqrt(model1_LifeSpan_small$cov[2,2])<0, pnorm(model1_LifeSpan_small$coef[2]/sqrt(model1_LifeSpan_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_small$coef[2]/sqrt(model1_LifeSpan_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_small = model1_LifeSpan_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_small$cov))
confintLower1_LifeSpan_small = model1_LifeSpan_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_small$cov))

LogBeta1_LifeSpan_small #0.1122171   
LogBetaStandardized1_LifeSpan_small #1.526667  
LogisticPval1_LifeSpan_small #0.1268437  
confintUpper1_LifeSpan_small
confintLower1_LifeSpan_small

confintUpper1_LifeSpan_small[2] #0.2562862    
confintLower1_LifeSpan_small[2]  #-0.03185195   


#LifeSpan - Large dogs - 232 Individuals

model1_LifeSpan_large <- glmmkin(LifeSpan ~ FROH, data = all_data_large
                               ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_large$theta
model1_LifeSpan_large$coefficients
model1_LifeSpan_large$cov

LogBeta1_LifeSpan_large = model1_LifeSpan_large$coef[2]
oddsratio1_LifeSpan_large = exp(LogBeta1_LifeSpan_large)
LogBetaStandardized1_LifeSpan_large = model1_LifeSpan_large$coef[2]/sqrt(model1_LifeSpan_large$cov[2,2])

LogisticPval1_LifeSpan_large = ifelse(model1_LifeSpan_large$coef[2]/sqrt(model1_LifeSpan_large$cov[2,2])<0, pnorm(model1_LifeSpan_large$coef[2]/sqrt(model1_LifeSpan_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_large$coef[2]/sqrt(model1_LifeSpan_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_large = model1_LifeSpan_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_large$cov))
confintLower1_LifeSpan_large = model1_LifeSpan_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_large$cov))

LogBeta1_LifeSpan_large #-0.1507638  
LogBetaStandardized1_LifeSpan_large #-1.189452  


LogisticPval1_LifeSpan_large #0.2342618    
confintUpper1_LifeSpan_large
confintLower1_LifeSpan_large

confintUpper1_LifeSpan_large[2] #0.09766741     
confintLower1_LifeSpan_large[2]  #-0.399195 

#####

#LifeSpan - All 466 Individuals Normalized with weight as covariate

model1_LifeSpan_modelW <- glmmkin(LifeSpan ~ FROH + Weight, data = all_data
                                ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelW$theta
model1_LifeSpan_modelW$coefficients
model1_LifeSpan_modelW$cov

#######FOR FROH#######
LogBeta1_LifeSpan_modelW  = model1_LifeSpan_modelW$coef[2]
oddsratio1_LifeSpan_modelW  = exp(LogBeta1_LifeSpan_modelW )
LogBetaStandardized1_LifeSpan_modelW  = model1_LifeSpan_modelW$coef[2]/sqrt(model1_LifeSpan_modelW$cov[2,2])

LogisticPval1_LifeSpan_modelW  = ifelse(model1_LifeSpan_modelW$coef[2]/sqrt(model1_LifeSpan_modelW$cov[2,2])<0, pnorm(model1_LifeSpan_modelW$coef[2]/sqrt(model1_LifeSpan_modelW$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW$coef[2]/sqrt(model1_LifeSpan_modelW$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW  = model1_LifeSpan_modelW$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW$cov))
confintLower1_LifeSpan_modelW = model1_LifeSpan_modelW$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW$cov))

LogBeta1_LifeSpan_modelW  #-0.02005714 
LogBetaStandardized1_LifeSpan_modelW  #-0.4035568 
LogisticPval1_LifeSpan_modelW  #0.6865387  
confintUpper1_LifeSpan_modelW 
confintLower1_LifeSpan_modelW 

confintUpper1_LifeSpan_modelW[2] #FROH = 0.07735665  
confintLower1_LifeSpan_modelW[2]  #FROH = -0.1174709 

#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelW_Weight  = model1_LifeSpan_modelW$coef[3]
oddsratio1_LifeSpan_modelW_Weight  = exp(LogBeta1_LifeSpan_modelW_Weight )
LogBetaStandardized1_LifeSpan_modelW_Weight  = model1_LifeSpan_modelW$coef[3]/sqrt(model1_LifeSpan_modelW$cov[3,3])

LogisticPval1_LifeSpan_modelW_Weight  = ifelse(model1_LifeSpan_modelW$coef[3]/sqrt(model1_LifeSpan_modelW$cov[3,3])<0, pnorm(model1_LifeSpan_modelW$coef[3]/sqrt(model1_LifeSpan_modelW$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW$coef[3]/sqrt(model1_LifeSpan_modelW$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW_Weight  = model1_LifeSpan_modelW$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW$cov))
confintLower1_LifeSpan_modelW_Weight = model1_LifeSpan_modelW$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW$cov))

LogBeta1_LifeSpan_modelW_Weight  #-0.6056084 
LogBetaStandardized1_LifeSpan_modelW_Weight  #-20.01897 
LogisticPval1_LifeSpan_modelW_Weight  #3.764519e-89 
confintUpper1_LifeSpan_modelW 
confintLower1_LifeSpan_modelW 

confintUpper1_LifeSpan_modelW[3] # Weight =    -0.546315 
confintLower1_LifeSpan_modelW[3]  #Weight = -0.6649018 
 

# 
#LifeSpan - All 234 Small Individuals Normalized with weight as covariate

model1_LifeSpan_modelW_small <- glmmkin(LifeSpan ~ FROH + Weight, data = all_data_small
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelW_small$theta
model1_LifeSpan_modelW_small$coefficients
model1_LifeSpan_modelW_small$cov

#######FOR FROH#######
LogBeta1_LifeSpan_modelW_small  = model1_LifeSpan_modelW_small$coef[2]
oddsratio1_LifeSpan_modelW_small  = exp(LogBeta1_LifeSpan_modelW_small )
LogBetaStandardized1_LifeSpan_modelW_small  = model1_LifeSpan_modelW_small$coef[2]/sqrt(model1_LifeSpan_modelW_small$cov[2,2])

LogisticPval1_LifeSpan_modelW_small  = ifelse(model1_LifeSpan_modelW_small$coef[2]/sqrt(model1_LifeSpan_modelW_small$cov[2,2])<0, pnorm(model1_LifeSpan_modelW_small$coef[2]/sqrt(model1_LifeSpan_modelW_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW_small$coef[2]/sqrt(model1_LifeSpan_modelW_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW_small  = model1_LifeSpan_modelW_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW_small$cov))
confintLower1_LifeSpan_modelW_small = model1_LifeSpan_modelW_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW_small$cov))

LogBeta1_LifeSpan_modelW_small  #0.1153463  
LogBetaStandardized1_LifeSpan_modelW_small  #1.563337  
LogisticPval1_LifeSpan_modelW_small  #0.1179733   
confintUpper1_LifeSpan_modelW_small 
confintLower1_LifeSpan_modelW_small 

confintUpper1_LifeSpan_modelW_small[2] #FROH = 0.2599591  
confintLower1_LifeSpan_modelW_small[2]  #FROH = -0.0292666 
   

#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelW_Weight_small  = model1_LifeSpan_modelW_small$coef[3]
oddsratio1_LifeSpan_modelW_Weight_small  = exp(LogBeta1_LifeSpan_modelW_Weight_small )
LogBetaStandardized1_LifeSpan_modelW_Weight_small  = model1_LifeSpan_modelW_small$coef[3]/sqrt(model1_LifeSpan_modelW_small$cov[3,3])

LogisticPval1_LifeSpan_modelW_Weight_small  = ifelse(model1_LifeSpan_modelW_small$coef[3]/sqrt(model1_LifeSpan_modelW_small$cov[3,3])<0, pnorm(model1_LifeSpan_modelW_small$coef[3]/sqrt(model1_LifeSpan_modelW_small$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW_small$coef[3]/sqrt(model1_LifeSpan_modelW_small$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW_Weight_small  = model1_LifeSpan_modelW_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW_small$cov))
confintLower1_LifeSpan_modelW_Weight_small = model1_LifeSpan_modelW_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW_small$cov))

LogBeta1_LifeSpan_modelW_Weight_small  #-0.01737661 
LogBetaStandardized1_LifeSpan_modelW_Weight_small  #-0.610978 
LogisticPval1_LifeSpan_modelW_Weight_small  #0.5412141  
confintUpper1_LifeSpan_modelW_small
confintLower1_LifeSpan_modelW_small 

confintUpper1_LifeSpan_modelW_small[3] # Weight =    0.03836705 
confintLower1_LifeSpan_modelW_small[3]  # Weight = -0.07312027 
  

#####################################################
#LifeSpan - All 232 large Individuals Normalized with weight as covariate

model1_LifeSpan_modelW_large <- glmmkin(LifeSpan ~ FROH + Weight, data = all_data_large
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelW_large$theta
model1_LifeSpan_modelW_large$coefficients
model1_LifeSpan_modelW_large$cov

#######FOR FROH#######
LogBeta1_LifeSpan_modelW_large  = model1_LifeSpan_modelW_large$coef[2]
oddsratio1_LifeSpan_modelW_large  = exp(LogBeta1_LifeSpan_modelW_large )
LogBetaStandardized1_LifeSpan_modelW_large  = model1_LifeSpan_modelW_large$coef[2]/sqrt(model1_LifeSpan_modelW_large$cov[2,2])

LogisticPval1_LifeSpan_modelW_large  = ifelse(model1_LifeSpan_modelW_large$coef[2]/sqrt(model1_LifeSpan_modelW_large$cov[2,2])<0, pnorm(model1_LifeSpan_modelW_large$coef[2]/sqrt(model1_LifeSpan_modelW_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW_large$coef[2]/sqrt(model1_LifeSpan_modelW_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW_large  = model1_LifeSpan_modelW_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW_large$cov))
confintLower1_LifeSpan_modelW_large = model1_LifeSpan_modelW_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW_large$cov))

LogBeta1_LifeSpan_modelW_large  #-0.2447967 
LogBetaStandardized1_LifeSpan_modelW_large  #-2.468505 
LogisticPval1_LifeSpan_modelW_large  #0.01356789    
confintUpper1_LifeSpan_modelW_large 
confintLower1_LifeSpan_modelW_large 

confintUpper1_LifeSpan_modelW_large[2] #FROH = -0.05042741 
confintLower1_LifeSpan_modelW_large[2]  #FROH = -0.4391661 
       

#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelW_Weight_large  = model1_LifeSpan_modelW_large$coef[3]
oddsratio1_LifeSpan_modelW_Weight_large  = exp(LogBeta1_LifeSpan_modelW_Weight_large )
LogBetaStandardized1_LifeSpan_modelW_Weight_large  = model1_LifeSpan_modelW_large$coef[3]/sqrt(model1_LifeSpan_modelW_large$cov[3,3])

LogisticPval1_LifeSpan_modelW_Weight_large  = ifelse(model1_LifeSpan_modelW_large$coef[3]/sqrt(model1_LifeSpan_modelW_large$cov[3,3])<0, pnorm(model1_LifeSpan_modelW_large$coef[3]/sqrt(model1_LifeSpan_modelW_large$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelW_large$coef[3]/sqrt(model1_LifeSpan_modelW_large$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelW_Weight_large  = model1_LifeSpan_modelW_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelW_large$cov))
confintLower1_LifeSpan_modelW_Weight_large = model1_LifeSpan_modelW_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelW_large$cov))

LogBeta1_LifeSpan_modelW_Weight_large  #-0.732298 
LogBetaStandardized1_LifeSpan_modelW_Weight_large  #-12.20726 
LogisticPval1_LifeSpan_modelW_Weight_large  #2.842989e-34 

confintUpper1_LifeSpan_modelW_large
confintLower1_LifeSpan_modelW_large 

confintUpper1_LifeSpan_modelW_large[3] # Weight =    -0.6147201 
confintLower1_LifeSpan_modelW_large[3]  # Weight = -0.8498759 
   

#####################################################
# With the interaction term as co variate - 466 All Individuals
model1_LifeSpan_modelA <- glmmkin(LifeSpan ~ FROH + Weight + FROH*Weight, data = all_data
                                ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelA$theta
model1_LifeSpan_modelA$coefficients
model1_LifeSpan_modelA$cov

LogBeta1_LifeSpan_modelA  = model1_LifeSpan_modelA$coef[2]
oddsratio1_LifeSpan_modelA  = exp(LogBeta1_LifeSpan_modelA )
LogBetaStandardized1_LifeSpan_modelA  = model1_LifeSpan_modelA$coef[2]/sqrt(model1_LifeSpan_modelA$cov[2,2])

LogisticPval1_LifeSpan_modelA  = ifelse(model1_LifeSpan_modelA$coef[2]/sqrt(model1_LifeSpan_modelA$cov[2,2])<0, pnorm(model1_LifeSpan_modelA$coef[2]/sqrt(model1_LifeSpan_modelA$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA$coef[2]/sqrt(model1_LifeSpan_modelA$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA  = model1_LifeSpan_modelA$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))
confintLower1_LifeSpan_modelA = model1_LifeSpan_modelA$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))

LogBeta1_LifeSpan_modelA  #0.3059172  
LogBetaStandardized1_LifeSpan_modelA  #3.902774  
LogisticPval1_LifeSpan_modelA  #9.509636e-05 
confintUpper1_LifeSpan_modelA 
confintLower1_LifeSpan_modelA 

confintUpper1_LifeSpan_modelA[2] #FROH=0.459551
confintLower1_LifeSpan_modelA[2]  #FROH = 0.1522835   


#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelA_Weight  = model1_LifeSpan_modelA$coef[3]
oddsratio1_LifeSpan_modelA_Weight  = exp(LogBeta1_LifeSpan_modelA_Weight )
LogBetaStandardized1_LifeSpan_modelA_Weight  = model1_LifeSpan_modelA$coef[3]/sqrt(model1_LifeSpan_modelA$cov[3,3])

LogisticPval1_LifeSpan_modelA_Weight  = ifelse(model1_LifeSpan_modelA$coef[3]/sqrt(model1_LifeSpan_modelA$cov[3,3])<0, pnorm(model1_LifeSpan_modelA$coef[3]/sqrt(model1_LifeSpan_modelA$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA$coef[3]/sqrt(model1_LifeSpan_modelA$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_Weight  = model1_LifeSpan_modelA$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))
confintLower1_LifeSpan_modelA_Weight = model1_LifeSpan_modelA$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))

LogBeta1_LifeSpan_modelA_Weight  #-0.2395484 
LogBetaStandardized1_LifeSpan_modelA_Weight  #-3.18155 
LogisticPval1_LifeSpan_modelA_Weight  #0.001464891  
confintUpper1_LifeSpan_modelA 
confintLower1_LifeSpan_modelA 

confintUpper1_LifeSpan_modelA[3]# Weight =    -0.09197415 
confintLower1_LifeSpan_modelA[3]  # Weight = -0.3871226 
 

#######FOR WEIGHT * FROH #######
LogBeta1_LifeSpan_modelA_WeightFROH  = model1_LifeSpan_modelA$coef[4]
oddsratio1_LifeSpan_modelA_WeightFROH  = exp(LogBeta1_LifeSpan_modelA_WeightFROH )
LogBetaStandardized1_LifeSpan_modelA_WeightFROH  = model1_LifeSpan_modelA$coef[4]/sqrt(model1_LifeSpan_modelA$cov[4,4])

LogisticPval1_LifeSpan_modelA_WeightFROH  = ifelse(model1_LifeSpan_modelA$coef[4]/sqrt(model1_LifeSpan_modelA$cov[4,4])<0, pnorm(model1_LifeSpan_modelA$coef[4]/sqrt(model1_LifeSpan_modelA$cov[4,4]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA$coef[4]/sqrt(model1_LifeSpan_modelA$cov[4,4]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_WeightFROH  = model1_LifeSpan_modelA$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))
confintLower1_LifeSpan_modelA_WeightFROH = model1_LifeSpan_modelA$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA$cov))

LogBeta1_LifeSpan_modelA_WeightFROH  #  -1.283368 
LogBetaStandardized1_LifeSpan_modelA_WeightFROH  #  -5.281364 
LogisticPval1_LifeSpan_modelA_WeightFROH  #1.282253e-07 
confintUpper1_LifeSpan_modelA 
confintLower1_LifeSpan_modelA 

confintUpper1_LifeSpan_modelA[4]# WeightFROH =      -0.807089 
confintLower1_LifeSpan_modelA[4]  # WeightFROH =  -1.759646 
   


# Small 234 individuals With the interaction term as co variate
model1_LifeSpan_modelA_small <- glmmkin(LifeSpan ~ FROH + Weight + FROH*Weight, data = all_data_small
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelA_small$theta
model1_LifeSpan_modelA_small$coefficients
model1_LifeSpan_modelA_small$cov

LogBeta1_LifeSpan_modelA_small  = model1_LifeSpan_modelA_small$coef[2]
oddsratio1_LifeSpan_modelA_small  = exp(LogBeta1_LifeSpan_modelA_small )
LogBetaStandardized1_LifeSpan_modelA_small  = model1_LifeSpan_modelA_small$coef[2]/sqrt(model1_LifeSpan_modelA_small$cov[2,2])

LogisticPval1_LifeSpan_modelA_small  = ifelse(model1_LifeSpan_modelA_small$coef[2]/sqrt(model1_LifeSpan_modelA_small$cov[2,2])<0, pnorm(model1_LifeSpan_modelA_small$coef[2]/sqrt(model1_LifeSpan_modelA_small$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_small$coef[2]/sqrt(model1_LifeSpan_modelA_small$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_small  = model1_LifeSpan_modelA_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))
confintLower1_LifeSpan_modelA_small = model1_LifeSpan_modelA_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))

LogBeta1_LifeSpan_modelA_small  #0.2434175   
LogBetaStandardized1_LifeSpan_modelA_small  #1.75596   
LogisticPval1_LifeSpan_modelA_small  #0.07909531     
confintUpper1_LifeSpan_modelA_small 
confintLower1_LifeSpan_modelA_small 

confintUpper1_LifeSpan_modelA_small[2] #FROH=0.5151197 
confintLower1_LifeSpan_modelA_small[2]  #FROH = -0.02828482 


#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelA_small_Weight  = model1_LifeSpan_modelA_small$coef[3]
oddsratio1_LifeSpan_modelA_small_Weight  = exp(LogBeta1_LifeSpan_modelA_small_Weight )
LogBetaStandardized1_LifeSpan_modelA_small_Weight  = model1_LifeSpan_modelA_small$coef[3]/sqrt(model1_LifeSpan_modelA_small$cov[3,3])

LogisticPval1_LifeSpan_modelA_small_Weight  = ifelse(model1_LifeSpan_modelA_small$coef[3]/sqrt(model1_LifeSpan_modelA_small$cov[3,3])<0, pnorm(model1_LifeSpan_modelA_small$coef[3]/sqrt(model1_LifeSpan_modelA_small$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_small$coef[3]/sqrt(model1_LifeSpan_modelA_small$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_small_Weight  = model1_LifeSpan_modelA_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))
confintLower1_LifeSpan_modelA_small_Weight = model1_LifeSpan_modelA_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))

LogBeta1_LifeSpan_modelA_small_Weight  #0.05826928 
LogBetaStandardized1_LifeSpan_modelA_small_Weight  #0.777636   
LogisticPval1_LifeSpan_modelA_small_Weight  #0.4367836  
confintUpper1_LifeSpan_modelA_small 
confintLower1_LifeSpan_modelA_small 

confintUpper1_LifeSpan_modelA_small[3]# Weight =    0.2051346     
confintLower1_LifeSpan_modelA_small[3]  # Weight = -0.08859607 

#######FOR WEIGHT * FROH #######
LogBeta1_LifeSpan_modelA_small_WeightFROH  = model1_LifeSpan_modelA_small$coef[4]
oddsratio1_LifeSpan_modelA_small_WeightFROH  = exp(LogBeta1_LifeSpan_modelA_small_WeightFROH )
LogBetaStandardized1_LifeSpan_modelA_small_WeightFROH  = model1_LifeSpan_modelA_small$coef[4]/sqrt(model1_LifeSpan_modelA_small$cov[4,4])

LogisticPval1_LifeSpan_modelA_small_WeightFROH  = ifelse(model1_LifeSpan_modelA_small$coef[4]/sqrt(model1_LifeSpan_modelA_small$cov[4,4])<0, pnorm(model1_LifeSpan_modelA_small$coef[4]/sqrt(model1_LifeSpan_modelA_small$cov[4,4]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_small$coef[4]/sqrt(model1_LifeSpan_modelA_small$cov[4,4]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_small_WeightFROH  = model1_LifeSpan_modelA_small$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))
confintLower1_LifeSpan_modelA_small_WeightFROH = model1_LifeSpan_modelA_small$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_small$cov))

LogBeta1_LifeSpan_modelA_small_WeightFROH  # -0.3189597 
LogBetaStandardized1_LifeSpan_modelA_small_WeightFROH  #  -1.091116 
LogisticPval1_LifeSpan_modelA_small_WeightFROH  #0.2752217    
confintUpper1_LifeSpan_modelA_small 
confintLower1_LifeSpan_modelA_small 

confintUpper1_LifeSpan_modelA_small[4]# WeightFROH =    0.2539958    
confintLower1_LifeSpan_modelA_small[4]  # WeightFROH =    -0.8919153 
  


# large 232 individuals With the interaction term as co variate
model1_LifeSpan_modelA_large <- glmmkin(LifeSpan ~ FROH + Weight + FROH*Weight, data = all_data_large
                                      ,kins = kinshipMat, id= "id",family = gaussian(link="identity"))

model1_LifeSpan_modelA_large$theta
model1_LifeSpan_modelA_large$coefficients
model1_LifeSpan_modelA_large$cov

LogBeta1_LifeSpan_modelA_large  = model1_LifeSpan_modelA_large$coef[2]
oddsratio1_LifeSpan_modelA_large  = exp(LogBeta1_LifeSpan_modelA_large )
LogBetaStandardized1_LifeSpan_modelA_large  = model1_LifeSpan_modelA_large$coef[2]/sqrt(model1_LifeSpan_modelA_large$cov[2,2])

LogisticPval1_LifeSpan_modelA_large  = ifelse(model1_LifeSpan_modelA_large$coef[2]/sqrt(model1_LifeSpan_modelA_large$cov[2,2])<0, pnorm(model1_LifeSpan_modelA_large$coef[2]/sqrt(model1_LifeSpan_modelA_large$cov[2,2]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_large$coef[2]/sqrt(model1_LifeSpan_modelA_large$cov[2,2]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_large  = model1_LifeSpan_modelA_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))
confintLower1_LifeSpan_modelA_large = model1_LifeSpan_modelA_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))

LogBeta1_LifeSpan_modelA_large  #0.08525623    
LogBetaStandardized1_LifeSpan_modelA_large  #0.583007 
LogisticPval1_LifeSpan_modelA_large  #0.5598886      
confintUpper1_LifeSpan_modelA_large 
confintLower1_LifeSpan_modelA_large 

confintUpper1_LifeSpan_modelA_large[2] #FROH=0.3718775  
confintLower1_LifeSpan_modelA_large[2]  #FROH = -0.201365 


#######FOR WEIGHT#######
LogBeta1_LifeSpan_modelA_large_Weight  = model1_LifeSpan_modelA_large$coef[3]
oddsratio1_LifeSpan_modelA_large_Weight  = exp(LogBeta1_LifeSpan_modelA_large_Weight )
LogBetaStandardized1_LifeSpan_modelA_large_Weight  = model1_LifeSpan_modelA_large$coef[3]/sqrt(model1_LifeSpan_modelA_large$cov[3,3])

LogisticPval1_LifeSpan_modelA_large_Weight  = ifelse(model1_LifeSpan_modelA_large$coef[3]/sqrt(model1_LifeSpan_modelA_large$cov[3,3])<0, pnorm(model1_LifeSpan_modelA_large$coef[3]/sqrt(model1_LifeSpan_modelA_large$cov[3,3]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_large$coef[3]/sqrt(model1_LifeSpan_modelA_large$cov[3,3]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_large_Weight  = model1_LifeSpan_modelA_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))
confintLower1_LifeSpan_modelA_large_Weight = model1_LifeSpan_modelA_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))

LogBeta1_LifeSpan_modelA_large_Weight  #-0.2576093 
LogBetaStandardized1_LifeSpan_modelA_large_Weight  #-1.537677 
LogisticPval1_LifeSpan_modelA_large_Weight  #0.1241277  
confintUpper1_LifeSpan_modelA_large 
confintLower1_LifeSpan_modelA_large 

confintUpper1_LifeSpan_modelA_large[3]# Weight =    0.0707525 
confintLower1_LifeSpan_modelA_large[3]  # Weight = -0.5859712 

#######FOR WEIGHT * FROH #######
LogBeta1_LifeSpan_modelA_large_WeightFROH  = model1_LifeSpan_modelA_large$coef[4]
oddsratio1_LifeSpan_modelA_large_WeightFROH  = exp(LogBeta1_LifeSpan_modelA_large_WeightFROH )
LogBetaStandardized1_LifeSpan_modelA_large_WeightFROH  = model1_LifeSpan_modelA_large$coef[4]/sqrt(model1_LifeSpan_modelA_large$cov[4,4])

LogisticPval1_LifeSpan_modelA_large_WeightFROH  = ifelse(model1_LifeSpan_modelA_large$coef[4]/sqrt(model1_LifeSpan_modelA_large$cov[4,4])<0, pnorm(model1_LifeSpan_modelA_large$coef[4]/sqrt(model1_LifeSpan_modelA_large$cov[4,4]),lower=TRUE)*2,pnorm(model1_LifeSpan_modelA_large$coef[4]/sqrt(model1_LifeSpan_modelA_large$cov[4,4]),lower=FALSE)*2)
confintUpper1_LifeSpan_modelA_large_WeightFROH  = model1_LifeSpan_modelA_large$coefficients + 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))
confintLower1_LifeSpan_modelA_large_WeightFROH = model1_LifeSpan_modelA_large$coefficients - 1.96*sqrt(diag(model1_LifeSpan_modelA_large$cov))

LogBeta1_LifeSpan_modelA_large_WeightFROH  #  -1.479784 
LogBetaStandardized1_LifeSpan_modelA_large_WeightFROH  #  -3.026994 
LogisticPval1_LifeSpan_modelA_large_WeightFROH  #0.002469987     
confintUpper1_LifeSpan_modelA_large 
confintLower1_LifeSpan_modelA_large 

confintUpper1_LifeSpan_modelA_large[4]# WeightFROH = -0.5216134 
confintLower1_LifeSpan_modelA_large[4]  # WeightFROH =     -2.437954 
   





































## Create a forest plot depending on how you want to represent!



#Creating a forest plot 
library(ggplot2)
Forest_plot <- data.frame(Phenotypes = c('Height', 'Weight','LifeSpan'), index = 1:3, 
                          Effect_Size= c(LogBeta1_height,LogBeta1_Weight,LogBeta1_LifeSpan),
                          lower=c(confintLower1_height[2],confintLower1_Weight[2],confintLower1_LifeSpan[2]),
                          upper=c(confintUpper1_height[2],confintUpper1_Weight[2],confintUpper1_LifeSpan[2]))

head(Forest_plot)
Plot_forest <- ggplot(data=Forest_plot, aes(y=index,x=Effect_Size,xmin=lower,xmax=upper)) + geom_point(size=4,color="orange") +geom_errorbarh(height=0.1)+
  scale_y_continuous(name = "",breaks = 1:nrow(Forest_plot), labels = Forest_plot$Phenotypes) + 
  labs(x='Effect Size', y = 'Phenotypes')    +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  coord_fixed(7) +
  theme_classic()

Plot_forest + theme(text=element_text(size=28), axis.text=element_text(size=28),
                    axis.title=element_text(size=28),
                    plot.title=element_text(size=28))

ggsave("Plot_forest.png", path="~/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Posters/Biology Student Research Showcase/Plots/",width = 15, height = 7.15 , dpi=300, units="in")


