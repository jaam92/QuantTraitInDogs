#Without_outliers PER00393 and PER00747

library (ggplot2)
library(tidyr)

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
summary_VillageDogs <- read.table("roh_summary_VillageDogs_75.txt")
summary_ChineseIndigenousDog <- read.table("roh_summary_ChineseIndigenousDog_15.txt")

library (dplyr)

Allspecies722_summary <- rbind (summary_AncienctSpitzDogs, summary_AllScentHound, summary_AllSightHound, summary_Herding,
                                summary_MastiffLike, summary_Retrievers, 
                                summary_Smallterriers, summary_WorkingDogs, summary_Terriers,
                                summary_ToyDogs, summary_Spaniels, summary_VillageDogs, summary_ChineseIndigenousDog)

# Insert Ids at the end of the datasheet. 

Allspecies722_summary$id <- rownames(Allspecies722_summary) # To have the row names as a column at the end of the data table

# sort alphabetically based on id 
Allspecies722_summary <- Allspecies722_summary[order(Allspecies722_summary$id), ]

# Load a table for ids and their breedgroup and then sort alphabetically based on id 

Breedgroup_information_ids <- read.csv("../../CSV_XLS_files/Only_ids_breedgroup_568individuals.csv")
Breedgroup_information_ids <- Breedgroup_information_ids[order(Breedgroup_information_ids$id), ]

# Change breed group names to add space 
Breedgroup_information_ids$Breed.Group <- ifelse(Breedgroup_information_ids$Breed.Group == "ChineseIndigenousDogs", "Chinese Indigenous Dogs", ifelse(Breedgroup_information_ids$Breed.Group  == "SmallTerriers", "Small Terriers",ifelse(Breedgroup_information_ids$Breed.Group  == "VillageDogs", "Village Dogs",  ifelse(Breedgroup_information_ids$Breed.Group  == "ToyDogs", "Toy Dogs", ifelse(Breedgroup_information_ids$Breed.Group == "WorkingDog", "Working Dogs",Breedgroup_information_ids$Breed.Group)))))


#Add breed groups to Allspecied722 file
Allspecies722_summary <- cbind(Allspecies722_summary,Breedgroup_information_ids)

# NOTE: nA is number of class A ROH, and A is the length of class A ROH
# sROH = sum length ROH, nROH - number of ROH 



# We don't need ntotal and total, remove those from the dataset

allspecies722_df <- data.frame(Allspecies722_summary)[,-c(14:15,17,18)] # Removed the last two columns - ntotal and total 


# Reorder the columns
allspecies722_data_reordered <- allspecies722_df[,c(14,15, 1:13)]


# S = All the sum length ROH - We are subsetting the file with just Sum lengths of ROH 
allspecies722_S <- allspecies722_data_reordered[,c(1,2,5,7,9,11,13,15)]

# N = Number of ROH - We are subsetting the file with just number of ROH 
allspecies722_N <- allspecies722_data_reordered[,c(1,2,4,6,8,10,12,14)]


#This object below is converting columns to rows, class= column representing new variable which is different classes - less than 1 million, more than ... A,B,C,D,E,F
# Values = the actual value of A,B,C,D,E,F 
# Code: Output fule <- Input File %>% gather ( Column 1 new name , Column 2 new name , Actual Column names )


allspecies722_rows_S <- allspecies722_S %>% gather(Sclass,Svalues,A:F)
Mbp = 1000000
allspecies722_rows_S$Svalues <- allspecies722_rows_S$Svalues/Mbp

# The output allspecies_rows_N would show just with N = number of ROH - changing columns to rows 
allspecies722_rows_N <- allspecies722_N %>% gather(Nclass,Nvalues,nA:nF)

#Delete the outliers - PER00747 and PER393
allspecies722_rows_N <- allspecies722_rows_N[!(allspecies722_rows_N$id=="PER00747" | allspecies722_rows_N$id=="PER00393"),]
allspecies722_rows_S <- allspecies722_rows_S[!(allspecies722_rows_S$id=="PER00747" | allspecies722_rows_S$id=="PER00393"),]

library (dplyr)

library (ggplot2)

#  violin plots for only greater than 5 Mbp i.e F 
OnlyF<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "F",]
s722_final_without_outliers_OnlyF <- ggplot(OnlyF, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyF <- s722_final_without_outliers_OnlyF  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("F"=">5 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyF_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyF + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyF_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyF_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyF_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyF <- sROH_vs_ROHsize_class_without_outliers_OnlyF + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))  + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))


sROH_vs_ROHsize_class_without_outliers_OnlyF
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyF.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyF.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

#  violin plots for only greater for class E - nE 
OnlyE<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "E",]
s722_final_without_outliers_OnlyE <- ggplot(OnlyE, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyE <- s722_final_without_outliers_OnlyE  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("E"="4-5 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyE_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyE + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyE_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyE_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyE_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyE <- sROH_vs_ROHsize_class_without_outliers_OnlyE + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))   + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))


sROH_vs_ROHsize_class_without_outliers_OnlyE
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyE.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyE.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")


#  violin plots for only greater for class D - nD 
OnlyD<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "D",]
s722_final_without_outliers_OnlyD <- ggplot(OnlyD, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyD <- s722_final_without_outliers_OnlyD  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("D"="3-5 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyD_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyD + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyD_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyD_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyD_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyD <- sROH_vs_ROHsize_class_without_outliers_OnlyD + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))   + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))


sROH_vs_ROHsize_class_without_outliers_OnlyD
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyD.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyD.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

#  violin plots for only greater for class C - nC 
OnlyC<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "C",]
s722_final_without_outliers_OnlyC <- ggplot(OnlyC, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyC <- s722_final_without_outliers_OnlyC  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("C"="2-3 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyC_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyC + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyC_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyC_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyC_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyC <- sROH_vs_ROHsize_class_without_outliers_OnlyC + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))  + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))


sROH_vs_ROHsize_class_without_outliers_OnlyC
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyC.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyC.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

#  violin plots for only greater for class B - nB 
OnlyB<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "B",]
s722_final_without_outliers_OnlyB <- ggplot(OnlyB, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyB <- s722_final_without_outliers_OnlyB  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("B"="1-2 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyB_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyB + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyB_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyB_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyB_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyB <- sROH_vs_ROHsize_class_without_outliers_OnlyB + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))  + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))
 

sROH_vs_ROHsize_class_without_outliers_OnlyB
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyB.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyB.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")


#  violin plots for only greater for class A - nA 
OnlyA<- allspecies722_rows_S[allspecies722_rows_S$Sclass == "A",]
s722_final_without_outliers_OnlyA <- ggplot(OnlyA, aes(Sclass, Svalues, color=Breed.Group, fill = Breed.Group))

sROH_vs_ROHsize_class_without_outliers_OnlyA <- s722_final_without_outliers_OnlyA  +
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +
  scale_fill_manual("Categories",values=c("red",
                                          "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                          "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple"))+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) +  
  scale_x_discrete(labels=c("A"="0.5-1 Mbp")) + 
  xlab("ROH size") + ylab("sROH (Mbp)") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12))

sROH_vs_ROHsize_class_without_outliers_OnlyA_without_legend <- sROH_vs_ROHsize_class_without_outliers_OnlyA + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1), show.legend = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(1), show.legend = FALSE) 

sROH_vs_ROHsize_class_without_outliers_OnlyA_without_legend

#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyA_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("sROH_vs_ROHsize_class_without_outliers_OnlyA_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

sROH_vs_ROHsize_class_without_outliers_OnlyA <- sROH_vs_ROHsize_class_without_outliers_OnlyA + geom_violin(trim = F, alpha = 0.5, draw_quantiles =c(0.5), position = position_dodge(1)) +
  geom_boxplot(width = 0.1, position = position_dodge(1))  + guides(color = guide_legend(ncol = 4) , fill = guide_legend(ncol = 4))


sROH_vs_ROHsize_class_without_outliers_OnlyA

library(ggpubr)
png("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/sROH_vs_ROH_class_grid_with_legend.png", width = 6.6, height = 7, res=350, units="in")
ggarrange(sROH_vs_ROHsize_class_without_outliers_OnlyA, sROH_vs_ROHsize_class_without_outliers_OnlyB, sROH_vs_ROHsize_class_without_outliers_OnlyC, sROH_vs_ROHsize_class_without_outliers_OnlyD, sROH_vs_ROHsize_class_without_outliers_OnlyE, sROH_vs_ROHsize_class_without_outliers_OnlyF, ncol = 2,nrow=3, common.legend = TRUE, legend = "bottom")
dev.off()

postscript("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/sROH_vs_ROH_class_grid_with_legend.eps", width = 6.6, height = 7)
t<- ggarrange(sROH_vs_ROHsize_class_without_outliers_OnlyA, sROH_vs_ROHsize_class_without_outliers_OnlyB, sROH_vs_ROHsize_class_without_outliers_OnlyC, sROH_vs_ROHsize_class_without_outliers_OnlyD, sROH_vs_ROHsize_class_without_outliers_OnlyE, sROH_vs_ROHsize_class_without_outliers_OnlyF, ncol = 2,nrow=3, common.legend = TRUE, legend = "bottom")
t
dev.off()
