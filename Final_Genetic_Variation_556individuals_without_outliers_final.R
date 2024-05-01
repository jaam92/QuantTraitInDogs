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


#Add breed groups to Allspecied722 file
Allspecies722_summary <- cbind(Allspecies722_summary,Breedgroup_information_ids)

#creating a plot for sROH and nROH 
Allspecies722_summary <- Allspecies722_summary[!(Allspecies722_summary$id=="PER00747" | Allspecies722_summary$id=="PER00393"),]

Allspecies722_summary <- Allspecies722_summary[,c(14,15,17,19)]
Mbp = 1000000
Allspecies722_summary$TOTAL <- Allspecies722_summary$TOTAL/Mbp 

nROH_vs_sROH_without_outliers <- ggplot(Allspecies722_summary, aes(TOTAL, nTOTAL, color=Breed.Group,shape=Breed.Group)) +
  geom_point(size=3.5)

nROH_vs_sROH_without_outliers  <- nROH_vs_sROH_without_outliers + 
  theme_bw() + theme(panel.grid.major = element_blank())+ 
  scale_shape_manual("Categories",values=c(20,20,20,20,20,20,18,20,20,1,20,20,20))+
  scale_color_manual("Categories",values=c("red",
                                           "green2", "gray71", "black", "goldenrod1", "springgreen4",
                                           "aquamarine1","peachpuff4","deeppink1","turquoise2","hotpink","blue1","purple")) +  
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = "white"),
        legend.key = element_blank()) + 
  xlab("sROH (Mbp)") + ylab("nROH") + 
  theme(text=element_text(size=12), axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12),
        legend.text=element_text(size=12)) 

nROH_vs_sROH_without_outliers
nROH_vs_sROH_without_outliers + geom_abline(intercept = 0, slope = 1)

#ggsave("nROH_vs_sROH_without_outliers.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("nROH_vs_sROH_without_outliers.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")


nROH_vs_sROH_without_outliers_without_legend <- nROH_vs_sROH_without_outliers +  theme(legend.position = "null")
nROH_vs_sROH_without_outliers_without_legend 

nROH_vs_sROH_without_outliers_without_legend + geom_abline(intercept = 0, slope = 1)

#ggsave("nROH_vs_sROH_without_outliers_without_legend.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")
#ggsave("nROH_vs_sROH_without_outliers_without_legend.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 8.5, height = 11, dpi=350, units="in")

nROH_sROH_grid <- nROH_vs_sROH_without_outliers_without_legend +  geom_abline(intercept = 0, slope = 1)+ facet_wrap(~Breed.Group,labeller = as_labeller(c("ChineseIndigenousDogs" = "Chinese Indigenous", "WorkingDog" = "Working Dogs", "VillageDogs" = "Village Dogs", "ToyDogs" = "Toy Dogs", "Terriers"= "Terriers","Spaniel" = "Spaniels","SmallTerriers" = "Small Terriers","Scent Hound" = "Scent Hound",
                                                                                                                                                "Ancient Spitz" ="Ancient Spitz", "Retriever" = "Retrievers", "Sight Hound" = "Sight Hound", "Herding" = "Herding Dogs", "Mastiff-like" = "Mastiff-like")))
nROH_sROH_grid <- nROH_sROH_grid+ theme(strip.background = element_rect(fill = "azure"))

ggsave("nROH_sROH_grid.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 6.5, height = 7, dpi=600, units="in")
ggsave("nROH_sROH_grid.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/GeneticVariation_plots/Without PER00747 and PER00393 - 556 Individuals/", width = 6.5, height = 7, dpi=600, units="in")


