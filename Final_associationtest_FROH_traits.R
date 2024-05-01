library(ggplot2)

FROH_confidenceIntervals <- read.csv("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/FROH_ConfidenceIntervals.csv")

Associationtest <- ggplot(FROH_confidenceIntervals, aes(x = Trait, y = LogBeta, colour = Significant)) +
  geom_hline(yintercept = 0, size = 1.25) + 
  geom_errorbar(aes(ymin=LowerBound, ymax=UpperBound), colour="gray40", width=0.25, size = 0.75) + 
  geom_point() + 
  coord_flip() +  
  scale_colour_manual(values = c("yes"= "red", "no"="black")) + 
  labs(x = "Trait", y="Effect Size ") +
  theme_bw() + 
  theme(axis.text.x = element_text( hjust= 0.5, vjust=1, size=12), 
        axis.text.y = element_text(size =12), 
        plot.title=element_text(size =12, face = "bold", hjust=0.5), 
        axis.title=element_text(size=15), 
        legend.position = "none") 
Associationtest


ggsave("Associationtest.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/AssociationTest_FROH_Trait/", width = 6.5, height = 7, dpi=600, units="in")
ggsave("Associationtest.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/AssociationTest_FROH_Trait/", width = 6.5, height = 7, units="in")



FROH_confidenceIntervals_quant <- read.csv("C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/CSV_XLS_files/FROH_ConfidenceIntervals_quantitiative.csv")

Associationtest_quantitative <- ggplot(FROH_confidenceIntervals_quant, aes(x = Trait, y = LogBeta, colour = Significant)) +
  geom_hline(yintercept = 0, size = 1.25) + 
  geom_errorbar(aes(ymin=LowerBound, ymax=UpperBound), colour="gray40", width=0.25, size = 0.75) + 
  geom_point() + 
  coord_flip() +  
  scale_colour_manual(values = c("yes"= "red", "no"="black")) + 
  labs(x = "Trait", y="Effect Size ") +
  theme_bw() + 
  theme(axis.text.x = element_text( hjust= 0.5, vjust=1, size=12), 
        axis.text.y = element_text(size =12), 
        plot.title=element_text(size =12, face = "bold", hjust=0.5), 
        axis.title=element_text(size=15), 
        legend.position = "none") 
Associationtest_quantitative


ggsave("Associationtest_quantititative.png", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/AssociationTest_FROH_Trait/", width = 6.5, height = 7, dpi=600, units="in")
ggsave("Associationtest_quantitative.eps", device = "eps", path="C:/Users/Sweetalana/OneDrive - The Pennsylvania State University/Desktop/Research Lab/722_Breeds/Plots_png/Final_Plots_558/AssociationTest_FROH_Trait/", width = 6.5, height = 7, units="in")
