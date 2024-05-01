library(qqman, lib.loc = "/storage/group/zps5164/default/bin/.R")
library(coda)

setwd("/storage/group/zps5164/default/sms8526/722g_Breeds_Project/Final_pvalues_phenotypes/Normalized_LifeSpan_pvalues/Final_chunks_pvalues/")

Chr1 <- read.table("Chr1_chunks_pvalues.txt")
Chr2 <- read.table("Chr2_chunks_pvalues.txt")
Chr3 <- read.table("Chr3_chunks_pvalues.txt")
Chr4 <- read.table("Chr4_chunks_pvalues.txt")
Chr5 <- read.table("Chr5_chunks_pvalues.txt")
Chr6 <- read.table("Chr6_chunks_pvalues.txt")
Chr7 <- read.table("Chr7_chunks_pvalues.txt")
Chr8 <- read.table("Chr8_chunks_pvalues.txt")
Chr9 <- read.table("Chr9_chunks_pvalues.txt")
Chr10 <- read.table("Chr10_chunks_pvalues.txt")
Chr11 <- read.table("Chr11_chunks_pvalues.txt")
Chr12 <- read.table("Chr12_chunks_pvalues.txt")
Chr13 <- read.table("Chr13_chunks_pvalues.txt")
Chr14 <- read.table("Chr14_chunks_pvalues.txt")
Chr15 <- read.table("Chr15_chunks_pvalues.txt")
Chr16 <- read.table("Chr16_chunks_pvalues.txt")
Chr17 <- read.table("Chr17_chunks_pvalues.txt")
Chr18 <- read.table("Chr18_chunks_pvalues.txt")
Chr19 <- read.table("Chr19_chunks_pvalues.txt")
Chr20 <- read.table("Chr20_chunks_pvalues.txt")
Chr21 <- read.table("Chr21_chunks_pvalues.txt")
Chr22 <- read.table("Chr22_chunks_pvalues.txt")
Chr23 <- read.table("Chr23_chunks_pvalues.txt")
Chr24 <- read.table("Chr24_chunks_pvalues.txt")
Chr25 <- read.table("Chr25_chunks_pvalues.txt")
Chr26 <- read.table("Chr26_chunks_pvalues.txt")
Chr27 <- read.table("Chr27_chunks_pvalues.txt")
Chr28 <- read.table("Chr28_chunks_pvalues.txt")
Chr29 <- read.table("Chr29_chunks_pvalues.txt")
Chr30 <- read.table("Chr30_chunks_pvalues.txt")
Chr31 <- read.table("Chr31_chunks_pvalues.txt")
Chr32 <- read.table("Chr32_chunks_pvalues.txt")
Chr33 <- read.table("Chr33_chunks_pvalues.txt")
Chr34 <- read.table("Chr34_chunks_pvalues.txt")
Chr35 <- read.table("Chr35_chunks_pvalues.txt")
Chr36 <- read.table("Chr36_chunks_pvalues.txt")
Chr37 <- read.table("Chr37_chunks_pvalues.txt")
Chr38 <- read.table("Chr38_chunks_pvalues.txt")


Chr1$SNP <- row.names(Chr1)
Chr2$SNP <- row.names(Chr2)
Chr3$SNP <- row.names(Chr3)
Chr4$SNP <- row.names(Chr4)
Chr5$SNP <- row.names(Chr5)
Chr6$SNP <- row.names(Chr6)
Chr7$SNP <- row.names(Chr7)
Chr8$SNP <- row.names(Chr8)
Chr9$SNP <- row.names(Chr9)
Chr10$SNP <- row.names(Chr10)
Chr11$SNP <- row.names(Chr11)
Chr12$SNP <- row.names(Chr12)
Chr13$SNP <- row.names(Chr13)
Chr14$SNP <- row.names(Chr14)
Chr15$SNP <- row.names(Chr15)
Chr16$SNP <- row.names(Chr16)
Chr17$SNP <- row.names(Chr17)
Chr18$SNP <- row.names(Chr18)
Chr19$SNP <- row.names(Chr19)
Chr20$SNP <- row.names(Chr20)
Chr21$SNP <- row.names(Chr21)
Chr22$SNP <- row.names(Chr22)
Chr23$SNP <- row.names(Chr23)
Chr24$SNP <- row.names(Chr24)
Chr25$SNP <- row.names(Chr25)
Chr26$SNP <- row.names(Chr26)
Chr27$SNP <- row.names(Chr27)
Chr28$SNP <- row.names(Chr28)
Chr29$SNP <- row.names(Chr29)
Chr30$SNP <- row.names(Chr30)
Chr31$SNP <- row.names(Chr31)
Chr32$SNP <- row.names(Chr32)
Chr33$SNP <- row.names(Chr33)
Chr34$SNP <- row.names(Chr34)
Chr35$SNP <- row.names(Chr35)
Chr36$SNP <- row.names(Chr36)
Chr37$SNP <- row.names(Chr37)
Chr38$SNP <- row.names(Chr38)

All_Chr_pvalues <- rbind(Chr1,Chr2,Chr3,Chr4,Chr5,Chr6,Chr7,Chr8,Chr9,Chr10,Chr11,Chr12,Chr13,Chr14,Chr15,Chr16,Chr17,Chr18,Chr19,Chr20,Chr21,Chr22,Chr23,Chr24,Chr25,Chr26,Chr27,Chr28,Chr29,Chr30,Chr31,Chr32,Chr33,Chr34,Chr35,Chr36,Chr37,Chr38)

class(All_Chr_pvalues$P)
typeof(All_Chr_pvalues$P)
colnames(All_Chr_pvalues)

All_Chr_pvalues <- All_Chr_pvalues[cumsum(rle(All_Chr_pvalues$P)$lengths),] 

Effectivesize <- effectiveSize(All_Chr_pvalues$P)
Effectivesize

SuggestiveSig <- 0.1/Effectivesize
GenomeWideSig <- 0.05/Effectivesize

SuggestiveSig
GenomeWideSig

logvalsuggestive <- -log10(SuggestiveSig)
logvalgenomewide <- -log10(GenomeWideSig)

logvalgenomewide
logvalsuggestive

chisq <- qchisq(1-All_Chr_pvalues$P,1)

lamda <- median(chisq)/qchisq(0.5,1)
lamda

All_Chr_pvalues$P <- All_Chr_pvalues$P^(1/lamda)

All_Chr_pvalues_significant <- All_Chr_pvalues
All_Chr_pvalues_significant$logP <- -log10(All_Chr_pvalues_significant$P)

All_Chr_pvalues_significant_abovegenomewide <- subset(All_Chr_pvalues_significant, logP > logvalgenomewide) 
All_Chr_pvalues_significant_abovesuggestive <- subset(All_Chr_pvalues_significant, logP > logvalsuggestive)


snpsOfInterest <- All_Chr_pvalues_significant_abovegenomewide
GenomeWideSig_All <- GenomeWideSig
SuggestiveSig_All <- SuggestiveSig
All_Chr_pvalues_All <- All_Chr_pvalues


setwd("/storage/group/zps5164/default/sms8526/722g_Breeds_Project/Final_pvalues_phenotypes/Normalized_LifeSpan_pvalues/Final_chunks_pvalues_small/")

Chr1 <- read.table("Chr1_chunks_pvalues.txt")
Chr2 <- read.table("Chr2_chunks_pvalues.txt")
Chr3 <- read.table("Chr3_chunks_pvalues.txt")
Chr4 <- read.table("Chr4_chunks_pvalues.txt")
Chr5 <- read.table("Chr5_chunks_pvalues.txt")
Chr6 <- read.table("Chr6_chunks_pvalues.txt")
Chr7 <- read.table("Chr7_chunks_pvalues.txt")
Chr8 <- read.table("Chr8_chunks_pvalues.txt")
Chr9 <- read.table("Chr9_chunks_pvalues.txt")
Chr10 <- read.table("Chr10_chunks_pvalues.txt")
Chr11 <- read.table("Chr11_chunks_pvalues.txt")
Chr12 <- read.table("Chr12_chunks_pvalues.txt")
Chr13 <- read.table("Chr13_chunks_pvalues.txt")
Chr14 <- read.table("Chr14_chunks_pvalues.txt")
Chr15 <- read.table("Chr15_chunks_pvalues.txt")
Chr16 <- read.table("Chr16_chunks_pvalues.txt")
Chr17 <- read.table("Chr17_chunks_pvalues.txt")
Chr18 <- read.table("Chr18_chunks_pvalues.txt")
Chr19 <- read.table("Chr19_chunks_pvalues.txt")
Chr20 <- read.table("Chr20_chunks_pvalues.txt")
Chr21 <- read.table("Chr21_chunks_pvalues.txt")
Chr22 <- read.table("Chr22_chunks_pvalues.txt")
Chr23 <- read.table("Chr23_chunks_pvalues.txt")
Chr24 <- read.table("Chr24_chunks_pvalues.txt")
Chr25 <- read.table("Chr25_chunks_pvalues.txt")
Chr26 <- read.table("Chr26_chunks_pvalues.txt")
Chr27 <- read.table("Chr27_chunks_pvalues.txt")
Chr28 <- read.table("Chr28_chunks_pvalues.txt")
Chr29 <- read.table("Chr29_chunks_pvalues.txt")
Chr30 <- read.table("Chr30_chunks_pvalues.txt")
Chr31 <- read.table("Chr31_chunks_pvalues.txt")
Chr32 <- read.table("Chr32_chunks_pvalues.txt")
Chr33 <- read.table("Chr33_chunks_pvalues.txt")
Chr34 <- read.table("Chr34_chunks_pvalues.txt")
Chr35 <- read.table("Chr35_chunks_pvalues.txt")
Chr36 <- read.table("Chr36_chunks_pvalues.txt")
Chr37 <- read.table("Chr37_chunks_pvalues.txt")
Chr38 <- read.table("Chr38_chunks_pvalues.txt")


Chr1$SNP <- row.names(Chr1)
Chr2$SNP <- row.names(Chr2)
Chr3$SNP <- row.names(Chr3)
Chr4$SNP <- row.names(Chr4)
Chr5$SNP <- row.names(Chr5)
Chr6$SNP <- row.names(Chr6)
Chr7$SNP <- row.names(Chr7)
Chr8$SNP <- row.names(Chr8)
Chr9$SNP <- row.names(Chr9)
Chr10$SNP <- row.names(Chr10)
Chr11$SNP <- row.names(Chr11)
Chr12$SNP <- row.names(Chr12)
Chr13$SNP <- row.names(Chr13)
Chr14$SNP <- row.names(Chr14)
Chr15$SNP <- row.names(Chr15)
Chr16$SNP <- row.names(Chr16)
Chr17$SNP <- row.names(Chr17)
Chr18$SNP <- row.names(Chr18)
Chr19$SNP <- row.names(Chr19)
Chr20$SNP <- row.names(Chr20)
Chr21$SNP <- row.names(Chr21)
Chr22$SNP <- row.names(Chr22)
Chr23$SNP <- row.names(Chr23)
Chr24$SNP <- row.names(Chr24)
Chr25$SNP <- row.names(Chr25)
Chr26$SNP <- row.names(Chr26)
Chr27$SNP <- row.names(Chr27)
Chr28$SNP <- row.names(Chr28)
Chr29$SNP <- row.names(Chr29)
Chr30$SNP <- row.names(Chr30)
Chr31$SNP <- row.names(Chr31)
Chr32$SNP <- row.names(Chr32)
Chr33$SNP <- row.names(Chr33)
Chr34$SNP <- row.names(Chr34)
Chr35$SNP <- row.names(Chr35)
Chr36$SNP <- row.names(Chr36)
Chr37$SNP <- row.names(Chr37)
Chr38$SNP <- row.names(Chr38)

All_Chr_pvalues <- rbind(Chr1,Chr2,Chr3,Chr4,Chr5,Chr6,Chr7,Chr8,Chr9,Chr10,Chr11,Chr12,Chr13,Chr14,Chr15,Chr16,Chr17,Chr18,Chr19,Chr20,Chr21,Chr22,Chr23,Chr24,Chr25,Chr26,Chr27,Chr28,Chr29,Chr30,Chr31,Chr32,Chr33,Chr34,Chr35,Chr36,Chr37,Chr38)

class(All_Chr_pvalues$P)
typeof(All_Chr_pvalues$P)
colnames(All_Chr_pvalues)

All_Chr_pvalues <- All_Chr_pvalues[cumsum(rle(All_Chr_pvalues$P)$lengths),] 

Effectivesize <- effectiveSize(All_Chr_pvalues$P)
Effectivesize

SuggestiveSig <- 0.1/Effectivesize
GenomeWideSig <- 0.05/Effectivesize

SuggestiveSig
GenomeWideSig

logvalsuggestive <- -log10(SuggestiveSig)
logvalgenomewide <- -log10(GenomeWideSig)

logvalgenomewide
logvalsuggestive

chisq <- qchisq(1-All_Chr_pvalues$P,1)

lamda <- median(chisq)/qchisq(0.5,1)
lamda

All_Chr_pvalues$P <- All_Chr_pvalues$P^(1/lamda)

All_Chr_pvalues_significant <- All_Chr_pvalues
All_Chr_pvalues_significant$logP <- -log10(All_Chr_pvalues_significant$P)

All_Chr_pvalues_significant_abovegenomewide <- subset(All_Chr_pvalues_significant, logP > logvalgenomewide) 
All_Chr_pvalues_significant_abovesuggestive <- subset(All_Chr_pvalues_significant, logP > logvalsuggestive)


snpsOfInterest <- All_Chr_pvalues_significant_abovegenomewide

GenomeWideSig_small <- GenomeWideSig
SuggestiveSig_small <- SuggestiveSig
All_Chr_pvalues_small <- All_Chr_pvalues

setwd("/storage/group/zps5164/default/sms8526/722g_Breeds_Project/Final_pvalues_phenotypes/Normalized_LifeSpan_pvalues/Final_chunks_pvalues_large/")

Chr1 <- read.table("Chr1_chunks_pvalues.txt")
Chr2 <- read.table("Chr2_chunks_pvalues.txt")
Chr3 <- read.table("Chr3_chunks_pvalues.txt")
Chr4 <- read.table("Chr4_chunks_pvalues.txt")
Chr5 <- read.table("Chr5_chunks_pvalues.txt")
Chr6 <- read.table("Chr6_chunks_pvalues.txt")
Chr7 <- read.table("Chr7_chunks_pvalues.txt")
Chr8 <- read.table("Chr8_chunks_pvalues.txt")
Chr9 <- read.table("Chr9_chunks_pvalues.txt")
Chr10 <- read.table("Chr10_chunks_pvalues.txt")
Chr11 <- read.table("Chr11_chunks_pvalues.txt")
Chr12 <- read.table("Chr12_chunks_pvalues.txt")
Chr13 <- read.table("Chr13_chunks_pvalues.txt")
Chr14 <- read.table("Chr14_chunks_pvalues.txt")
Chr15 <- read.table("Chr15_chunks_pvalues.txt")
Chr16 <- read.table("Chr16_chunks_pvalues.txt")
Chr17 <- read.table("Chr17_chunks_pvalues.txt")
Chr18 <- read.table("Chr18_chunks_pvalues.txt")
Chr19 <- read.table("Chr19_chunks_pvalues.txt")
Chr20 <- read.table("Chr20_chunks_pvalues.txt")
Chr21 <- read.table("Chr21_chunks_pvalues.txt")
Chr22 <- read.table("Chr22_chunks_pvalues.txt")
Chr23 <- read.table("Chr23_chunks_pvalues.txt")
Chr24 <- read.table("Chr24_chunks_pvalues.txt")
Chr25 <- read.table("Chr25_chunks_pvalues.txt")
Chr26 <- read.table("Chr26_chunks_pvalues.txt")
Chr27 <- read.table("Chr27_chunks_pvalues.txt")
Chr28 <- read.table("Chr28_chunks_pvalues.txt")
Chr29 <- read.table("Chr29_chunks_pvalues.txt")
Chr30 <- read.table("Chr30_chunks_pvalues.txt")
Chr31 <- read.table("Chr31_chunks_pvalues.txt")
Chr32 <- read.table("Chr32_chunks_pvalues.txt")
Chr33 <- read.table("Chr33_chunks_pvalues.txt")
Chr34 <- read.table("Chr34_chunks_pvalues.txt")
Chr35 <- read.table("Chr35_chunks_pvalues.txt")
Chr36 <- read.table("Chr36_chunks_pvalues.txt")
Chr37 <- read.table("Chr37_chunks_pvalues.txt")
Chr38 <- read.table("Chr38_chunks_pvalues.txt")


Chr1$SNP <- row.names(Chr1)
Chr2$SNP <- row.names(Chr2)
Chr3$SNP <- row.names(Chr3)
Chr4$SNP <- row.names(Chr4)
Chr5$SNP <- row.names(Chr5)
Chr6$SNP <- row.names(Chr6)
Chr7$SNP <- row.names(Chr7)
Chr8$SNP <- row.names(Chr8)
Chr9$SNP <- row.names(Chr9)
Chr10$SNP <- row.names(Chr10)
Chr11$SNP <- row.names(Chr11)
Chr12$SNP <- row.names(Chr12)
Chr13$SNP <- row.names(Chr13)
Chr14$SNP <- row.names(Chr14)
Chr15$SNP <- row.names(Chr15)
Chr16$SNP <- row.names(Chr16)
Chr17$SNP <- row.names(Chr17)
Chr18$SNP <- row.names(Chr18)
Chr19$SNP <- row.names(Chr19)
Chr20$SNP <- row.names(Chr20)
Chr21$SNP <- row.names(Chr21)
Chr22$SNP <- row.names(Chr22)
Chr23$SNP <- row.names(Chr23)
Chr24$SNP <- row.names(Chr24)
Chr25$SNP <- row.names(Chr25)
Chr26$SNP <- row.names(Chr26)
Chr27$SNP <- row.names(Chr27)
Chr28$SNP <- row.names(Chr28)
Chr29$SNP <- row.names(Chr29)
Chr30$SNP <- row.names(Chr30)
Chr31$SNP <- row.names(Chr31)
Chr32$SNP <- row.names(Chr32)
Chr33$SNP <- row.names(Chr33)
Chr34$SNP <- row.names(Chr34)
Chr35$SNP <- row.names(Chr35)
Chr36$SNP <- row.names(Chr36)
Chr37$SNP <- row.names(Chr37)
Chr38$SNP <- row.names(Chr38)

All_Chr_pvalues <- rbind(Chr1,Chr2,Chr3,Chr4,Chr5,Chr6,Chr7,Chr8,Chr9,Chr10,Chr11,Chr12,Chr13,Chr14,Chr15,Chr16,Chr17,Chr18,Chr19,Chr20,Chr21,Chr22,Chr23,Chr24,Chr25,Chr26,Chr27,Chr28,Chr29,Chr30,Chr31,Chr32,Chr33,Chr34,Chr35,Chr36,Chr37,Chr38)

class(All_Chr_pvalues$P)
typeof(All_Chr_pvalues$P)
colnames(All_Chr_pvalues)

All_Chr_pvalues <- All_Chr_pvalues[cumsum(rle(All_Chr_pvalues$P)$lengths),] 

Effectivesize <- effectiveSize(All_Chr_pvalues$P)
Effectivesize

SuggestiveSig <- 0.1/Effectivesize
GenomeWideSig <- 0.05/Effectivesize

SuggestiveSig
GenomeWideSig

logvalsuggestive <- -log10(SuggestiveSig)
logvalgenomewide <- -log10(GenomeWideSig)

logvalgenomewide
logvalsuggestive

chisq <- qchisq(1-All_Chr_pvalues$P,1)

lamda <- median(chisq)/qchisq(0.5,1)
lamda

All_Chr_pvalues$P <- All_Chr_pvalues$P^(1/lamda)

All_Chr_pvalues_significant <- All_Chr_pvalues

All_Chr_pvalues_significant$logP <- -log10(All_Chr_pvalues_significant$P)

All_Chr_pvalues_significant_abovegenomewide <- subset(All_Chr_pvalues_significant, logP > logvalgenomewide) 
All_Chr_pvalues_significant_abovesuggestive <- subset(All_Chr_pvalues_significant, logP > logvalsuggestive)


snpsOfInterest <- All_Chr_pvalues_significant_abovegenomewide


GenomeWideSig_large <- GenomeWideSig
SuggestiveSig_large <- SuggestiveSig
All_Chr_pvalues_large <- All_Chr_pvalues

library(ggplot2,lib.loc = "/storage/group/zps5164/default/bin/.R")
library(ggpubr, lib.loc = "/storage/group/zps5164/default/bin/.R")
library(gridExtra, lib.loc= "/storage/group/zps5164/default/bin/.R")
library(gridGraphics,lib.loc= "/storage/group/zps5164/default/bin/.R")

plot1 <- function(){  par(mar = c(1,4, 1.5, 0.75))
manhattan(All_Chr_pvalues_All,chr = "CHR",
          bp = "BP",
          p = "P",
          genomewideline  = -log10(GenomeWideSig_All),suggestiveline = -log10(SuggestiveSig_All),col=c("Blue1","Coral 1"))
text( y = c(4.7,5.1,5.2,4.7), x=c(1500000000,1500009000,1890000000,1890000000), labels = c("ZXDC","TXNRD3","DRD3","TIGIT"), col = "purple", cex = 0.8)


}


plot2 <- function(){
  par(mar = c(1.5, 4, 1, 0.75))

manhattan(All_Chr_pvalues_small,chr = "CHR",
          bp = "BP",
          p = "P",
          genomewideline  = -log10(GenomeWideSig_small),suggestiveline = -log10(SuggestiveSig_small),col=c("Blue1","Coral 1"), xlab = "")
text( y = c(7.67), x=c(1050000900), labels = c("RUNX2"), col = "purple", cex = 0.8)

}

plot3 <- function(){  par(mar = c(5, 4, 3, 0.75))
manhattan(All_Chr_pvalues_large,chr = "CHR",
          bp = "BP",
          p = "P",
          genomewideline  = -log10(GenomeWideSig_large),suggestiveline = -log10(SuggestiveSig_large),col=c("Blue1","Coral 1"))
text(y=c(5.0,6.6,5.8,7.4), x=c(1555000900,1555000900,1555000900,1555000900),labels =c("SLC41A3","CFAP100","ZXDC","TXNRD3"), col = "purple", cex = 0.8)
}



options(bitmapType='cairo')
library(cowplot,lib.loc= "/storage/group/zps5164/default/bin/.R")

postscript(file= "/storage/group/zps5164/default/sms8526/722g_Breeds_Project/scripts/scripts_R_pvalues/Normalized_Manhattan_plots_newest/plots_and_tables/Manhattan_lifespan_grid.eps", height = 9, width = 6.5)
ggarrange(plot1, plot2, plot3, labels = c("A","B","C"), ncol =1, nrow = 3,
          plot_margin = unit(c(0,0,0,0), "cm"))+ bgcolor("white") 
par(mar = c(0, 0, 0, 0))
dev.off()

plot_png <- ggarrange(plot1, plot2, plot3, labels = c("A","B","C"), ncol =1, nrow = 3)+ bgcolor("white") 
ggsave("/storage/group/zps5164/default/sms8526/722g_Breeds_Project/scripts/scripts_R_pvalues/Normalized_Manhattan_plots_newest/plots_and_tables/Manhattan_lifespan_grid.png", plot_png,height = 9, width =6.5, dpi = 600, units ="in" )


