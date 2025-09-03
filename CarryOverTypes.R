### SOS50_EOS50_SOS50 -----------------------------------------------------
rm(list=ls())
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(broom)
library(ggpointdensity)

TT_GOSIF1 <- fread("D:/0 Work/Work/3 BNU/Lab/2 Manuscripts/MS16 PheCarryover_correct/20250708 NEE appeal/1 Data analysis/0 Code and data preparation/Upload/CarryOver.csv")

phe_type <- TT_GOSIF1 %>%
  dplyr::mutate(
      PHE_SOS50_EOS50_SOS50 = case_when(
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 < 0.05 & R_EOS50_SOS50 > 0 & R_SOS50_EOS50 > 0 ~ "++",
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 < 0.05 & R_EOS50_SOS50 > 0 & R_SOS50_EOS50 < 0 ~ "+-",
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 < 0.05 & R_EOS50_SOS50 < 0 & R_SOS50_EOS50 > 0 ~ "-+",
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 < 0.05 & R_EOS50_SOS50 < 0 & R_SOS50_EOS50 < 0 ~ "--",
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 > 0.05 & R_EOS50_SOS50 > 0 ~ "+0",
      P_EOS50_SOS50 < 0.05 & P_SOS50_EOS50 > 0.05 & R_EOS50_SOS50 < 0 ~ "-0",
      P_EOS50_SOS50 > 0.05 & P_SOS50_EOS50 < 0.05 & R_SOS50_EOS50 > 0 ~ "0+",
      P_EOS50_SOS50 > 0.05 & P_SOS50_EOS50 < 0.05 & R_SOS50_EOS50 < 0 ~ "0-",
      TRUE ~ "00"))

##### carryover types -------------------------------------------------
TT_GOSIF <- phe_type
ylgn_colors <- c("#46327e", "#3d4e8a", "#31688e", "#277f8e", "#1fa187", "#3fbc73","#5ec962","#fde725","gray65")
TT_GOSIF <- subset(TT_GOSIF, PHE_SOS50_EOS50_SOS50 != "00")
TT_GOSIF$PHE_SOS50_EOS50_SOS50<-factor(TT_GOSIF$PHE_SOS50_EOS50_SOS50,levels=c("+0",  "0-",  "0+", "+-", "++",  "-0","--", "-+"))

g1 <- ggplot(data = TT_GOSIF, mapping = aes(x = PHE_SOS50_EOS50_SOS50,fill = PHE_SOS50_EOS50_SOS50)) +
  geom_bar(stat = 'count',position = position_dodge(0.8),width = 0.7)+
  labs(x="Phenological carryover types",y="Frequency (%)")+
  coord_cartesian(ylim = c(540, 11500))+
  scale_y_continuous(breaks = seq(0, 11500, 2003),
                     labels = c("0","5","10","15","20","25"))+
  # scale_x_discrete(labels=c("++", "+-", "+0","-+","--", "-0","0+", "0-","00"))+
  scale_fill_manual(values = ylgn_colors) +
  ggtitle("(b)")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text   =element_text(size=24,family="sans"),
        axis.title=element_text(size=24,family="sans"),
        axis.text.x =element_text(size=24,family="sans"),
        legend.text =element_text(size=20,family="sans"),
        legend.title=element_text(size=20,family="sans"),
        plot.title = element_text(family = "sans", size = 24, face="bold"))+
  theme(legend.title=element_blank())+
  theme(legend.position="none")+
  annotate("text", x = 5.25, y = 10000, label = "significant: 37%", color = "black", size = 8,family="sans")+
  annotate("text", x = 5.7, y = 9000, label = "non-significant: 63%", color = "black", size = 8,family="sans")


##### relationship between the two carryover types ----------------------------------------------------
TT_GOSIF <- phe_type
g2 <- ggplot(TT_GOSIF,aes(x = R_EOS50_SOS50, y = R_SOS50_EOS50))+
  geom_pointdensity()+

  scale_color_gradientn(name = "count",
                        colors = c("#424086", "#3b528b", "#33638d", "#2c728e", "#21918c", "#3fbc73", "#fde725"),
                        values = c(0, 0.03, 0.05, 0.15, 0.3, 0.6, 1),
                        breaks = c(5000, 15000, 25000, 30000))+
  ggtitle("(a)") +
  coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(-4, 4)) +
  scale_x_continuous(breaks = seq(-4, 4, 2)) +
  scale_y_continuous(breaks = seq(-4, 4, 2)) +
  theme() +
  theme_bw() +
  labs(x="Effect of SOS50 on EOS50",y="Effect of EOS50 on SOS50")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.6) + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.6)+
  theme(panel.grid = element_blank()) + 
  theme(plot.title   = element_text(family = "sans", size = 24, face="bold"),
        axis.text    = element_text(size = 24, family = "sans"),
        axis.title.x = element_text(size = 24, family = "sans"),
        axis.title.y = element_text(size = 24, family = "sans"),
        legend.text  = element_text(size = 20, family = "sans"),
        legend.title = element_text(size = 20, family = "sans"),
        legend.position = c(0.85, 0.15))+
  annotate("text", x = 4, y = 2, label = "++ (37%)", size = 8, color = "black", hjust = 1, family="sans")+
  annotate("text", x = 4, y = -1.5, label = "+- (47%)", size = 8, color = "black", hjust = 1, family="sans")+
  annotate("text", x = -2, y = -2, label = "-- (8%)", size = 8, color = "black", hjust = 1, family="sans")+
  annotate("text", x = -2, y = 2, label = "-+ (8%)", size = 8, color = "black", hjust = 1, family="sans")

##### output figure-----------------------------------------------
Fig.3 <- ggarrange (g1, g2, ncol = 2,nrow = 1, common.legend = F,
                     font.label = list(size = 24,face = "bold",family="sans"))
ggsave("Fig.3.jpg", width = 15, height = 7.5)
