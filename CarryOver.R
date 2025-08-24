# Carry-over effect between SOS and EOS  ---------------------------------------
rm(list=ls())
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(broom)

TT_GOSIF <- fread("D:/0 Work/1 Data analysis/carryover_effect/CarryOver.csv") 


### SOS50_EOS50_SOS50 ----------------------------------------------------
#### EOS50_SOS50 -------------------------------------------------------
g2 <- ggplot()+
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           color = "transparent", fill = "lightgray", linewidth = 0.1)+
  geom_point(data = TT_GOSIF,aes(Lon, Lat, color = R_EOS50_SOS50),
             alpha = 1,size=0.2,shape = 15) +
  scale_color_gradient2(limits = c(-0.6, 0.6),low = "#2F5597"  , mid = "#FFFFE5",
                        high = "#F21A00",oob=scales::squish)+
  theme_minimal()+ 
  coord_cartesian(xlim=c(-175,175),ylim = c(-55,85))+
  scale_x_continuous(breaks = seq(-180,180, 60))+
  scale_y_continuous(breaks = seq(-50,50, 50))+
  ggtitle("(a) SOS50-EOS50")+
  theme(panel.grid=element_blank())+
  labs(x="Latitude",y=NULL,color = "Effect (days/day)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(size=18,family="sans",color = "white"),
        axis.title = element_text(size=22,family="sans",color = "white"),
        plot.title = element_text(family = "sans", size = 24, face="bold"),
        axis.title.y=element_text(size=22,family="sans"),
        legend.text =element_text(size=20,family="sans"),
        legend.title=element_text(size=20,family="sans", margin = margin(b = 20)),
        legend.key.size = unit(1.75, "lines"))+
  theme(legend.position = c(0.12, 0.28))

#### SOS50_EOS50 -------------------------------------------------------
g22 <- ggplot()+
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           color = "transparent", fill = "lightgray", linewidth = 0.1)+
  geom_point(data = TT_GOSIF,aes(Lon, Lat, color = R_SOS50_EOS50),
             alpha = 1,size=0.2,shape = 15) +
  scale_color_gradient2(limits = c(-0.6, 0.6),low = "#2F5597"  , mid = "#FFFFE5",
                        high = "#F21A00",oob=scales::squish)+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-175,175),ylim = c(-55,85))+
  scale_x_continuous(breaks = seq(-180,180, 60))+
  scale_y_continuous(breaks = seq(-50,50, 50))+
  ggtitle("(c) EOS50-SOS50")+
  theme(panel.grid=element_blank())+
  labs(x="Latitude",y=NULL,color = "Effect (days/day)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text =  element_text(size=18,family="sans",color = "white"),
        axis.title = element_text(size=22,family="sans",color = "white"),
        plot.title = element_text(family = "sans", size = 24, face="bold"),
        axis.title.y=element_text(size=22,family="sans"),
        legend.text =element_text(size=20,family="sans"),
        legend.title=element_text(size=20,family="sans", margin = margin(b = 20)),
        legend.key.size = unit(1.75, "lines"))+
  theme(legend.position = c(0.12, 0.28))

#### EOS50_SOS50 - SOS50_EOS50 ---------------------------------------------------------
TT_GOSIF$diff1 <- abs(TT_GOSIF$R_EOS50_SOS50)-abs(TT_GOSIF$R_SOS50_EOS50)

g222 <- ggplot()+
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           color = "transparent", fill = "lightgray", linewidth = 0.1)+
  geom_point(data = TT_GOSIF,aes(Lon, Lat, color = diff1),
             alpha = 1,size=0.2,shape = 15) +
  scale_color_gradient2(limits = c(-0.5, 0.5),low = "#2F5597"  , mid = "#FFFFE5",
                        high = "#F21A00",oob=scales::squish)+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-175,175),ylim = c(-55,85))+
  scale_x_continuous(breaks = seq(-180,180, 60))+
  scale_y_continuous(breaks = seq(-50,50, 50))+
  ggtitle("(e) Dominant carryover effect")+
  theme(panel.grid=element_blank())+
  labs(x="Latitude",y=NULL,color = "Effect difference")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(size=18,family="sans",color = "white"),
        axis.title = element_text(size=22,family="sans",color = "white"),
        plot.title = element_text(family = "sans", size = 24, face="bold"),
        axis.title.y=element_text(size=22,family="sans"),
        legend.text =element_text(size=20,family="sans"),
        legend.title=element_text(size=20,family="sans", margin = margin(b = 20)),
        legend.key.size = unit(1.75, "lines"))+
  theme(legend.position = c(0.12, 0.28))

#### Latitude variation --------------------------------------------------
data_t <- TT_GOSIF %>%
  mutate(Lat = cut(Lat, breaks = c(-55, -25, -20, -15, -10, -5, 0,
                                   2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,
                                   42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84),
                   labels = c(-40,-22.5,-17.5,-12.5,-7.5,-2.5,
                              1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,
                              41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83), right = FALSE)) %>%
  group_by(Lat) %>%
  dplyr::summarize(sample_count = n(),
                   mean_eos_c = mean(R_EOS50_SOS50),
                   sd_eos_c   = sd(R_EOS50_SOS50)/sqrt(n()),
                   mean_sos_c = mean(R_SOS50_EOS50),
                   sd_sos_c   = sd(R_SOS50_EOS50)/sqrt(n()),
                   mean_10_c = mean(diff1),
                   sd_10_c   = sd(diff1)/sqrt(n()))%>%
  filter(sample_count >= 50)

data_t <- apply(data_t, 2, as.numeric)
data_t <- as.data.frame(data_t)

g_2 <- ggplot(data_t, aes(x=Lat, y=mean_eos_c)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2, color = "gray")+
  geom_ribbon(aes(ymin = mean_eos_c-sd_eos_c, ymax = mean_eos_c+sd_eos_c), fill = "#FE8602", alpha = 0.3) +
  geom_line(aes(x=Lat, y = mean_eos_c), linewidth = 1.2, color = "darkorange1") +
  ylim(-0.05,0.4)+
  xlim(-55,85)+  
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 18, family = "sans"),
    axis.title = element_text(size=22,family="sans"),
    plot.title = element_text(family = "sans", size = 24, face = "bold"),
    axis.title.y = element_text(size = 18, family = "sans"),
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black") 
  ) +
  ggtitle("(b)") +
  coord_flip() +
  labs(x = NULL, y = "Effect (days/day)")

g_22 <- ggplot(data_t, aes(Lat,mean_sos_c)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2, color = "gray")+
  geom_ribbon(aes(ymin = mean_sos_c - sd_sos_c, ymax = mean_sos_c + sd_sos_c), fill = "#F21A00", alpha = 0.3) +  # 误差区间 for mean2
  geom_line(aes(y = mean_sos_c), linewidth = 1.2, color = "red")+
  ylim(-0.25,0.25)+
  xlim(-55,85)+  
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 18, family = "sans"),
    axis.title = element_text(size=22,family="sans"),
    plot.title = element_text(family = "sans", size = 24, face = "bold"),
    axis.title.y = element_text(size = 18, family = "sans"),
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "black"), 
    axis.line.y = element_line(color = "black")  
  ggtitle("(d)") +
  coord_flip() +
  labs(x = NULL, y = "Effect (days/day)")

g_222 <- ggplot(data_t, aes(x=Lat, y=mean_10_c)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2, color = "gray")+
  geom_ribbon(aes(ymin = mean_10_c-sd_10_c, ymax = mean_10_c+sd_10_c), fill = "blue", alpha = 0.3) +
  geom_line(aes(x=Lat, y = mean_10_c), linewidth = 1.2, color = "#2F5597") +
  ylim(-0.6,0.3)+
  xlim(-55,85)+  
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 18, family = "sans"),
    axis.title = element_text(size=22,family="sans"),
    plot.title = element_text(family = "sans", size = 24, face = "bold"),
    axis.title.y = element_text(size = 18, family = "sans"),
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "black"), 
    axis.line.y = element_line(color = "black")
  ) +
  ggtitle("(f)") +
  coord_flip() +
  labs(x = NULL, y = "Effect difference")

#### output figure --------------------------------------------------
plot2 <- grid.arrange(g2, g_2, ncol = 2, widths = c(0.8, 0.2))
plot22 <- grid.arrange(g22, g_22, ncol = 2, widths = c(0.8, 0.2))
plot222 <- grid.arrange(g222, g_222, ncol = 2, widths = c(0.8, 0.2))
Fig.2 <- ggarrange (plot2, plot22, plot222, ncol = 1,nrow = 3, common.legend = F,
                    font.label = list(size = 24,face = "bold",family="sans"))
ggsave("2 Fig.2d.jpg", width = 15, height = 20)
ggsave("2 Fig.2d.pdf", width = 15, height = 20)
