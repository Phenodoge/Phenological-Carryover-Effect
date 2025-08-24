# 1 extract meterology data based on mean date of phenophases ----------------------------------------------------------
rm(list=ls())
library(data.table)
library(dplyr)
library(ranger)
library(tictoc)

Phen0 <- fread("D:/0 Work/1 Data analysis/1 PhenologyClimateData/1_Pheno.csv")
Temp  <- readRDS("D:/0 Work/1 Data analysis/1 PhenologyClimateData/Temp_list.rds")
Prec  <- readRDS("D:/0 Work/1 Data analysis/1 PhenologyClimateData/Prec_list.rds")
Radi  <- readRDS("D:/0 Work/1 Data analysis/1 PhenologyClimateData/Radi_list.rds")

replace_values <- c(15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180)

for (value in replace_values) {
  Phen0[[paste0("Tair_f_inst_Mean_Greenup_", value)]] <- NA
  Phen0[[paste0("Tair_f_inst_Mean_MidGreenup_", value)]] <- NA
  Phen0[[paste0("Tair_f_inst_Mean_Maturity_", value)]] <- NA
  Phen0[[paste0("Tair_f_inst_Mean_Senescence_", value)]] <- NA
  Phen0[[paste0("Tair_f_inst_Mean_MidGreendown_", value)]] <- NA
  Phen0[[paste0("Tair_f_inst_Mean_Dormancy_", value)]] <- NA
  
  Phen0[[paste0("Rainf_f_tavg_Greenup_", value)]] <- NA
  Phen0[[paste0("Rainf_f_tavg_MidGreenup_", value)]] <- NA
  Phen0[[paste0("Rainf_f_tavg_Maturity_", value)]] <- NA
  Phen0[[paste0("Rainf_f_tavg_Senescence_", value)]] <- NA
  Phen0[[paste0("Rainf_f_tavg_MidGreendown_", value)]] <- NA
  Phen0[[paste0("Rainf_f_tavg_Dormancy_", value)]] <- NA
  
  Phen0[[paste0("Swnet_tavg_Greenup_", value)]] <- NA
  Phen0[[paste0("Swnet_tavg_MidGreenup_", value)]] <- NA
  Phen0[[paste0("Swnet_tavg_Maturity_", value)]] <- NA
  Phen0[[paste0("Swnet_tavg_Senescence_", value)]] <- NA
  Phen0[[paste0("Swnet_tavg_MidGreendown_", value)]] <- NA
  Phen0[[paste0("Swnet_tavg_Dormancy_", value)]] <- NA
}

### Extract climate ###
Phen_1 <- Phen0[Phen0$Dormancy_DOY_mean >= 365, ]
Phe_Mete <- as.data.frame(matrix(0,ncol=0,nrow = 0)) 

tic()
for (nian in unique(Phen_1$Year)){
  selecte_year <- subset(Phen_1, Year == nian)
  
  temp_data0 <- Temp[[paste0("Temp", nian-1)]]
  prec_data0 <- Prec[[paste0("Prec", nian-1)]]
  radi_data0 <- Radi[[paste0("Radi", nian-1)]]
  
  DOY <- dim(temp_data0)[2]-5
  
  temp_data1 <- Temp[[paste0("Temp", nian)]]
  prec_data1 <- Prec[[paste0("Prec", nian)]]
  radi_data1 <- Radi[[paste0("Radi", nian)]]
  
  temp_data <- temp_data0 %>%
    left_join(temp_data1, by = c("system.index", "Part", ".geo")) 
  
  prec_data <- prec_data0 %>%
    left_join(prec_data1, by = c("system.index", "Part", ".geo")) 
  
  radi_data <- radi_data0 %>%
    left_join(radi_data1, by = c("system.index", "Part", ".geo")) 
  
  temp_data <- temp_data[, !(names(temp_data) %in% c("system.index", "Part", "x.x", "y.x", ".geo"))]
  prec_data <- prec_data[, !(names(prec_data) %in% c("system.index", "Part", "x.x", "y.x", ".geo"))]
  radi_data <- radi_data[, !(names(radi_data) %in% c("system.index", "Part", "x.x", "y.x", ".geo"))]
  
  for (i in 1:nrow(selecte_year)) {
    lon <- round(selecte_year$Lon[i], 3)
    lat <- round(selecte_year$Lat[i], 3)
    year <- selecte_year$Year[i]
    
    Greenup      <- selecte_year$Greenup_DOY_mean[i]
    MidGreenup   <- selecte_year$MidGreenup_DOY_mean[i]
    Maturity     <- selecte_year$Maturity_DOY_mean[i]
    Senescence   <- selecte_year$Senesc_DOY_mean[i]
    MidGreendown <- selecte_year$MidGreendown_DOY_mean[i]
    Dormancy     <- selecte_year$Dormancy_DOY_mean[i]
    
    temp_row <- subset(temp_data, x.y == lon & y.y == lat)
    prec_row <- subset(prec_data, x.y == lon & y.y == lat)
    radi_row <- subset(radi_data, x.y == lon & y.y == lat)
    
    if (nrow(temp_row) == 1 & nrow(prec_row) == 1 & nrow(radi_row) == 1 ) {
      
      for (value in replace_values) {
        
        Tair_f_inst_Mean_Greenup      <- temp_row[, (Greenup      - value + DOY + 1):(Greenup      + DOY)]
        Tair_f_inst_Mean_MidGreenup   <- temp_row[, (MidGreenup   - value + DOY + 1):(MidGreenup   + DOY)]
        Tair_f_inst_Mean_Maturity     <- temp_row[, (Maturity     - value + DOY + 1):(Maturity     + DOY)]
        Tair_f_inst_Mean_Senescence   <- temp_row[, (Senescence   - value + DOY + 1):(Senescence   + DOY)]
        Tair_f_inst_Mean_MidGreendown <- temp_row[, (MidGreendown - value + DOY + 1):(MidGreendown + DOY)]
        Tair_f_inst_Mean_Dormancy     <- temp_row[, (Dormancy     - value + DOY + 1):(Dormancy     + DOY)]
        
        Rainf_f_tavg_Greenup          <- prec_row[, (Greenup      - value + DOY + 1):(Greenup      + DOY)]
        Rainf_f_tavg_MidGreenup       <- prec_row[, (MidGreenup   - value + DOY + 1):(MidGreenup   + DOY)]
        Rainf_f_tavg_Maturity         <- prec_row[, (Maturity     - value + DOY + 1):(Maturity     + DOY)]
        Rainf_f_tavg_Senescence       <- prec_row[, (Senescence   - value + DOY + 1):(Senescence   + DOY)]
        Rainf_f_tavg_MidGreendown     <- prec_row[, (MidGreendown - value + DOY + 1):(MidGreendown + DOY)]
        Rainf_f_tavg_Dormancy         <- prec_row[, (Dormancy     - value + DOY + 1):(Dormancy     + DOY)]
        
        Swnet_tavg_Greenup            <- radi_row[, (Greenup      - value + DOY + 1):(Greenup      + DOY)]
        Swnet_tavg_MidGreenup         <- radi_row[, (MidGreenup   - value + DOY + 1):(MidGreenup   + DOY)]
        Swnet_tavg_Maturity           <- radi_row[, (Maturity     - value + DOY + 1):(Maturity     + DOY)]
        Swnet_tavg_Senescence         <- radi_row[, (Senescence   - value + DOY + 1):(Senescence   + DOY)]
        Swnet_tavg_MidGreendown       <- radi_row[, (MidGreendown - value + DOY + 1):(MidGreendown + DOY)]
        Swnet_tavg_Dormancy           <- radi_row[, (Dormancy     - value + DOY + 1):(Dormancy     + DOY)]
        
        selecte_year[[paste0("Tair_f_inst_Mean_Greenup_", value)]][i]      <- mean(unlist(Tair_f_inst_Mean_Greenup), na.rm = TRUE)
        selecte_year[[paste0("Tair_f_inst_Mean_MidGreenup_", value)]][i]   <- mean(unlist(Tair_f_inst_Mean_MidGreenup), na.rm = TRUE)
        selecte_year[[paste0("Tair_f_inst_Mean_Maturity_", value)]][i]     <- mean(unlist(Tair_f_inst_Mean_Maturity), na.rm = TRUE)
        selecte_year[[paste0("Tair_f_inst_Mean_Senescence_", value)]][i]   <- mean(unlist(Tair_f_inst_Mean_Senescence), na.rm = TRUE)
        selecte_year[[paste0("Tair_f_inst_Mean_MidGreendown_", value)]][i] <- mean(unlist(Tair_f_inst_Mean_MidGreendown), na.rm = TRUE)
        selecte_year[[paste0("Tair_f_inst_Mean_Dormancy_", value)]][i]     <- mean(unlist(Tair_f_inst_Mean_Dormancy), na.rm = TRUE)
        
        selecte_year[[paste0("Rainf_f_tavg_Greenup_", value)]][i]      <- sum(unlist(Rainf_f_tavg_Greenup), na.rm = TRUE)
        selecte_year[[paste0("Rainf_f_tavg_MidGreenup_", value)]][i]   <- sum(unlist(Rainf_f_tavg_MidGreenup), na.rm = TRUE)
        selecte_year[[paste0("Rainf_f_tavg_Maturity_", value)]][i]     <- sum(unlist(Rainf_f_tavg_Maturity), na.rm = TRUE)
        selecte_year[[paste0("Rainf_f_tavg_Senescence_", value)]][i]   <- sum(unlist(Rainf_f_tavg_Senescence), na.rm = TRUE)
        selecte_year[[paste0("Rainf_f_tavg_MidGreendown_", value)]][i] <- sum(unlist(Rainf_f_tavg_MidGreendown), na.rm = TRUE)
        selecte_year[[paste0("Rainf_f_tavg_Dormancy_", value)]][i]     <- sum(unlist(Rainf_f_tavg_Dormancy), na.rm = TRUE)
        
        selecte_year[[paste0("Swnet_tavg_Greenup_", value)]][i]      <- sum(unlist(Swnet_tavg_Greenup), na.rm = TRUE)
        selecte_year[[paste0("Swnet_tavg_MidGreenup_", value)]][i]   <- sum(unlist(Swnet_tavg_MidGreenup), na.rm = TRUE)
        selecte_year[[paste0("Swnet_tavg_Maturity_", value)]][i]     <- sum(unlist(Swnet_tavg_Maturity), na.rm = TRUE)
        selecte_year[[paste0("Swnet_tavg_Senescence_", value)]][i]   <- sum(unlist(Swnet_tavg_Senescence), na.rm = TRUE)
        selecte_year[[paste0("Swnet_tavg_MidGreendown_", value)]][i] <- sum(unlist(Swnet_tavg_MidGreendown), na.rm = TRUE)
        selecte_year[[paste0("Swnet_tavg_Dormancy_", value)]][i]     <- sum(unlist(Swnet_tavg_Dormancy), na.rm = TRUE)
      }
    } 
  }
  Phe_Mete = rbind(Phe_Mete, selecte_year)  
}
toc()

fwrite(Phe_Mete, file = "D:/0 Work/1 Data analysis/1 PhenologyClimateData/data_GOSIF_mete_clean.csv")
