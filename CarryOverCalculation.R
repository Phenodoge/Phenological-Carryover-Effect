# Calculate the carryover effect without standardization #######################

### EOS50 ~ SOS50 --------------------------------------------------------------
rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(tictoc)

tic()
data_mete <- fread("D:/0 Work/1 Data analysis/carryover_effect/data_GOSIF_mete_clean.csv")
DOM_EOS50_SOS50 <- fread("D:/0 Work/1 Data analysis/carryover_effect/preseason_EOS50_SOS50.csv")

pixel=unique(data_mete$geometry);length(pixel)
sta=as.data.frame(matrix(0,ncol=0,nrow = 0))
for (i in 1:length(pixel)) {
  data_pixel=data_mete[data_mete$geometry==pixel[i],]
  
  #### EOS50~SOS50
  opt_pre_eos50_sos50 = DOM_EOS50_SOS50[DOM_EOS50_SOS50$geometry==pixel[i]]
  
  ##get preseason length
  opt_pre_eos50_sos50_tem = opt_pre_eos50_sos50$max_tem*15
  
  pre_eos50_sos50_tem <- "Tair_f_inst_Mean_MidGreendown_00"
  pre_eos50_sos50_tem_new <- str_replace(pre_eos50_sos50_tem, "00", as.character(opt_pre_eos50_sos50_tem))
  
  ### Drivers of EOS50 [MidGreendown_DOY], MidGreenup_DOY, Tair_f_inst_Mean_MidGreendown
  data_pixel_eos50_sos50 <- cbind(Year = data_pixel$Year,
                                  MidGreendown_DOY = data_pixel$MidGreendown_DOY,
                                  MidGreenup_DOY = data_pixel$MidGreenup_DOY, 
                                  Tem_Value = data_pixel[[pre_eos50_sos50_tem_new]])
  
  data_pixel_eos50_sos50 <- as.data.frame(data_pixel_eos50_sos50 )
  opt_lm_eos50_sos50 <- summary(lm(MidGreendown_DOY ~ MidGreenup_DOY + Tem_Value + Year,data_pixel_eos50_sos50))$coefficients
  
  sta[i,1]=pixel[i]#gemetry
  sta[i,2]=unique(data_pixel$Lon)#lon
  sta[i,3]=unique(data_pixel$Lat)#lat
  sta[i,4]=opt_lm_eos50_sos50[2,1] #eos50_sos50_C
  sta[i,5]=opt_lm_eos50_sos50[3,1] #eos50_sos50_Tem_C
  sta[i,6]=opt_lm_eos50_sos50[2,4] #eos50_sos50_P
  sta[i,7]=opt_lm_eos50_sos50[3,4] #eos50_sos50_Tem_P
}

names(sta)<-c("Geometry", "Lon", "Lat", 
              "EOS50_SOS50_C","EOS50_SOS50_Tem_C",
              "EOS50_SOS50_P","EOS50_SOS50_Tem_P")
fwrite(sta, file = "D:/0 Work/1 Data analysis/carryover_effect/EOS50_SOS_no_scale.csv")



### SOS50 ~ EOS50 --------------------------------------------------------------
rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(tictoc)

data_mete <- fread("D:/0 Work/1 Data analysis/carryover_effect/data_GOSIF_mete_clean.csv")
DOM_SOS50_EOS50 <- fread("D:/0 Work/1 Data analysis/carryover_effect/preseason_SOS50_EOS50.csv")
pixel=unique(data_mete$geometry);length(pixel)
sta=as.data.frame(matrix(0,ncol=0,nrow = 0))

for (i in 1:length(pixel)) {
  data_pixel=data_mete[data_mete$geometry==pixel[i],]
  
  #### SOS50 ~ EOS50
  opt_pre_SOS50_EOS50 = DOM_SOS50_EOS50[DOM_SOS50_EOS50$geometry==pixel[i]]
  
  ##get preseason length
  opt_pre_SOS50_EOS50_tem = opt_pre_SOS50_EOS50$max_tem*15
  pre_SOS50_EOS50_tem <- "Tair_f_inst_Mean_MidGreenup_00"
  pre_SOS50_EOS50_tem_new <- str_replace(pre_SOS50_EOS50_tem, "00", as.character(opt_pre_SOS50_EOS50_tem))
  
  ### Drivers of SOS50 [MidGreenup_DOY], MidGreendown_DOY_pre, Tair_f_inst_Mean_MidGreenup
  data_pixel_SOS50_EOS50 <- cbind(Year = data_pixel$Year,
                                  MidGreenup_DOY = data_pixel$MidGreenup_DOY,
                                  MidGreendown_DOY_pre = data_pixel$MidGreendown_DOY_pre, 
                                  Tem_Value = data_pixel[[pre_SOS50_EOS50_tem_new]])
  
  data_pixel_SOS50_EOS50 <- as.data.frame(data_pixel_SOS50_EOS50 )
  opt_lm_SOS50_EOS50 <- summary(lm(MidGreenup_DOY ~ MidGreendown_DOY_pre + Tem_Value + Year,data_pixel_SOS50_EOS50))$coefficients
  
  #### write results
  sta[i,1]=pixel[i]#gemetry
  sta[i,2]=unique(data_pixel$Lon)#lon
  sta[i,3]=unique(data_pixel$Lat)#lat
  sta[i,4]=opt_lm_SOS50_EOS50[2,1] #SOS50_EOS50_C
  sta[i,5]=opt_lm_SOS50_EOS50[3,1] #SOS50_EOS50_Tem_C
  sta[i,6]=opt_lm_SOS50_EOS50[2,4] #SOS50_EOS50_P
  sta[i,7]=opt_lm_SOS50_EOS50[3,4] #SOS50_EOS50_Tem_P
}

names(sta)<-c("Geometry", "Lon", "Lat", 
              "SOS50_EOS50_C","SOS50_EOS50_Tem_C",
              "SOS50_EOS50_P","SOS50_EOS50_Tem_P")
fwrite(sta, file = "D:/0 Work/1 Data analysis/carryover_effect/SOS50_EOS_no_scale.csv")
