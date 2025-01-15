library(readxl)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(lubridate)

rain_snow<-read_xlsx("SNow_dominated_rain.xlsx")
rain_snow<-rain_snow[,c('Site','Snow_Rain')]
rain_snow$Snow_Rain_Index<-NA
rain_snow$Snow_Rain_Index[rain_snow$Snow_Rain==1]<-'Snow'
rain_snow$Snow_Rain_Index[rain_snow$Snow_Rain==0]<-'Rain'

calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (nBE)
  BE <- mean(simulated - observed)/mean(observed)
  # Calculate Mean Absolute Error (nMAE)
  MAE <- mean(abs(simulated - observed))/mean(observed)
  # Calculate Root Mean Square Difference (nRMSD)
  RMSD <- sqrt(mean((simulated - observed)^2))/mean(observed)
  # Calculate R² using lm
  r_squared <- {
    model <- lm(simulated ~ observed)
    summary(model)$r.squared
  }
  return(list(
    Bias_Error = round(BE,2),
    Mean_Absolute_Error = round(MAE,2),
    Root_Mean_Square_Difference = round(RMSD,2),
    R_Squared = round(r_squared,2)
  ))
}
Fix_conv<-28.35
# Plotting
Disalexi<-"Analysis/Disalexi"
PTJPL<-"Analysis/PTJPL"
MODIS<-"Analysis/MODIS"
SFET_NLDAS_CERES<-"Analysis/SFET_CERES_NLDAS"
SFET_NLDAS_MODIS<-"Analysis/SFET_NLDAS_MODIS"
SFET_Site<-"Analysis/SFET_Site"
SSEBOP<-"Analysis/SSEBOP"

site_list<-list.files(SFET_Site)
R2_save<-NULL
be_save<-NULL
mae_save<-NULL
rmsd_save<-NULL
daily_save<-NULL

for (i in site_list) {
  # tryCatch({
  sitename<-strsplit(i,'[.]')[[1]][1]
  sfet_site<-read_xlsx(paste0(SFET_Site,'/',i))
  sfet_nldas_ceres<-read_xlsx(paste0(SFET_NLDAS_CERES,'/',i))
  sfet_nldas_modis<-read_xlsx(paste0(SFET_NLDAS_MODIS,'/',i))
  ssebop<-read_xlsx(paste0(SSEBOP,'/',i))
  
  sfet_site$Date<-as.Date(sfet_site$Date)
  sfet_nldas_ceres$Date<-as.Date(sfet_nldas_ceres$Date)
  sfet_nldas_modis$Date<-as.Date(sfet_nldas_modis$Date)
  ssebop$Date<-as.Date(ssebop$Date)
  
  sfet_nldas_ceres<-sfet_nldas_ceres[,c('Date','SFET_CERES_NLDAS_LE')]
  sfet_nldas_modis<-sfet_nldas_modis[,c('Date','SFET_NLDAS_MODIS_LE')]
  ssebop<-ssebop[,c('Date','SSEBOP')]
  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]
  
  if(nrow(sfet_site)>2){ 
    daily_save<-rbind(daily_save,data.frame(Site=sitename,sfet_site))
  }
  #  }, error=function(e){})
}
daily_save=daily_save[daily_save$Site!='US-ADR',]
daily_save=daily_save[daily_save$Site!='US-CPk',]

snow_data<-daily_save[daily_save$Site %in% rain_snow$Site[rain_snow$Snow_Rain==1],]
rain_data<-daily_save[daily_save$Site %in% rain_snow$Site[rain_snow$Snow_Rain==0],]

## all period

all_snow_met<-calculate_all_metrics(observed = snow_data$LE_Obs,simulated = snow_data$LE_SFET)
all_rain_met<-calculate_all_metrics(observed = rain_data$LE_Obs,simulated = rain_data$LE_SFET)

## Summer+autumn (June, July, August, spet, oct, nov)
snow_data_SA<-snow_data[month(snow_data$Date) %in% c(6,7,8,9,10,11),]
rain_data_SA<-rain_data[month(rain_data$Date) %in% c(6,7,8,9,10,11),]

all_snow_SA_met<-calculate_all_metrics(observed = snow_data_SA$LE_Obs,simulated = snow_data_SA$LE_SFET)
all_rain_SA_met<-calculate_all_metrics(observed = rain_data_SA$LE_Obs,simulated = rain_data_SA$LE_SFET)

## Spring+Summer+autumn (March,April, MAy,June, July, August, spet, oct, nov)
snow_data_SSA<-snow_data[month(snow_data$Date) %in% c(3,4,5,6,7,8,9,10,11),]
rain_data_SSA<-rain_data[month(rain_data$Date) %in% c(3,4,5,6,7,8,9,10,11),]

all_snow_SSA_met<-calculate_all_metrics(observed = snow_data_SSA$LE_Obs,simulated = snow_data_SSA$LE_SFET)
all_rain_SSA_met<-calculate_all_metrics(observed = rain_data_SSA$LE_Obs,simulated = rain_data_SSA$LE_SFET)

all_1=data.frame(ID='ALL',Snow=all_snow_met$R_Squared,Rain=all_rain_met$R_Squared)
sa_1=data.frame(ID='SA',Snow=all_snow_SA_met$R_Squared,Rain=all_rain_SA_met$R_Squared)
ssa_1=data.frame(ID='SSA',Snow=all_snow_SSA_met$R_Squared,Rain=all_rain_SSA_met$R_Squared)

common_r2=rbind(all_1,sa_1,ssa_1)
all_1=data.frame(ID='ALL',Snow=all_snow_met$Root_Mean_Square_Difference,Rain=all_rain_met$Root_Mean_Square_Difference)
sa_1=data.frame(ID='SA',Snow=all_snow_SA_met$Root_Mean_Square_Difference,Rain=all_rain_SA_met$Root_Mean_Square_Difference)
ssa_1=data.frame(ID='SSA',Snow=all_snow_SSA_met$Root_Mean_Square_Difference,Rain=all_rain_SSA_met$Root_Mean_Square_Difference)

common_rmse=rbind(all_1,sa_1,ssa_1)

all_1=data.frame(ID='ALL',Snow=all_snow_met$Bias_Error,Rain=all_rain_met$Bias_Error)
sa_1=data.frame(ID='SA',Snow=all_snow_SA_met$Bias_Error,Rain=all_rain_SA_met$Bias_Error)
ssa_1=data.frame(ID='SSA',Snow=all_snow_SSA_met$Bias_Error,Rain=all_rain_SSA_met$Bias_Error)

common_be=rbind(all_1,sa_1,ssa_1)

data_long1 <- tidyr::pivot_longer(common_r2, cols = c("Snow", "Rain"), names_to = "Type", values_to = "Value")
data_long2 <- tidyr::pivot_longer(common_rmse, cols = c("Snow", "Rain"), names_to = "Type", values_to = "Value")
data_long3 <- tidyr::pivot_longer(common_be, cols = c("Snow", "Rain"), names_to = "Type", values_to = "Value")

g1 <- ggplot(data_long1, aes(x = Type, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) + 
  facet_wrap(~ID, scales = "free_x", nrow = 1) +  
  labs(x = NULL, y = "R2") +  
  theme_minimal() +
  ylim(0, 1) +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
    panel.grid.minor = element_blank(), 
    axis.text.y = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"), 
    strip.text = element_text(size = 12,face='bold'),
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    legend.position = "none",
    axis.line.x = element_line(color = "black", size = 0.5) 
  )
g2 <- ggplot(data_long2, aes(x = Type, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.8),  
            axis.text.y = element_text(size = 12, face = "bold"),  
            axis.title.y = element_text(size = 14, face = "bold"), 
            vjust = -0.5, size = 4) +  
  facet_wrap(~ID, scales = "free_x", nrow = 1) + 
  labs(x = NULL, y = "nRMSD (mm/day)") +  
  theme_minimal() +
  ylim(0, 0.8) +
  theme( axis.text.y = element_text(size = 12, face = "bold"), 
         axis.title.y = element_text(size = 14, face = "bold"), 
         panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
         panel.grid.minor = element_blank(),
         strip.text = element_blank(),
         axis.text.x = element_blank(),  
         axis.ticks.x = element_blank(), 
         legend.position = "none",
         axis.line.x = element_line(color = "black", size = 0.5) 
  )
g3 <- ggplot(data_long3, aes(x = Type, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) + 
  facet_wrap(~ID, scales = "free_x", nrow = 1) +  
  labs(x = NULL, y = "nBE (mm/day)") +  
  theme_minimal() +
  ylim(-0.2, 0.2) +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
    panel.grid.minor = element_blank(),
    strip.text = element_blank(), 
    axis.text.y = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    legend.position = "bottom",  
    legend.key.height = unit(1, "cm"),  
    legend.key.width = unit(4, "cm"),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 10, face = "bold"),
    axis.line.x = element_line(color = "black", size = 0.5) 
  )
g4 <- grid.arrange(g1, g2, g3, ncol = 1,heights = c(1, 1, 1.5))
ggsave("Plots_paper/FigS9.png", plot = g4, width = 12, height = 9, dpi = 1000,units = 'in')
