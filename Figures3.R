library(readr)
library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(raster)
library(cowplot)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(rio)
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
Disalexi<-"Analysis/Disalexi"
PTJPL<-"Analysis/PTJPL"
MODIS<-"Analysis/MODIS"
SFET_NLDAS_CERES<-"Analysis/SFET_CERES_NLDAS"
SFET_NLDAS_MODIS<-"Analysis/SFET_NLDAS_MODIS"
SFET_Site<-"Analysis/SFET_Site"
SSEBOP<-"Analysis/SSEBOP"
# daily with full data
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

  sfet_site<-left_join(sfet_site,ssebop,by=c('Date'='Date'))
  sfet_site$SSEBOP<-sfet_site$SSEBOP
  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]
  
  if(nrow(sfet_site)>2){ # atleast 2 data
    if(length(unique(sfet_site$SSEBOP))>1){
      met_sfet_site=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$LE_SFET )
      met_ssebop=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$SSEBOP )
      
      daily_save<-rbind(daily_save,data.frame(Site=sitename,sfet_site))
     
      
      R2_save<-rbind(R2_save,data.frame(Site=sitename,R2_sfet_site=met_sfet_site$R_Squared,
                                        R2_ssebop=met_ssebop$R_Squared))
      
      be_save<-rbind(be_save,data.frame(Site=sitename,be_sfet_site=met_sfet_site$Bias_Error,
                                        be_ssebop=met_ssebop$Bias_Error))
      
      mae_save<-rbind(mae_save,data.frame(Site=sitename,mae_sfet_site=met_sfet_site$Mean_Absolute_Error,
                                          mae_ssebop=met_ssebop$Mean_Absolute_Error))
      
      rmsd_save<-rbind(rmsd_save,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site$Root_Mean_Square_Difference,
                                            rmsd_ssebop=met_ssebop$Root_Mean_Square_Difference))
      
    }}
  #  }, error=function(e){})
}

# first plot - daily with full data
map<-read_xlsx('Mean_annual_ppt.xlsx')
R2_save<-left_join(R2_save,map)
rmsd_save<-left_join(rmsd_save,map)
be_save<-left_join(be_save,map)
# write.csv(R2_save,'rain_vs_r2.csv',row.names = F)
# write.csv(rmsd_save,'rain_vs_rmsd.csv',row.names = F)
# Load data
rain_vs_r2 <- read.csv('rain_vs_r2.csv')
rain_vs_rmsd <- read.csv('D:/Script/rain_vs_rmsd.csv')
## rain or snow
rain_snow<-read_xlsx("SNow_dominated_rain.xlsx")
rain_snow<-rain_snow[,c('Site','Snow_Rain')]
rain_vs_r2<-left_join(rain_vs_r2,rain_snow)
rain_vs_rmsd<-left_join(rain_vs_rmsd,rain_snow)
custom_plot1 <- function(rainfall, y_values,plot_label, snow_rain, title, y_label, y_lim, identifier) {
  model <- lm(y_values ~ poly(rainfall, 2, raw = TRUE))
  predicted <- predict(model, newdata = data.frame(rainfall = rainfall), interval = "confidence")
  coefficients <- coef(model)
  r2_value <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]
  plot_data <- data.frame(
    Rainfall = rainfall,
    Y = y_values,
    Snow_Rain = factor(snow_rain, levels = c(0, 1), labels = c("Rain", "Snow")),
    Predicted = predicted[, "fit"],
    Lower = predicted[, "lwr"],
    Upper = predicted[, "upr"]
  )
  theme1 <- theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"), 
    panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title.x = element_text(face = "bold"),
    axis.title.y = if (identifier %in% c(1, 5)) element_text(face = "bold") else element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.ticks = element_line(color = "black", size = 1), 
    axis.ticks.length = unit(0.2, "cm"),
    plot.margin = if (identifier %in% c(1, 5)) unit(c(0.5, 0.5, 0.5, 0.5), "cm") else unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    legend.position = "none" 
  )
  if(sign(coefficients[3])<0){
    annotate11<-  annotate("text", x = 300, y = max(y_lim) - 0.15 * diff(y_lim),
                           label = sprintf("y = %.4f + %.4f x %.4f x²", (coefficients[1]), (coefficients[2]), (coefficients[3])),
                           hjust = 0, size = 3.5)
  }else{
    annotate11<-  annotate("text", x = 300, y = max(y_lim) - 0.15 * diff(y_lim),
                           label = sprintf("y = %.4f + %.4f x + %.4f x²", (coefficients[1]), (coefficients[2]), (coefficients[3])),
                           hjust = 0, size = 3.5) 
  }

  ggplot(plot_data, aes(x = Rainfall, y = Y, color = Snow_Rain)) +
    geom_point(alpha = 0.5, size = 1.5) + 
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
    geom_line(aes(y = Predicted), color = "black", size = 0.5) + 
    scale_x_continuous(limits = c(250, 1500), expand = c(0, 0)) +
    scale_y_continuous(limits = y_lim) +
    scale_color_manual(values = c("red", "blue")) + 
    labs(
      x = ifelse(y_label == "nRMSD", "Mean Annual Precipitation (mm)", "Mean Annual Precipitation (mm)"),
      y = y_label,
      title = title
    ) +
    annotate("text", x = 300, y = max(y_lim) - 0.05 * diff(y_lim), label = sprintf("P-value = %.2e", p_value), hjust = 0, size = 3.5) +
    annotate("text", x = 300, y = max(y_lim) - 0.1 * diff(y_lim), label = sprintf("R² = %.3f", r2_value), hjust = 0, size = 3.5) +
    annotate11 +
    annotate("text", x = 300, y = max(y_lim) - 0.2 * diff(y_lim), label = sprintf("N = %.0f", nrow(plot_data)), hjust = 0, size = 3.5) +
    theme_minimal(base_size = 12) +
    annotate("text", x = Inf, y = Inf, label = plot_label, hjust = 1.2, vjust = 2, size = 5, fontface = "bold", color = "black")+
    theme1
}

k1 <- custom_plot1(rainfall = rain_vs_r2$Mean.Average.Precipitation..mm.,plot_label="(a)",snow_rain = rain_vs_r2$Snow_Rain,y_values = rain_vs_r2$R2_sfet_site, title = '',
                   y_label = expression(bold(R^2)), y_lim = c(0, 1),identifier=1)

custom_plot2 <- function(rainfall, y_values,plot_label, snow_rain, title, y_label, y_lim, identifier) {
  model <- lm(y_values ~ poly(rainfall, 2, raw = TRUE))
  predicted <- predict(model, newdata = data.frame(rainfall = rainfall), interval = "confidence")
  coefficients <- coef(model)
  r2_value <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]
  plot_data <- data.frame(
    Rainfall = rainfall,
    Y = y_values,
    Snow_Rain = factor(snow_rain, levels = c(0, 1), labels = c("Rain", "Snow")),
    Predicted = predicted[, "fit"],
    Lower = predicted[, "lwr"],
    Upper = predicted[, "upr"]
  )
  if (identifier %in% c(10)) {
    theme1 <- theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"), 
      panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      axis.ticks = element_line(color = "black", size = 1), 
      axis.ticks.length = unit(0.2, "cm"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1), 
      legend.position = "none" 
    )
  } else {
    theme1 <- theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"), 
      panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      axis.ticks = element_line(color = "black", size = 1), 
      axis.ticks.length = unit(0.2, "cm"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.position = "none" 
    )
  }
  if(sign(coefficients[3])<0){
    annotate11<-annotate("text", x = 0.05, y = max(y_lim) - 0.15 * diff(y_lim),
                               label = sprintf("y = %.4f + %.4f x %.4f x²", coefficients[1], coefficients[2], coefficients[3]),
                               hjust = 0, size = 3.5)
  }else{
    annotate11<-annotate("text", x = 0.05, y = max(y_lim) - 0.15 * diff(y_lim),
                               label = sprintf("y = %.4f + %.4f x + %.4f x²", coefficients[1], coefficients[2], coefficients[3]),
                               hjust = 0, size = 3.5)
  }
  ggplot(plot_data, aes(x = Rainfall, y = Y, color = Snow_Rain)) +
    geom_point(alpha = 0.5, size = 1.5, show.legend = FALSE) + 
    geom_line(aes(y = Predicted), color = "black", size = 0.5, show.legend = FALSE) + 
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ylim(y_lim) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3, show.legend = FALSE) + 
    scale_color_manual(values = c("red", "blue")) + 
    labs(
      x = ifelse(y_label == "nRMSD", "Aridity Index", "Aridity Index"),
      y = y_label,
      title = title
    ) +
    annotate("text", x = 0.05, y = max(y_lim) - 0.05 * diff(y_lim), label = sprintf("P-value = %.2e", p_value), hjust = 0, size = 3.5) +
    annotate("text", x = 0.05, y = max(y_lim) - 0.1 * diff(y_lim), label = sprintf("R² = %.3f", r2_value), hjust = 0, size = 3.5) +
    annotate11+
    annotate("text", x = 0.05, y = max(y_lim) - 0.2 * diff(y_lim), label = sprintf("N = %.0f", nrow(plot_data)), hjust = 0, size = 3.5) +
    theme_minimal(base_size = 12) +
    annotate("text", x = Inf, y = Inf, label = plot_label, hjust = 1.2, vjust = 2, size = 5, fontface = "bold", color = "black")+
    theme1
}
p1_ref <- custom_plot2(rainfall = rain_vs_r2$Aridity_Index,y_values = rain_vs_r2$R2_sfet_site,plot_label="(b)",snow_rain = rain_vs_r2$Snow_Rain, title = '',
                   y_label = expression(bold(R^2)), y_lim = c(0, 1),identifier=8)
calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (BE)
  BE <- mean(simulated - observed)/mean(observed)
  # Calculate Mean Absolute Error (MAE)
  MAE <- mean(abs(simulated - observed))/mean(observed)
  # Calculate Root Mean Square Difference (RMSD)
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
Disalexi<-"Analysis/Disalexi"
PTJPL<-"Analysis/PTJPL"
MODIS<-"Analysis/MODIS"
SFET_NLDAS_CERES<-"Analysis/SFET_CERES_NLDAS"
SFET_NLDAS_MODIS<-"Analysis/SFET_NLDAS_MODIS"
SFET_Site<-"Analysis/SFET_Site"
SSEBOP<-"Analysis/SSEBOP"
# reading clear and cloudy days
clearday<-read_excel('Clear_days_MODIS_LST.xlsx')
cloudyday<-read_excel('Cloudy_days_MODIS_LST.xlsx')
clearday$Date<-as.Date(clearday$Date)
cloudyday$Date<-as.Date(cloudyday$Date)
# daily with full data
site_list<-list.files(SFET_Site)
R2_save_clear<-NULL
be_save_clear<-NULL
mae_save_clear<-NULL
rmsd_save_clear<-NULL

R2_save_cloudy<-NULL
be_save_cloudy<-NULL
mae_save_cloudy<-NULL
rmsd_save_cloudy<-NULL

daily_save_clear<-NULL
daily_save_cloudy<-NULL

for (i in site_list) {
  # tryCatch({
  sitename<-strsplit(i,'[.]')[[1]][1]
  sfet_site<-read_xlsx(paste0(SFET_Site,'/',i))
  sfet_nldas_ceres<-read_xlsx(paste0(SFET_NLDAS_CERES,'/',i))
  ssebop<-read_xlsx(paste0(SSEBOP,'/',i))
  
  sfet_site$Date<-as.Date(sfet_site$Date)
  sfet_nldas_ceres$Date<-as.Date(sfet_nldas_ceres$Date)
  ssebop$Date<-as.Date(ssebop$Date)
  
  sfet_nldas_ceres<-sfet_nldas_ceres[,c('Date','SFET_CERES_NLDAS_LE')]
  ssebop<-ssebop[,c('Date','SSEBOP')]
  
  sfet_site<-left_join(sfet_site,ssebop,by=c('Date'='Date'))
  sfet_site$SSEBOP<-sfet_site$SSEBOP
  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]
  ## segregating clear and cloudy days
  clearday1<-clearday[clearday$ID==sitename,]
  cloudyday1<-cloudyday[cloudyday$ID==sitename,]
  clearday1<-left_join(clearday1,sfet_site,by=c('Date'='Date'))
  clearday1<-na.omit(clearday1)
  
  cloudyday1<-left_join(cloudyday1,sfet_site,by=c('Date'='Date'))
  cloudyday1<-na.omit(cloudyday1)

  if(nrow(clearday1)>2&nrow(cloudyday1)>2){ 
    if(length(unique(clearday1$SSEBOP))>1&length(unique(cloudyday1$SSEBOP))>1){
      print(nrow(clearday1))
      # clear days
      met_sfet_site_clear=calculate_all_metrics(observed = clearday1$LE_Obs,simulated =clearday1$LE_SFET )
      met_ssebop_clear=calculate_all_metrics(observed = clearday1$LE_Obs,simulated =clearday1$SSEBOP )
      
      # cloudy days
      met_sfet_site_cloudy=calculate_all_metrics(observed = cloudyday1$LE_Obs,simulated =cloudyday1$LE_SFET )
      met_ssebop_cloudy=calculate_all_metrics(observed = cloudyday1$LE_Obs,simulated =cloudyday1$SSEBOP )
    
      daily_save_clear<-rbind(daily_save_clear,data.frame(Site=sitename,clearday1))
      daily_save_cloudy<-rbind(daily_save_cloudy,data.frame(Site=sitename,cloudyday1))
    
      R2_save_clear<-rbind(R2_save_clear,data.frame(Site=sitename,R2_sfet_site=met_sfet_site_clear$R_Squared,
                                                    
                                                    R2_ssebop=met_ssebop_clear$R_Squared))
      
      be_save_clear<-rbind(be_save_clear,data.frame(Site=sitename,be_sfet_site=met_sfet_site_clear$Bias_Error,
                                                    
                                                    be_ssebop=met_ssebop_clear$Bias_Error))
      
      mae_save_clear<-rbind(mae_save_clear,data.frame(Site=sitename,mae_sfet_site=met_sfet_site_clear$Mean_Absolute_Error,
                                                      
                                                      mae_ssebop=met_ssebop_clear$Mean_Absolute_Error))
      
      rmsd_save_clear<-rbind(rmsd_save_clear,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site_clear$Root_Mean_Square_Difference,
                                                        
                                                        rmsd_ssebop=met_ssebop_clear$Root_Mean_Square_Difference))
      
      # cloudy
      R2_save_cloudy<-rbind(R2_save_cloudy,data.frame(Site=sitename,R2_sfet_site=met_sfet_site_cloudy$R_Squared,
                                                      
                                                      R2_ssebop=met_ssebop_cloudy$R_Squared))
      
      be_save_cloudy<-rbind(be_save_cloudy,data.frame(Site=sitename,be_sfet_site=met_sfet_site_cloudy$Bias_Error,
                                                      
                                                      be_ssebop=met_ssebop_cloudy$Bias_Error))
      
      mae_save_cloudy<-rbind(mae_save_cloudy,data.frame(Site=sitename,mae_sfet_site=met_sfet_site_cloudy$Mean_Absolute_Error,
                                                        
                                                        mae_ssebop=met_ssebop_cloudy$Mean_Absolute_Error))
      
      rmsd_save_cloudy<-rbind(rmsd_save_cloudy,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site_cloudy$Root_Mean_Square_Difference,
                                                          
                                                          rmsd_ssebop=met_ssebop_cloudy$Root_Mean_Square_Difference))
      
    }}
  #  }, error=function(e){})
}
# first plot - daily with full data
data_r2_clear <- data.frame(
  SFET_Site = R2_save_clear$R2_sfet_site,
  SSEBOP = R2_save_clear$R2_ssebop
)

data_mae_clear <- data.frame(
  SFET_Site = mae_save_clear$mae_sfet_site,
  SSEBOP = mae_save_clear$mae_ssebop
)

data_rmsd_clear <- data.frame(
  SFET_Site = rmsd_save_clear$rmsd_sfet_site,
  SSEBOP = rmsd_save_clear$rmsd_ssebop
)

data_be_clear <- data.frame(
  SFET_Site = be_save_clear$be_sfet_site,
  SSEBOP = be_save_clear$be_ssebop
)

data_r2_cloudy <- data.frame(
  SFET_Site = R2_save_cloudy$R2_sfet_site,
  SSEBOP = R2_save_cloudy$R2_ssebop
)

data_mae_cloudy <- data.frame(
  SFET_Site = mae_save_cloudy$mae_sfet_site,
  SSEBOP = mae_save_cloudy$mae_ssebop
)

data_rmsd_cloudy <- data.frame(
  SFET_Site = rmsd_save_cloudy$rmsd_sfet_site,
  SSEBOP = rmsd_save_cloudy$rmsd_ssebop
)

data_be_cloudy <- data.frame(
  SFET_Site = be_save_cloudy$be_sfet_site,
  SSEBOP = be_save_cloudy$be_ssebop
)

summary_r2_clear <- data.frame(
  Column = colnames(data_r2_clear),
  Min = round(sapply(data_r2_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2_clear, median, na.rm = TRUE), 2) 
)

summary_rmsd_clear <- data.frame(
  Column = colnames(data_rmsd_clear), 
  Min = round(sapply(data_rmsd_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd_clear, median, na.rm = TRUE), 2) 
)

summary_be_clear <- data.frame(
  Column = colnames(data_be_clear), 
  Min = round(sapply(data_be_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be_clear, median, na.rm = TRUE), 2) 
)

summary_r2_cloudy <- data.frame(
  Column = colnames(data_r2_cloudy), 
  Min = round(sapply(data_r2_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2_cloudy, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2_cloudy, mean, na.rm = TRUE), 2),
  Median = round(sapply(data_r2_cloudy, median, na.rm = TRUE), 2)
)

summary_rmsd_cloudy <- data.frame(
  Column = colnames(data_rmsd_cloudy), 
  Min = round(sapply(data_rmsd_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd_cloudy, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd_cloudy, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd_cloudy, median, na.rm = TRUE), 2) 
)

summary_be_cloudy <- data.frame(
  Column = colnames(data_be_cloudy), 
  Min = round(sapply(data_be_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be_cloudy, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be_cloudy, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be_cloudy, median, na.rm = TRUE), 2) 
)

list1<-list('summary_r2_clear'=summary_r2_clear,'summary_rmsd_clear'=summary_rmsd_clear,
            'summary_be_clear'=summary_be_clear,'summary_r2_cloudy'=summary_r2_cloudy,
            'summary_rmsd_cloudy'=summary_rmsd_cloudy,
            'summary_be_cloudy'=summary_be_cloudy)

data_lim <- data.frame(
  A = data_r2_clear$SFET_Site,
  B = data_r2_cloudy$SFET_Site,
  C=data_r2_clear$SSEBOP,
  D=data_r2_cloudy$SSEBOP
)

data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
data <- data.frame(
  Clear = data_r2_clear$SFET_Site,
  Cloudy = data_r2_cloudy$SFET_Site
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities1 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_1 <- ceiling(max(sapply(densities1, function(d) max(d$y))))
p1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),      
    breaks = seq(min1, max1, length.out = 5) 
  )+ scale_y_continuous(
    limits = c(0, max_1), 
    expand = c(0, 0),       
    breaks = seq(0, max_1, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = 0.2, 
              y = seq(0, max_1, length.out = 5)[4]+(seq(0, max_1, length.out = 5)[5]-seq(0, max_1, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])),size = 4)+
  annotate("text", x = 0.2,
           y =seq(0, max_1, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])),size = 4)+
  annotate(
    "text",
    x = Inf, 
    y = Inf,   
    label = "(d)",
    hjust = 1.1, 
    vjust = 1.5,  
    fontface = "bold", 
    size = 5       
  )+
  labs(
    title = "",
    x = "R2",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(  panel.grid.major = element_line(color = "gray", linetype = "dotted"), 
          panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", axis.ticks = element_line(color = "black", size = 1), 
          axis.ticks.length = unit(0.2, "cm"), axis.text = element_text(face = "bold"),             
          axis.title = element_text(face = "bold"), 
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

dry_wet_folder<-"Dry_Wet"
calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (BE)
  BE <- mean(simulated - observed)/mean(observed)
  # Calculate Mean Absolute Error (MAE)
  MAE <- mean(abs(simulated - observed))/mean(observed)
  # Calculate Root Mean Square Difference (RMSD)
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
R2_save_clear<-NULL
be_save_clear<-NULL
mae_save_clear<-NULL
rmsd_save_clear<-NULL

R2_save_cloudy<-NULL
be_save_cloudy<-NULL
mae_save_cloudy<-NULL
rmsd_save_cloudy<-NULL

daily_save_clear<-NULL
daily_save_cloudy<-NULL

for (i in site_list) {
  # tryCatch({
  sitename<-strsplit(i,'[.]')[[1]][1]
  
  # reading clear and cloudy days
  all_day<-import_list(paste0(dry_wet_folder,'/',sitename,'.xlsx'))
  clearday<-all_day$dry_period
  cloudyday<-all_day$wet_period
  clearday$Date<-as.Date(clearday$Date)
  cloudyday$Date<-as.Date(cloudyday$Date)
  clearday<-clearday[,c('Date','fPET')]
  cloudyday<-cloudyday[,c('Date','fPET')]
  
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
  
  sfet_site<-left_join(sfet_site,ssebop,by=c('Date'='Date'))
  sfet_site$SSEBOP<-sfet_site$SSEBOP
  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]
  ## segregating clear and cloudy days
  
  clearday1<-left_join(clearday,sfet_site,by=c('Date'='Date'))
  clearday1<-na.omit(clearday1)
  
  cloudyday1<-left_join(cloudyday,sfet_site,by=c('Date'='Date'))
  cloudyday1<-na.omit(cloudyday1)
  
  if(nrow(clearday1)>2&nrow(cloudyday1)>2){ 
    if(length(unique(clearday1$SSEBOP))>1&length(unique(cloudyday1$SSEBOP))>1){
      print(nrow(clearday1))
      # clear days
      met_sfet_site_clear=calculate_all_metrics(observed = clearday1$LE_Obs,simulated =clearday1$LE_SFET )
      met_ssebop_clear=calculate_all_metrics(observed = clearday1$LE_Obs,simulated =clearday1$SSEBOP )
      
      # cloudy days
      met_sfet_site_cloudy=calculate_all_metrics(observed = cloudyday1$LE_Obs,simulated =cloudyday1$LE_SFET )
      
      met_ssebop_cloudy=calculate_all_metrics(observed = cloudyday1$LE_Obs,simulated =cloudyday1$SSEBOP )
      
      daily_save_clear<-rbind(daily_save_clear,data.frame(Site=sitename,clearday1))
      daily_save_cloudy<-rbind(daily_save_cloudy,data.frame(Site=sitename,cloudyday1))
      
      R2_save_clear<-rbind(R2_save_clear,data.frame(Site=sitename,R2_sfet_site=met_sfet_site_clear$R_Squared,
                                                    R2_ssebop=met_ssebop_clear$R_Squared))
      
      be_save_clear<-rbind(be_save_clear,data.frame(Site=sitename,be_sfet_site=met_sfet_site_clear$Bias_Error,
                                                    be_ssebop=met_ssebop_clear$Bias_Error))
      
      mae_save_clear<-rbind(mae_save_clear,data.frame(Site=sitename,mae_sfet_site=met_sfet_site_clear$Mean_Absolute_Error,
                                                      mae_ssebop=met_ssebop_clear$Mean_Absolute_Error))
      
      rmsd_save_clear<-rbind(rmsd_save_clear,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site_clear$Root_Mean_Square_Difference,
                                                        rmsd_ssebop=met_ssebop_clear$Root_Mean_Square_Difference))
      
      # cloudy
      R2_save_cloudy<-rbind(R2_save_cloudy,data.frame(Site=sitename,R2_sfet_site=met_sfet_site_cloudy$R_Squared,
                                                      R2_ssebop=met_ssebop_cloudy$R_Squared))
      
      be_save_cloudy<-rbind(be_save_cloudy,data.frame(Site=sitename,be_sfet_site=met_sfet_site_cloudy$Bias_Error,
                                                      be_ssebop=met_ssebop_cloudy$Bias_Error))
      
      mae_save_cloudy<-rbind(mae_save_cloudy,data.frame(Site=sitename,mae_sfet_site=met_sfet_site_cloudy$Mean_Absolute_Error,
                                                        mae_ssebop=met_ssebop_cloudy$Mean_Absolute_Error))
      
      rmsd_save_cloudy<-rbind(rmsd_save_cloudy,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site_cloudy$Root_Mean_Square_Difference,
                                                          rmsd_ssebop=met_ssebop_cloudy$Root_Mean_Square_Difference))
      
    }}
  #  }, error=function(e){})
}

data_r2_clear <- data.frame(
  SFET_Site = R2_save_clear$R2_sfet_site,
  SSEBOP = R2_save_clear$R2_ssebop
)

data_mae_clear <- data.frame(
  SFET_Site = mae_save_clear$mae_sfet_site,
  SSEBOP = mae_save_clear$mae_ssebop
)

data_rmsd_clear <- data.frame(
  SFET_Site = rmsd_save_clear$rmsd_sfet_site,
  SSEBOP = rmsd_save_clear$rmsd_ssebop
)

data_be_clear <- data.frame(
  SFET_Site = be_save_clear$be_sfet_site,
  SSEBOP = be_save_clear$be_ssebop
)

data_r2_cloudy <- data.frame(
  SFET_Site = R2_save_cloudy$R2_sfet_site,
  SSEBOP = R2_save_cloudy$R2_ssebop
)

data_mae_cloudy <- data.frame(
  SFET_Site = mae_save_cloudy$mae_sfet_site,
  SSEBOP = mae_save_cloudy$mae_ssebop
)

data_rmsd_cloudy <- data.frame(
  SFET_Site = rmsd_save_cloudy$rmsd_sfet_site,
  SSEBOP = rmsd_save_cloudy$rmsd_ssebop
)

data_be_cloudy <- data.frame(
  SFET_Site = be_save_cloudy$be_sfet_site,
  SSEBOP = be_save_cloudy$be_ssebop
)

summary_r2_clear <- data.frame(
  Column = colnames(data_r2_clear), 
  Min = round(sapply(data_r2_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2_clear, median, na.rm = TRUE), 2) 
)

summary_rmsd_clear <- data.frame(
  Column = colnames(data_rmsd_clear), 
  Min = round(sapply(data_rmsd_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd_clear, median, na.rm = TRUE), 2) 
)

summary_be_clear <- data.frame(
  Column = colnames(data_be_clear), 
  Min = round(sapply(data_be_clear, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be_clear, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be_clear, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be_clear, median, na.rm = TRUE), 2) 
)

summary_r2_cloudy <- data.frame(
  Column = colnames(data_r2_cloudy), 
  Min = round(sapply(data_r2_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2_cloudy, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2_cloudy, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2_cloudy, median, na.rm = TRUE), 2) 
)

summary_rmsd_cloudy <- data.frame(
  Column = colnames(data_rmsd_cloudy), 
  Min = round(sapply(data_rmsd_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd_cloudy, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd_cloudy, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd_cloudy, median, na.rm = TRUE), 2) 
)

summary_be_cloudy <- data.frame(
  Column = colnames(data_be_cloudy), 
  Min = round(sapply(data_be_cloudy, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be_cloudy, max, na.rm = TRUE), 2),
  Mean = round(sapply(data_be_cloudy, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be_cloudy, median, na.rm = TRUE), 2) 
)

list1<-list('summary_r2_clear'=summary_r2_clear,'summary_rmsd_clear'=summary_rmsd_clear,
            'summary_be_clear'=summary_be_clear,'summary_r2_cloudy'=summary_r2_cloudy,
            'summary_rmsd_cloudy'=summary_rmsd_cloudy,
            'summary_be_cloudy'=summary_be_cloudy)
# Load necessary library
data_lim <- data.frame(
  A = data_r2_clear$SFET_Site,
  B = data_r2_cloudy$SFET_Site,
  C=data_r2_clear$SSEBOP,
  D=data_r2_cloudy$SSEBOP
)

data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
# Sample data
data <- data.frame(
  SES = data_r2_clear$SFET_Site,
  MES = data_r2_cloudy$SFET_Site
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities1 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_1 <- ceiling(max(sapply(densities1, function(d) max(d$y))))

p1_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_1), 
    expand = c(0, 0),       
    breaks = seq(0, max_1, length.out = 5) 
  )+annotate("text", x = 0.2, 
             y = seq(0, max_1, length.out = 5)[4]+(seq(0, max_1, length.out = 5)[5]-seq(0, max_1, length.out = 5)[4])/2,
             label = bquote(SES[median] == .(median_lines$Value[1])),size = 4)+
  annotate("text", x = 0.2,
           y =seq(0, max_1, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])),size = 4)+
  annotate(
    "text",
    x = Inf,  
    y = Inf,   
    label = "(c)",
    hjust = 1.1,  
    vjust = 1.5,   
    fontface = "bold",  
    size = 5       
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +
  labs(
    title = "",
    x = "R2",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(  panel.grid.major = element_line(color = "gray", linetype = "dotted"), 
          panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", axis.ticks = element_line(color = "black", size = 1), 
          axis.ticks.length = unit(0.2, "cm"), axis.text = element_text(face = "bold"),             
          axis.title = element_text(face = "bold"), 
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

final_plot <- plot_grid(k1, p1_ref, p1_1, p1, nrow = 2,rel_heights = c(0.85, 1),rel_widths = c(1,1))
ggsave("Plots_paper/Fig3_main.png", final_plot, width = 10, height = 8, dpi = 1000,units = 'in',bg = 'white')


