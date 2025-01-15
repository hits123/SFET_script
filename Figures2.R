library(readr)
library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(raster)
library(ggplot2)
library(reshape2)
library(gridExtra)

calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate normalized Bias Error (nBE)
  BE <- mean(simulated - observed)/mean(observed)
  # Calculate normalizedMean Absolute Error (nMAE)
  MAE <- mean(abs(simulated - observed))/mean(observed)
  # Calculate normalizedRoot Mean Square Difference (nRMSD)
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
  if(nrow(sfet_site)>2){ # removing the sites where any products has unique value =1
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

map<-read_xlsx('Mean_annual_ppt.xlsx') # obtained from ameriflux metadata
R2_save<-left_join(R2_save,map)
rmsd_save<-left_join(rmsd_save,map)
be_save<-left_join(be_save,map)

# write.csv(R2_save,'rain_vs_r2.csv',row.names = F)
# write.csv(rmsd_save,'rain_vs_rmsd.csv',row.names = F)

# Load data
rain_vs_r2 <- read.csv('rain_vs_r2.csv')
rain_vs_rmsd <- read.csv('rain_vs_rmsd.csv')
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
    Snow_Rain = factor(snow_rain, levels = c(0, 1), labels = c("Rain", "Snow")), # Convert Snow_Rain to factor
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
    plot.margin = if (identifier %in% c(1, 5)) unit(c(0.5, 0.5, 0.5, 1), "cm") else unit(c(0.5, 0.5, 0.5, 1.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    legend.position = "none" 
  )

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
    annotate("text", x = 300, y = max(y_lim) - 0.05 * diff(y_lim), label = sprintf("P-value = %.2e", p_value), hjust = 0, size = 4) +
    annotate("text", x = 300, y = max(y_lim) - 0.1 * diff(y_lim), label = sprintf("R² = %.3f", r2_value), hjust = 0, size = 4) +
    annotate("text", x = 300, y = max(y_lim) - 0.15 * diff(y_lim),
             label = sprintf("y = %.4f + %.4f x + %.4f x²", (coefficients[1]), (coefficients[2]), (coefficients[3])),
             hjust = 0, size = 4) +
    annotate("text", x = 300, y = max(y_lim) - 0.2 * diff(y_lim), label = sprintf("N = %.0f", nrow(plot_data)), hjust = 0, size = 4) +
    theme_minimal(base_size = 12) +
    annotate("text", x = Inf, y = Inf, label = plot_label, hjust = 1.2, vjust = 2, size = 5, fontface = "bold", color = "black")+
    theme1
}

k1 <- custom_plot1(rainfall = rain_vs_r2$Mean.Average.Precipitation..mm.,plot_label="(a)",snow_rain = rain_vs_r2$Snow_Rain,y_values = rain_vs_r2$R2_sfet_site, title = '',
                   y_label = expression(bold(R^2)), y_lim = c(0, 1),identifier=1)

k4 <- custom_plot1(rainfall = rain_vs_r2$Mean.Average.Precipitation..mm.,plot_label="(a)",snow_rain = rain_vs_r2$Snow_Rain,y_values =rain_vs_r2$R2_ssebop, title ='',
                   y_label = expression(bold(R^2)),  y_lim = c(0, 1),identifier=1)

k1_1 <- custom_plot1(rainfall = rain_vs_rmsd$Mean.Average.Precipitation..mm.,plot_label="(a)",snow_rain = rain_vs_rmsd$Snow_Rain,y_values =rain_vs_rmsd$rmsd_sfet_site,title = '',
                     y_label = "nRMSD", y_lim =  c(0, 2),identifier=1)

k4_1 <- custom_plot1(rainfall = rain_vs_rmsd$Mean.Average.Precipitation..mm.,plot_label="(a)",snow_rain = rain_vs_rmsd$Snow_Rain,y_values = rain_vs_rmsd$rmsd_ssebop, title ='',
                     y_label = "nRMSD", y_lim =  c(0, 2),identifier=1)

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
      plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"),
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
      plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.position = "none" 
    )
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
    annotate("text", x = 0.05, y = max(y_lim) - 0.05 * diff(y_lim), label = sprintf("P-value = %.2e", p_value), hjust = 0, size = 4) +
    annotate("text", x = 0.05, y = max(y_lim) - 0.1 * diff(y_lim), label = sprintf("R² = %.3f", r2_value), hjust = 0, size = 4) +
      annotate("text", x = 0.05, y = max(y_lim) - 0.15 * diff(y_lim),
               label = sprintf("y = %.4f + %.4f x + %.4f x²", coefficients[1], coefficients[2], coefficients[3]),
               hjust = 0, size = 4)+
    annotate("text", x = 0.05, y = max(y_lim) - 0.2 * diff(y_lim), label = sprintf("N = %.0f", nrow(plot_data)), hjust = 0, size = 4) +
    theme_minimal(base_size = 12) +
    annotate("text", x = Inf, y = Inf, label = plot_label, hjust = 1.2, vjust = 2, size = 5, fontface = "bold", color = "black")+
    theme1
}
p1 <- custom_plot2(rainfall = rain_vs_r2$Aridity_Index,y_values = rain_vs_r2$R2_sfet_site,plot_label="(b)",snow_rain = rain_vs_r2$Snow_Rain, title = '',
                   y_label = expression(bold(R^2)), y_lim = c(0, 1),identifier=8)

p4 <- custom_plot2(rainfall = rain_vs_r2$Aridity_Index,y_values =rain_vs_r2$R2_ssebop,plot_label="(b)",snow_rain = rain_vs_r2$Snow_Rain, title ='',
                   y_label = expression(bold(R^2)),  y_lim = c(0, 1),identifier=8)

p1_1 <- custom_plot2(rainfall = rain_vs_rmsd$Aridity_Index,y_values =rain_vs_rmsd$rmsd_sfet_site,plot_label="(b)",snow_rain = rain_vs_rmsd$Snow_Rain,title = '',
                     y_label = "nRMSD", y_lim =  c(0, 2),identifier=8)

p4_1 <- custom_plot2(rainfall = rain_vs_rmsd$Aridity_Index,y_values = rain_vs_rmsd$rmsd_ssebop,plot_label="(b)",snow_rain = rain_vs_rmsd$Snow_Rain, title ='',
                     y_label = "nRMSD", y_lim =  c(0, 2),identifier=8)
rain_plot1 <- grid.arrange(k1, p1, nrow = 1)
rain_plot2 <- grid.arrange(k4, p4, nrow = 1)
rain_plot3 <- grid.arrange(k1_1, p1_1, nrow = 1)
rain_plot4 <- grid.arrange(k4_1, p4_1, nrow = 1)
#ggsave("D:/Script/Plots_paper/Fig_new_sfet_r2.png", rain_plot1, width = 10, height = 5, dpi = 1000,units = 'in')
ggsave("Plots_paper/FigS7.png", rain_plot2, width = 10, height = 5, dpi = 1000,units = 'in')
ggsave("Plots_paper/FigS6.png", rain_plot3, width = 10, height = 5, dpi = 1000,units = 'in')
ggsave("Plots_paper/FigS8.png", rain_plot4, width = 10, height = 5, dpi = 1000,units = 'in')





