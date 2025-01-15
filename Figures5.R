library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(rio)
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
# Sample data
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
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_1, length.out = 5)[4]+(seq(0, max_1, length.out = 5)[5]-seq(0, max_1, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_1, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,   
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,   
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
data <- data.frame(
  Clear = data_r2_clear$SSEBOP,
  Cloudy = data_r2_cloudy$SSEBOP
)
data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities2 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_2 <- ceiling(max(sapply(densities2, function(d) max(d$y))))
p2<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+ scale_y_continuous(
    limits = c(0, max_2), 
    expand = c(0, 0),       
    breaks = seq(0, max_2, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_2, length.out = 5)[4]+(seq(0, max_2, length.out = 5)[5]-seq(0, max_2, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_2, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,   
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5          
  )+
  labs(
    title = '',
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

plot1<-grid.arrange(p1, p2, ncol = 1, heights = c(1, 1.2))
data_lim <- data.frame(
  A = data_rmsd_clear$SFET_Site,
  B = data_rmsd_cloudy$SFET_Site,
  C=data_rmsd_clear$SSEBOP,
  D=data_rmsd_cloudy$SSEBOP
)

data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
set.seed(123)
data <- data.frame(
  Clear = data_rmsd_clear$SFET_Site,
  Cloudy = data_rmsd_cloudy$SFET_Site
)
data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)

densities3 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_3 <- ceiling(max(sapply(densities3, function(d) max(d$y))))
p3<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_3), 
    expand = c(0, 0),       
    breaks = seq(0, max_3, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_3, length.out = 5)[4]+(seq(0, max_3, length.out = 5)[5]-seq(0, max_3, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_3, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf, 
    y = Inf,   
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5       
  )+
  labs(
    title = "",
    x = "nRMSD",
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

data <- data.frame(
  Clear = data_rmsd_clear$SSEBOP,
  Cloudy= data_rmsd_cloudy$SSEBOP
)
data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)

densities4 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_4 <- ceiling(max(sapply(densities4, function(d) max(d$y))))
p4<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1),
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_4), 
    expand = c(0, 0),       
    breaks = seq(0, max_4, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_4, length.out = 5)[4]+(seq(0, max_4, length.out = 5)[5]-seq(0, max_4, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_4, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf, 
    y = Inf,   
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5          
  )+
  labs(
    title = '',
    x = "nRMSD",
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

plot2<-grid.arrange(p3, p4, ncol = 1, heights = c(1, 1.2))
data_lim <- data.frame(
  A = data_be_clear$SFET_Site,
  B = data_be_cloudy$SFET_Site,
  C=data_be_clear$SSEBOP,
  D=data_be_cloudy$SSEBOP
)

data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
# Sample data
set.seed(123)
data <- data.frame(
  Clear = data_be_clear$SFET_Site,
  Cloudy = data_be_cloudy$SFET_Site
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities5 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_5 <- ceiling(max(sapply(densities5, function(d) max(d$y))))
p5<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_5), 
    expand = c(0, 0),       
    breaks = seq(0, max_5, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  )+annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
             y = seq(0, max_5, length.out = 5)[4]+(seq(0, max_5, length.out = 5)[5]-seq(0, max_5, length.out = 5)[4])/2,
             label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_5, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf, 
    y = Inf,  
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,  
    fontface = "bold", 
    size = 5           
  )+
  labs(
    title = "",
    x = "nBE",
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

data <- data.frame(
  Clear = data_be_clear$SSEBOP,
  Cloudy = data_be_cloudy$SSEBOP
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)

densities6 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_6 <- ceiling(max(sapply(densities6, function(d) max(d$y))))

p6<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_6), 
    expand = c(0, 0),       
    breaks = seq(0, max_6, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_6, length.out = 5)[4]+(seq(0, max_6, length.out = 5)[5]-seq(0, max_6, length.out = 5)[4])/2,
              label = bquote(Clear[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_6, length.out = 5)[4] , label = bquote(Cloudy[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,  
    label = "(b)",
    hjust = -0.1,  
    vjust = 1.1,  
    fontface = "bold", 
    size =5           
  )+
  labs(
    title = '',
    x = "nBE",
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

plot3<-grid.arrange(p5, p6, ncol = 1, heights = c(1, 1.2))

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
  )+annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
             y = seq(0, max_1, length.out = 5)[4]+(seq(0, max_1, length.out = 5)[5]-seq(0, max_1, length.out = 5)[4])/2,
             label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_1, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,  
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
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

data <- data.frame(
  SES = data_r2_clear$SSEBOP,
  MES = data_r2_cloudy$SSEBOP
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities2 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_2 <- ceiling(max(sapply(densities2, function(d) max(d$y))))

p2_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_2), 
    expand = c(0, 0),       
    breaks = seq(0, max_2, length.out = 5) 
  )+annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
             y = seq(0, max_2, length.out = 5)[4]+(seq(0, max_2, length.out = 5)[5]-seq(0, max_2, length.out = 5)[4])/2,
             label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_2, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf, 
    y = Inf,   
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5           
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +
  labs(
    title = '',
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

plot1_1<-grid.arrange(p1_1, p2_1, ncol = 1, heights = c(1, 1.2))

data_lim <- data.frame(
  A = data_rmsd_clear$SFET_Site,
  B = data_rmsd_cloudy$SFET_Site,
  C=data_rmsd_clear$SSEBOP,
  D=data_rmsd_cloudy$SSEBOP
)

data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
# Sample data
set.seed(123)
data <- data.frame(
  SES = data_rmsd_clear$SFET_Site,
  MES = data_rmsd_cloudy$SFET_Site
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities3 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_3 <- ceiling(max(sapply(densities3, function(d) max(d$y))))

p3_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),      
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_3), 
    expand = c(0, 0),       
    breaks = seq(0, max_3, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_3, length.out = 5)[4]+(seq(0, max_3, length.out = 5)[5]-seq(0, max_3, length.out = 5)[4])/2,
              label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_3, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,   
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5         
  )+
  labs(
    title = "",
    x = "nRMSD",
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

data <- data.frame(
  SES = data_rmsd_clear$SSEBOP,
  MES = data_rmsd_cloudy$SSEBOP
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities4 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_4 <- ceiling(max(sapply(densities4, function(d) max(d$y))))

p4_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_4), 
    expand = c(0, 0),       
    breaks = seq(0, max_4, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_4, length.out = 5)[4]+(seq(0, max_4, length.out = 5)[5]-seq(0, max_4, length.out = 5)[4])/2,
              label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_4, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,   
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5           
  )+
  labs(
    title = '',
    x = "nRMSD",
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

plot2_1<-grid.arrange(p3_1, p4_1, ncol = 1, heights = c(1, 1.2))
data_lim <- data.frame(
  A = data_be_clear$SFET_Site,
  B = data_be_cloudy$SFET_Site,
  C=data_be_clear$SSEBOP,
  D=data_be_cloudy$SSEBOP
)
data_lim <- melt(data_lim, variable.name = "Days", value.name = "Value")
min1<-floor(min(data_lim$Value))
max1<-ceiling(max(data_lim$Value))
# Sample data
set.seed(123)
data <- data.frame(
  SES = data_be_clear$SFET_Site,
  MES = data_be_cloudy$SFET_Site
)
data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities5 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_5 <- ceiling(max(sapply(densities5, function(d) max(d$y))))

p5_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_5), 
    expand = c(0, 0),      
    breaks = seq(0, max_5, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_5, length.out = 5)[4]+(seq(0, max_5, length.out = 5)[5]-seq(0, max_5, length.out = 5)[4])/2,
              label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_5, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf, 
    y = Inf,  
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold", 
    size = 5          
  )+
  labs(
    title = "",
    x = "nBE",
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


data <- data.frame(
  SES = data_be_clear$SSEBOP,
  MES = data_be_cloudy$SSEBOP
)

data_melted <- melt(data, variable.name = "Days", value.name = "Value")
median_lines <- aggregate(Value ~ Days, data = data_melted, median)
densities6 <- lapply(split(data_melted$Value, data_melted$Days), density)
max_6 <- ceiling(max(sapply(densities6, function(d) max(d$y))))

p6_1<-ggplot(data_melted, aes(x = Value, fill = Days, color = Days)) +
  scale_x_continuous(
    limits = c(min1, max1), 
    expand = c(0, 0),       
    breaks = seq(min1, max1, length.out = 5) 
  )+scale_y_continuous(
    limits = c(0, max_6), 
    expand = c(0, 0),       
    breaks = seq(0, max_6, length.out = 5) 
  )+
  geom_density(alpha = 0.5, size = 1) +geom_vline(
    data = median_lines, 
    aes(xintercept = Value, color = Days), 
    linetype = "dashed", size = 0.8
  ) +annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2, 
              y = seq(0, max_6, length.out = 5)[4]+(seq(0, max_6, length.out = 5)[5]-seq(0, max_6, length.out = 5)[4])/2,
              label = bquote(SES[median] == .(median_lines$Value[1])))+
  annotate("text", x = seq(min1, max1, length.out = 5)[4]+(seq(min1, max1, length.out = 5)[5]-seq(min1, max1, length.out = 5)[4])/2,
           y =seq(0, max_6, length.out = 5)[4] , label = bquote(MES[median] == .(median_lines$Value[2])))+
  annotate(
    "text",
    x = -Inf,  
    y = Inf,   
    label = "(a)",
    hjust = -0.1,  
    vjust = 1.1,   
    fontface = "bold",  
    size = 5           
  )+
  
  labs(
    title = '',
    x = "nBE",
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

plot3_1<-grid.arrange(p5_1, p6_1, ncol = 1, heights = c(1, 1.2))

plot_new1<-grid.arrange( p1_1,p1, ncol = 1, heights = c(1, 1))
plot_new2<-grid.arrange(p2_1, p2, ncol = 1, heights = c(1, 1))
plot_new3<-grid.arrange(p3_1, p3, ncol = 1, heights = c(1, 1))
plot_new4<-grid.arrange(p4_1, p4, ncol = 1, heights = c(1, 1))
plot_new5<-grid.arrange(p5_1, p5, ncol = 1, heights = c(1, 1))
plot_new6<-grid.arrange(p6_1, p6, ncol = 1, heights = c(1, 1))

#ggsave("D:/Script/Plots_paper/Fig_new1_R2_SFET.png", plot_new1, width = 8, height = 8, dpi = 500,units = 'in')
ggsave("Plots_paper/FigS10.png", plot_new2, width = 8, height = 8, dpi = 500,units = 'in')
ggsave("Plots_paper/FigS11.png", plot_new3, width = 8, height = 8, dpi = 500,units = 'in')
ggsave("Plots_paper/FigS12.png", plot_new4, width = 8, height = 8, dpi = 500,units = 'in')
ggsave("Plots_paper/FigS13.png", plot_new5, width = 8, height = 8, dpi = 500,units = 'in')
ggsave("Plots_paper/FigS14.png", plot_new6, width = 8, height = 8, dpi = 500,units = 'in')












#ggsave("D:/subtle/Work/November2024/Plots_paper1/Fig5_R2_clear_cloudy.png", plot1, width = 10, height = 8, dpi = 500,units = 'in')
#ggsave("D:/subtle/Work/November2024/Plots_paper1/Fig5_nRMSD_clear_cloudy.png", plot2, width = 10, height = 8, dpi = 500,units = 'in')
#ggsave("D:/subtle/Work/November2024/Plots_paper1/Fig5_nBE_clear_cloudy.png", plot3, width = 10, height = 8, dpi = 500,units = 'in')







