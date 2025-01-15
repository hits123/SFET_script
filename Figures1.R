library(xlsx)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(readxl)
library(dplyr)

calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (BE)
  BE <- mean(simulated - observed)
  # Calculate Mean Absolute Error (MAE)
  MAE <- mean(abs(simulated - observed))
  # Calculate Root Mean Square Difference (RMSD)
  RMSD <- sqrt(mean((simulated - observed)^2))
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
# Plotting with analysis
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

  if(nrow(sfet_site)>2){ # min 2 datapoints
    if(length(unique(sfet_site$SSEBOP))>1){
      print(nrow(sfet_site))
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


data_r2 <- data.frame(
  SFET = R2_save$R2_sfet_site,
  SSEBOP = R2_save$R2_ssebop
)

data_mae <- data.frame(
  SFET = mae_save$mae_sfet_site,
  SSEBOp = mae_save$mae_ssebop
)

data_rmsd <- data.frame(
  SFET = rmsd_save$rmsd_sfet_site,
  SSEBOp = rmsd_save$rmsd_ssebop
)

data_be <- data.frame(
  SFET = be_save$be_sfet_site,
  SSEBOp = be_save$be_ssebop
)

summary_r2 <- data.frame(
  Column = colnames(data_r2), 
  Min = round(sapply(data_r2, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2, median, na.rm = TRUE), 2) 
)

summary_rmsd <- data.frame(
  Column = colnames(data_rmsd),
  Min = round(sapply(data_rmsd, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd, median, na.rm = TRUE), 2)
)
summary_be <- data.frame(
  Column = colnames(data_be), 
  Min = round(sapply(data_be, min, na.rm = TRUE), 2),
  Max = round(sapply(data_be, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be, median, na.rm = TRUE), 2) 
)
list1<-list('summary_r2'=summary_r2,'summary_rmsd'=summary_rmsd,'summary_be'=summary_be)
openxlsx::write.xlsx(list1, file = "paper1_plot_daily.xlsx") # saving few tables to tally statistics

data_long_r2 <- melt(data_r2, variable.name = "Model", value.name = "Value")
data_long_rmsd <- melt(data_rmsd, variable.name = "Model", value.name = "Value")
data_long_be <- melt(data_be, variable.name = "Model", value.name = "Value")

identify_outliers <- function(data_long) {
  data_long %>%
    group_by(Model) %>%
    filter(Value < quantile(Value, 0.25) - 1.5 * IQR(Value) |
             Value > quantile(Value, 0.75) + 1.5 * IQR(Value))
}

outliers_r2 <- identify_outliers(data_long_r2)
outliers_rmsd <- identify_outliers(data_long_rmsd)
outliers_be <- identify_outliers(data_long_be)
create_boxplot <- function(data, outliers, plot_label, y_label, ylim_range, y_breaks,n_value) {
  whisker_data <- data %>%
    group_by(Model) %>%
    summarize(
      MinWhisker = max(min(Value), quantile(Value, 0.25) - 1.5 * IQR(Value)),
      MaxWhisker = min(max(Value), quantile(Value, 0.75) + 1.5 * IQR(Value))
    )
  
  ggplot(data, aes(x = Model, y = Value)) +
    geom_boxplot(fill = "gray", color = "black", outlier.shape = NA) + 
    geom_point(data = data, aes(x = Model, y = Value), position = position_jitter(width = 0.2), size = 1, color = "lightcoral") +  
    geom_point(data = whisker_data, aes(x = Model, y = MinWhisker), shape = 1, color = "black", size = 3) + 
    geom_point(data = whisker_data, aes(x = Model, y = MaxWhisker), shape = 1, color = "black", size = 3) +  
    coord_cartesian(ylim = ylim_range) +  
    scale_y_continuous(breaks = y_breaks) +  
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8, face = "bold", color = "black",angle = 0), 
      axis.text.y = element_text(size = 8, face = "bold", color = "black"), 
      axis.title.x = element_blank(),  
      axis.title.y = element_text(size = 8, face = "bold", color = "black"), 
      plot.title = element_blank(), 
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    ) +
    labs(
      y = y_label
    ) +annotate("text", x = Inf, y = Inf, label = paste0("N: ", n_value), hjust = 1.2, vjust = 2, size = 4, fontface = "bold", color = "black")+
    annotate("text", x = -Inf, y = Inf, label = plot_label, hjust = -0.2, vjust = 1.2, size = 4, fontface = "bold", color = "black") +  
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "black", fill = NA, size = 0.5)  
}

y_breaks_r2 <- seq(0, 1, by = 0.2)
y_breaks_rmsd <- seq(0, 2, by = 0.5)
y_breaks_be <- seq(-1, 1, by = 0.5)

plot_a <- create_boxplot(
  data_long_r2, outliers_r2, "(a)", 
  bquote(bold(R)^bold(2)),  # Bold R²
  c(0, 1), y_breaks_r2,n_value=length(R2_save$Site)
)

plot_b <- create_boxplot(
  data_long_rmsd, outliers_rmsd, "(a)", 
  bquote(bold(RMSD)~bold("(")*bold(mm)~bold(day)^bold(-1)*bold(")")),  
  c(0, 2), y_breaks_rmsd,n_value = length(R2_save$Site)
)

plot_c <- create_boxplot(
  data_long_be, outliers_be, "(a)", 
  bquote(bold(Bias~Error)~bold("(")*bold(mm)~bold(day)^bold(-1)*bold(")")),  
  c(-1, 1), y_breaks_be,n_value = length(R2_save$Site)
)

common_name_daily_save<-R2_save$Site



### Disalexi and PTJPL


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

daily_dis_save<-NULL
for (i in site_list) {
  # tryCatch({
  sitename<-strsplit(i,'[.]')[[1]][1]
  sfet_site<-read_xlsx(paste0(SFET_Site,'/',i))
  sfet_nldas_ceres<-read_xlsx(paste0(SFET_NLDAS_CERES,'/',i))
  sfet_nldas_modis<-read_xlsx(paste0(SFET_NLDAS_MODIS,'/',i))
  ssebop<-read_xlsx(paste0(SSEBOP,'/',i))
  ptjpl<-read_xlsx(paste0(PTJPL,'/',i))
  disalexi<-read_xlsx(paste0(Disalexi,'/',i))
  
  
  sfet_site$Date<-as.Date(sfet_site$Date)
  sfet_nldas_ceres$Date<-as.Date(sfet_nldas_ceres$Date)
  sfet_nldas_modis$Date<-as.Date(sfet_nldas_modis$Date)
  ssebop$Date<-as.Date(ssebop$Date)
  ptjpl$Date<-as.Date(ptjpl$Date)
  disalexi$Date<-as.Date(disalexi$Date)
  
  sfet_nldas_ceres<-sfet_nldas_ceres[,c('Date','SFET_CERES_NLDAS_LE')]
  sfet_nldas_modis<-sfet_nldas_modis[,c('Date','SFET_NLDAS_MODIS_LE')]
  ssebop<-ssebop[,c('Date','SSEBOP')]
  ptjpl<-ptjpl[,c('Date','ECO3ETPTJPL_001_EVAPOTRANSPIRATION_PT_JPL_ETdaily')]
  disalexi<-disalexi[,c('Date','ECO3ETALEXI_001_EVAPOTRANSPIRATION_ALEXI_ETdaily')]
  
  sfet_site<-left_join(sfet_site,ssebop,by=c('Date'='Date'))
  sfet_site<-left_join(sfet_site,ptjpl,by=c('Date'='Date'))
  sfet_site<-left_join(sfet_site,disalexi,by=c('Date'='Date'))
  
  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]

  if(nrow(sfet_site)>2){ 
    if(length(unique(sfet_site$SSEBOP))>1){
      print(nrow(sfet_site))
      met_sfet_site=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$LE_SFET )
      met_ssebop=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$SSEBOP )
      met_ptjpl=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$ECO3ETPTJPL_001_EVAPOTRANSPIRATION_PT_JPL_ETdaily )
      met_disalexi=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$ECO3ETALEXI_001_EVAPOTRANSPIRATION_ALEXI_ETdaily )
      
      daily_dis_save<-rbind(daily_dis_save,data.frame(Site=sitename,sfet_site))
      
      R2_save<-rbind(R2_save,data.frame(Site=sitename,R2_sfet_site=met_sfet_site$R_Squared,
                                       
                                        R2_ssebop=met_ssebop$R_Squared,
                                        R2_ptjpl=met_ptjpl$R_Squared,R2_disalexi=met_disalexi$R_Squared))
      
      
      be_save<-rbind(be_save,data.frame(Site=sitename,be_sfet_site=met_sfet_site$Bias_Error,
                                        
                                        be_ssebop=met_ssebop$Bias_Error,
                                        be_ptjpl=met_ptjpl$Bias_Error,be_disalexi=met_disalexi$Bias_Error))
      
      mae_save<-rbind(mae_save,data.frame(Site=sitename,mae_sfet_site=met_sfet_site$Mean_Absolute_Error,
                                         
                                          mae_ssebop=met_ssebop$Mean_Absolute_Error,
                                          mae_ptjpl=met_ptjpl$Mean_Absolute_Error,mae_disalexi=met_disalexi$Mean_Absolute_Error))
      
      rmsd_save<-rbind(rmsd_save,data.frame(Site=sitename,rmsd_sfet_site=met_sfet_site$Root_Mean_Square_Difference,
                                            
                                            rmsd_ssebop=met_ssebop$Root_Mean_Square_Difference,
                                            rmsd_ptjpl=met_ptjpl$Root_Mean_Square_Difference,rmsd_disalexi=met_disalexi$Root_Mean_Square_Difference))
      
      
       }}
  # }, error=function(e){})
}

data_r2 <- data.frame(
  SFET = R2_save$R2_sfet_site,
  SSEBOp = R2_save$R2_ssebop,DisALEXI=R2_save$R2_disalexi,
  PTJPL=R2_save$R2_ptjpl
)

data_mae <- data.frame(
  SFET = mae_save$mae_sfet_site,
 SSEBOp = mae_save$mae_ssebop,DisALEXI=mae_save$mae_disalexi,
  PTJPL=mae_save$mae_ptjpl
)

data_rmsd <- data.frame(
  SFET = rmsd_save$rmsd_sfet_site,
  SSEBOp = rmsd_save$rmsd_ssebop,DisALEXI=rmsd_save$rmsd_disalexi,
  PTJPL=rmsd_save$rmsd_ptjpl
)

data_be <- data.frame(
  SFET = be_save$be_sfet_site,
 SSEBOp = be_save$be_ssebop,DisALEXI=be_save$be_disalexi,
  PTJPL=be_save$be_ptjpl
)

summary_r2 <- data.frame(
  Column = colnames(data_r2), 
  Min = round(sapply(data_r2, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2, median, na.rm = TRUE), 2)
)

summary_rmsd <- data.frame(
  Column = colnames(data_rmsd), 
  Min = round(sapply(data_rmsd, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd, median, na.rm = TRUE), 2) 
)
summary_be <- data.frame(
  Column = colnames(data_be), 
  Min = round(sapply(data_be, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be, median, na.rm = TRUE), 2) 
)
list1<-list('summary_r2'=summary_r2,'summary_rmsd'=summary_rmsd,'summary_be'=summary_be)
openxlsx::write.xlsx(list1, file = "paper1_plot_daily_ecostress.xlsx")


data_long_r2 <- melt(data_r2, variable.name = "Model", value.name = "Value")
data_long_rmsd <- melt(data_rmsd, variable.name = "Model", value.name = "Value")
data_long_be <- melt(data_be, variable.name = "Model", value.name = "Value")

identify_outliers <- function(data_long) {
  data_long %>%
    group_by(Model) %>%
    filter(Value < quantile(Value, 0.25) - 1.5 * IQR(Value) |
             Value > quantile(Value, 0.75) + 1.5 * IQR(Value))
}

outliers_r2 <- identify_outliers(data_long_r2)
outliers_rmsd <- identify_outliers(data_long_rmsd)
outliers_be <- identify_outliers(data_long_be)
create_boxplot <- function(data, outliers, plot_label, y_label, ylim_range, y_breaks,n_value) {
  whisker_data <- data %>%
    group_by(Model) %>%
    summarize(
      MinWhisker = max(min(Value), quantile(Value, 0.25) - 1.5 * IQR(Value)),
      MaxWhisker = min(max(Value), quantile(Value, 0.75) + 1.5 * IQR(Value))
    )
  
  ggplot(data, aes(x = Model, y = Value)) +
    geom_boxplot(fill = "gray", color = "black", outlier.shape = NA) +  
    geom_point(data = data, aes(x = Model, y = Value), position = position_jitter(width = 0.2), size = 1, color = "lightcoral") +  
    geom_point(data = whisker_data, aes(x = Model, y = MinWhisker), shape = 1, color = "black", size = 3) +  
    geom_point(data = whisker_data, aes(x = Model, y = MaxWhisker), shape = 1, color = "black", size = 3) +  
    coord_cartesian(ylim = ylim_range) + 
    scale_y_continuous(breaks = y_breaks) +  
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8, face = "bold", color = "black",angle = 0), 
      axis.text.y = element_text(size = 8, face = "bold", color = "black"),
      axis.title.x = element_blank(), 
      axis.title.y = element_text(size = 8, face = "bold", color = "black"), 
      plot.title = element_blank(), 
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    ) +
    labs(
      y = y_label
    ) +annotate("text", x = Inf, y = Inf, label = paste0("N: ", n_value), hjust = 1.2, vjust = 2, size = 4, fontface = "bold", color = "black")+
    annotate("text", x = -Inf, y = Inf, label = plot_label, hjust = -0.2, vjust = 1.2, size = 4, fontface = "bold", color = "black") +  
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "black", fill = NA, size = 0.5)  
}


y_breaks_r2 <- seq(0, 1, by = 0.2)
y_breaks_rmsd <- seq(0, 9, by = 1)
y_breaks_be <- seq(-2, 8, by = 1)

plot_a_dis <- create_boxplot(
  data_long_r2, outliers_r2, "(b)", 
  bquote(bold(R)^bold(2)),  
  c(0, 1), y_breaks_r2,n_value = length(R2_save$Site)
)

plot_b_dis <- create_boxplot(
  data_long_rmsd, outliers_rmsd, "(b)", 
  bquote(bold(RMSD)~bold("(")*bold(mm)~bold(day)^bold(-1)*bold(")")), c(0, 9), y_breaks_rmsd,n_value = length(R2_save$Site)
)

plot_c_dis <- create_boxplot(
  data_long_be, outliers_be, "(b)", 
  bquote(bold(Bias~Error)~bold("(")*bold(mm)~bold(day)^bold(-1)*bold(")")), c(-2, 8), y_breaks_be,n_value = length(R2_save$Site)
)

common_name_daily_dis_save<-R2_save$Site

## MODIS 8-day
calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (BE)
  BE <- mean(simulated - observed)
  # Calculate Mean Absolute Error (MAE)
  MAE <- mean(abs(simulated - observed))
  # Calculate Root Mean Square Difference (RMSD)
  RMSD <- sqrt(mean((simulated - observed)^2))
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

# Plotting
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
df2<-NULL
for (i in site_list) {
  # tryCatch({
  sitename<-strsplit(i,'[.]')[[1]][1]
  sfet_site<-read_xlsx(paste0(SFET_Site,'/',i))
  sfet_nldas_ceres<-read_xlsx(paste0(SFET_NLDAS_CERES,'/',i))
  sfet_nldas_modis<-read_xlsx(paste0(SFET_NLDAS_MODIS,'/',i))
  ssebop<-read_xlsx(paste0(SSEBOP,'/',i))
  modis<-read_xlsx(paste0(MODIS,'/',i))
  sfet_site$Date<-as.Date(sfet_site$Date)
  sfet_nldas_ceres$Date<-as.Date(sfet_nldas_ceres$Date)
  sfet_nldas_modis$Date<-as.Date(sfet_nldas_modis$Date)
  ssebop$Date<-as.Date(ssebop$Date)
  modis$Date1<-as.Date(modis$Date1)
  modis$Date2<-as.Date(modis$Date2)
  
  sfet_nldas_ceres<-sfet_nldas_ceres[,c('Date','SFET_CERES_NLDAS_LE')]
  sfet_nldas_modis<-sfet_nldas_modis[,c('Date','SFET_NLDAS_MODIS_LE')]
  ssebop<-ssebop[,c('Date','SSEBOP')]
  modis<-modis[,c('Date1','Date2','MODIS_ET_First','DayDifference')]
  
  df1<-NULL
  for (j in 1:nrow(modis)) {
    date1<-modis$Date1[j]
    date2<-modis$Date2[j]
    sfet_site_sub<-sfet_site[(sfet_site$Date>=date1)&(sfet_site$Date<=date2),]
    sfet_nldas_ceres_sub<-sfet_nldas_ceres[(sfet_nldas_ceres$Date>=date1)&(sfet_nldas_ceres$Date<=date2),]
    sfet_nldas_modis_sub<-sfet_nldas_modis[(sfet_nldas_modis$Date>=date1)&(sfet_nldas_modis$Date<=date2),]
    ssebop_sub<-ssebop[(ssebop$Date>=date1)&(ssebop$Date<=date2),]
    if(nrow(sfet_site_sub)==8){
      print(nrow(sfet_site_sub))
      
      df1<-rbind(df1,data.frame(Site=sitename,Date1=date1,Date2=date2,Modis_ET=modis$MODIS_ET_First[j],
                                Modis_DD=modis$DayDifference[j],
                                sfet_site_sub,ssebop_sub))
    }
  }
  df2<-rbind(df2,df1) 
}


df3<-NULL
unique_date<-unique(df2$Date1)
for (k in unique_date) {
  df2_sub<-df2[df2$Date1==k,]
  if(nrow(df2_sub)==8){
    df3=rbind(df3,data.frame(Site=df2_sub$Site[1],Modis_ET=df2_sub$Modis_ET[1],
                             Modis_DD=df2_sub$Modis_DD[1],LE_sfet=sum(df2_sub$LE_SFET,na.rm = T),
                             LE_sfet_count=length(na.omit(df2_sub$LE_SFET)),LE_obs=sum(df2_sub$LE_Obs,na.rm = T),
                             LE_obs_count=length(na.omit(df2_sub$LE_Obs)),
                             LE_SSEBOP=sum(df2_sub$SSEBOP,na.rm = T),
                             LE_SSEBOP_count=length(na.omit(df2_sub$SSEBOP))))
    
  }}
df3<-df3[df3$LE_sfet_count==8&df3$LE_obs_count==8&df3$LE_SSEBOP_count==8,]

modis_save<-NULL
R2_save<-NULL
be_save<-NULL
mae_save<-NULL
rmsd_save<-NULL

for (k1 in unique(df3$Site)) {
  df3_sub=df3[df3$Site==k1,]
  if(nrow(df3_sub)>2){ 
    print(nrow(df3_sub))
    met_sfet_site=calculate_all_metrics(observed = df3_sub$LE_obs,simulated =df3_sub$LE_sfet )
    met_ssebop=calculate_all_metrics(observed = df3_sub$LE_obs,simulated =df3_sub$LE_SSEBOP )
    met_modis=calculate_all_metrics(observed = df3_sub$LE_obs,simulated =df3_sub$Modis_ET )
    
    modis_save<-rbind(modis_save,data.frame(Site=k1,df3_sub))
    
    
    R2_save<-rbind(R2_save,data.frame(Site=k1,R2_sfet_site=met_sfet_site$R_Squared,
                                      
                                      R2_ssebop=met_ssebop$R_Squared,
                                      R2_modis=met_modis$R_Squared))
    
    be_save<-rbind(be_save,data.frame(Site=k1,be_sfet_site=met_sfet_site$Bias_Error,
                                      
                                      be_ssebop=met_ssebop$Bias_Error,
                                      be_modis=met_modis$Bias_Error))
    
    mae_save<-rbind(mae_save,data.frame(Site=k1,mae_sfet_site=met_sfet_site$Mean_Absolute_Error,
                                        
                                        mae_ssebop=met_ssebop$Mean_Absolute_Error,
                                        mae_modis=met_modis$Mean_Absolute_Error))
    
    rmsd_save<-rbind(rmsd_save,data.frame(Site=k1,rmsd_sfet_site=met_sfet_site$Root_Mean_Square_Difference,
                                          
                                          rmsd_ssebop=met_ssebop$Root_Mean_Square_Difference,
                                          rmsd_modis=met_modis$Root_Mean_Square_Difference)) 
    
  }
  #}
  
}

be_save<-be_save[-14,] # Due to NaN value for R2 at row 14
R2_save<-R2_save[-14,]
rmsd_save<-rmsd_save[-14,]
modis_img<-na.omit(R2_save)
data_r2 <- data.frame(
  SFET = R2_save$R2_sfet_site,
  SSEBOp = R2_save$R2_ssebop,
  MODIS=R2_save$R2_modis
)

data_mae <- data.frame(
  SFET = mae_save$mae_sfet_site,
  SSEBOp = mae_save$mae_ssebop,
  MODIS=mae_save$mae_modis
)

data_rmsd <- data.frame(
  SFET = rmsd_save$rmsd_sfet_site,
  SSEBOp = rmsd_save$rmsd_ssebop,
  MODIS=rmsd_save$rmsd_modis
)

data_be <- data.frame(
  SFET = be_save$be_sfet_site,
  SSEBOp = be_save$be_ssebop,
  MODIS=be_save$be_modis
)

summary_r2 <- data.frame(
  Column = colnames(data_r2), 
  Min = round(sapply(data_r2, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_r2, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_r2, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_r2, median, na.rm = TRUE), 2) 
)

summary_rmsd <- data.frame(
  Column = colnames(data_rmsd), 
  Min = round(sapply(data_rmsd, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_rmsd, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_rmsd, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_rmsd, median, na.rm = TRUE), 2) 
)
summary_be <- data.frame(
  Column = colnames(data_be), 
  Min = round(sapply(data_be, min, na.rm = TRUE), 2), 
  Max = round(sapply(data_be, max, na.rm = TRUE), 2), 
  Mean = round(sapply(data_be, mean, na.rm = TRUE), 2), 
  Median = round(sapply(data_be, median, na.rm = TRUE), 2) 
)
list1<-list('summary_r2'=summary_r2,'summary_rmsd'=summary_rmsd,'summary_be'=summary_be)
openxlsx::write.xlsx(list1, file = "paper1_plot_8day.xlsx")

data_long_r2 <- melt(data_r2, variable.name = "Model", value.name = "Value")
data_long_rmsd <- melt(data_rmsd, variable.name = "Model", value.name = "Value")
data_long_be <- melt(data_be, variable.name = "Model", value.name = "Value")

identify_outliers <- function(data_long) {
  data_long %>%
    group_by(Model) %>%
    filter(Value < quantile(Value, 0.25) - 1.5 * IQR(Value) |
             Value > quantile(Value, 0.75) + 1.5 * IQR(Value))
}

outliers_r2 <- identify_outliers(data_long_r2)
outliers_rmsd <- identify_outliers(data_long_rmsd)
outliers_be <- identify_outliers(data_long_be)
create_boxplot <- function(data, outliers, plot_label, y_label, ylim_range, y_breaks,n_value) {
  whisker_data <- data %>%
    group_by(Model) %>%
    summarize(
      MinWhisker = max(min(Value), quantile(Value, 0.25) - 1.5 * IQR(Value)),
      MaxWhisker = min(max(Value), quantile(Value, 0.75) + 1.5 * IQR(Value))
    )
  
  ggplot(data, aes(x = Model, y = Value)) +
    geom_boxplot(fill = "gray", color = "black", outlier.shape = NA) + 
    geom_point(data = data, aes(x = Model, y = Value), position = position_jitter(width = 0.2), size = 1, color = "lightcoral") +  
    geom_point(data = whisker_data, aes(x = Model, y = MinWhisker), shape = 1, color = "black", size = 3) +  
    geom_point(data = whisker_data, aes(x = Model, y = MaxWhisker), shape = 1, color = "black", size = 3) + 
    coord_cartesian(ylim = ylim_range) +  
    scale_y_continuous(breaks = y_breaks) +  
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8, face = "bold", color = "black",angle = 0), 
      axis.text.y = element_text(size = 8, face = "bold", color = "black"),
      axis.title.x = element_blank(), 
      axis.title.y = element_text(size = 8, face = "bold", color = "black"), 
      plot.title = element_blank(), 
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    ) +
    labs(
      y = y_label
    ) +annotate("text", x = Inf, y = Inf, label = paste0("N: ", n_value), hjust = 1.2, vjust = 2, size = 4, fontface = "bold", color = "black")+
    annotate("text", x = -Inf, y = Inf, label = plot_label, hjust = -0.2, vjust = 1.2, size = 4, fontface = "bold", color = "black") +  
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "black", fill = NA, size = 0.5) 
}

y_breaks_r2 <- seq(0, 1, by = 0.2)
y_breaks_rmsd <- seq(2, 16, by = 2)
y_breaks_be <- seq(-10, 8, by = 2)

plot_a_mod <- create_boxplot(
  data_long_r2, outliers_r2, "(c)", 
  bquote(bold(R)^bold(2)), 
  c(0, 1), y_breaks_r2,n_value = length(R2_save$Site)
)

plot_b_mod <- create_boxplot(
  data_long_rmsd, outliers_rmsd, "(c)", 
  bquote(bold(RMSD)~bold("(")*bold(mm)~bold(8*day)^bold(-1)*bold(")")), c(2, 16), y_breaks_rmsd,n_value = length(R2_save$Site)
)

plot_c_mod <- create_boxplot(
  data_long_be, outliers_be, "(c)", 
  bquote(bold(Bias~Error)~bold("(")*bold(mm)~bold(8*day)^bold(-1)*bold(")")), c(-10, 8), y_breaks_be,n_value = length(R2_save$Site)
)

png("Plots_paper/Fig2_main.png", width = 6.5, height = 6.5, units = "in", res = 500)
grid.arrange(plot_a, plot_a_dis, plot_a_mod, ncol = 1)
dev.off()

png("Plots_paper/FigS1.png", width = 6.5, height = 6.5, units = "in", res = 500)
grid.arrange(plot_b, plot_b_dis, plot_b_mod, ncol = 1)
dev.off()

png("Plots_paper/FigS2.png", width = 6.5, height = 6.5, units = "in", res = 500)
grid.arrange(plot_c, plot_c_dis, plot_c_mod, ncol = 1)
dev.off()

common_name_daily_modis_save<-R2_save$Site

# for plotting in python
modis_save<-modis_save[modis_save$Site!='US-Rws',] ## it does not come into common sites in the analysis which are 33 in total
write.xlsx(daily_save,paste0("output/Pooled_daily.xlsx"))
write.xlsx(daily_dis_save,paste0("output/Pooled_daily_Dis.xlsx"))
write.xlsx(modis_save,paste0("output/Pooled_modis.xlsx"))

## Saving data for SFET configurations
calculate_all_metrics <- function(observed, simulated) {
  if (length(observed) != length(simulated)) {
    stop("The observed and simulated values must have the same length.")
  }
  # Calculate Bias Error (BE)
  BE <- mean(simulated - observed)
  # Calculate Mean Absolute Error (MAE)
  MAE <- mean(abs(simulated - observed))
  # Calculate Root Mean Square Difference (RMSD)
  RMSD <- sqrt(mean((simulated - observed)^2))
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
  
  sfet_site<-left_join(sfet_site,sfet_nldas_ceres,by=c('Date'='Date'))
  sfet_site<-left_join(sfet_site,sfet_nldas_modis,by=c('Date'='Date'))

  sfet_site<-na.omit(sfet_site)
  sfet_site<-sfet_site[sfet_site$LE_Obs>0,]

  if(nrow(sfet_site)>2){ 
      print(nrow(sfet_site))
      met_sfet_site=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$LE_SFET )
      met_nldas_ceres=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$SFET_CERES_NLDAS_LE )
      met_nldas_modis=calculate_all_metrics(observed = sfet_site$LE_Obs,simulated =sfet_site$SFET_NLDAS_MODIS_LE )

      daily_save<-rbind(daily_save,data.frame(Site=sitename,sfet_site))
  #}
  }
  #  }, error=function(e){})
}

write.xlsx(daily_save,paste0("output/Pooled_SFET_conf.xlsx"))
