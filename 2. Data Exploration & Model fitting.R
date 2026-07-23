# Data Exploration & Models

### Set Working Directory
#-------------------------------------------------------------------------------
  setwd("C:/Users/jeret/Desktop/Daten Bachelorarbeit")
#-------------------------------------------------------------------------------

###Load Libraries
#-------------------------------------------------------------------------------
  library(olsrr)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(glmmTMB)
  library(corrplot)
  library(splines)
  library(performance)
  library(ranger)
  library(caret)
  library(GGally)
  library(broom)
#-------------------------------------------------------------------------------

### Load CSVs
#-------------------------------------------------------------------------------
  CH4 <- read_csv("CH4.csv")
  RECO <- read_csv ("RECO.csv")
#-------------------------------------------------------------------------------

### ggplot Publish Theme
#-------------------------------------------------------------------------------
  theme_pub <- function(base_size = 11, base_family = "") {
    theme_classic(base_size = base_size, base_family = base_family) +
      theme(
        plot.title = element_text(face = "bold", size = base_size + 3, margin = ggplot2::margin(b = +5)),
        plot.subtitle = element_text(size = base_size, margin = ggplot2::margin(b = +10)),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(linewidth = 0.4),
        axis.ticks = element_line(linewidth = 0.4),
        legend.title = element_text(face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "grey97", color = "grey80", linewidth = 0.4),
        legend.key = element_rect(fill = "grey97", color = NA),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
#-------------------------------------------------------------------------------
  
### Prechoose Variables by Correlation Matrices
#-------------------------------------------------------------------------------
  #CH4 Corrplot
    CH4_ON <- CH4 %>%
      select(flux_CH4, Tair, ST2, ST5, ST10, WT, RH, PAR)
    cor_CH4 <- cor(
      CH4_ON,
      use = "pairwise.complete.obs",
      method = "pearson")
    CH4_names <- c(
      "CH4 flux",
      "Air temp.",
      "Soil temp. 2 cm",
      "Soil temp. 5 cm",
      "Soil temp. 10 cm",
      "Water table",
      "Rel. humidity",
      "PAR")
    rownames(cor_CH4) <- CH4_names
    colnames(cor_CH4) <- CH4_names
    png(
      "figures/CH4_corrplot.png",
      width = 1800,
      height = 1800,
      res = 300)
    corrplot(
      cor_CH4,
      method = "number",
      tl.col = "black",
      tl.cex = 0.9,
      number.cex = 0.7)
    dev.off()
  #RECO Corrplot
    RECO_ON <- RECO %>%
      select(CO2.flux_mg,Tair, ST2, ST5, ST10, WT, RH, PAR)
    cor_RECO <- cor(
      RECO_ON,
      use = "pairwise.complete.obs",
      method = "pearson")
    RECO_names <- c(
      "CO2 flux",
      "Air temp. ",
      "Soil temp. 2 cm",
      "Soil temp. 5 cm",
      "Soil temp. 10 cm",
      "Water table",
      "Rel. humidity",
      "PAR")
    rownames(cor_RECO) <- RECO_names
    colnames(cor_RECO) <- RECO_names
    png(
      "figures/RECO_corrplot.png",
      width = 1800,
      height = 1800,
      res = 300)
    corrplot(
      cor_RECO,
      method = "number",
      tl.col = "black",
      tl.cex = 0.9,
      number.cex = 0.7)
    dev.off()
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 1. Detection of Outliers
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

### Check for Outliers
#-------------------------------------------------------------------------------
  # Dot Plot Function
      plot_sorted <- function(data, date_col, plot_col, x_axis) {
        data <- data[order(data[[date_col]]), ]
        data$order <- seq_len(nrow(data))
        ggplot(data, aes(x = .data[[plot_col]], y = order)) +
          geom_segment(
            aes(
              x = 0,
              xend = .data[[plot_col]],
              y = order,
              yend = order
            ),
            color = "grey80"
          ) +
          geom_point(size = 1) +
          labs(
            x = x_axis,
            y = "Sorted by Date"
          ) +
          theme_pub()
      }
  # Response Variable
    CH4_d <- plot_sorted(data = CH4, date_col = "Date", plot_col = "flux_CH4", x_axis = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")))
    RECO_d <- plot_sorted(data = RECO, date_col = "Date", plot_col = "CO2.flux_mg", x_axis = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ s^{-1} * "]")))
  # Covariates
    #CH4 (ST5, WT, RH, PAR)
      CH4_d_ST <- plot_sorted(data = CH4, date_col = "Date", plot_col = "ST10", x_axis = "Soil Temperature at 10 cm [°C]")
      CH4_d_WT <- plot_sorted(data = CH4, date_col = "Date", plot_col = "WT", x_axis = "Level Depth to Water [cm]")
      CH4_d_RH <- plot_sorted(data = CH4, date_col = "Date", plot_col = "RH", x_axis = "Relative Humidity [%]")
      CH4_d_PAR <- plot_sorted(data = CH4, date_col = "Date", plot_col = "PAR", x_axis = expression(bold("Photosynthetically Active Radiation [" * µmol ~ m^{-2} ~ s^{-1} * "]")))
    #RECO (ST5, WT, RH, PAR)
      RECO_d_ST <- plot_sorted(data = RECO, date_col = "Date", plot_col = "ST10", x_axis = "Soil Temperature at 10 cm [°C]")
      RECO_d_WT <- plot_sorted(data = RECO, date_col = "Date", plot_col = "WT", x_axis = "Level Depth to Water [cm]")
      RECO_d_RH <- plot_sorted(data = RECO, date_col = "Date", plot_col = "RH", x_axis = "Relative Humidity [%]")
      RECO_d_PAR <- plot_sorted(data = RECO, date_col = "Date", plot_col = "PAR", x_axis = expression(bold("Photosynthetically Active Radiation [" * µmol ~ m^{-2} ~ s^{-1} * "]")))
  # Export Dot Charts
    plots_d <- list(
      CH4_d = CH4_d,
      CH4_d_ST = CH4_d_ST,
      CH4_d_WT = CH4_d_WT,
      CH4_d_RH = CH4_d_RH,
      CH4_d_PAR = CH4_d_PAR,
      RECO_d = RECO_d,
      RECO_d_ST = RECO_d_ST,
      RECO_d_WT = RECO_d_WT,
      RECO_d_RH = RECO_d_RH,
      RECO_d_PAR = RECO_d_PAR)
    dir.create("figures", showWarnings = FALSE)
    for (name in names(plots_d)) {
      ggsave(
        filename = paste0("figures/", name, ".png"),
        plot = plots_d[[name]],
        width = 6,
        height = 4,
        dpi = 600)}
#-------------------------------------------------------------------------------

### Removing Outliers
#-------------------------------------------------------------------------------
  # Response Variables
     RECO <- RECO %>%
       filter(CO2.flux_mg <= 4500)
  # Covariates
    # CH4
      CH4 <- CH4 %>%
        filter(WT <= 80)
    # RECO
      RECO <- RECO %>%
        filter(RH >= 0)
      RECO <- RECO[!(RECO$Date %in% as.POSIXct(c(
        "2019-06-14 15:36:34",
        "2019-06-14 15:41:23",
        "2019-06-14 15:47:15"
      ), tz = "UTC")), ]
#--> carry over to Data Preparation 
#-------------------------------------------------------------------------------

### Setup Initial Models
#-------------------------------------------------------------------------------
  #LM
    CH4.lm <- lm(flux_CH4 ~ ST10 + WT + RH + PAR + plant_type, data=CH4)
    RECO.lm <- lm(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data=RECO)
  #RF
    CH4.rf <- ranger(flux_CH4 ~ ST10 + WT + RH + PAR + plant_type, data = CH4, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    RECO.rf <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 2. Evaluation of Homogeneity and Normality in Y
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # Linear Regression
    CH4_aug <- augment(CH4.lm)
    RECO_aug <- augment(RECO.lm)
  # Residuals vs Fitted
    CH4_rvf <- ggplot(CH4_aug, aes(x = .fitted, y = .resid)) +
      geom_point(size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "Fitted values",
        y = "Residuals"
      ) +
      theme_pub()
    RECO_rvf <- ggplot(RECO_aug, aes(x = .fitted, y = .resid)) +
      geom_point(size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "Fitted values",
        y = "Residuals"
      ) +
      theme_pub()
  # Normality Q-Q plot:
    CH4_qq <- ggplot(CH4_aug, aes(sample = .std.resid)) +
      stat_qq(size = 1) +
      stat_qq_line() +
      labs(
        x = "Theoretical Quantiles",
        y = "Standardized Residuals"
      ) +
      theme_pub()
    RECO_qq <- ggplot(RECO_aug, aes(sample = .std.resid)) +
      stat_qq(size = 1) +
      stat_qq_line() +
      labs(
        x = "Theoretical Quantiles",
        y = "Standardized Residuals"
      ) +
      theme_pub()
    plots_r <- list(
      CH4_rvf = CH4_rvf,
      RECO_rvf = RECO_rvf,
      CH4_qq = CH4_qq,
      RECO_qq = RECO_qq)
    dir.create("figures", showWarnings = FALSE)
    for (name in names(plots_r)) {
      ggsave(
        filename = paste0("figures/", name, ".png"),
        plot = plots_r[[name]],
        width = 6,
        height = 4,
        dpi = 600)}
  # Random Forest
    # ==> Isnt needed because neither normality or homogenity is assumed 
  # GLMM
    # ==> Instead check if Response follows assumed distribution family
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 3. Check for Zero Inflation
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  

### Look for Zero Inflation
#-------------------------------------------------------------------------------
  # CH4  
    p_CH4_z <- ggplot(CH4, aes(x = flux_CH4)) +
      geom_histogram(binwidth = 500, fill = "tomato", color = "black") +
      labs(
        x = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
        y = "Frequency"
      ) +
    theme_pub()
    ggsave(
      filename = "figures/p_CH4_z.png",
      plot = p_CH4_z,
      width = 6,
      height = 3,
      dpi = 600)
  # RECO
    p_RECO_z <- ggplot(RECO, aes(x = CO2.flux_mg)) +
      geom_histogram(binwidth = 50, fill = "tomato", color = "black") +
      labs(
        x = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ s^{-1} * "]")),
        y = "Frequency"
      ) +
    theme_pub()
    ggsave(
      filename = "figures/p_RECO_z.png",
      plot = p_RECO_z,
      width = 6,
      height = 3,
      dpi = 600)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 4. Check for Collinearity between Covariates 
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  

### Test for Collinearity
#-------------------------------------------------------------------------------
  # CH4
    ols_vif_tol(CH4.lm)
  # RECO
    ols_vif_tol(RECO.lm)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 5. Check Relationships between X & Y Variables & Condideration of Interactions
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  

### Relationships between variables
#-------------------------------------------------------------------------------
  # CH4
    CH4_pairs <- ggpairs(CH4[, 3:12]) +
      theme_pub() +
      theme(
        strip.text = element_text(size = 7),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1)) 
    ggsave(
      filename = "figures/CH4_pairs.png",
      plot = CH4_pairs,
      width = 10,
      height = 9,
      dpi = 600)
  # RECO
    RECO_pairs <- ggpairs(RECO[, 3:12]) +
      theme_pub() +
      theme(
        strip.text = element_text(size = 7),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1)) 
    ggsave(
      filename = "figures/RECO_pairs.png",
      plot = RECO_pairs,
      width = 10,
      height = 9,
      dpi = 600)
  # WT and CH4 Flux
    p_CH4_WT <- ggplot(CH4, aes(x = WT, y = flux_CH4)) +
      geom_line(color = "grey50", linewidth = 0.5) +
      geom_point(size = 1) +
      labs(
        x = "Water Table [cm]",
        y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
      ) +
      theme_pub()
    ggsave(
      filename = "figures/p_CH4_WT.png",
      plot = p_CH4_WT,
      width = 6,
      height = 4,
      dpi = 600)
    # WT and RECO Flux
    p_RECO_WT <- ggplot(RECO, aes(x = WT, y = CO2.flux_mg)) +
      geom_line(color = "grey50", linewidth = 0.5) +
      geom_point(size = 1) +
      labs(
        x = "Water Table [cm]",
        y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]"))
      ) +
      theme_pub()
    ggsave(
      filename = "figures/p_RECO_WT.png",
      plot = p_RECO_WT,
      width = 6,
      height = 4,
      dpi = 600)
  # ST10 and CH4 Flux
    p_CH4_ST10 <- ggplot(CH4, aes(x = ST10, y = flux_CH4)) +
      geom_line(color = "grey50", linewidth = 0.5) +
      geom_point(size = 1) +
      labs(
        x = "Soil Temperature at 10 cm depth [°C]",
        y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
      ) +
      theme_pub()
    ggsave(
      filename = "figures/p_CH4_ST10.png",
      plot = p_CH4_ST10,
      width = 6,
      height = 4,
      dpi = 600)
   # ST5 and RECO Flux
    p_RECO_ST5 <- ggplot(RECO, aes(x = ST5, y = CO2.flux_mg)) +
      geom_line(color = "grey50", linewidth = 0.5) +
      geom_point(size = 1) +
      labs(
        x = "Soil Temperature at 5 cm depth [°C]",
        y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]"))
      ) +
      theme_pub()
    ggsave(
      filename = "figures/p_RECO_ST5.png",
      plot = p_RECO_ST5,
      width = 6,
      height = 4,
      dpi = 600)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 6. Independence of Response Variable
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  

### Look for Independence of Response Variable
#-------------------------------------------------------------------------------
  # CH4
    lmtest::dwtest(CH4.lm)
    # Time series plot
      p_CH4_TS <- ggplot(CH4, aes(x = Date, y = flux_CH4)) +
        geom_line(color = "grey50", linewidth = 0.5) +
        geom_point(size = 1) +
        labs(
          x = "Date",
          y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
        ) +
        theme_pub()
    # Site dependence plot
      p_CH4_SD <- ggplot(CH4, aes(x = Site, y = flux_CH4)) +
        geom_line(color = "grey50", linewidth = 0.5) +
        geom_point(size = 1) +
        labs(
          x = "Site",
          y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
        ) +
        theme_pub()
  # RECO
    lmtest::dwtest(RECO.lm)
    # Time series plot
      p_RECO_TS <- ggplot(RECO, aes(x = Date, y = CO2.flux_mg)) +
        geom_line(color = "grey50", linewidth = 0.5) +
        geom_point(size = 1) +
        labs(
          x = "Date",
          y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ s^{-1} * "]"))
        ) +
        theme_pub()
    # Site dependence plot
      p_RECO_SD <- ggplot(RECO, aes(x = Site, y = CO2.flux_mg)) +
        geom_line(color = "grey50", linewidth = 0.5) +
        geom_point(size = 1) +
        labs(
          x = "Site",
          y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ s^{-1} * "]"))
        ) +
        theme_pub()
  # Export Plots:
    plots_D <- list(
      p_CH4_TS = p_CH4_TS,
      p_CH4_SD = p_CH4_SD,
      p_RECO_TS = p_RECO_TS,
      p_RECO_SD = p_RECO_SD
    )
    dir.create("figures", showWarnings = FALSE)
    for (name in names(plots_D)) {
      ggsave(
        filename = paste0("figures/", name, ".png"),
        plot = plots_D[[name]],
        width = 6,
        height = 4,
        dpi = 600
      )}
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Test Models with Interactions and adjusted Relationships
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 

### Multi Linear Models
#-------------------------------------------------------------------------------
  # CH4
    CH4.lm <- lm(flux_CH4 ~ ST10 + WT + RH + PAR + plant_type, data=CH4)
    summary(CH4.lm)
    CH4.lm.2 <- lm(flux_CH4 ~ ns(ST10, df = 3) + WT + RH + PAR + plant_type, data=CH4)
    summary(CH4.lm.2)
    AIC(CH4.lm, CH4.lm.2)
    CH4.lm.3 <- lm(flux_CH4 ~ (ST10 + WT + RH + PAR) * plant_type, data=CH4)
    summary(CH4.lm.3)
    AIC(CH4.lm, CH4.lm.3)
    CH4.lm.4 <- lm(flux_CH4 ~ ST10 * WT + RH + PAR + plant_type, data=CH4)
    summary(CH4.lm.4)
    AIC(CH4.lm, CH4.lm.4)
    CH4.lm.5 <- lm(flux_CH4 ~ ns(ST10, df = 3) * WT + RH + PAR + plant_type, data=CH4)
    summary(CH4.lm.5)
    AIC(CH4.lm.4, CH4.lm.5)
    CH4.lm.6 <- lm(flux_CH4 ~ ns(ST10, df = 3) * WT + RH + plant_type, data=CH4)
    summary(CH4.lm.6)
    AIC(CH4.lm.5, CH4.lm.6)
    CH4.lm.7 <- lm(flux_CH4 ~ ns(ST10, df = 3) * WT + RH + PAR, data=CH4)
    summary(CH4.lm.7)
    AIC(CH4.lm.6, CH4.lm.7)
    CH4.lm.8 <- lm(flux_CH4 ~ ns(ST10, df = 3) * WT + RH , data=CH4)
    summary(CH4.lm.8)
    AIC(CH4.lm.7, CH4.lm.8)
    par(mfrow=c(2,2))
    plot(CH4.lm.8)
  # RECO
    RECO.lm <- lm(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data=RECO)
    summary(RECO.lm)
    RECO.lm.2 <- lm(CO2.flux_mg ~ ns(ST5, df =3 ) + WT + RH + PAR + plant_type, data=RECO)
    summary(RECO.lm.2)
    AIC(RECO.lm, RECO.lm.2)
    RECO.lm.3 <- lm(CO2.flux_mg ~ ns(ST5, df = 3) * WT + RH + PAR + plant_type, data=RECO)
    summary(RECO.lm.3)
    AIC(RECO.lm.2, RECO.lm.3)
    RECO.lm.4 <- lm(CO2.flux_mg ~ ns(ST5, df = 3) * WT + RH + ST5 * PAR + plant_type, data=RECO)
    summary(RECO.lm.4)
    AIC(RECO.lm.3, RECO.lm.4)
    RECO.lm.5 <- lm(CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH + ST5 * PAR) * plant_type, data=RECO)
    summary(RECO.lm.5)
    AIC(RECO.lm.4, RECO.lm.5)
    RECO.lm.6 <- lm(CO2.flux_mg ~ (ns(ST5, df = 3) * WT + ST5:PAR + PAR + RH) * plant_type, data=RECO)
    summary(RECO.lm.6)
    AIC(RECO.lm.4, RECO.lm.6)
    par(mfrow=c(2,2))
    plot(RECO.lm.6)
#-------------------------------------------------------------------------------

### Random Forest Models
#-------------------------------------------------------------------------------
  # CH4
    CH4.rf <- ranger(flux_CH4 ~ ST10 + WT + RH + PAR + plant_type, data = CH4, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    print(CH4.rf)
    importance(CH4.rf)
    CH4.rf.2 <- ranger(flux_CH4 ~ ST10 + WT + RH + PAR, data = CH4, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    print(CH4.rf.2)
    importance(CH4.rf.2)
    CH4.rf$prediction.error
    CH4.rf.2$prediction.error
    CH4.rf.3 <- ranger(flux_CH4 ~ ST10 + WT + RH + plant_type, data = CH4, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    print(CH4.rf.3)
    importance(CH4.rf.3)
    CH4.rf$prediction.error
    CH4.rf.3$prediction.error
    # check best mtry
      results <- data.frame()
      for(m in 1:3){
        rf <- ranger(
          flux_CH4 ~ ST10 + WT + RH,
          data = CH4,
          mtry = m,
          num.trees = 500,
          importance = "permutation",
          seed = 123)
        results <- rbind(results,
                         data.frame(
                           mtry = m,
                           mse = rf$prediction.error,
                           rsq = rf$r.squared
                         ))}
      results
      # --> mtry = 3 is best
    CH4.rf.4 <- ranger(flux_CH4 ~ ST10 + WT + RH + plant_type, data = CH4, mtry = 3, importance = "permutation", num.trees = 1000, seed = 123)
    print(CH4.rf.4)
    importance(CH4.rf.4)
    CH4.rf.3$prediction.error
    CH4.rf.4$prediction.error
    CH4.rf.5 <- ranger(flux_CH4 ~ ST10 + WT + RH + plant_type, data = CH4, mtry = 3, importance = "permutation", num.trees = 2000, seed = 123)
    print(CH4.rf.5)
    importance(CH4.rf.5)
    CH4.rf.4$prediction.error
    CH4.rf.5$prediction.error
    CH4.rf.6 <- ranger(flux_CH4 ~ ST10 + WT + RH + plant_type, data = CH4, mtry = 3, splitrule = "extratrees", importance = "permutation", num.trees = 1000, seed = 123)
    print(CH4.rf.6)
    importance(CH4.rf.6)
    CH4.rf.4$prediction.error
    CH4.rf.6$prediction.error
    # Check best min.node.size
      results_node <- data.frame()
      for(n in c(1, 3, 5, 10, 20)){
        rf <- ranger(
          flux_CH4 ~ ST10 + WT + RH + plant_type,
          data = CH4,
          mtry = 3,
          splitrule = "extratrees",
          min.node.size = n,
          num.trees = 1000,
          importance = "permutation",
          seed = 123)
        results_node <- rbind(
          results_node,
          data.frame(
            min.node.size = n,
            mse = rf$prediction.error,
            rsq = rf$r.squared
          ))}
      results_node 
      # -> min.node.size = 10 is best
    CH4.rf.7 <- ranger(flux_CH4 ~ ST10 + WT + RH + plant_type, data = CH4, mtry = 3, min.node.size = 10, splitrule = "extratrees", importance = "permutation", num.trees = 1000, seed = 123)
    print(CH4.rf.7)
    importance(CH4.rf.7)
    CH4.rf.6$prediction.error
    CH4.rf.7$prediction.error
    # --> Best Model
  # RECO
    RECO.rf <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    print(RECO.rf)
    importance(RECO.rf)
    RECO.rf.2 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR , data = RECO, mtry = 3, importance = "permutation", num.trees = 500, seed = 123)
    print(RECO.rf.2)
    importance(RECO.rf.2)
    RECO.rf$prediction.error
    RECO.rf.2$prediction.error #-> dont remove plant_type
    # check best mtry
      results <- data.frame()
      for(m in 1:5){
        rf <- ranger(
          CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type,
          data = RECO,
          mtry = m,
          num.trees = 500,
          importance = "permutation",
          seed = 123)
        results <- rbind(results,
                         data.frame(
                           mtry = m,
                           mse = rf$prediction.error,
                           rsq = rf$r.squared
                         ))}
      results
      # --> mtry = 2 is best
    RECO.rf.3 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 2, importance = "permutation", num.trees = 500, seed = 123)
    print(RECO.rf.3)
    importance(RECO.rf.3)
    RECO.rf$prediction.error
    RECO.rf.3$prediction.error
    RECO.rf.4 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 2, importance = "permutation", num.trees = 1000, seed = 123)
    print(RECO.rf.4)
    importance(RECO.rf.4)
    RECO.rf.3$prediction.error
    RECO.rf.4$prediction.error
    RECO.rf.5 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 2, importance = "permutation", num.trees = 2000, seed = 123)
    print(RECO.rf.5)
    importance(RECO.rf.5)
    RECO.rf.4$prediction.error #--> keep 4 
    RECO.rf.5$prediction.error
    RECO.rf.6 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 2, splitrule = "extratrees", importance = "permutation", num.trees = 1000, seed = 123)
    print(RECO.rf.6)
    importance(RECO.rf.6)
    RECO.rf.4$prediction.error 
    RECO.rf.6$prediction.error
    # Check best min.node.size
      results_node <- data.frame()
      for(n in c(1, 3, 5, 10, 20)){
        rf <- ranger(
          CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type,
          data = RECO,
          mtry = 2,
          splitrule = "extratrees",
          min.node.size = n,
          num.trees = 1000,
          importance = "permutation",
          seed = 123)
        results_node <- rbind(
          results_node,
          data.frame(
            min.node.size = n,
            mse = rf$prediction.error,
            rsq = rf$r.squared))
      }
      results_node 
      # min.node.size = 3 is best
    RECO.rf.7 <- ranger(CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, data = RECO, mtry = 2, min.node.size = 3, splitrule = "extratrees", importance = "permutation", num.trees = 1000, seed = 123)
    print(RECO.rf.7)
    importance(RECO.rf.7)
    RECO.rf.6$prediction.error 
    RECO.rf.7$prediction.error
  # --> Best Model
#-------------------------------------------------------------------------------
  
### GLMMs
#-------------------------------------------------------------------------------
  # CH4
    # Choose distribution family
      hist(CH4$flux_CH4)
      plot(density(CH4$flux_CH4))
      qqnorm(CH4$flux_CH4); qqline(CH4$flux_CH4)
    # Adjust CH4 flux to not be negative:
      CH4 <- CH4 %>%
        mutate(flux_CH4.a = flux_CH4 + 261.5227132)
    # WT Threshold
      CH4$WT_low  <- pmin(CH4$WT, 20)
      CH4$WT_high <- pmax(CH4$WT - 20, 0)
    # Convert Date & Site to Factor
      CH4$Site_F <- as.factor(CH4$Site)
      CH4$Date_F <- as.factor(CH4$Date)
    # Models
      CH4.gm <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 3) * WT_high + RH + PAR + plant_type + 
        (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm)
      r2(CH4.gm)
      CH4.gm.2 <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 3) * WT_high + RH + plant_type + 
          (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.2)
      r2(CH4.gm.2)
      AIC(CH4.gm, CH4.gm.2)
      CH4.gm.3 <- glmmTMB(
        flux_CH4.a ~ (WT_low  + ns(ST10, df = 3) * WT_high + RH) * plant_type + 
          (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.3)
      r2(CH4.gm.3)
      AIC(CH4.gm.2, CH4.gm.3)
      CH4.gm.4 <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 3) * WT_high + RH + (1 | plant_type) + 
          (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.4)
      r2(CH4.gm.4)
      AIC(CH4.gm.2, CH4.gm.4)
      CH4.gm.5 <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 3) * WT_high + RH + 
          (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.5)
      r2(CH4.gm.5)
      AIC(CH4.gm.2, CH4.gm.5) #--> best
      CH4.gm.6 <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 2) * WT_high + RH + 
          (1 | Site_F) + (1 | Date_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.6)
      r2(CH4.gm.6)
      AIC(CH4.gm.5, CH4.gm.6)
      #--> Best Model
      CH4.gm.7 <- glmmTMB(
        flux_CH4.a ~ WT_low  + ns(ST10, df = 2) * WT_high + RH + 
          (1 | Site_F),
        data = CH4,
        family = tweedie(link = "log"))
      summary(CH4.gm.7)
      r2(CH4.gm.7)
      AIC(CH4.gm.6, CH4.gm.7)
  #RECO
    # Choose distribution family
      hist(RECO$CO2.flux_mg)
      plot(density(RECO$CO2.flux_mg))
      qqnorm(RECO$CO2.flux_mg); qqline(RECO$CO2.flux_mg)
    # Convert Site to Factor
      RECO$Site_F <- as.factor(RECO$Site)
      RECO$Date_F <- as.factor(RECO$Date)
    # Models
      RECO.gm <- glmmTMB(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH) * plant_type + (1 | Site_F),
        data = RECO,
        family = tweedie)
      summary(RECO.gm)
      r2(RECO.gm)
      RECO.gm.2 <- glmmTMB(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH) + (1 | Site_F),
        data = RECO,
        family = tweedie(link= "log"))
      summary(RECO.gm.2)
      r2(RECO.gm.2)
      AIC(RECO.gm, RECO.gm.2)
      RECO.gm.3 <- glmmTMB(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH + plant_type + RH:plant_type + ns(ST5,df=3):plant_type) + (1 | Site_F),
        data = RECO,
        family = tweedie(link= "log"))
      summary(RECO.gm.3)
      r2(RECO.gm.3)
      AIC(RECO.gm, RECO.gm.3)
      RECO.gm.4 <- glmmTMB(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH + plant_type + ns(ST5,df=3):plant_type) + (1 | Site_F),
        data = RECO,
        family = tweedie(link= "log"))
      summary(RECO.gm.4)
      r2(RECO.gm.4)
      AIC(RECO.gm, RECO.gm.4) 
    #--> Best model because best middleground between prediction performance and simplicity and fewer nonsignificant terms
      RECO.gm.5 <- glmmTMB(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + RH + plant_type + ns(ST5,df=3):plant_type) + (1 | Site_F) + (1 | Date_F),
        data = RECO,
        family = tweedie(link= "log"))
      summary(RECO.gm.5)
      r2(RECO.gm.5)
      AIC(RECO.gm.4, RECO.gm.5) 
#-------------------------------------------------------------------------------
      
### Data Setup for Tests
#-------------------------------------------------------------------------------
  # Adjust CH4 flux to not be negative:
    CH4 <- CH4 %>%
      mutate(flux_CH4.a = flux_CH4 + 261.5227132)
  # WT Threshold
    CH4$WT_low  <- pmin(CH4$WT, 20)
    CH4$WT_high <- pmax(CH4$WT - 20, 0)
  # Convert Date & Site to Factor
    CH4$Site_F <- as.factor(CH4$Site)
    CH4$Date_F <- as.factor(CH4$Date)
  # Convert Site to Factor
    RECO$Site_F <- as.factor(RECO$Site)
    RECO$Date_F <- as.factor(RECO$Date)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Test Models Predictions vs. Observed with Train/Test split by Date
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 

### New Date Column for RECO
#-------------------------------------------------------------------------------
  RECO$Date_tr <- as.Date(RECO$Date_F)
#-------------------------------------------------------------------------------
    
### Test Multi Linear Models
#-------------------------------------------------------------------------------
  #CH4
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(CH4$Date_F)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- CH4[CH4$Date_F %in% train_dates, ]
      test_data  <- CH4[!CH4$Date_F %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      CH4.lm.8.train <- lm(
        flux_CH4 ~ ns(ST10, df = 3) * WT + RH,
        data = train_data)
      summary(CH4.lm.8.train)
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(CH4.lm.8.train, newdata = test_data)
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      # RMSE
        RMSE_value <- RMSE(pred_test, test_data$flux_CH4)
      # MAE
        MAE_value <- MAE(pred_test, test_data$flux_CH4)
      # R-squared
        R2_value <- R2(pred_test, test_data$flux_CH4)
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = test_data$flux_CH4,
          pred_test = pred_test)
      # plot
        p_CH4_LM <-  ggplot(df, aes(x = observed, y = pred_test)) +
            geom_point(shape = 16) +
            geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
            labs(
              x = expression(bold(Observed ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              y = expression(bold(Predicted ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            ) +
            theme_pub()
  # RECO
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(RECO$Date_tr)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- RECO[RECO$Date_tr %in% train_dates, ]
      test_data  <- RECO[!RECO$Date_tr %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      RECO.lm.6.train <- lm(
        CO2.flux_mg ~ (ns(ST5, df = 3) * WT + ST5:PAR + PAR + RH) * plant_type,
        data = train_data)
      summary(RECO.lm.6.train)
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(RECO.lm.6.train, newdata = test_data)
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      # RMSE
        RMSE_value <- RMSE(pred_test, test_data$CO2.flux_mg)
      # MAE
        MAE_value <- MAE(pred_test, test_data$CO2.flux_mg)
      # R-squared
        R2_value <- R2(pred_test, test_data$CO2.flux_mg)
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = test_data$CO2.flux_mg,
          pred_test = pred_test)
    # plot
      p_RECO_LM  <- ggplot(df, aes(x = observed, y = pred_test)) +
          geom_point(shape = 16) +
          geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
          labs(
            x = expression(bold(Observed ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            y = expression(bold(Predicted ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
          ) +
          theme_pub()
#-------------------------------------------------------------------------------
  
### Test GLMMs
#-------------------------------------------------------------------------------
  # CH4
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(CH4$Date_F)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- CH4[CH4$Date_F %in% train_dates, ]
      test_data  <- CH4[!CH4$Date_F %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      CH4.gm.6.train <- glmmTMB(
        flux_CH4.a ~ WT_low +
          ns(ST10, df = 2) * WT_high +
          RH +
          (1 | Site_F) +
          (1 | Date_F),
        data = train_data,
        family = tweedie(link = "log"))
      summary(CH4.gm.6.train)
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(CH4.gm.6.train, newdata = test_data, type = "response", allow.new.levels = TRUE)
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      observed <- test_data$flux_CH4.a
      # RMSE
       RMSE_value <- RMSE(pred_test, observed)
      # MAE
        MAE_value <- MAE(pred_test, observed)
      # Test R²
        R2_value <- cor(pred_test, observed)^2
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = observed,
          pred_test = pred_test)
      # plot
        p_CH4_GLMM <-  ggplot(df, aes(x = observed, y = pred_test)) +
            geom_point(shape = 16) +
            geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
            labs(
              x = expression(bold(Observed ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              y = expression(bold(Predicted ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            ) +
            theme_pub()
  # RECO
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(RECO$Date_tr)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- RECO[RECO$Date_tr %in% train_dates, ]
      test_data  <- RECO[!RECO$Date_tr %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      RECO.gm.4.train <- glmmTMB(
        CO2.flux_mg ~ 
          (ns(ST5, df = 3) * WT + RH + plant_type + ns(ST5,df=3):plant_type) + (1 | Site_F),
        data = train_data,
        family = tweedie(link = "log"))
      summary(RECO.gm.4.train)
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(RECO.gm.4.train, newdata = test_data, type = "response", allow.new.levels = TRUE)
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      observed <- test_data$CO2.flux_mg
      # RMSE
        RMSE_value <- RMSE(pred_test, observed)
      # MAE
        MAE_value <- MAE(pred_test, observed)
      # Test R²
        R2_value <- cor(pred_test, observed)^2
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = observed,
          pred_test = pred_test)
      # plot
        p_RECO_GLMM <-  ggplot(df, aes(x = observed, y = pred_test)) +
            geom_point(shape = 16) +
            geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
            labs(
              x = expression(bold(Observed ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
              y = expression(bold(Predicted ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            ) +
            theme_pub()
#-------------------------------------------------------------------------------

### Test Random Forest Models
#-------------------------------------------------------------------------------
  # CH4
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(CH4$Date_F)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- CH4[CH4$Date_F %in% train_dates, ]
      test_data  <- CH4[!CH4$Date_F %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      CH4.rf.7.train <- ranger(
        flux_CH4 ~ ST10 + WT + RH + plant_type,
        data = train_data,
        mtry = 3,
        min.node.size = 10,
        splitrule = "extratrees",
        importance = "permutation",
        num.trees = 1000,
        seed = 123)
      CH4.rf.7.train
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(CH4.rf.7.train, data = test_data)$predictions
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      observed <- test_data$flux_CH4
      # RMSE
        RMSE_value <- RMSE(pred_test, observed)
      # MAE
        MAE_value <- MAE(pred_test, observed)
      # Test R²
        R2_value <- cor(pred_test, observed)^2
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = observed,
          pred_test = pred_test)
      # plot
        p_CH4_RF <- ggplot(df, aes(x = observed, y = pred_test)) +
            geom_point(shape = 16) +
            geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
            labs(
              x = expression(bold(Observed ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              y = expression(bold(Predicted ~ CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            ) +
            theme_pub()
  # RECO
    # Set seed for reproducibility
      set.seed(123)
    # -----------------------------
    # 1. Train/Test Split
    # -----------------------------
      unique_dates <- unique(RECO$Date_tr)
      train_dates <- sample(
        unique_dates,
        size = 0.8 * length(unique_dates))
      train_data <- RECO[RECO$Date_tr %in% train_dates, ]
      test_data  <- RECO[!RECO$Date_tr %in% train_dates, ]
    # -----------------------------
    # 2. Fit model on training set
    # -----------------------------
      RECO.rf.7.train <- ranger(
        CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type,
        data = train_data,
        mtry = 2,
        min.node.size = 3,
        splitrule = "extratrees",
        importance = "permutation",
        num.trees = 1000,
        seed = 123)
      RECO.rf.7.train
    # -----------------------------
    # 3. Predict on test set
    # -----------------------------
      pred_test <- predict(RECO.rf.7.train, data = test_data)$predictions
    # -----------------------------
    # 4. Evaluate predictive accuracy
    # -----------------------------
      observed <- test_data$CO2.flux_mg
      # RMSE
        RMSE_value <- RMSE(pred_test, observed)
      # MAE
        MAE_value <- MAE(pred_test, observed)
      # Test R²
        R2_value <- cor(pred_test, observed)^2
      # Print results
        cat("Test RMSE:", RMSE_value, "\n")
        cat("Test MAE :", MAE_value, "\n")
        cat("Test R²  :", R2_value, "\n")
    # -----------------------------
    # Observed vs Predicted Plot
    # -----------------------------
      # Create a data frame
        df <- data.frame(
          observed = observed,
          pred_test = pred_test)
      # plot
        p_RECO_RF <- ggplot(df, aes(x = observed, y = pred_test)) +
            geom_point(shape = 16) +
            geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
            labs(
              x = expression(bold(Observed ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
              y = expression(bold(Predicted ~ CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            ) +
            theme_pub()
#-------------------------------------------------------------------------------

### Export Observed vs. Predicted plots
#-------------------------------------------------------------------------------
  plots_O_P <- list(
    p_CH4_LM = p_CH4_LM,
    p_CH4_RF = p_CH4_RF,
    p_CH4_GLMM = p_CH4_GLMM,
    p_RECO_LM = p_RECO_LM,
    p_RECO_RF = p_RECO_RF,
    p_RECO_GLMM = p_RECO_GLMM)
  dir.create("figures", showWarnings = FALSE)
  for (name in names(plots_O_P)) {
    ggsave(
      filename = paste0("figures/", name, ".png"),
      plot = plots_O_P[[name]],
      width = 6,
      height = 6,
      dpi = 600
    )}
#-------------------------------------------------------------------------------
  
### Plot to look for Reason for Spike in RECO
#-------------------------------------------------------------------------------
  RECO$Date1 <- as.POSIXct(
    RECO$Date,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC")
  p_RECO_T <- ggplot(RECO, aes(x = Date1, y = ST5)) +
    geom_line(color = "grey50", linewidth = 0.5) +
    geom_point(size = 1) +
    scale_x_datetime(
      date_breaks = "1 month",
      date_labels = "%b%Y"   # Jan, Feb, Mar, ...
    ) +
    labs(
      x = "Date",
      y = "Soil Temperature at 5 cm depth [°C]"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 6))
  ggsave(
    filename = "figures/p_RECO_T.png",
    plot = p_RECO_T,
    width = 6,
    height = 4,
    dpi = 600)
#-------------------------------------------------------------------------------