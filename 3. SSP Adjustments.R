# SSP Adjustment

### Set Working Directory
#-------------------------------------------------------------------------------
  setwd("C:/Users/jeret/Desktop/Daten Bachelorarbeit")
#-------------------------------------------------------------------------------

### Load Libraries
#-------------------------------------------------------------------------------
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(glmmTMB)
  library(ranger)
  library(splines)
#-------------------------------------------------------------------------------

### Load CSVs
#-------------------------------------------------------------------------------
  CH4 <- read_csv("CH4.csv")
  RECO <- read_csv ("RECO.csv")
  Temp_change_2040_2060 <- read_delim("Temp_change_2040-2060.csv",delim = ";", locale = locale(decimal_mark = ","))
  Temp_change_2080_2100 <- read_delim("Temp_change_2080-2100.csv",delim = ";", locale = locale(decimal_mark = ","))
  Prec_change_2040_2060 <- read_delim("Prec_change_2040-2060.csv",delim = ";", locale = locale(decimal_mark = ","))
  Prec_change_2080_2100 <- read_delim("Prec_change_2080-2100.csv",delim = ";", locale = locale(decimal_mark = ","))
#-------------------------------------------------------------------------------

# ggplot Publish Theme
#-------------------------------------------------------------------------------
  theme_pub <- function(base_size = 11, base_family = "") {
    theme_classic(base_size = base_size, base_family = base_family) +
      theme(
        plot.title = element_text(face = "bold", size = base_size + 3, margin = ggplot2::margin(b = +10)),
        plot.subtitle = element_text(size = base_size),
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

### Run Best Models:
#-------------------------------------------------------------------------------
  # Data Adjustment for CH4 Model:
    # Adjust CH4 flux to not be negative:
      CH4 <- CH4 %>%
        mutate(flux_CH4.a = flux_CH4 + 261.5227132)
    # WT Threshold
      CH4$WT_low  <- pmin(CH4$WT, 20)
      CH4$WT_high <- pmax(CH4$WT - 20, 0)
    # Convert Date & Site to Factor
      CH4$Site_F <- as.factor(CH4$Site)
      CH4$Date_F <- as.factor(CH4$Date)
  # CH4 Model
    CH4.gm.6 <- glmmTMB(
      flux_CH4.a ~ WT_low  + ns(ST10, df = 2) * WT_high + RH + (1 | Site_F) + (1 | Date_F),
      data = CH4,
      family = tweedie(link = "log")
    )
  # RECO Model
    RECO.rf.7 <- ranger(
      CO2.flux_mg ~ ST5 + WT + RH + PAR + plant_type, 
      data = RECO,
      mtry = 2, min.node.size = 3,
      splitrule = "extratrees", importance = "permutation",
      num.trees = 1000, seed = 123)
#-------------------------------------------------------------------------------
    
### Base predictions:
#-------------------------------------------------------------------------------
  CH4$pre <- predict(CH4.gm.6, type = "response") - 261.5227132
  RECO$pre <- predict(RECO.rf.7, data = RECO)$predictions
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Plot measured base flux data vs base model predictions monthly
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### CH4 base
#-------------------------------------------------------------------------------
  plot_data_CH4_base <- CH4 %>%
    select(
      Date,
      flux_CH4,
      pre
    ) %>%
    pivot_longer(
      cols = c(flux_CH4, pre),
      names_to = "Scenario",
      values_to = "Flux"
    ) %>%
    mutate(
      Date = as.Date(Date),
      Scenario = factor(
        Scenario,
        levels = c("flux_CH4", "pre"),
        labels = c("Measured base flux", "Base model prediction")
      )
    )
  p_CH4_base <- ggplot(
    plot_data_CH4_base,
    aes(x = Date, y = Flux, color = Scenario)
  ) +
    geom_line(alpha = 0.35, linewidth = 0.4) +
    geom_smooth(
      se = FALSE,
      method = "loess",
      span = 0.15,
      linewidth = 1
    ) +
    scale_color_manual(
      values = c(
        "Measured base flux" = "#4D4D4D",
        "Base model prediction" = "#3B6FB6"
      )
    ) +
    theme_pub() +
    labs(
      x = "Date",
      y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
    )+
    theme(legend.title = element_blank())
#-------------------------------------------------------------------------------

### RECO base
#-------------------------------------------------------------------------------
  plot_data_RECO_base <- RECO %>%
    select(
      Date,
      CO2.flux_mg,
      pre
    ) %>%
    pivot_longer(
      cols = c(CO2.flux_mg, pre),
      names_to = "Scenario",
      values_to = "Flux"
    ) %>%
    mutate(
      Date = as.Date(Date),
      Scenario = factor(
        Scenario,
        levels = c("CO2.flux_mg", "pre"),
        labels = c("Measured base flux", "Base model prediction")
      )
    )
  p_RECO_base <- ggplot(
    plot_data_RECO_base,
    aes(x = Date, y = Flux, color = Scenario)
  ) +
    geom_line(alpha = 0.35, linewidth = 0.4) +
    geom_smooth(
      se = FALSE,
      method = "loess",
      span = 0.15,
      linewidth = 1
    ) +
    scale_color_manual(
      values = c(
        "Measured base flux" = "#4D4D4D",
        "Base model prediction" = "#3B6FB6"
      )
    ) +
    theme_pub() +
    labs(
      x = "Date",
      y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]"))
    )+
    theme(legend.title = element_blank())
#-------------------------------------------------------------------------------
  
### CH4 monthly
#-------------------------------------------------------------------------------
  plot_data_CH4_base_month <- CH4 %>%
    select(
      Date,
      flux_CH4,
      pre
    ) %>%
    pivot_longer(
      cols = c(flux_CH4, pre),
      names_to = "Scenario",
      values_to = "Flux"
    ) %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.numeric(format(Date, "%m")),
      Month = factor(month.abb[Month_num], levels = month.abb),
      Scenario = factor(
        Scenario,
        levels = c("flux_CH4", "pre"),
        labels = c("Measured base flux", "Base model prediction")
      )
    ) %>%
    group_by(Month_num, Month, Scenario) %>%
    summarise(
      Flux = mean(Flux, na.rm = TRUE),
      .groups = "drop"
    )
  p_CH4_base_month <- ggplot(
    plot_data_CH4_base_month,
    aes(x = Month_num, y = Flux, color = Scenario, group = Scenario)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    scale_color_manual(
      values = c(
        "Measured base flux" = "#4D4D4D",
        "Base model prediction" = "#3B6FB6"
      )
    ) +
    theme_pub() +
    labs(
      x = "Month",
      y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]"))
    )+
    theme(legend.title = element_blank())
#-------------------------------------------------------------------------------

### RECO monthly
#-------------------------------------------------------------------------------
  plot_data_RECO_base_month <- RECO %>%
    select(
      Date,
      CO2.flux_mg,
      pre
    ) %>%
    pivot_longer(
      cols = c(CO2.flux_mg, pre),
      names_to = "Scenario",
      values_to = "Flux"
    ) %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.numeric(format(Date, "%m")),
      Month = factor(month.abb[Month_num], levels = month.abb),
      Scenario = factor(
        Scenario,
        levels = c("CO2.flux_mg", "pre"),
        labels = c("Measured base flux", "Base model prediction")
      )
    ) %>%
    group_by(Month_num, Month, Scenario) %>%
    summarise(
      Flux = mean(Flux, na.rm = TRUE),
      .groups = "drop"
    )
  p_RECO_base_month <- ggplot(
    plot_data_RECO_base_month,
    aes(x = Month_num, y = Flux, color = Scenario, group = Scenario)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    scale_color_manual(
      values = c(
        "Measured base flux" = "#4D4D4D",
        "Base model prediction" = "#3B6FB6"
      )
    ) +
    theme_pub() +
    labs(
      x = "Month",
      y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]"))
    )+
    theme(legend.title = element_blank())
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Adjust Climate Data to predict possible future CH4 & RECO flux
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 

### Prepare Month matching:
#-------------------------------------------------------------------------------
  # Extract month from CH4 dates
    CH4$Month <- month.abb[as.numeric(format(as.Date(CH4$Date), "%m"))]
  # Extract month from RECO dates
    RECO$Month <- month.abb[as.numeric(format(as.Date(RECO$Date), "%m"))]
#-------------------------------------------------------------------------------

### Adjust Temperature
#-------------------------------------------------------------------------------
  # Adjust Temperature:
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Temp_change <- data.frame(
      Month = Temp_change_2080_2100$Category)
    Temp_change$SSP1_2040_2060 <- Temp_change_2040_2060$`SSP1-1.9`
    Temp_change$SSP2_2040_2060 <- Temp_change_2040_2060$`SSP2-4.5`
    Temp_change$SSP5_2040_2060 <- Temp_change_2040_2060$`SSP5-8.5`
    Temp_change$SSP1_2080_2100 <- Temp_change_2080_2100$`SSP1-1.9`
    Temp_change$SSP2_2080_2100 <- Temp_change_2080_2100$`SSP2-4.5`
    Temp_change$SSP5_2080_2100 <- Temp_change_2080_2100$`SSP5-8.5`
    # Month match
      month_match_CH4 <- match(CH4$Month, Temp_change$Month)
      month_match_RECO <- match(RECO$Month, Temp_change$Month)
    # 2040-2060
      # SSP1
        CH4_ST_40_60_1 <- CH4
        CH4_ST_40_60_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2040_2060[month_match_CH4]
        RECO_ST_40_60_1 <- RECO
        RECO_ST_40_60_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2040_2060[month_match_RECO]
      # SSP2
        CH4_ST_40_60_2 <- CH4
        CH4_ST_40_60_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2040_2060[month_match_CH4]
        RECO_ST_40_60_2 <- RECO
        RECO_ST_40_60_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2040_2060[month_match_RECO]
      # SSP5
        CH4_ST_40_60_5 <- CH4
        CH4_ST_40_60_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2040_2060[month_match_CH4]
        RECO_ST_40_60_5 <- RECO
        RECO_ST_40_60_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2040_2060[month_match_RECO]
    # 2010-2100
      # SSP1
        CH4_ST_80_100_1 <- CH4
        CH4_ST_80_100_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2080_2100[month_match_CH4]
        RECO_ST_80_100_1 <- RECO
        RECO_ST_80_100_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2080_2100[month_match_RECO]
      #SSP2
        CH4_ST_80_100_2 <- CH4
        CH4_ST_80_100_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2080_2100[month_match_CH4]
        RECO_ST_80_100_2 <- RECO
        RECO_ST_80_100_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2080_2100[month_match_RECO]
      #SSP5
        CH4_ST_80_100_5 <- CH4
        CH4_ST_80_100_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2080_2100[month_match_CH4]
        RECO_ST_80_100_5 <- RECO
        RECO_ST_80_100_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2080_2100[month_match_RECO]
  # Predict with adjusted Temperature:
    #CH4
      CH4$preT_40_60_1 <- predict(CH4.gm.6, newdata = CH4_ST_40_60_1, type = 'response')
      CH4$preT_80_100_1 <- predict(CH4.gm.6, newdata = CH4_ST_80_100_1, type = 'response')
      CH4$preT_40_60_2 <- predict(CH4.gm.6, newdata = CH4_ST_40_60_2, type = 'response')
      CH4$preT_80_100_2 <- predict(CH4.gm.6, newdata = CH4_ST_80_100_2, type = 'response')
      CH4$preT_40_60_5 <- predict(CH4.gm.6, newdata = CH4_ST_40_60_5, type = 'response')
      CH4$preT_80_100_5 <- predict(CH4.gm.6, newdata = CH4_ST_80_100_5, type = 'response')
      # take back adjusted CH4 flux
        CH4$preT_40_60_1 <- CH4$preT_40_60_1 - 261.5227132
        CH4$preT_80_100_1 <- CH4$preT_80_100_1 - 261.5227132
        CH4$preT_40_60_2 <- CH4$preT_40_60_2 - 261.5227132
        CH4$preT_80_100_2 <- CH4$preT_80_100_2 - 261.5227132
        CH4$preT_40_60_5 <- CH4$preT_40_60_5 - 261.5227132
        CH4$preT_80_100_5 <-CH4$preT_80_100_5 - 261.5227132
    #RECO
      RECO$preT_40_60_1 <- predict(RECO.rf.7, data = RECO_ST_40_60_1)$predictions
      RECO$preT_80_100_1 <- predict(RECO.rf.7, data = RECO_ST_80_100_1)$predictions
      RECO$preT_40_60_2 <- predict(RECO.rf.7, data = RECO_ST_40_60_2)$predictions
      RECO$preT_80_100_2 <- predict(RECO.rf.7, data = RECO_ST_80_100_2)$predictions
      RECO$preT_40_60_5 <- predict(RECO.rf.7, data = RECO_ST_40_60_5)$predictions
      RECO$preT_80_100_5 <- predict(RECO.rf.7, data = RECO_ST_80_100_5)$predictions
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
  # Prepare Data for plotting
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      # 2040-2060
        # Select Date + prediction columns
          plot_data_CH4_T_40_60 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preT_40_60_1,
                   preT_40_60_2,
                   preT_40_60_5,) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_T_40_60_month <- plot_data_CH4_T_40_60 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # 2080-2100
        # Select Date + prediction columns
          plot_data_CH4_T_80_100 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preT_80_100_1,
                   preT_80_100_2,
                   preT_80_100_5) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_T_80_100_month <- plot_data_CH4_T_80_100 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP1
        plot_data_CH4_T_SSP1 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preT_40_60_1,
            preT_80_100_1
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP2
        plot_data_CH4_T_SSP2 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preT_40_60_2,
            preT_80_100_2
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP5
        plot_data_CH4_T_SSP5 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preT_40_60_5,
            preT_80_100_5
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
    #RECO
      # 2040-2060
        # Select Date + prediction columns
          plot_data_RECO_T_40_60 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preT_40_60_1,
                   preT_40_60_2,
                   preT_40_60_5,) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_T_40_60_month <- plot_data_RECO_T_40_60 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # 2080-2100
        # Select Date + prediction columns
          plot_data_RECO_T_80_100 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preT_80_100_1,
                   preT_80_100_2,
                   preT_80_100_5) %>%
          # Convert from wide to long format
            pivot_longer(
              cols = -Date,
              names_to = "Scenario",
              values_to = "Prediction"
            )
            plot_data_RECO_T_80_100_month <- plot_data_RECO_T_80_100 %>%
              mutate(
                Date = as.Date(Date),
                Month_num = as.numeric(format(Date, "%m")),
                Month = factor(month.abb[Month_num], levels = month.abb)
              ) %>%
              group_by(Month_num, Month, Scenario) %>%
              summarise(
                Prediction = mean(Prediction, na.rm = TRUE),
                .groups = "drop"
              )
      # SSP1
        plot_data_RECO_T_SSP1 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preT_40_60_1,
            preT_80_100_1
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP2
        plot_data_RECO_T_SSP2 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preT_40_60_2,
            preT_80_100_2
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP5
        plot_data_RECO_T_SSP5 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preT_40_60_5,
            preT_80_100_5
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      # 2040-2060
        p_CH4_T_40_60 <- ggplot(plot_data_CH4_T_40_60, aes(x = Date,
                                                             y = Prediction,
                                                             color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"     = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1" = "#B3DE69",
              "preT_40_60_2" = "#FDB462",
              "preT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preT_40_60_1" = "Model predictions for SSP1",
              "preT_40_60_2" = "Model predictions for SSP2",
              "preT_40_60_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2040-2060 month
        p_CH4_T_40_60_month <- ggplot(
          plot_data_CH4_T_40_60_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"     = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1" = "#B3DE69",
              "preT_40_60_2" = "#FDB462",
              "preT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
          labels = c(
            "flux_CH4" = "Measured",
            "preT_40_60_1" = "Model predictions for SSP1",
            "preT_40_60_2" = "Model predictions for SSP2",
            "preT_40_60_5" = "Model predictions for SSP5"
          )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100
        p_CH4_T_80_100 <- ggplot(plot_data_CH4_T_80_100, aes(x = Date,
                                                               y = Prediction,
                                                               color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preT_80_100_1" = "#B3DE69",
              "preT_80_100_2" = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preT_80_100_1" = "Model predictions for SSP1",
              "preT_80_100_2" = "Model predictions for SSP2",
              "preT_80_100_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100 month
        p_CH4_T_80_100_month <- ggplot(
          plot_data_CH4_T_80_100_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"     = "#4D4D4D",
              "pre"          = NA,
              "preT_80_100_1" = "#B3DE69",
              "preT_80_100_2" = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preT_80_100_1" = "Model predictions for SSP1",
              "preT_80_100_2" = "Model predictions for SSP2",
              "preT_80_100_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP1
        p_CH4_T_SSP1 <- ggplot(
          plot_data_CH4_T_SSP1,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1"  = "#FDB462",
              "preT_80_100_1" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preT_40_60_1"  = "2040–2059",
              "preT_80_100_1" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP2
        p_CH4_T_SSP2 <- ggplot(
          plot_data_CH4_T_SSP2,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_2"  = "#FDB462",
              "preT_80_100_2" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preT_40_60_2"  = "2040–2059",
              "preT_80_100_2" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP5
        p_CH4_T_SSP5 <- ggplot(
          plot_data_CH4_T_SSP5,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_5"  = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preT_40_60_5"  = "2040–2059",
              "preT_80_100_5" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
    #RECO
      # 2040-2060
        p_RECO_T_40_60 <- ggplot(plot_data_RECO_T_40_60, aes(x = Date,
                                                               y = Prediction,
                                                               color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"  = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1" = "#B3DE69",
              "preT_40_60_2" = "#FDB462",
              "preT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preT_40_60_1" = "Model predictions for SSP1",
              "preT_40_60_2" = "Model predictions for SSP2",
              "preT_40_60_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      #2040-2060 month
        p_RECO_T_40_60_month <- ggplot(
          plot_data_RECO_T_40_60_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"     = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1" = "#B3DE69",
              "preT_40_60_2" = "#FDB462",
              "preT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preT_40_60_1" = "Model predictions for SSP1",
              "preT_40_60_2" = "Model predictions for SSP2",
              "preT_40_60_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100
        p_RECO_T_80_100 <- ggplot(plot_data_RECO_T_80_100, aes(x = Date,
                                                                 y = Prediction,
                                                                 color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"   = "#4D4D4D",
              "pre"          = NA,
              "preT_80_100_1" = "#B3DE69",
              "preT_80_100_2" = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preT_80_100_1" = "Model predictions for SSP1",
              "preT_80_100_2" = "Model predictions for SSP2",
              "preT_80_100_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100
        p_RECO_T_80_100_month <- ggplot(
          plot_data_RECO_T_80_100_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"     = "#4D4D4D",
              "pre"          = NA,
              "preT_80_100_1" = "#B3DE69",
              "preT_80_100_2" = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preT_80_100_1" = "Model predictions for SSP1",
              "preT_80_100_2" = "Model predictions for SSP2",
              "preT_80_100_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP1
        p_RECO_T_SSP1 <- ggplot(
          plot_data_RECO_T_SSP1,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_1"  = "#FDB462",
              "preT_80_100_1" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preT_40_60_1"  = "2040–2059",
              "preT_80_100_1" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP2
        p_RECO_T_SSP2 <- ggplot(
          plot_data_RECO_T_SSP2,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_2"  = "#FDB462",
              "preT_80_100_2" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preT_40_60_2"  = "2040–2060",
              "preT_80_100_2" = "2080–2100"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP5
        p_RECO_T_SSP5 <- ggplot(
          plot_data_RECO_T_SSP5,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preT_40_60_5"  = "#FDB462",
              "preT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preT_40_60_5"  = "2040–2060",
              "preT_80_100_5" = "2080–2100"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------

### Adjust WT:
#-------------------------------------------------------------------------------
  # Adjust WT
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Prec_change <- data.frame(
      Month = Prec_change_2080_2100$Category)
    Prec_change$hist <- Prec_change_2040_2060$`Historical Ref. Period 1995-2014`
    Prec_change$SSP1_2040_2060 <- Prec_change_2040_2060$`SSP1-1.9`
    Prec_change$SSP2_2040_2060 <- Prec_change_2040_2060$`SSP2-4.5`
    Prec_change$SSP5_2040_2060 <- Prec_change_2040_2060$`SSP5-8.5`
    Prec_change$SSP1_2080_2100 <- Prec_change_2080_2100$`SSP1-1.9`
    Prec_change$SSP2_2080_2100 <- Prec_change_2080_2100$`SSP2-4.5`
    Prec_change$SSP5_2080_2100 <- Prec_change_2080_2100$`SSP5-8.5`
    # Month Match
      month_match_RECO_WT <- match(RECO$Month, Prec_change$Month)
      month_match_CH4_WT <- match(CH4$Month, Prec_change$Month)
    Prec_change <- Prec_change %>%
      mutate(
        SSP1_2040_2060 = ((SSP1_2040_2060 - hist) / hist),
        SSP2_2040_2060 = ((SSP2_2040_2060 - hist) / hist),
        SSP5_2040_2060 = ((SSP5_2040_2060 - hist) / hist),
        SSP1_2080_2100 = ((SSP1_2080_2100 - hist) / hist),
        SSP2_2080_2100 = ((SSP2_2080_2100 - hist) / hist),
        SSP5_2080_2100 = ((SSP5_2080_2100 - hist) / hist))
  # 2040-2060
    # SSP1
      CH4_WT_40_60_1 <- CH4
      CH4_WT_40_60_1$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP1_2040_2060[month_match_CH4_WT])
      CH4_WT_40_60_1$WT_low  <- pmin(CH4_WT_40_60_1$WT, 20)
      CH4_WT_40_60_1$WT_high <- pmax(CH4_WT_40_60_1$WT - 20, 0)
      RECO_WT_40_60_1 <- RECO
      RECO_WT_40_60_1$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP1_2040_2060[month_match_RECO_WT])
    # SSP2
      CH4_WT_40_60_2 <- CH4
      CH4_WT_40_60_2$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP2_2040_2060[month_match_CH4_WT])
      CH4_WT_40_60_2$WT_low  <- pmin(CH4_WT_40_60_2$WT, 20)
      CH4_WT_40_60_2$WT_high <- pmax(CH4_WT_40_60_2$WT - 20, 0)
      RECO_WT_40_60_2 <- RECO
      RECO_WT_40_60_2$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP2_2040_2060[month_match_RECO_WT])
    # SSP5
      CH4_WT_40_60_5 <- CH4
      CH4_WT_40_60_5$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP5_2040_2060[month_match_CH4_WT])
      CH4_WT_40_60_5$WT_low  <- pmin(CH4_WT_40_60_5$WT, 20)
      CH4_WT_40_60_5$WT_high <- pmax(CH4_WT_40_60_5$WT - 20, 0)
      RECO_WT_40_60_5 <- RECO
      RECO_WT_40_60_5$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP5_2040_2060[month_match_RECO_WT])
  # 2080-2100
    # SSP1
      CH4_WT_80_100_1 <- CH4
      CH4_WT_80_100_1$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP1_2080_2100[month_match_CH4_WT])
      CH4_WT_80_100_1$WT_low  <- pmin(CH4_WT_80_100_1$WT, 20)
      CH4_WT_80_100_1$WT_high <- pmax(CH4_WT_80_100_1$WT - 20, 0)
      RECO_WT_80_100_1 <- RECO
      RECO_WT_80_100_1$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP1_2080_2100[month_match_RECO_WT])
    #SSP2
      CH4_WT_80_100_2 <- CH4
      CH4_WT_80_100_2$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP2_2080_2100[month_match_CH4_WT])
      CH4_WT_80_100_2$WT_low  <- pmin(CH4_WT_80_100_2$WT, 20)
      CH4_WT_80_100_2$WT_high <- pmax(CH4_WT_80_100_2$WT - 20, 0)
      RECO_WT_80_100_2 <- RECO
      RECO_WT_80_100_2$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP2_2080_2100[month_match_RECO_WT])
    #SSP5
      CH4_WT_80_100_5 <- CH4
      CH4_WT_80_100_5$WT <- CH4$WT - (abs(CH4$WT) * Prec_change$SSP5_2080_2100[month_match_CH4_WT])
      CH4_WT_80_100_5$WT_low  <- pmin(CH4_WT_80_100_5$WT, 20)
      CH4_WT_80_100_5$WT_high <- pmax(CH4_WT_80_100_5$WT - 20, 0)
      RECO_WT_80_100_5 <- RECO
      RECO_WT_80_100_5$WT <- RECO$WT - (abs(RECO$WT) * Prec_change$SSP5_2080_2100[month_match_RECO_WT])
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  #Predict with adjusted WT
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      CH4$preWT_40_60_1 <- predict(CH4.gm.6, newdata = CH4_WT_40_60_1, type = 'response')
      CH4$preWT_80_100_1 <- predict(CH4.gm.6, newdata = CH4_WT_80_100_1, type = 'response')
      CH4$preWT_40_60_2 <- predict(CH4.gm.6, newdata = CH4_WT_40_60_2, type = 'response')
      CH4$preWT_80_100_2 <- predict(CH4.gm.6, newdata = CH4_WT_80_100_2, type = 'response')
      CH4$preWT_40_60_5 <- predict(CH4.gm.6, newdata = CH4_WT_40_60_5, type = 'response')
      CH4$preWT_80_100_5 <- predict(CH4.gm.6, newdata = CH4_WT_80_100_5, type = 'response')
      # take back adjusted CH4 flux
        CH4$preWT_40_60_1 <- CH4$preWT_40_60_1 - 261.5227132
        CH4$preWT_80_100_1 <- CH4$preWT_80_100_1 - 261.5227132
        CH4$preWT_40_60_2 <- CH4$preWT_40_60_2 - 261.5227132
        CH4$preWT_80_100_2 <- CH4$preWT_80_100_2 - 261.5227132
        CH4$preWT_40_60_5 <- CH4$preWT_40_60_5 - 261.5227132
        CH4$preWT_80_100_5 <-CH4$preWT_80_100_5 - 261.5227132
    #RECO
      RECO$preWT_40_60_1 <- predict(RECO.rf.7, data = RECO_WT_40_60_1)$predictions
      RECO$preWT_80_100_1 <- predict(RECO.rf.7, data = RECO_WT_80_100_1)$predictions
      RECO$preWT_40_60_2 <- predict(RECO.rf.7, data = RECO_WT_40_60_2)$predictions
      RECO$preWT_80_100_2 <- predict(RECO.rf.7, data = RECO_WT_80_100_2)$predictions
      RECO$preWT_40_60_5 <- predict(RECO.rf.7, data = RECO_WT_40_60_5)$predictions
      RECO$preWT_80_100_5 <- predict(RECO.rf.7, data = RECO_WT_80_100_5)$predictions
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  # Prepare Data for plotting
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      # 2040-2060
        # Select Date + prediction columns
          plot_data_CH4_WT_40_60<- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preWT_40_60_1,
                   preWT_40_60_2,
                   preWT_40_60_5,) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_WT_40_60_month <- plot_data_CH4_WT_40_60 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # 2080-2100
        # Select Date + prediction columns
          plot_data_CH4_WT_80_100 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preWT_80_100_1,
                   preWT_80_100_2,
                   preWT_80_100_5) %>%
          
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_WT_80_100_month <- plot_data_CH4_WT_80_100 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP1
        plot_data_CH4_WT_SSP1 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preWT_40_60_1,
            preWT_80_100_1
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP2
        plot_data_CH4_WT_SSP2 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preWT_40_60_2,
            preWT_80_100_2
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP5
        plot_data_CH4_WT_SSP5 <- CH4 %>%
          select(
            Date,
            flux_CH4,
            pre,
            preWT_40_60_5,
            preWT_80_100_5
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
    #RECO
      # 2040-2060
        # Select Date + prediction columns
          plot_data_RECO_WT_40_60 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preWT_40_60_1,
                   preWT_40_60_2,
                   preWT_40_60_5,) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_WT_40_60_month <- plot_data_RECO_WT_40_60 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # 2080-2100
        # Select Date + prediction columns
          plot_data_RECO_WT_80_100 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preWT_80_100_1,
                   preWT_80_100_2,
                   preWT_80_100_5) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_WT_80_100_month <- plot_data_RECO_WT_80_100 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP1
        plot_data_RECO_WT_SSP1 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preWT_40_60_1,
            preWT_80_100_1
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP2
        plot_data_RECO_WT_SSP2 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preWT_40_60_2,
            preWT_80_100_2
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
      # SSP5
        plot_data_RECO_WT_SSP5 <- RECO %>%
          select(
            Date,
            CO2.flux_mg,
            pre,
            preWT_40_60_5,
            preWT_80_100_5
          ) %>%
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          ) %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      # 2040-2060
        p_CH4_WT_40_60 <- ggplot(plot_data_CH4_WT_40_60, aes(x = Date,
                                                                 y = Prediction,
                                                                 color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1" = "#B3DE69",
              "preWT_40_60_2" = "#FDB462",
              "preWT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preWT_40_60_1" = "Model predictions for SSP1",
              "preWT_40_60_2" = "Model predictions for SSP2",
              "preWT_40_60_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2040-2060 month
        p_CH4_WT_40_60_month <- ggplot(
          plot_data_CH4_WT_40_60_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"     = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1" = "#B3DE69",
              "preWT_40_60_2" = "#FDB462",
              "preWT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preWT_40_60_1" = "Model predictions for SSP1",
              "preWT_40_60_2" = "Model predictions for SSP2",
              "preWT_40_60_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
        
      # 2080- 2100
        p_CH4_WT_80_100 <- ggplot(plot_data_CH4_WT_80_100, aes(x = Date,
                                                                   y = Prediction,
                                                                   color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"       = "#4D4D4D",
              "pre"          = NA,
              "preWT_80_100_1" = "#B3DE69",
              "preWT_80_100_2" = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preWT_80_100_1" = "Model predictions for SSP1",
              "preWT_80_100_2" = "Model predictions for SSP2",
              "preWT_80_100_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100 month
        p_CH4_WT_80_100_month <- ggplot(
          plot_data_CH4_WT_80_100_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"     = "#4D4D4D",
              "pre"          = NA,
              "preWT_80_100_1" = "#B3DE69",
              "preWT_80_100_2" = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4" = "Measured",
              "preWT_80_100_1" = "Model predictions for SSP1",
              "preWT_80_100_2" = "Model predictions for SSP2",
              "preWT_80_100_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP1
        p_CH4_WT_SSP1 <- ggplot(
          plot_data_CH4_WT_SSP1,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1"  = "#FDB462",
              "preWT_80_100_1" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preWT_40_60_1"  = "2040–2059",
              "preWT_80_100_1" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP2
        p_CH4_WT_SSP2 <- ggplot(
          plot_data_CH4_WT_SSP2,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_2"  = "#FDB462",
              "preWT_80_100_2" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preWT_40_60_2"  = "2040–2059",
              "preWT_80_100_2" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP5
        p_CH4_WT_SSP5 <- ggplot(
          plot_data_CH4_WT_SSP5,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "flux_CH4"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_5"  = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "flux_CH4"      = "Measured",
              "preWT_40_60_5"  = "2040–2059",
              "preWT_80_100_5" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
    #RECO
      # 2040-2060
        p_RECO_WT_40_60 <- ggplot(plot_data_RECO_WT_40_60, aes(x = Date,
                                                                   y = Prediction,
                                                                   color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"   = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1" = "#B3DE69",
              "preWT_40_60_2" = "#FDB462",
              "preWT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preWT_40_60_1" = "Model predictions for SSP1",
              "preWT_40_60_2" = "Model predictions for SSP2",
              "preWT_40_60_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2040-2060 month
        p_RECO_WT_40_60_month <- ggplot(
          plot_data_RECO_WT_40_60_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"     = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1" = "#B3DE69",
              "preWT_40_60_2" = "#FDB462",
              "preWT_40_60_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preWT_40_60_1" = "Model predictions for SSP1",
              "preWT_40_60_2" = "Model predictions for SSP2",
              "preWT_40_60_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100
        p_RECO_WT_80_100 <- ggplot(plot_data_RECO_WT_80_100, aes(x = Date,
                                                                     y = Prediction,
                                                                     color = Scenario)) +
          geom_line(alpha = 0.3, linewidth = 0.4) +
          geom_smooth(
            se = FALSE,
            method = "loess",
            span = 0.15,
            linewidth = 1
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"    = "#4D4D4D",
              "pre"          = NA,
              "preWT_80_100_1" = "#B3DE69",
              "preWT_80_100_2" = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preWT_80_100_1" = "Model predictions for SSP1",
              "preWT_80_100_2" = "Model predictions for SSP2",
              "preWT_80_100_5" = "Model predictions for SSP5"
            )
          ) +
          theme_pub() +
          labs(
            x = "Date",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # 2080-2100 month
        p_RECO_WT_80_100_month <- ggplot(
          plot_data_RECO_WT_80_100_month,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"     = "#4D4D4D",
              "pre"          = NA,
              "preWT_80_100_1" = "#B3DE69",
              "preWT_80_100_2" = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg" = "Measured",
              "preWT_80_100_1" = "Model predictions for SSP1",
              "preWT_80_100_2" = "Model predictions for SSP2",
              "preWT_80_100_5" = "Model predictions for SSP5"
            )) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP1
        p_RECO_WT_SSP1 <- ggplot(
          plot_data_RECO_WT_SSP1,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_1"  = "#FDB462",
              "preWT_80_100_1" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preWT_40_60_1"  = "2040–2059",
              "preWT_80_100_1" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP2
        p_RECO_WT_SSP2 <- ggplot(
          plot_data_RECO_WT_SSP2,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_2"  = "#FDB462",
              "preWT_80_100_2" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preWT_40_60_2"  = "2040–2059",
              "preWT_80_100_2" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
      # SSP5
        p_RECO_WT_SSP5 <- ggplot(
          plot_data_RECO_WT_SSP5,
          aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
        ) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = c(
              "CO2.flux_mg"      = "#4D4D4D",
              "pre"          = NA,
              "preWT_40_60_5"  = "#FDB462",
              "preWT_80_100_5" = "#E57373"
            ),
            breaks = function(x) x[x != "pre"],
            labels = c(
              "CO2.flux_mg"      = "Measured",
              "preWT_40_60_5"  = "2040–2059",
              "preWT_80_100_5" = "2080–2099"
            )
          ) +
          theme_pub() +
          labs(
            x = "Month",
            y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
            color = "Scenario"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------

### Adjust WT fixed:
#-------------------------------------------------------------------------------
  # Adjust WT by fixed Values
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # 5 cm lower
      CH4_WT_5 <- CH4
      CH4_WT_5$WT <- CH4$WT + 5
      CH4_WT_5$WT_low  <- pmin(CH4_WT_5$WT, 20)
      CH4_WT_5$WT_high <- pmax(CH4_WT_5$WT - 20, 0)
      RECO_WT_5<- RECO
      RECO_WT_5$WT <- RECO$WT + 5
    # 10 cm lower
      CH4_WT_10 <- CH4
      CH4_WT_10$WT <- CH4$WT + 10
      CH4_WT_10$WT_low  <- pmin(CH4_WT_10$WT, 20)
      CH4_WT_10$WT_high <- pmax(CH4_WT_10$WT - 20, 0)
      RECO_WT_10 <- RECO
      RECO_WT_10$WT <- RECO$WT + 10
    # 5 cm higher
      CH4_WT_5h <- CH4
      CH4_WT_5h$WT <- CH4$WT - 5
      CH4_WT_5h$WT_low  <- pmin(CH4_WT_5h$WT, 20)
      CH4_WT_5h$WT_high <- pmax(CH4_WT_5h$WT - 20, 0)
      RECO_WT_5h <- RECO
      RECO_WT_5h$WT <- RECO$WT - 5
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  #Predict with adjusted WT
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
    #CH4
      CH4$preWT_5 <- predict(CH4.gm.6, newdata = CH4_WT_5, type = 'response')
      CH4$preWT_10 <- predict(CH4.gm.6, newdata = CH4_WT_10, type = 'response')
      CH4$preWT_5h <- predict(CH4.gm.6, newdata = CH4_WT_5h, type = 'response')
      # take back adjusted CH4 flux
        CH4$preWT_5 <- CH4$preWT_5 - 261.5227132
        CH4$preWT_10 <- CH4$preWT_10 - 261.5227132
        CH4$preWT_5h <- CH4$preWT_5h - 261.5227132
    #RECO
      RECO$preWT_5 <- predict(RECO.rf.7, data = RECO_WT_5)$predictions
      RECO$preWT_10 <- predict(RECO.rf.7, data = RECO_WT_10)$predictions
      RECO$preWT_5h <- predict(RECO.rf.7, data = RECO_WT_5h)$predictions
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  # Prepare Data for plotting
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      # Select Date + prediction columns
        plot_data_CH4_WT_F <- CH4 %>%
          select(Date,
                 flux_CH4,
                 pre,
                 preWT_5,
                   preWT_10,
                   preWT_5h) %>%
      # Convert from wide to long format
        pivot_longer(
          cols = -Date,
          names_to = "Scenario",
          values_to = "Prediction"
        )
        plot_data_CH4_WT_F_month <- plot_data_CH4_WT_F %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
    #RECO
      # Select Date + prediction columns
      plot_data_RECO_WT_F <- RECO %>%
        select(Date,
               CO2.flux_mg,
                 pre,
               preWT_5,
               preWT_10,
               preWT_5h) %>%
              
      # Convert from wide to long format
        pivot_longer(
          cols = -Date,
          names_to = "Scenario",
          values_to = "Prediction"
        )
        plot_data_RECO_WT_F_month <- plot_data_RECO_WT_F %>%
          mutate(
            Date = as.Date(Date),
            Month_num = as.numeric(format(Date, "%m")),
            Month = factor(month.abb[Month_num], levels = month.abb)
          ) %>%
          group_by(Month_num, Month, Scenario) %>%
          summarise(
            Prediction = mean(Prediction, na.rm = TRUE),
            .groups = "drop"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      p_CH4_WT_F_month <- ggplot(
        plot_data_CH4_WT_F_month,
        aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 1) +
        scale_x_continuous(
            breaks = 1:12,
          labels = month.abb
        ) +
        scale_color_manual(
          values = c(
            "flux_CH4"     = "#4D4D4D",
            "pre"          = NA,
            "preWT_5h" = "#B3DE69",
            "preWT_5" = "#FDB462",
            "preWT_10" = "#E57373"
          ),
          breaks = function(x) x[x != "pre"],
          labels = c(
            "flux_CH4" = "Measured",
            "preWT_5h" = "5 cm higher WT",
            "preWT_5" = "5 cm lower WT",
            "preWT_10" = "10 cm lower WT"
          )) +
        theme_pub() +
        labs(
          x = "Month",
          y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
          color = "Scenario"
        )
    # RECO
      p_RECO_WT_F_month <- ggplot(
        plot_data_RECO_WT_F_month,
        aes(x = Month_num, y = Prediction, color = Scenario, group = Scenario)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 1) +
        scale_x_continuous(
          breaks = 1:12,
          labels = month.abb
        ) +
        scale_color_manual(
          values = c(
            "CO2.flux_mg"     = "#4D4D4D",
            "pre"          = NA,
            "preWT_5h" = "#B3DE69",
            "preWT_5" = "#FDB462",
            "preWT_10" = "#E57373"
          ),
          breaks = function(x) x[x != "pre"],
          labels = c(
            "CO2.flux_mg" = "Measured",
            "preWT_5h" = "5 cm higher WT",
            "preWT_5" = "5 cm lower WT",
            "preWT_10" = "10 cm lower WT"
          )) +
        theme_pub() +
        labs(
          x = "Month",
          y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
          color = "Scenario"
        )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
      
### Adjust Water Table and Temperature:
#-------------------------------------------------------------------------------
  
  # Adjust WT and Temperature
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # 5 cm less
      # 2040-2060
        # SSP1
          CH4_WT_5_T_40_60_1 <- CH4
          CH4_WT_5_T_40_60_1$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_40_60_1$WT_high<- CH4$WT_high + 5
          CH4_WT_5_T_40_60_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2040_2060[month_match_CH4]
          RECO_WT_5_T_40_60_1 <- RECO
          RECO_WT_5_T_40_60_1$WT <- RECO$WT + 5
          RECO_WT_5_T_40_60_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2040_2060[month_match_RECO]
        # SSP2
          CH4_WT_5_T_40_60_2 <- CH4
          CH4_WT_5_T_40_60_2$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_40_60_2$WT_high <- CH4$WT_high + 5
          CH4_WT_5_T_40_60_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2040_2060[month_match_CH4]
          RECO_WT_5_T_40_60_2 <- RECO
          RECO_WT_5_T_40_60_2$WT <- RECO$WT + 5
          RECO_WT_5_T_40_60_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2040_2060[month_match_RECO]
        # SSP5
          CH4_WT_5_T_40_60_5 <- CH4
          CH4_WT_5_T_40_60_5$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_40_60_5$WT_high <- CH4$WT_high + 5
          CH4_WT_5_T_40_60_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2040_2060[month_match_CH4]
          RECO_WT_5_T_40_60_5 <- RECO
          RECO_WT_5_T_40_60_5$WT <- RECO$WT + 5
          RECO_WT_5_T_40_60_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2040_2060[month_match_RECO]
      # 2080-2100
        # SSP1
          CH4_WT_5_T_80_100_1 <- CH4
          CH4_WT_5_T_80_100_1$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_80_100_1$WT_high <- CH4$WT_high + 5
          CH4_WT_5_T_80_100_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2080_2100[month_match_CH4]
          RECO_WT_5_T_80_100_1 <- RECO
          RECO_WT_5_T_80_100_1$WT <- RECO$WT + 5
          RECO_WT_5_T_80_100_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2080_2100[month_match_RECO]
        #SSP2
          CH4_WT_5_T_80_100_2 <- CH4
          CH4_WT_5_T_80_100_2$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_80_100_2$WT_high <- CH4$WT_high + 5
          CH4_WT_5_T_80_100_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2080_2100[month_match_CH4]
          RECO_WT_5_T_80_100_2 <- RECO
          RECO_WT_5_T_80_100_2$WT <- RECO$WT + 5
          RECO_WT_5_T_80_100_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2080_2100[month_match_RECO]
        #SSP5
          CH4_WT_5_T_80_100_5 <- CH4
          CH4_WT_5_T_80_100_5$WT_low <- CH4$WT_low + 5
          CH4_WT_5_T_80_100_5$WT_high <- CH4$WT_high + 5
          CH4_WT_5_T_80_100_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2080_2100[month_match_CH4]
          RECO_WT_5_T_80_100_5 <- RECO
          RECO_WT_5_T_80_100_5$WT <- RECO$WT + 5
          RECO_WT_5_T_80_100_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2080_2100[month_match_RECO]
    # 10 less
      # 2040-2060
        # SSP1
          CH4_WT_10_T_40_60_1 <- CH4
          CH4_WT_10_T_40_60_1$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_40_60_1$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_40_60_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2040_2060[month_match_CH4]
          RECO_WT_10_T_40_60_1 <- RECO
          RECO_WT_10_T_40_60_1$WT <- RECO$WT + 10
          RECO_WT_10_T_40_60_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2040_2060[month_match_RECO]
        # SSP2
          CH4_WT_10_T_40_60_2 <- CH4
          CH4_WT_10_T_40_60_2$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_40_60_2$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_40_60_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2040_2060[month_match_CH4]
          RECO_WT_10_T_40_60_2 <- RECO
          RECO_WT_10_T_40_60_2$WT <- RECO$WT + 10
          RECO_WT_10_T_40_60_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2040_2060[month_match_RECO]
        # SSP5
          CH4_WT_10_T_40_60_5 <- CH4
          CH4_WT_10_T_40_60_5$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_40_60_5$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_40_60_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2040_2060[month_match_CH4]
          RECO_WT_10_T_40_60_5 <- RECO
          RECO_WT_10_T_40_60_5$WT <- RECO$WT + 10
          RECO_WT_10_T_40_60_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2040_2060[month_match_RECO]
      # 2080-2100
        # SSP1
          CH4_WT_10_T_80_100_1 <- CH4
          CH4_WT_10_T_80_100_1$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_80_100_1$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_80_100_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2080_2100[month_match_CH4]
          RECO_WT_10_T_80_100_1 <- RECO
          RECO_WT_10_T_80_100_1$WT <- RECO$WT + 10
          RECO_WT_10_T_80_100_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2080_2100[month_match_RECO]
        # SSP2
          CH4_WT_10_T_80_100_2 <- CH4
          CH4_WT_10_T_80_100_2$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_80_100_2$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_80_100_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2080_2100[month_match_CH4]
          RECO_WT_10_T_80_100_2 <- RECO
          RECO_WT_10_T_80_100_2$WT <- RECO$WT + 10
          RECO_WT_10_T_80_100_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2080_2100[month_match_RECO]
        # SSP5
          CH4_WT_10_T_80_100_5 <- CH4
          CH4_WT_10_T_80_100_5$WT_low <- CH4$WT_low + 10
          CH4_WT_10_T_80_100_5$WT_high <- CH4$WT_high + 10
          CH4_WT_10_T_80_100_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2080_2100[month_match_CH4]
          RECO_WT_10_T_80_100_5 <- RECO
          RECO_WT_10_T_80_100_5$WT <- RECO$WT + 10
          RECO_WT_10_T_80_100_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2080_2100[month_match_RECO]
    # 5 cm more
      # 2040-2060
        # SSP1
          CH4_WT_5h_T_40_60_1 <- CH4
          CH4_WT_5h_T_40_60_1$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_40_60_1$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_40_60_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2040_2060[month_match_CH4]
          RECO_WT_5h_T_40_60_1 <- RECO
          RECO_WT_5h_T_40_60_1$WT <- RECO$WT - 5
          RECO_WT_5h_T_40_60_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2040_2060[month_match_RECO]
        # SSP2
          CH4_WT_5h_T_40_60_2 <- CH4
          CH4_WT_5h_T_40_60_2$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_40_60_2$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_40_60_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2040_2060[month_match_CH4]
          RECO_WT_5h_T_40_60_2 <- RECO
          RECO_WT_5h_T_40_60_2$WT <- RECO$WT - 5
          RECO_WT_5h_T_40_60_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2040_2060[month_match_RECO]
        # SSP5
          CH4_WT_5h_T_40_60_5 <- CH4
          CH4_WT_5h_T_40_60_5$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_40_60_5$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_40_60_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2040_2060[month_match_CH4]
          RECO_WT_5h_T_40_60_5 <- RECO
          RECO_WT_5h_T_40_60_5$WT <- RECO$WT - 5
          RECO_WT_5h_T_40_60_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2040_2060[month_match_RECO]
      # 2080-2100
        # SSP1
          CH4_WT_5h_T_80_100_1 <- CH4
          CH4_WT_5h_T_80_100_1$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_80_100_1$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_80_100_1$ST10 <- CH4$ST10 + Temp_change$SSP1_2080_2100[month_match_CH4]
          RECO_WT_5h_T_80_100_1 <- RECO
          RECO_WT_5h_T_80_100_1$WT <- RECO$WT - 5
          RECO_WT_5h_T_80_100_1$ST5 <- RECO$ST5 + Temp_change$SSP1_2080_2100[month_match_RECO]
        # SSP2
          CH4_WT_5h_T_80_100_2 <- CH4
          CH4_WT_5h_T_80_100_2$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_80_100_2$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_80_100_2$ST10 <- CH4$ST10 + Temp_change$SSP2_2080_2100[month_match_CH4]
          RECO_WT_5h_T_80_100_2 <- RECO
          RECO_WT_5h_T_80_100_2$WT <- RECO$WT - 5
          RECO_WT_5h_T_80_100_2$ST5 <- RECO$ST5 + Temp_change$SSP2_2080_2100[month_match_RECO]
        # SSP5
          CH4_WT_5h_T_80_100_5 <- CH4
          CH4_WT_5h_T_80_100_5$WT_low <- CH4$WT_low - 5
          CH4_WT_5h_T_80_100_5$WT_high <- CH4$WT_high - 5
          CH4_WT_5h_T_80_100_5$ST10 <- CH4$ST10 + Temp_change$SSP5_2080_2100[month_match_CH4]
          RECO_WT_5h_T_80_100_5 <- RECO
          RECO_WT_5h_T_80_100_5$WT <- RECO$WT - 5
          RECO_WT_5h_T_80_100_5$ST5 <- RECO$ST5 + Temp_change$SSP5_2080_2100[month_match_RECO]
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Predict with adjusted WT & T:
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      CH4$preWT_5_T_40_60_1 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_40_60_1, type = 'response')
      CH4$preWT_5_T_80_100_1 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_80_100_1, type = 'response')
      CH4$preWT_5_T_40_60_2 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_40_60_2, type = 'response')
      CH4$preWT_5_T_80_100_2 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_80_100_2, type = 'response')
      CH4$preWT_5_T_40_60_5 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_40_60_5, type = 'response')
      CH4$preWT_5_T_80_100_5 <- predict(CH4.gm.6, newdata = CH4_WT_5_T_80_100_5, type = 'response')
      CH4$preWT_10_T_40_60_1 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_40_60_1, type = 'response')
      CH4$preWT_10_T_80_100_1 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_80_100_1, type = 'response')
      CH4$preWT_10_T_40_60_2 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_40_60_2, type = 'response')
      CH4$preWT_10_T_80_100_2 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_80_100_2, type = 'response')
      CH4$preWT_10_T_40_60_5 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_40_60_5, type = 'response')
      CH4$preWT_10_T_80_100_5 <- predict(CH4.gm.6, newdata = CH4_WT_10_T_80_100_5, type = 'response')
      CH4$preWT_5h_T_40_60_1 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_40_60_1, type = 'response')
      CH4$preWT_5h_T_80_100_1 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_80_100_1, type = 'response')
      CH4$preWT_5h_T_40_60_2 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_40_60_2, type = 'response')
      CH4$preWT_5h_T_80_100_2 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_80_100_2, type = 'response')
      CH4$preWT_5h_T_40_60_5 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_40_60_5, type = 'response')
      CH4$preWT_5h_T_80_100_5 <- predict(CH4.gm.6, newdata = CH4_WT_5h_T_80_100_5, type = 'response')
      # take back adjusted CH4 flux
        CH4$preWT_5_T_40_60_1 <- CH4$preWT_5_T_40_60_1 - 261.5227132
        CH4$preWT_5_T_80_100_1 <- CH4$preWT_5_T_80_100_1 - 261.5227132
        CH4$preWT_5_T_40_60_2 <- CH4$preWT_5_T_40_60_2 - 261.5227132
        CH4$preWT_5_T_80_100_2 <- CH4$preWT_5_T_80_100_2 - 261.5227132
        CH4$preWT_5_T_40_60_5 <- CH4$preWT_5_T_40_60_5 - 261.5227132
        CH4$preWT_5_T_80_100_5 <- CH4$preWT_5_T_80_100_5 - 261.5227132
        CH4$preWT_10_T_40_60_1 <- CH4$preWT_10_T_40_60_1 - 261.5227132
        CH4$preWT_10_T_80_100_1 <- CH4$preWT_10_T_80_100_1 - 261.5227132
        CH4$preWT_10_T_40_60_2 <- CH4$preWT_10_T_40_60_2 - 261.5227132
        CH4$preWT_10_T_80_100_2 <- CH4$preWT_10_T_80_100_2 - 261.5227132
        CH4$preWT_10_T_40_60_5 <- CH4$preWT_10_T_40_60_5 - 261.5227132
        CH4$preWT_10_T_80_100_5 <-CH4$preWT_10_T_80_100_5 - 261.5227132
        CH4$preWT_5h_T_40_60_1 <- CH4$preWT_5h_T_40_60_1 - 261.5227132
        CH4$preWT_5h_T_80_100_1 <- CH4$preWT_5h_T_80_100_1 - 261.5227132
        CH4$preWT_5h_T_40_60_2 <- CH4$preWT_5h_T_40_60_2 - 261.5227132
        CH4$preWT_5h_T_80_100_2 <- CH4$preWT_5h_T_80_100_2 - 261.5227132
        CH4$preWT_5h_T_40_60_5 <- CH4$preWT_5h_T_40_60_5 - 261.5227132
        CH4$preWT_5h_T_80_100_5 <- CH4$preWT_5h_T_80_100_5 - 261.5227132
    #RECO
      RECO$preWT_5_T_40_60_1 <- predict(RECO.rf.7, data = RECO_WT_5_T_40_60_1)$predictions
      RECO$preWT_5_T_80_100_1 <- predict(RECO.rf.7, data = RECO_WT_5_T_80_100_1)$predictions
      RECO$preWT_5_T_40_60_2 <- predict(RECO.rf.7, data = RECO_WT_5_T_40_60_2)$predictions
      RECO$preWT_5_T_80_100_2 <- predict(RECO.rf.7, data = RECO_WT_5_T_80_100_2)$predictions
      RECO$preWT_5_T_40_60_5 <- predict(RECO.rf.7, data = RECO_WT_5_T_40_60_5)$predictions
      RECO$preWT_5_T_80_100_5 <- predict(RECO.rf.7, data = RECO_WT_5_T_80_100_5)$predictions
      RECO$preWT_10_T_40_60_1 <- predict(RECO.rf.7, data = RECO_WT_10_T_40_60_1)$predictions
      RECO$preWT_10_T_80_100_1 <- predict(RECO.rf.7, data = RECO_WT_10_T_80_100_1)$predictions
      RECO$preWT_10_T_40_60_2 <- predict(RECO.rf.7, data = RECO_WT_10_T_40_60_2)$predictions
      RECO$preWT_10_T_80_100_2 <- predict(RECO.rf.7, data = RECO_WT_10_T_80_100_2)$predictions
      RECO$preWT_10_T_40_60_5 <- predict(RECO.rf.7, data = RECO_WT_10_T_40_60_5)$predictions
      RECO$preWT_10_T_80_100_5 <- predict(RECO.rf.7, data = RECO_WT_10_T_80_100_5)$predictions
      RECO$preWT_5h_T_40_60_1 <- predict(RECO.rf.7, data = RECO_WT_5h_T_40_60_1)$predictions
      RECO$preWT_5h_T_80_100_1 <- predict(RECO.rf.7, data = RECO_WT_5h_T_80_100_1)$predictions
      RECO$preWT_5h_T_40_60_2 <- predict(RECO.rf.7, data = RECO_WT_5h_T_40_60_2)$predictions
      RECO$preWT_5h_T_80_100_2 <- predict(RECO.rf.7, data = RECO_WT_5h_T_80_100_2)$predictions
      RECO$preWT_5h_T_40_60_5 <- predict(RECO.rf.7, data = RECO_WT_5h_T_40_60_5)$predictions
      RECO$preWT_5h_T_80_100_5 <- predict(RECO.rf.7, data = RECO_WT_5h_T_80_100_5)$predictions
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Prepare Data for plotting
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #CH4
      # SSP1
        # Select Date + prediction columns
          plot_data_CH4_WT_T_SSP1 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preWT_5_T_40_60_1,
                   preWT_5_T_80_100_1,
                   preWT_10_T_40_60_1,
                   preWT_10_T_80_100_1,
                   preWT_5h_T_40_60_1,
                   preWT_5h_T_80_100_1
                   ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_WT_T_SSP1_month <- plot_data_CH4_WT_T_SSP1 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP2
        # Select Date + prediction columns
          plot_data_CH4_WT_T_SSP2 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preWT_5_T_40_60_2,
                   preWT_5_T_80_100_2,
                   preWT_10_T_40_60_2,
                   preWT_10_T_80_100_2,
                   preWT_5h_T_40_60_2,
                   preWT_5h_T_80_100_2
            ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_WT_T_SSP2_month <- plot_data_CH4_WT_T_SSP2 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP5
        # Select Date + prediction columns
          plot_data_CH4_WT_T_SSP5 <- CH4 %>%
            select(Date,
                   flux_CH4,
                   pre,
                   preWT_5_T_40_60_5,
                   preWT_5_T_80_100_5,
                   preWT_10_T_40_60_5,
                   preWT_10_T_80_100_5,
                   preWT_5h_T_40_60_5,
                   preWT_5h_T_80_100_5
            ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_CH4_WT_T_SSP5_month <- plot_data_CH4_WT_T_SSP5 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
    #RECO
      # SSP1
        # Select Date + prediction columns
          plot_data_RECO_WT_T_SSP1 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preWT_5_T_40_60_1,
                   preWT_5_T_80_100_1,
                   preWT_10_T_40_60_1,
                   preWT_10_T_80_100_1,
                   preWT_5h_T_40_60_1,
                   preWT_5h_T_80_100_1
            ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_WT_T_SSP1_month <- plot_data_RECO_WT_T_SSP1 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP2
        # Select Date + prediction columns
          plot_data_RECO_WT_T_SSP2 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preWT_5_T_40_60_2,
                   preWT_5_T_80_100_2,
                   preWT_10_T_40_60_2,
                   preWT_10_T_80_100_2,
                   preWT_5h_T_40_60_2,
                   preWT_5h_T_80_100_2
            ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_WT_T_SSP2_month <- plot_data_RECO_WT_T_SSP2 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
      # SSP5
        # Select Date + prediction columns
          plot_data_RECO_WT_T_SSP5 <- RECO %>%
            select(Date,
                   CO2.flux_mg,
                   pre,
                   preWT_5_T_40_60_5,
                   preWT_5_T_80_100_5,
                   preWT_10_T_40_60_5,
                   preWT_10_T_80_100_5,
                   preWT_5h_T_40_60_5,
                   preWT_5h_T_80_100_5
            ) %>%
        # Convert from wide to long format
          pivot_longer(
            cols = -Date,
            names_to = "Scenario",
            values_to = "Prediction"
          )
          plot_data_RECO_WT_T_SSP5_month <- plot_data_RECO_WT_T_SSP5 %>%
            mutate(
              Date = as.Date(Date),
              Month_num = as.numeric(format(Date, "%m")),
              Month = factor(month.abb[Month_num], levels = month.abb)
            ) %>%
            group_by(Month_num, Month, Scenario) %>%
            summarise(
              Prediction = mean(Prediction, na.rm = TRUE),
              .groups = "drop"
            )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

  # Plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      # SSP1
          p_CH4_WT_T_SSP1 <- ggplot(plot_data_CH4_WT_T_SSP1_month, aes(x = Month_num,
                                                                     y = Prediction,
                                                                     color = Scenario)) +
            geom_line(alpha = 0.7, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "flux_CH4"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_1" = "#FDD9A0",
                "preWT_5_T_80_100_1" = "#FDB462",
                "preWT_10_T_40_60_1" = "#F4B6B6",
                "preWT_10_T_80_100_1" = "#E57373",
                "preWT_5h_T_40_60_1" = "#D9F0A3",
                "preWT_5h_T_80_100_1" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "flux_CH4" = "Measured",
                "preWT_5_T_40_60_1" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_1" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_1" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_1" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_1" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_1" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
      # SSP2
          p_CH4_WT_T_SSP2 <- ggplot(plot_data_CH4_WT_T_SSP2_month, aes(x = Month_num,
                                                                             y = Prediction,
                                                                             color = Scenario)) +
            geom_line(alpha = 0.7, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "flux_CH4"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_2" = "#FDD9A0",
                "preWT_5_T_80_100_2" = "#FDB462",
                "preWT_10_T_40_60_2" = "#F4B6B6",
                "preWT_10_T_80_100_2" = "#E57373",
                "preWT_5h_T_40_60_2" = "#D9F0A3",
                "preWT_5h_T_80_100_2" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "flux_CH4" = "Measured",
                "preWT_5_T_40_60_2" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_2" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_2" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_2" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_2" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_2" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
      # SSP5
          p_CH4_WT_T_SSP5 <- ggplot(plot_data_CH4_WT_T_SSP5_month, aes(x = Month_num,
                                                                             y = Prediction,
                                                                             color = Scenario)) +
            geom_line(alpha = 0.7, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "flux_CH4"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_5" = "#FDD9A0",
                "preWT_5_T_80_100_5" = "#FDB462",
                "preWT_10_T_40_60_5" = "#F4B6B6",
                "preWT_10_T_80_100_5" = "#E57373",
                "preWT_5h_T_40_60_5" = "#D9F0A3",
                "preWT_5h_T_80_100_5" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "flux_CH4" = "Measured",
                "preWT_5_T_40_60_5" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_5" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_5" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_5" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_5" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_5" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
    #RECO
      # SSP1
          p_RECO_WT_T_SSP1 <- ggplot(plot_data_RECO_WT_T_SSP1_month, aes(x = Month_num,
                                                                     y = Prediction,
                                                                     color = Scenario)) +
            geom_line(alpha = 0.6, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "CO2.flux_mg"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_1" = "#FDD9A0",
                "preWT_5_T_80_100_1" = "#FDB462",
                "preWT_10_T_40_60_1" = "#F4B6B6",
                "preWT_10_T_80_100_1" = "#E57373",
                "preWT_5h_T_40_60_1" = "#D9F0A3",
                "preWT_5h_T_80_100_1" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "CO2.flux_mg" = "Measured",
                "preWT_5_T_40_60_1" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_1" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_1" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_1" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_1" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_1" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
      # SSP2
          p_RECO_WT_T_SSP2 <- ggplot(plot_data_RECO_WT_T_SSP2_month, aes(x = Month_num,
                                                                             y = Prediction,
                                                                             color = Scenario)) +
            geom_line(alpha = 0.7, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "CO2.flux_mg"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_2" = "#FDD9A0",
                "preWT_5_T_80_100_2" = "#FDB462",
                "preWT_10_T_40_60_2" = "#F4B6B6",
                "preWT_10_T_80_100_2" = "#E57373",
                "preWT_5h_T_40_60_2" = "#D9F0A3",
                "preWT_5h_T_80_100_2" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "CO2.flux_mg" = "Measured",
                "preWT_5_T_40_60_2" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_2" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_2" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_2" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_2" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_2" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
      # SSP5
          p_RECO_WT_T_SSP5 <- ggplot(plot_data_RECO_WT_T_SSP5_month, aes(x = Month_num,
                                                                             y = Prediction,
                                                                             color = Scenario)) +
            geom_line(alpha = 0.7, linewidth = 0.5) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = c(
                "CO2.flux_mg"      = "#4D4D4D",
                "pre"          = NA,
                "preWT_5_T_40_60_5" = "#FDD9A0",
                "preWT_5_T_80_100_5" = "#FDB462",
                "preWT_10_T_40_60_5" = "#F4B6B6",
                "preWT_10_T_80_100_5" = "#E57373",
                "preWT_5h_T_40_60_5" = "#D9F0A3",
                "preWT_5h_T_80_100_5" = "#B3DE69"
              ),
              breaks = function(x) x[x != "pre"],
              labels = c(
                "CO2.flux_mg" = "Measured",
                "preWT_5_T_40_60_5" = "2040-2059 with 5 cm WT-lowering",
                "preWT_5_T_80_100_5" = "2080-2099 with 5 cm WT-lowering",
                "preWT_10_T_40_60_5" = "2040-2059 with 10 cm WT-lowering",
                "preWT_10_T_80_100_5" = "2080-2099 with 10 cm WT-lowering",
                "preWT_5h_T_40_60_5" = "2040-2059 with 5 cm WT-rise",
                "preWT_5h_T_80_100_5" = "2080-2099 with 5 cm WT-rise"
              )
            ) +
            theme_pub() +
            labs(
              x = "Month",
              y = expression(bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")),
              color = "Scenario"
            )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------


### SSP5: no WT change vs 5 cm less WT, separated by plant_type
#-------------------------------------------------------------------------------

  # Adjust WT with already adjusted T
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      CH4_SSP5_noWT_40_60 <- CH4_ST_40_60_5
      CH4_SSP5_noWT_80_100 <- CH4_ST_80_100_5
      CH4_SSP5_5cmWT_40_60 <- CH4_ST_40_60_5
      CH4_SSP5_5cmWT_40_60$WT_low <- CH4$WT_low + 5
      CH4_SSP5_5cmWT_40_60$WT_high <- CH4$WT_high + 5
      CH4_SSP5_5cmWT_80_100 <- CH4_ST_80_100_5
      CH4_SSP5_5cmWT_80_100$WT_low <- CH4$WT_low + 5
      CH4_SSP5_5cmWT_80_100$WT_high <- CH4$WT_high + 5
    # RECO
      RECO_SSP5_noWT_40_60 <- RECO_ST_40_60_5
      RECO_SSP5_noWT_80_100 <- RECO_ST_80_100_5
      RECO_SSP5_5cmWT_40_60 <- RECO_ST_40_60_5
      RECO_SSP5_5cmWT_40_60$WT <- RECO$WT + 5
      RECO_SSP5_5cmWT_80_100 <- RECO_ST_80_100_5
      RECO_SSP5_5cmWT_80_100$WT <- RECO$WT + 5
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Predict with adjusted WT & T
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      CH4$pre_SSP5_noWT_40_60 <- predict(CH4.gm.6, newdata = CH4_SSP5_noWT_40_60, type = "response") - 261.5227132
      CH4$pre_SSP5_noWT_80_100 <- predict(CH4.gm.6, newdata = CH4_SSP5_noWT_80_100, type = "response") - 261.5227132
      CH4$pre_SSP5_5cmWT_40_60 <- predict(CH4.gm.6, newdata = CH4_SSP5_5cmWT_40_60, type = "response") - 261.5227132
      CH4$pre_SSP5_5cmWT_80_100 <- predict(CH4.gm.6, newdata = CH4_SSP5_5cmWT_80_100, type = "response") - 261.5227132
    
    # RECO
      RECO$pre_SSP5_noWT_40_60 <- predict(RECO.rf.7, data = RECO_SSP5_noWT_40_60)$predictions
      RECO$pre_SSP5_noWT_80_100 <- predict(RECO.rf.7, data = RECO_SSP5_noWT_80_100)$predictions
      RECO$pre_SSP5_5cmWT_40_60 <- predict(RECO.rf.7, data = RECO_SSP5_5cmWT_40_60)$predictions
      RECO$pre_SSP5_5cmWT_80_100 <- predict(RECO.rf.7, data = RECO_SSP5_5cmWT_80_100)$predictions
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
  # Prepare mean MONTHLY measured flux for each plant type
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    plant_levels <- c(
      "Carex a.",
      "Phalaris a.",
      "Phragmites a.",
      "Typha l.",
      "Typha a."
    )
    # CH4
      mean_monthly_measured_CH4_plant <- CH4 %>%
        mutate(
          Date = as.Date(Date),
          Month_num = as.numeric(format(Date, "%m")),
          Month = factor(
            month.abb[Month_num],
            levels = month.abb
          ),
          plant_type = factor(
            plant_type,
            levels = c(1, 2, 3, 4, 5),
            labels = plant_levels
          )
        ) %>%
        group_by(
          plant_type,
          Month_num,
          Month
        ) %>%
        summarise(
          Mean_measured_flux = mean(flux_CH4, na.rm = TRUE),
          .groups = "drop"
        )
    # RECO
      mean_monthly_measured_RECO_plant <- RECO %>%
        mutate(
          Date = as.Date(Date),
          Month_num = as.numeric(format(Date, "%m")),
          Month = factor(
            month.abb[Month_num],
            levels = month.abb
          ),
          plant_type = factor(
            plant_type,
            levels = c(1, 2, 3, 4, 5),
            labels = plant_levels
          )
        ) %>%
        group_by(
          plant_type,
          Month_num,
          Month
        ) %>%
        summarise(
          Mean_measured_flux = mean(CO2.flux_mg, na.rm = TRUE),
          .groups = "drop"
        )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Prepare Data for Plotting
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CH4
      plot_data_CH4_SSP5_plant <- CH4 %>%
        select(
          Date,
          plant_type,
          pre_SSP5_noWT_40_60,
          pre_SSP5_5cmWT_40_60,
          pre_SSP5_noWT_80_100,
          pre_SSP5_5cmWT_80_100
        ) %>%
        pivot_longer(
          cols = -c(Date, plant_type),
          names_to = "Scenario",
          values_to = "Prediction"
        ) %>%
        mutate(
          Date = as.Date(Date),
          Month_num = as.numeric(format(Date, "%m")),
          Month = factor(month.abb[Month_num], levels = month.abb),
          Time_period = case_when(
            grepl("40_60", Scenario) ~ "2040–2060",
            grepl("80_100", Scenario) ~ "2080–2100"
          ),
          WT_change = case_when(
            grepl("noWT", Scenario) ~ "No WT change",
            grepl("5cmWT", Scenario) ~ "-5 cm WT"
          ),
          plant_type = factor(
            plant_type,
            levels = c(1, 2, 3, 4, 5),
            labels = plant_levels
          ),
          Line_group = paste(plant_type, WT_change, sep = " - ")
        ) %>%
        group_by(Time_period, plant_type, WT_change, Line_group, Month_num, Month) %>%
        summarise(
          Prediction = mean(Prediction, na.rm = TRUE),
          .groups = "drop"
        )
    # RECO
      plot_data_RECO_SSP5_plant <- RECO %>%
        select(
          Date,
          plant_type,
          pre_SSP5_noWT_40_60,
          pre_SSP5_5cmWT_40_60,
          pre_SSP5_noWT_80_100,
          pre_SSP5_5cmWT_80_100
        ) %>%
        pivot_longer(
          cols = -c(Date, plant_type),
          names_to = "Scenario",
          values_to = "Prediction"
        ) %>%
        mutate(
          Date = as.Date(Date),
          Month_num = as.numeric(format(Date, "%m")),
          Month = factor(month.abb[Month_num], levels = month.abb),
          Time_period = case_when(
            grepl("40_60", Scenario) ~ "2040–2060",
            grepl("80_100", Scenario) ~ "2080–2100"
          ),
          WT_change = case_when(
            grepl("noWT", Scenario) ~ "No WT change",
            grepl("5cmWT", Scenario) ~ "-5 cm WT"
          ),
          plant_type = factor(
            plant_type,
            levels = c(1, 2, 3, 4, 5),
            labels = plant_levels
          ),
           Line_group = paste(plant_type, WT_change, sep = " - ")
        ) %>%
        group_by(Time_period, plant_type, WT_change, Line_group, Month_num, Month) %>%
        summarise(
          Prediction = mean(Prediction, na.rm = TRUE),
          .groups = "drop"
        )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # colors
      plant_colors <- c(
        "Carex a."      = "#BFE6B8",
        "Phalaris a."   = "#7BC87C",
        "Phragmites a." = "#35A853",
        "Typha l."      = "#087830",
        "Typha a."   = "#003B1F"
      )
    # Labels
      plant_labels <- c(
        "Carex a."      = "<i>Carex a.</i>",
        "Phalaris a."   = "<i>Phalaris a.</i>",
        "Phragmites a." = "<i>Phragmites a.</i>",
        "Typha l."      = "<i>Typha l.</i>",
        "Typha a."      = "<i>Typha a.</i>"
      )
    # Plot with and without WT change
      # CH4
         # 2040–2060
           p_CH4_SSP5_2040_2060 <- ggplot(
             filter(plot_data_CH4_SSP5_plant, Time_period == "2040–2060"),
             aes(
               x = Month_num,
               y = Prediction,
               color = plant_type,
               linetype = WT_change,
               group = Line_group
             )
           ) +
             geom_line(linewidth = 1) +
             geom_point(size = 1) +
             scale_x_continuous(
               breaks = 1:12,
               labels = month.abb
             ) +
             scale_color_manual(
               values = plant_colors,
               labels = plant_labels
             ) +
             theme_pub() +
             labs(
               x = "Month",
               y = expression(
                 bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")
               ),
               color = "Plant species",
               linetype = "WT change"
             )
         # 2080–2100
           p_CH4_SSP5_2080_2100 <- ggplot(
             filter(plot_data_CH4_SSP5_plant, Time_period == "2080–2100"),
             aes(
               x = Month_num,
               y = Prediction,
               color = plant_type,
               linetype = WT_change,
               group = Line_group
             )
           ) +
             geom_line(linewidth = 1) +
             geom_point(size = 1) +
             scale_x_continuous(
               breaks = 1:12,
               labels = month.abb
             ) +
             scale_color_manual(
               values = plant_colors,
               labels = plant_labels
             ) +
             theme_pub() +
             theme(legend.text = ggtext::element_markdown()) +
             labs(
               x = "Month",
               y = expression(
                 bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")
               ),
               color = "Plant species",
               linetype = "WT change"
             )
      # RECO
        # 2040–2060
          p_RECO_SSP5_2040_2060 <- ggplot(
            filter(plot_data_RECO_SSP5_plant, Time_period == "2040–2060"),
            aes(
              x = Month_num,
              y = Prediction,
              color = plant_type,
              linetype = WT_change,
              group = Line_group
            )
          ) +
            geom_line(linewidth = 1) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = plant_colors,
              labels = plant_labels
            ) +
            theme_pub() +
            theme(legend.text = ggtext::element_markdown()) +
            labs(
              x = "Month",
              y = expression(
                bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")
              ),
              color = "Plant species",
              linetype = "WT change"
            )
        # 2080–2100
          p_RECO_SSP5_2080_2100 <- ggplot(
            filter(plot_data_RECO_SSP5_plant, Time_period == "2080–2100"),
            aes(
              x = Month_num,
              y = Prediction,
              color = plant_type,
              linetype = WT_change,
              group = Line_group
            )
          ) +
            geom_line(linewidth = 1) +
            geom_point(size = 1) +
            scale_x_continuous(
              breaks = 1:12,
              labels = month.abb
            ) +
            scale_color_manual(
              values = plant_colors,
              labels = plant_labels
            ) +
            theme_pub() +
            theme(legend.text = ggtext::element_markdown()) +
            labs(
              x = "Month",
              y = expression(
                bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")
              ),
              color = "Plant species",
              linetype = "WT change"
            )
    # Monthly measured means vs. predictions
    # CH4
       # 2040–2060
         p_CH4_SSP5_2040_2060_m <- ggplot(
           filter(plot_data_CH4_SSP5_plant, Time_period == "2040–2060", WT_change == "No WT change"),
           aes(
             x = Month_num,
             y = Prediction,
             color = plant_type,
             group = plant_type
           )
         ) +
           geom_line(aes(linetype = "Prediction"), linewidth = 1) +
           geom_point(size = 1) +
           geom_line(
             data = mean_monthly_measured_CH4_plant,
             aes(
               x = Month_num,
               y = Mean_measured_flux,
               color = plant_type,
               group = plant_type,
               linetype = "Measured mean"
             ),
             linewidth = 0.9,
             inherit.aes = FALSE
           ) +
           
           scale_x_continuous(
             breaks = 1:12,
             labels = month.abb
           ) +
           scale_color_manual(
             values = plant_colors,
             labels = plant_labels
           ) +
           scale_linetype_manual(
             name = NULL,
             breaks = c("Prediction", "Measured mean"),
             values = c(
               "Prediction" = "solid",
               "Measured mean" = "dashed"
             )
           ) +
           theme_pub() +
           theme(legend.text = ggtext::element_markdown()) +
           theme(
             legend.key.width = unit(1, "cm")
           ) +
           labs(
             x = "Month",
             y = expression(
               bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")
             ),
             color = "Plant species"
           )
       # 2080–2100
         p_CH4_SSP5_2080_2100_m <- ggplot(
           filter(plot_data_CH4_SSP5_plant, Time_period == "2080–2100", WT_change == "No WT change"),
           aes(
             x = Month_num,
             y = Prediction,
             color = plant_type,
             group = plant_type
           )
         ) +
           geom_line(aes(linetype = "Prediction"), linewidth = 1) +
           geom_point(size = 1) +
           geom_line(
             data = mean_monthly_measured_CH4_plant,
             aes(
               x = Month_num,
               y = Mean_measured_flux,
               color = plant_type,
               group = plant_type,
               linetype = "Measured mean"
             ),
             linewidth = 0.9,
             inherit.aes = FALSE
           ) +
           scale_x_continuous(
             breaks = 1:12,
             labels = month.abb
           ) +
           scale_color_manual(
             values = plant_colors,
             labels = plant_labels
           ) +
           scale_linetype_manual(
             name = NULL,
             breaks = c("Prediction", "Measured mean"),
             values = c(
               "Prediction" = "solid",
               "Measured mean" = "dashed"
             )
           ) +
           theme_pub() +
           theme(legend.text = ggtext::element_markdown()) +
           theme(
             legend.key.width = unit(1, "cm")
           ) +
           labs(
             x = "Month",
             y = expression(
               bold(CH[4] ~ Flux ~ "[" * µg ~ m^{-2} ~ h^{-1} * "]")
             ),
             color = "Plant species"
           )
    # RECO
      # 2040–2060
        p_RECO_SSP5_2040_2060_m <- ggplot(
          filter(plot_data_RECO_SSP5_plant, Time_period == "2040–2060", WT_change == "No WT change"),
          aes(
            x = Month_num,
            y = Prediction,
            color = plant_type,
            group = plant_type
          )
        ) +
          geom_line(aes(linetype = "Prediction"), linewidth = 1) +
          geom_point(size = 1) +
          geom_line(
            data = mean_monthly_measured_RECO_plant,
            aes(
              x = Month_num,
              y = Mean_measured_flux,
              color = plant_type,
              group = plant_type,
              linetype = "Measured mean"
            ),
            linewidth = 0.9,
            inherit.aes = FALSE
          ) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = plant_colors,
            labels = plant_labels
          ) +
          scale_linetype_manual(
            name = NULL,
            breaks = c("Prediction", "Measured mean"),
            values = c(
              "Prediction" = "solid",
              "Measured mean" = "dashed"
            )
          ) +
          theme_pub() +
          theme(legend.text = ggtext::element_markdown()) +
          theme(
            legend.key.width = unit(1, "cm")
          ) +
          labs(
            x = "Month",
            y = expression(
              bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")
            ),
            color = "Plant species"
          )
      # 2080–2100
        p_RECO_SSP5_2080_2100_m <- ggplot(
          filter(plot_data_RECO_SSP5_plant, Time_period == "2080–2100", WT_change == "No WT change"),
          aes(
            x = Month_num,
            y = Prediction,
            color = plant_type,
            group = plant_type
          )
        ) +
          geom_line(aes(linetype = "Prediction"), linewidth = 1) +
          geom_point(size = 1) +
          geom_line(
            data = mean_monthly_measured_RECO_plant,
            aes(
              x = Month_num,
              y = Mean_measured_flux,
              color = plant_type,
              group = plant_type,
              linetype = "Measured mean"
            ),
            linewidth = 0.9,
            inherit.aes = FALSE
          ) +
          scale_x_continuous(
            breaks = 1:12,
            labels = month.abb
          ) +
          scale_color_manual(
            values = plant_colors,
            labels = plant_labels
          ) +
          scale_linetype_manual(
            name = NULL,
            breaks = c("Prediction", "Measured mean"),
            values = c(
              "Prediction" = "solid",
              "Measured mean" = "dashed"
            )
          ) +
          theme_pub() +
          theme(legend.text = ggtext::element_markdown()) +
          theme(
            legend.key.width = unit(1, "cm")
          ) +
          labs(
            x = "Month",
            y = expression(
              bold(CO[2] ~ Flux ~ "[" * mg ~ m^{-2} ~ h^{-1} * "]")
            ),
            color = "Plant species"
          )
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Export plots
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  plots_pre <- list(
    p_CH4_base = p_CH4_base,
    P_RECO_base = p_RECO_base,
    p_CH4_base_month = p_CH4_base_month,
    p_RECO_base_month = p_RECO_base_month,
    p_CH4_T_40_60 = p_CH4_T_40_60,
    p_CH4_T_80_100 = p_CH4_T_80_100,
    p_RECO_T_40_60 = p_RECO_T_40_60,
    p_RECO_T_80_100 = p_RECO_T_80_100,
    p_CH4_T_40_60_month = p_CH4_T_40_60_month,
    p_CH4_T_80_100_month = p_CH4_T_80_100_month,
    p_RECO_T_40_60_month = p_RECO_T_40_60_month,
    p_RECO_T_80_100_month = p_RECO_T_80_100_month,
    p_CH4_T_SSP1 = p_CH4_T_SSP1,
    p_CH4_T_SSP2 = p_CH4_T_SSP2,
    p_CH4_T_SSP5 = p_CH4_T_SSP5,
    p_RECO_T_SSP1 = p_RECO_T_SSP1,
    p_RECO_T_SSP2 = p_RECO_T_SSP2,
    p_RECO_T_SSP5 = p_RECO_T_SSP5,
          
    p_CH4_WT_40_60 = p_CH4_WT_40_60,
    p_CH4_WT_80_100 = p_CH4_WT_80_100,
    p_RECO_WT_40_60 = p_RECO_WT_40_60,
    p_RECO_WT_80_100 = p_RECO_WT_80_100,
    p_CH4_WT_40_60_month = p_CH4_WT_40_60_month,
    p_CH4_WT_80_100_month = p_CH4_WT_80_100_month,
    p_RECO_WT_40_60_month = p_RECO_WT_40_60_month,
    p_RECO_WT_80_100_month = p_RECO_WT_80_100_month,
    p_CH4_WT_SSP1 = p_CH4_WT_SSP1,
    p_CH4_WT_SSP2 = p_CH4_WT_SSP2,
    p_CH4_WT_SSP5 = p_CH4_WT_SSP5,
    p_RECO_WT_SSP1 = p_RECO_WT_SSP1,
    p_RECO_WT_SSP2 = p_RECO_WT_SSP2,
    p_RECO_WT_SSP5 = p_RECO_WT_SSP5,
          
    p_CH4_WT_T_SSP1 = p_CH4_WT_T_SSP1,
    p_CH4_WT_T_SSP2 = p_CH4_WT_T_SSP2,
    p_CH4_WT_T_SSP5 = p_CH4_WT_T_SSP5,
    p_RECO_WT_T_SSP1 = p_RECO_WT_T_SSP1,
    p_RECO_WT_T_SSP2 = p_RECO_WT_T_SSP2,
    p_RECO_WT_T_SSP5 = p_RECO_WT_T_SSP5,
    
    p_CH4_WT_F_month = p_CH4_WT_F_month,
    p_RECO_WT_F_month = p_RECO_WT_F_month,
    
    p_CH4_SSP5_2040_2060 =  p_CH4_SSP5_2040_2060,
    p_CH4_SSP5_2080_2100 = p_CH4_SSP5_2080_2100,
    p_RECO_SSP5_2040_2060 = p_RECO_SSP5_2040_2060,
    p_RECO_SSP5_2080_2100 = p_RECO_SSP5_2080_2100,
    
    p_CH4_SSP5_2040_2060_m =  p_CH4_SSP5_2040_2060_m,
    p_CH4_SSP5_2080_2100_m = p_CH4_SSP5_2080_2100_m,
    p_RECO_SSP5_2040_2060_m = p_RECO_SSP5_2040_2060_m,
    p_RECO_SSP5_2080_2100_m = p_RECO_SSP5_2080_2100_m
    )
  dir.create("figures9", showWarnings = FALSE)
  for (name in names(plots_pre)) {
    ggsave(
    filename = paste0("figures9/", name, ".png"),
    plot = plots_pre[[name]],
    width = 8,
    height = 6,
    dpi = 600
    )}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Monthly mean tables: measured base data + all predictions
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

### CH4 monthly means
#-------------------------------------------------------------------------------
  CH4_selected <- CH4 %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.numeric(format(Date, "%m")),
      Month = factor(month.abb[Month_num], levels = month.abb)
    ) %>%
    select(
      Month_num,
      Month,
      Base_data = flux_CH4,
      any_of(c(
        "pre",
        "preT_40_60_1", "preT_40_60_2", "preT_40_60_5",
        "preT_80_100_1", "preT_80_100_2", "preT_80_100_5",
        "preWT_40_60_1", "preWT_40_60_2", "preWT_40_60_5",
        "preWT_80_100_1", "preWT_80_100_2", "preWT_80_100_5",
        "preWT_5", "preWT_10", "preWT_5h",
        "preWT_5_T_40_60_1", "preWT_5_T_40_60_2", "preWT_5_T_40_60_5",
        "preWT_5_T_80_100_1", "preWT_5_T_80_100_2", "preWT_5_T_80_100_5",
        "preWT_10_T_40_60_1", "preWT_10_T_40_60_2", "preWT_10_T_40_60_5",
        "preWT_10_T_80_100_1", "preWT_10_T_80_100_2", "preWT_10_T_80_100_5",
        "preWT_5h_T_40_60_1", "preWT_5h_T_40_60_2", "preWT_5h_T_40_60_5",
        "preWT_5h_T_80_100_1", "preWT_5h_T_80_100_2", "preWT_5h_T_80_100_5"
      ))
    )
  CH4_monthly_means <- CH4_selected %>%
    group_by(Month_num, Month) %>%
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(Month_num)
  CH4_annual_means <- CH4_monthly_means %>%
    summarise(
      across(
        where(is.numeric) & !all_of("Month_num"),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    mutate(
      Month_num = 13,
      Month = "Annual mean",
      .before = 1
    )
  CH4_monthly_means <- CH4_monthly_means %>%
    mutate(Month = as.character(Month)) %>%
    bind_rows(CH4_annual_means)
#-------------------------------------------------------------------------------

### RECO monthly means
#-------------------------------------------------------------------------------
  RECO_selected <- RECO %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.numeric(format(Date, "%m")),
      Month = factor(month.abb[Month_num], levels = month.abb)
    ) %>%
    select(
      Month_num,
      Month,
      Base_data = CO2.flux_mg,
      any_of(c(
        "pre",
        "preT_40_60_1", "preT_40_60_2", "preT_40_60_5",
        "preT_80_100_1", "preT_80_100_2", "preT_80_100_5",
        "preWT_40_60_1", "preWT_40_60_2", "preWT_40_60_5",
        "preWT_80_100_1", "preWT_80_100_2", "preWT_80_100_5",
        "preWT_5", "preWT_10", "preWT_5h",
        "preWT_5_T_40_60_1", "preWT_5_T_40_60_2", "preWT_5_T_40_60_5",
        "preWT_5_T_80_100_1", "preWT_5_T_80_100_2", "preWT_5_T_80_100_5",
        "preWT_10_T_40_60_1", "preWT_10_T_40_60_2", "preWT_10_T_40_60_5",
        "preWT_10_T_80_100_1", "preWT_10_T_80_100_2", "preWT_10_T_80_100_5",
        "preWT_5h_T_40_60_1", "preWT_5h_T_40_60_2", "preWT_5h_T_40_60_5",
        "preWT_5h_T_80_100_1", "preWT_5h_T_80_100_2", "preWT_5h_T_80_100_5"
      ))
    )
  RECO_monthly_means <- RECO_selected %>%
    group_by(Month_num, Month) %>%
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(Month_num)
  RECO_annual_means <- RECO_monthly_means %>%
    summarise(
      across(
        where(is.numeric) & !all_of("Month_num"),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    mutate(
      Month_num = 13,
      Month = "Annual mean",
      .before = 1
    )
  RECO_monthly_means <- RECO_monthly_means %>%
    mutate(Month = as.character(Month)) %>%
    bind_rows(RECO_annual_means)
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Percent difference of monthly prediction means relative to base data
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

### CH4 percent differences
#-------------------------------------------------------------------------------
  CH4_monthly_percent_diff <- CH4_monthly_means %>%
    mutate(
      across(
        -c(Month_num, Month, Base_data),
        ~ ((.x - Base_data) / Base_data) * 100,
        .names = "{.col}_percent_diff"
      )
    ) %>%
    select(Month_num, Month, ends_with("_percent_diff"))
#-------------------------------------------------------------------------------
  
### RECO percent differences
#-------------------------------------------------------------------------------
  RECO_monthly_percent_diff <- RECO_monthly_means %>%
    mutate(
      across(
        -c(Month_num, Month, Base_data),
        ~ ((.x - Base_data) / Base_data) * 100,
        .names = "{.col}_percent_diff"
      )
    ) %>%
    select(Month_num, Month, ends_with("_percent_diff"))
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Export monthly means by plant type -SSP5_8.5 2080-2099
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
### CH4: measured monthly means by plant type
#-------------------------------------------------------------------------------
  CH4_measured_monthly_plant_type <- CH4 %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.integer(format(Date, "%m")),
      Month = factor(
        month.abb[Month_num],
        levels = month.abb,
        ordered = TRUE
      )
    ) %>%
    group_by(Month_num, Month, plant_type) %>%
    summarise(
      mean_measured_CH4_flux = ifelse(
        all(is.na(flux_CH4)),
        NA_real_,
        mean(flux_CH4, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### RECO: measured monthly means by plant type
#-------------------------------------------------------------------------------
  RECO_measured_monthly_plant_type <- RECO %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.integer(format(Date, "%m")),
      Month = factor(
        month.abb[Month_num],
        levels = month.abb,
        ordered = TRUE
      )
    ) %>%
    group_by(Month_num, Month, plant_type) %>%
    summarise(
      mean_measured_RECO = ifelse(
        all(is.na(CO2.flux_mg)),
        NA_real_,
        mean(CO2.flux_mg, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### CH4: SSP5-8.5 predictions by plant type, 2080–2100
#-------------------------------------------------------------------------------
  CH4_SSP5_2080_2100_monthly_plant_type <- CH4 %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.integer(format(Date, "%m")),
      Month = factor(
        month.abb[Month_num],
        levels = month.abb,
        ordered = TRUE
      )
    ) %>%
    group_by(Month_num, Month, plant_type) %>%
    summarise(
      mean_predicted_CH4_T_SSP5_2080_2100 = ifelse(
        all(is.na(preT_80_100_5)),
        NA_real_,
        mean(preT_80_100_5, na.rm = TRUE)
      ),
      mean_predicted_CH4_WT5_T_SSP5_2080_2100 = ifelse(
        all(is.na(preWT_5_T_80_100_5)),
        NA_real_,
        mean(preWT_5_T_80_100_5, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### RECO: SSP5-8.5 predictions by plant type, 2080–2100
#-------------------------------------------------------------------------------
  RECO_SSP5_2080_2100_monthly_plant_type <- RECO %>%
    mutate(
      Date = as.Date(Date),
      Month_num = as.integer(format(Date, "%m")),
      Month = factor(
        month.abb[Month_num],
        levels = month.abb,
        ordered = TRUE
      )
    ) %>%
    group_by(Month_num, Month, plant_type) %>%
    summarise(
      mean_predicted_RECO_T_SSP5_2080_2100 = ifelse(
        all(is.na(preT_80_100_5)),
        NA_real_,
        mean(preT_80_100_5, na.rm = TRUE)
      ),
      mean_predicted_RECO_WT5_T_SSP5_2080_2100 = ifelse(
        all(is.na(preWT_5_T_80_100_5)),
        NA_real_,
        mean(preWT_5_T_80_100_5, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### Combine CH4 monthly tables
#-------------------------------------------------------------------------------
  CH4_monthly_means_combined <- full_join(
    CH4_measured_monthly_plant_type,
    CH4_SSP5_2080_2100_monthly_plant_type,
    by = c("Month_num", "Month", "plant_type")
  ) %>%
    mutate(
      Month = as.character(Month)
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### Calculate CH4 yearly means from monthly means
#-------------------------------------------------------------------------------
  CH4_yearly_means_plant_type <- CH4_monthly_means_combined %>%
    group_by(plant_type) %>%
    summarise(
      mean_measured_CH4_flux = ifelse(
        all(is.na(mean_measured_CH4_flux)),
        NA_real_,
        mean(mean_measured_CH4_flux, na.rm = TRUE)
      ),
      mean_predicted_CH4_T_SSP5_2080_2100 = ifelse(
        all(is.na(mean_predicted_CH4_T_SSP5_2080_2100)),
        NA_real_,
        mean(mean_predicted_CH4_T_SSP5_2080_2100, na.rm = TRUE)
      ),
      mean_predicted_CH4_WT5_T_SSP5_2080_2100 = ifelse(
        all(is.na(mean_predicted_CH4_WT5_T_SSP5_2080_2100)),
        NA_real_,
        mean(mean_predicted_CH4_WT5_T_SSP5_2080_2100, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      Month_num = 13L,
      Month = "Yearly mean",
      .before = 1
    )
#-------------------------------------------------------------------------------

### Add CH4 yearly rows and calculate percentage differences
#-------------------------------------------------------------------------------
  CH4_monthly_means_combined <- bind_rows(
    CH4_monthly_means_combined,
    CH4_yearly_means_plant_type
  ) %>%
    mutate(
      difference_percent_T = ifelse(
        is.na(mean_measured_CH4_flux) |
          mean_measured_CH4_flux == 0 |
          is.na(mean_predicted_CH4_T_SSP5_2080_2100),
        NA_real_,
        (
          (
            mean_predicted_CH4_T_SSP5_2080_2100 -
              mean_measured_CH4_flux
          ) /
            mean_measured_CH4_flux
        ) * 100
      ),
      difference_percent_WT5_T = ifelse(
        is.na(mean_measured_CH4_flux) |
          mean_measured_CH4_flux == 0 |
          is.na(mean_predicted_CH4_WT5_T_SSP5_2080_2100),
        NA_real_,
        (
          (
            mean_predicted_CH4_WT5_T_SSP5_2080_2100 -
              mean_measured_CH4_flux
          ) /
            mean_measured_CH4_flux
        ) * 100
      )
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### Combine RECO monthly tables
#-------------------------------------------------------------------------------
  RECO_monthly_means_combined <- full_join(
    RECO_measured_monthly_plant_type,
    RECO_SSP5_2080_2100_monthly_plant_type,
    by = c("Month_num", "Month", "plant_type")
  ) %>%
    mutate(
      Month = as.character(Month)
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------

### Calculate RECO yearly means from monthly means
#-------------------------------------------------------------------------------
  RECO_yearly_means_plant_type <- RECO_monthly_means_combined %>%
    group_by(plant_type) %>%
    summarise(
      mean_measured_RECO = ifelse(
        all(is.na(mean_measured_RECO)),
        NA_real_,
        mean(mean_measured_RECO, na.rm = TRUE)
      ),
      mean_predicted_RECO_T_SSP5_2080_2100 = ifelse(
        all(is.na(mean_predicted_RECO_T_SSP5_2080_2100)),
        NA_real_,
        mean(mean_predicted_RECO_T_SSP5_2080_2100, na.rm = TRUE)
      ),
      mean_predicted_RECO_WT5_T_SSP5_2080_2100 = ifelse(
        all(is.na(mean_predicted_RECO_WT5_T_SSP5_2080_2100)),
        NA_real_,
        mean(mean_predicted_RECO_WT5_T_SSP5_2080_2100, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      Month_num = 13L,
      Month = "Yearly mean",
      .before = 1
    )
#-------------------------------------------------------------------------------

### Add RECO yearly rows and calculate percentage differences
#-------------------------------------------------------------------------------
  RECO_monthly_means_combined <- bind_rows(
    RECO_monthly_means_combined,
    RECO_yearly_means_plant_type
  ) %>%
    mutate(
      difference_percent_T = ifelse(
        is.na(mean_measured_RECO) |
          mean_measured_RECO == 0 |
          is.na(mean_predicted_RECO_T_SSP5_2080_2100),
        NA_real_,
        (
          (
            mean_predicted_RECO_T_SSP5_2080_2100 -
              mean_measured_RECO
          ) /
            mean_measured_RECO
        ) * 100
      ),
      difference_percent_WT5_T = ifelse(
        is.na(mean_measured_RECO) |
          mean_measured_RECO == 0 |
          is.na(mean_predicted_RECO_WT5_T_SSP5_2080_2100),
        NA_real_,
        (
          (
            mean_predicted_RECO_WT5_T_SSP5_2080_2100 -
              mean_measured_RECO
          ) /
            mean_measured_RECO
        ) * 100
      )
    ) %>%
    arrange(plant_type, Month_num)
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Monthly and yearly means for fixed WT scenarios
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

### CH4
#-------------------------------------------------------------------------------
  # Calculate monthly means
    CH4_WT_means <- CH4 %>%
      mutate(
        Date = as.Date(Date),
        Month_num = as.numeric(format(Date, "%m")),
        Month = month.abb[Month_num]
      ) %>%
      group_by(Month_num, Month) %>%
      summarise(
        Observed = ifelse(
          all(is.na(flux_CH4)),
          NA_real_,
          mean(flux_CH4, na.rm = TRUE)
        ),
        Base_prediction = ifelse(
          all(is.na(pre)),
          NA_real_,
          mean(pre, na.rm = TRUE)
        ),
        `WT -10 cm` = ifelse(
          all(is.na(preWT_10)),
          NA_real_,
          mean(preWT_10, na.rm = TRUE)
        ),
        `WT -5 cm` = ifelse(
          all(is.na(preWT_5)),
          NA_real_,
          mean(preWT_5, na.rm = TRUE)
        ),
        `WT +5 cm` = ifelse(
          all(is.na(preWT_5h)),
          NA_real_,
          mean(preWT_5h, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      arrange(Month_num)
  # Calculate yearly means from the monthly means
    CH4_WT_yearly <- CH4_WT_means %>%
      summarise(
        Month_num = 13,
        Month = "Yearly mean",
    
        Observed = ifelse(
          all(is.na(Observed)),
          NA_real_,
          mean(Observed, na.rm = TRUE)
        ),
        Base_prediction = ifelse(
          all(is.na(Base_prediction)),
          NA_real_,
          mean(Base_prediction, na.rm = TRUE)
        ),
        `WT -10 cm` = ifelse(
          all(is.na(`WT -10 cm`)),
          NA_real_,
          mean(`WT -10 cm`, na.rm = TRUE)
        ),
        `WT -5 cm` = ifelse(
          all(is.na(`WT -5 cm`)),
          NA_real_,
          mean(`WT -5 cm`, na.rm = TRUE)
        ),
        `WT +5 cm` = ifelse(
          all(is.na(`WT +5 cm`)),
          NA_real_,
          mean(`WT +5 cm`, na.rm = TRUE)
        )
      )
  # Add yearly row and calculate percentage differences
    CH4_WT_means <- bind_rows(
      CH4_WT_means,
      CH4_WT_yearly
    ) %>%
      mutate(
        `Base prediction difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(Base_prediction),
          NA_real_,
          ((Base_prediction - Observed) / Observed) * 100
        ),
        `WT -10 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT -10 cm`),
          NA_real_,
          ((`WT -10 cm` - Observed) / Observed) * 100
        ),
        `WT -5 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT -5 cm`),
          NA_real_,
          ((`WT -5 cm` - Observed) / Observed) * 100
        ),
        `WT +5 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT +5 cm`),
          NA_real_,
          ((`WT +5 cm` - Observed) / Observed) * 100
        )
      ) %>%
      arrange(Month_num) %>%
      select(
        Month,
        Observed,
        Base_prediction,
        `Base prediction difference (%)`,
        `WT -10 cm`,
        `WT -10 cm difference (%)`,
        `WT -5 cm`,
        `WT -5 cm difference (%)`,
        `WT +5 cm`,
        `WT +5 cm difference (%)`
      )
#-------------------------------------------------------------------------------

### RECO
#-------------------------------------------------------------------------------
  # Calculate monthly means
    RECO_WT_means <- RECO %>%
      mutate(
        Date = as.Date(Date),
        Month_num = as.numeric(format(Date, "%m")),
        Month = month.abb[Month_num]
      ) %>%
      group_by(Month_num, Month) %>%
      summarise(
        Observed = ifelse(
          all(is.na(CO2.flux_mg)),
          NA_real_,
          mean(CO2.flux_mg, na.rm = TRUE)
        ),
        Base_prediction = ifelse(
          all(is.na(pre)),
          NA_real_,
          mean(pre, na.rm = TRUE)
        ),
        `WT -10 cm` = ifelse(
          all(is.na(preWT_10)),
          NA_real_,
          mean(preWT_10, na.rm = TRUE)
        ),
        `WT -5 cm` = ifelse(
          all(is.na(preWT_5)),
          NA_real_,
          mean(preWT_5, na.rm = TRUE)
        ),
        `WT +5 cm` = ifelse(
          all(is.na(preWT_5h)),
          NA_real_,
          mean(preWT_5h, na.rm = TRUE)
        ),
       .groups = "drop"
      ) %>%
      arrange(Month_num)
  # Calculate yearly means from the monthly means
    RECO_WT_yearly <- RECO_WT_means %>%
      summarise(
        Month_num = 13,
        Month = "Yearly mean",
        Observed = ifelse(
          all(is.na(Observed)),
          NA_real_,
          mean(Observed, na.rm = TRUE)
        ),
        Base_prediction = ifelse(
          all(is.na(Base_prediction)),
          NA_real_,
          mean(Base_prediction, na.rm = TRUE)
        ),
        `WT -10 cm` = ifelse(
          all(is.na(`WT -10 cm`)),
          NA_real_,
          mean(`WT -10 cm`, na.rm = TRUE)
        ),
        `WT -5 cm` = ifelse(
          all(is.na(`WT -5 cm`)),
          NA_real_,
          mean(`WT -5 cm`, na.rm = TRUE)
        ),
        `WT +5 cm` = ifelse(
          all(is.na(`WT +5 cm`)),
          NA_real_,
          mean(`WT +5 cm`, na.rm = TRUE)
        )
      )
  # Add yearly row and calculate percentage differences
    RECO_WT_means <- bind_rows(
      RECO_WT_means,
      RECO_WT_yearly
    ) %>%
      mutate(
        `Base prediction difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(Base_prediction),
          NA_real_,
          ((Base_prediction - Observed) / Observed) * 100
        ),
    
        `WT -10 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT -10 cm`),
          NA_real_,
          ((`WT -10 cm` - Observed) / Observed) * 100
        ),
        `WT -5 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT -5 cm`),
          NA_real_,
          ((`WT -5 cm` - Observed) / Observed) * 100
        ),
        `WT +5 cm difference (%)` = ifelse(
          is.na(Observed) |
            Observed == 0 |
            is.na(`WT +5 cm`),
          NA_real_,
          ((`WT +5 cm` - Observed) / Observed) * 100
        )
      ) %>%
      arrange(Month_num) %>%
      select(
        Month,
        Observed,
        Base_prediction,
        `Base prediction difference (%)`,
        `WT -10 cm`,
        `WT -10 cm difference (%)`,
        `WT -5 cm`,
        `WT -5 cm difference (%)`,
        `WT +5 cm`,
        `WT +5 cm difference (%)`
      )
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Export CSVs
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  output_folder <- "CSVs"
  dir.create(output_folder, showWarnings = FALSE)
  write_csv(CH4_monthly_means, file.path(output_folder, "CH4_monthly_means.csv"))
  write_csv(RECO_monthly_means, file.path(output_folder, "RECO_monthly_means.csv"))
  write_csv(CH4_monthly_percent_diff, file.path(output_folder, "CH4_monthly_percent_diff.csv"))
  write_csv(RECO_monthly_percent_diff, file.path(output_folder, "RECO_monthly_percent_diff.csv"))
  write_csv(CH4_monthly_means_combined, file.path(output_folder,"CH4_monthly_by_plant_type.csv"),na = "")
  write_csv(RECO_monthly_means_combined, file.path(output_folder,"RECO_monthly_by_plant_type.csv"),na = "")
  write_csv(CH4_WT_means, file.path(output_folder, "CH4_fixed_WT_monthly_yearly_means.csv"))
  write_csv(RECO_WT_means, file.path(output_folder, "RECO_fixed_WT_monthly_yearly_means.csv"))
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  