# DATA PREPARATION

### Set Working Directory
#-------------------------------------------------------------------------------
setwd("C:/Users/jeret/Desktop/Daten Bachelorarbeit")
#-------------------------------------------------------------------------------

### Load Needed Libraries:
#-------------------------------------------------------------------------------
  library(tidyverse)
  library(lubridate)
  library(data.table)
#-------------------------------------------------------------------------------

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Read Files
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

### Climate Data
#-------------------------------------------------------------------------------
  # List Files
    files1 <- list.files(path = "Temperature", pattern = "\\.dat$", full.names = TRUE)
  # Read Each File Into Its Own Table 
    T_list <- files1 %>%
      set_names(basename(.)) %>%
      map(~ read_csv(.x))
  # Naming Data
    names(T_list) <- c("FSM-F_PAR", "FSM-F_Carex_T", "FSM-F_Phalaris_T", "FSM-F_Phragmites_T", "FSM-F_Typha_T", "LM_PAR", "LM_Carex_T", "LM_Phalaris_T", "LM_Typha_T", "RH_PAR", "RH_Carex_T", "RH_Phalaris_T")
#-------------------------------------------------------------------------------

### Water Table Data
#-------------------------------------------------------------------------------
  # List Files
    files2 <- list.files(path = "Water Table", pattern = "\\.csv$", full.names = TRUE)
  # Read Each File Into Its Own Table
    data_list2 <- files2 %>%
      set_names(~ tools::file_path_sans_ext(basename(.))) %>%
      map(~ {
        skip_lines <- if (basename(.x) == "2021_LM_Carex_2020-11-25_2022-01-18_WT.csv") 71 else 72
        read_delim(.x,
               delim = ";",
               skip = skip_lines,
               trim_ws = TRUE,
               show_col_types = FALSE) %>%
        select(where(~ !all(is.na(.))))})
  # Water Table Data Combination
    WT_list <- data_list2[1:8] %>%
      split(ceiling(seq_along(.) / 2)) %>%
      map(~ {
        .x[[2]] <- .x[[2]][-1, ]  # Remove First Row Of Second Table
        bind_rows(.x)
      })
    WT_list <- c(WT_list, data_list2[9:length(data_list2)])
  # Naming Data
    names(WT_list) <- c("FSM-F_Carex_WT", "FSM-F_Phalaris_WT", "FSM-F_Phragmites_WT", "FSM-F_Typha_WT","LM_Carex_WT","LM_Phalaris_WT","LM_Typha_WT","RH_Carex_WT","RH_Phalaris_WT")
#-------------------------------------------------------------------------------

### CO2 Data
#-------------------------------------------------------------------------------
  # List Files
    files3 <- list.files(path = "CO2", pattern = "\\.csv$", full.names = TRUE)
  # Read Each File Into Its Own Table
    CO2_list <- files3 %>%
      set_names(~ tools::file_path_sans_ext(basename(.))) %>%
      map(~ {
  # Read CSVs
    df <- read_delim(
      .x,
      delim = ",",
      trim_ws = TRUE,
      show_col_types = FALSE
      )
  # Remove Fully Empty Columns
    df %>% select(where(~ !all(is.na(.))))
    })
  # Naming Data
    names(CO2_list) <- c("FSM-F_Carex_GPP", "FSM-F_Phalaris_GPP", "FSM-F_Phragmites_GPP", "FSM-F_Typha_GPP", "FSM-F_Carex_NEE", "FSM-F_Phalaris_NEE", "FSM-F_Phragmites_NEE", "FSM-F_Typha_NEE", "FSM-F_Carex_RECO", "FSM-F_Phalaris_RECO", "FSM-F_Phragmites_RECO", "FSM-F_Typha_RECO", "LM_Carex_GPP", "LM_Phalaris_GPP", "LM_Typha_GPP", "LM_Carex_NEE", "LM_Phalaris_NEE", "LM_Typha_NEE", "LM_Carex_RECO", "LM_Phalaris_RECO", "LM_Typha_RECO", "RH_Carex_GPP", "RH_Phalaris_GPP", "RH_Carex_NEE", "RH_Phalaris_NEE", "RH_Carex_RECO", "RH_Phalaris_RECO")
#-------------------------------------------------------------------------------

### CH4 Data
#-------------------------------------------------------------------------------
  # List Files
    files4 <- list.files(path = "CH4", pattern = "\\.csv$", full.names = TRUE)
  # Read Each File Into Its Own Table
    CH4_list <- files4 %>%
      set_names(~ tools::file_path_sans_ext(basename(.))) %>%
      map(~ {
    # Read CSVs
      df <- read_csv2(
        .x,
        trim_ws = TRUE,
        show_col_types = FALSE
        )
    # Remove Fully Empty Columns
      df %>% select(where(~ !all(is.na(.))))
      })
  # Naming Data
    names(CH4_list) <- c("FSM-F_Carex_CH4_1","FSM-F_Carex_CH4_2","FSM-F_Carex_CH4_3","FSM-F_Phalaris_CH4_1","FSM-F_Phalaris_CH4_2","FSM-F_Phalaris_CH4_3","FSM-F_Phragmites_CH4_1","FSM-F_Phragmites_CH4_2","FSM-F_Phragmites_CH4_3","FSM-F_Typha_CH4_1","FSM-F_Typha_CH4_2","FSM-F_Typha_CH4_3","LM_Carex_CH4_1","LM_Carex_CH4_2","LM_Carex_CH4_3","LM_Phalaris_CH4_1","LM_Phalaris_CH4_2","LM_Phalaris_CH4_3","LM_Typha_CH4_1","LM_Typha_CH4_2","LM_Typha_CH4_3","RH_Carex_CH4_1","RH_Carex_CH4_2","RH_Carex_CH4_3","RH_Phalaris_CH4_1","RH_Phalaris_CH4_2","RH_Phalaris_CH4_3")
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Data Preparation
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
### Adjust Date Columns Of CH4 To Acutal Dates:
#-------------------------------------------------------------------------------
  CH4_list <- CH4_list %>%
    map(~ {
      df <- .
    # Case: Datetimes Column (With Time)
      if ("datetimes" %in% names(df)) {
        df <- df %>%
          mutate(Date = as.Date(datetimes, format = "%d-%m-%Y %H:%M:%S")) %>%
          select(1:3, Date, everything(), -datetimes)}
    # Case: Date Column (Without Time, Dot Format)
      if ("Date" %in% names(df)) {
        df <- df %>%
          mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
      # Reorder Date To 3rd Column
        if (!("datetimes" %in% names(df))) {
          df <- df %>% select(1:3, Date, everything())}}
        df
      })
#-------------------------------------------------------------------------------

### Combined CH4 Table With Source Column
#-------------------------------------------------------------------------------
  # Extract Column Names From Columns 2–5 Of The First Table
    target_names <- names(CH4_list[[1]])[2:5]
  # Create New Combined Table With A Source Column
    CH4_combined <- CH4_list %>%
      imap(~ .x %>%
         select(2:5) %>%
         set_names(target_names) %>%
         mutate(
           across(3:4, ~ as.numeric(gsub(",", ".", .))),
           source_table = .y)
      ) %>%
      bind_rows()
#-------------------------------------------------------------------------------

### Mean Water Tables
#-------------------------------------------------------------------------------
  get_base_name <- function(source_name) {
    parts <- str_split(source_name, "_")[[1]]
    paste(parts[1:2], collapse = "_")}
  # Precompute WT_list DateTime Column As POSIXct
    WT_list <- map(WT_list, ~ .x %>%
      mutate(datetime = dmy_hms(`Date and Time`)))
  # For Each Row In CH4_combined, Compute The WT Means
    WT_means_df <- CH4_combined %>%
      mutate(RowID = row_number()) %>%
      pmap_dfr(function(...){
        row <- list(...)
        source_table <- row$source_table
        Date <- row$Date
  # Determine WT Table
        base_name <- get_base_name(source_table)
        WT_table_name <- paste0(base_name, "_WT")
        if (WT_table_name %in% names(WT_list)) {
          WT_table <- WT_list[[WT_table_name]]
  # Filter Same Day And 9–11 AM
    filtered <- WT_table %>%
      filter(
      as.Date(datetime) == Date,
      (hour(datetime) >= 9 & hour(datetime) < 11) |
      (hour(datetime) == 11 & minute(datetime) == 0))
  # Filter Same Day And 0–11 AM
    filtered_2 <- WT_table %>%
      filter(
        as.Date(datetime) == Date,
        hour(datetime) >= 0 &
          (hour(datetime) < 11 |
          (hour(datetime) == 11 & minute(datetime) == 0)))
  # Means For 9–11 AM
    if (nrow(filtered) > 0) {means <- colMeans(filtered[3:5], na.rm = TRUE)}
      else {means <- rep(NA_real_, 3)}
  # Means For 0–11 AM
    if (nrow(filtered_2) > 0) {means_2 <- colMeans(filtered_2[3:5], na.rm = TRUE)} 
      else {means_2 <- rep(NA_real_, 3)}}
      else {means <- rep(NA_real_, 3)means_2 <- rep(NA_real_, 3)}
    tibble(
      RowID = row$RowID,
      WT_Pressure_mean = means[1],
      WT_T_mean = means[2],
      WT_mean = means[3],
      WT_mean_2 = means_2[3])
    })
#-------------------------------------------------------------------------------

### Compute T Means Per CH4 Row
#-------------------------------------------------------------------------------
  T_means_df <- CH4_combined %>%
    mutate(RowID = row_number()) %>%
    pmap_dfr(function(...){
      row <- list(...)
      source_table <- row$source_table
      Date <- row$Date
  # Match T Table
    base_name <- get_base_name(source_table)
    T_table_name <- paste0(base_name, "_T")
    if (T_table_name %in% names(T_list)) {
      T_table <- T_list[[T_table_name]]
  # Filter Same Day And 9–11 AM
    filtered <- T_table %>%
      filter(
        as.Date(datetime) == Date,
        (hour(datetime) >= 9 & hour(datetime) < 11) |
        (hour(datetime) == 11 & minute(datetime) == 0))
  # Means Of Columns 3:7
    if (nrow(filtered) > 0) {means <- colMeans(filtered[3:7], na.rm = TRUE)} 
    else {means <- rep(NA_real_, 5)}} 
    else {means <- rep(NA_real_, 5)}
    tibble(
      RowID = row$RowID,
      ST_2_mean = means[1],
      ST_5_mean = means[2],
      ST_10_mean = means[3],
      AT_20_mean = means[4],
      RH_mean = means[5])
    })
#-------------------------------------------------------------------------------

### Compute PAR Means Per CH4 Row
#-------------------------------------------------------------------------------
  PAR_means_df <- CH4_combined %>%
    mutate(RowID = row_number()) %>%
    pmap_dfr(function(...){
      row <- list(...)
      source_table <- row$source_table
      Date <- row$Date
  # Extract Only First Part Before "_"
    first_part <- str_split(source_table, "_")[[1]][1]
  # Match PAR Table
    PAR_table_name <- paste0(first_part, "_PAR")
    if (PAR_table_name %in% names(T_list)) {
      PAR_table <- T_list[[PAR_table_name]]
  # Filter Same Day And 9–11 AM
    filtered <- PAR_table %>%
      filter(
        as.Date(datetime) == Date,
        (hour(datetime) >= 9 & hour(datetime) < 11) |
          (hour(datetime) == 11 & minute(datetime) == 0))
  # Mean Of PAR Column
    if (nrow(filtered) > 0) {PAR_mean <- colMeans(filtered[3], na.rm = TRUE)} 
      else {PAR_mean <- NA_real_}} 
  tibble(
  RowID = row$RowID,
  PAR_mean = PAR_mean)
  })  
#-------------------------------------------------------------------------------

### Compute Site as Values for CH4
#-------------------------------------------------------------------------------
  CH4_combined <- CH4_combined %>%
      mutate(
      Site = case_when(
        str_detect(Site, "Carex_1") & str_detect(source_table, "FSM") ~ 1,
        str_detect(Site, "Carex_2") & str_detect(source_table, "FSM") ~ 2,
        str_detect(Site, "Carex_3") & str_detect(source_table, "FSM") ~ 3,
        str_detect(Site, "Phalaris_1") & str_detect(source_table, "FSM") ~ 4,
        str_detect(Site, "Phalaris_2") & str_detect(source_table, "FSM") ~ 5,
        str_detect(Site, "Phalaris_3") & str_detect(source_table, "FSM") ~ 6,
        str_detect(Site, "Phragmites_1") & str_detect(source_table, "FSM") ~ 7,
        str_detect(Site, "Phragmites_2") & str_detect(source_table, "FSM") ~ 8,
        str_detect(Site, "Phragmites_3") & str_detect(source_table, "FSM") ~ 9,
        str_detect(Site, "Typha_1") & str_detect(source_table, "FSM") ~ 10,
        str_detect(Site, "Typha_2") & str_detect(source_table, "FSM") ~ 11,
        str_detect(Site, "Typha_3") & str_detect(source_table, "FSM") ~ 12,
        str_detect(Site, "Carex_1") & str_detect(source_table, "LM") ~ 13,
        str_detect(Site, "Carex_2") & str_detect(source_table, "LM") ~ 14,
        str_detect(Site, "Carex_3") & str_detect(source_table, "LM") ~ 15,
        str_detect(Site, "Phalaris_1") & str_detect(source_table, "LM") ~ 16,
        str_detect(Site, "Phalaris_2") & str_detect(source_table, "LM") ~ 17,
        str_detect(Site, "Phalaris_3") & str_detect(source_table, "LM") ~ 18,
        str_detect(Site, "Typha_1") & str_detect(source_table, "LM") ~ 19,
        str_detect(Site, "Typha_2") & str_detect(source_table, "LM") ~ 20,
        str_detect(Site, "Typha_3") & str_detect(source_table, "LM") ~ 21,
        str_detect(Site, "Carex_1") & str_detect(source_table, "RH") ~ 22,
        str_detect(Site, "Carex_2") & str_detect(source_table, "RH") ~ 23,
        str_detect(Site, "Carex_3") & str_detect(source_table, "RH") ~ 24,
        str_detect(Site, "Phalaris_1") & str_detect(source_table, "RH") ~ 25,
        str_detect(Site, "Phalaris_2") & str_detect(source_table, "RH") ~ 26,
        str_detect(Site, "Phalaris_3") & str_detect(source_table, "RH") ~ 27,
        TRUE ~ NA_real_
      ))
#-------------------------------------------------------------------------------    
    
### Create Final CH4 Table
#-------------------------------------------------------------------------------
  CH4 <- CH4_combined %>%
    mutate(RowID = row_number()) %>%
    select(1:5, RowID) %>%  
    left_join(WT_means_df %>% select(RowID, 2:5), by = "RowID") %>%  # WT Cols 2:4
    left_join(T_means_df %>% select(RowID, 2:6), by = "RowID") %>%  # T Cols 2:6
    left_join(PAR_means_df %>% select(RowID, 2), by = "RowID") %>% # PAR Col 2
    select(-RowID)
#-------------------------------------------------------------------------------

### Adjust Date And Time Columns Of CO2 Tables
#-------------------------------------------------------------------------------
  CO2_list <- imap(CO2_list, function(df, nm) {
    if (grepl("GPP$", nm)) {
      df <- df %>%
        mutate(datetime = as.POSIXct(
          paste(Date, Time),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ))
    }
    df
  })
#-------------------------------------------------------------------------------

### Fit Nearest WT And T Values To CO2 Values
#-------------------------------------------------------------------------------
  CO2_list_updated <- imap(CO2_list, function(co2_df, co2_name) {
  # Identify Matching Prefix
    prefix <- sub("^([^_]+_[^_]+).*", "\\1", co2_name)
    t_name  <- names(T_list)[grepl(paste0("^", prefix), names(T_list))]
    wt_name <- names(WT_list)[grepl(paste0("^", prefix), names(WT_list))]
  # If No Matches, Return Original
    if (length(t_name) == 0 || length(wt_name) == 0) return(co2_df)
  # Copy Tables
    co2_dt <- as.data.table(copy(co2_df))
    t_dt   <- as.data.table(copy(T_list[[t_name[1]]]))
    wt_dt  <- as.data.table(copy(WT_list[[wt_name[1]]]))
  # Datetime Conversion 
    co2_dt[, datetime := as.POSIXct(datetime)]
    t_dt[, datetime   := as.POSIXct(datetime)]
    wt_dt[, datetime  := as.POSIXct(datetime)]
    setorder(co2_dt, datetime)
    setorder(t_dt, datetime)
    setorder(wt_dt, datetime)
  # Match T
    idx_t <- findInterval(co2_dt$datetime, t_dt$datetime)
    idx_t[idx_t == 0] <- 1
    idx_t[idx_t > nrow(t_dt)] <- nrow(t_dt)
    idx_t2 <- pmin(idx_t + 1, nrow(t_dt))
    diff_t1 <- abs(co2_dt$datetime - t_dt$datetime[idx_t])
    diff_t2 <- abs(co2_dt$datetime - t_dt$datetime[idx_t2])
    nearest_t <- ifelse(diff_t2 < diff_t1, idx_t2, idx_t)
  # Match WT
    idx_w <- findInterval(co2_dt$datetime, wt_dt$datetime)
    idx_w[idx_w == 0] <- 1
    idx_w[idx_w > nrow(wt_dt)] <- nrow(wt_dt)
    idx_w2 <- pmin(idx_w + 1, nrow(wt_dt))
    diff_w1 <- abs(co2_dt$datetime - wt_dt$datetime[idx_w])
    diff_w2 <- abs(co2_dt$datetime - wt_dt$datetime[idx_w2])
    nearest_w <- ifelse(diff_w2 < diff_w1, idx_w2, idx_w)
  # Attach Results
    co2_dt[, `:=`(
  # T Variables
    datetime_T = t_dt$datetime[nearest_t],
    ST2_C      = t_dt$ST2_C[nearest_t],
    ST5_C      = t_dt$ST5_C[nearest_t],
    ST10_C     = t_dt$ST10_C[nearest_t],
    AirT20_C   = t_dt$AirT20_C[nearest_t],
    RH_percent = t_dt$RH[nearest_t],
  # WT Variables
    datetime_WT = wt_dt$datetime[nearest_w],
    WT_pressure    = wt_dt[["Pressure (kPa)"]][nearest_w],
    WT_temperature = wt_dt[["Temperature (C)"]][nearest_w],
    WT_depth_cm = wt_dt[["Level Depth To Water (cm)"]][nearest_w]
  )]
  return(co2_dt)
  })
#-------------------------------------------------------------------------------

### Keep Only Relevant Columns
#-------------------------------------------------------------------------------
  CO2_list_final <- imap(CO2_list_updated, function(dt, name) {
    dt <- as.data.table(copy(dt))
  # Case 1: NEE or RECO Tables
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (grepl("NEE$|RECO$", name)) {
      keep_cols <- intersect(
        c("replicate", "Tair", "ST2", "ST5", "ST10",
          "CO2.flux_mg", "datetime", "datetime_WT", "WT_depth_cm", "PAR", "RH_percent", "WT_pressure", "WT_temperature"),
        names(dt)
      )
      return(dt[, ..keep_cols])
    }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
  # Case 2: GPP Tables
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (grepl("GPP$", name)) {
    # Rename Columns First
      rename_map <- list(
        ST2_C     = "ST2",
        ST5_C     = "ST5",
        ST10_C    = "ST10",
        AirT20_C  = "Tair",
        Replicate = "recplicate"
      )
      for (old in names(rename_map)) {
        if (old %in% names(dt)) {
          setnames(dt, old, rename_map[[old]])
        }
      }
      keep_cols <- intersect(
        c("replicate", "GPP_flux",
          "datetime", "datetime_WT", "datetime_T",
          "ST2", "ST5", "ST10", "Tair",
          "WT_depth_cm", "PAR", "RH_percent", "WT_pressure", "WT_temperature"),
        names(dt)
      )
      return(dt[, ..keep_cols])
    }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
  # Fallback: Unchanged
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    dt
    })
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------

### Extract Combined GPP, NEE, RECO Tables
#-------------------------------------------------------------------------------
  # Helper: Extract Prefix Up To Second Underscore
    get_prefix <- function(x) sub("^([^_]+_[^_]+).*", "\\1", x)
  # GPP
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    GPP_all <- imap(CO2_list_final, function(dt, name) {
      if (grepl("GPP$", name)) {
        dt <- as.data.table(copy(dt))
        dt[, source_table := get_prefix(name)]
        return(dt)
      }
      NULL
    }) |> rbindlist(fill = TRUE)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # RECO
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    RECO_all <- imap(CO2_list_final, function(dt, name) {
      if (grepl("RECO$", name)) {
        dt <- as.data.table(copy(dt))
        dt[, source_table := get_prefix(name)]
        return(dt)
      }
      NULL
    }) |> rbindlist(fill = TRUE)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # NEE
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    NEE_all <- imap(CO2_list_final, function(dt, name) {
      if (grepl("NEE$", name)) {
        dt <- as.data.table(copy(dt))
        dt[, source_table := get_prefix(name)]
        return(dt)
      }
      NULL
    }) |> rbindlist(fill = TRUE)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
  
### Compute Site as Values for RECO
#-------------------------------------------------------------------------------
  RECO_all <- RECO_all %>%
    mutate(
      Site = case_when(
        str_detect(replicate, "Carex_1") & str_detect(source_table, "FSM") ~ 1,
        str_detect(replicate, "Carex_2") & str_detect(source_table, "FSM") ~ 2,
        str_detect(replicate, "Carex_3") & str_detect(source_table, "FSM") ~ 3,
        str_detect(replicate, "Phalaris_1") & str_detect(source_table, "FSM") ~ 4,
        str_detect(replicate, "Phalaris_2") & str_detect(source_table, "FSM") ~ 5,
        str_detect(replicate, "Phalaris_3") & str_detect(source_table, "FSM") ~ 6,
        str_detect(replicate, "Phragmites_1") & str_detect(source_table, "FSM") ~ 7,
        str_detect(replicate, "Phragmites_2") & str_detect(source_table, "FSM") ~ 8,
        str_detect(replicate, "Phragmites_3") & str_detect(source_table, "FSM") ~ 9,
        str_detect(replicate, "Typha_1") & str_detect(source_table, "FSM") ~ 10,
        str_detect(replicate, "Typha_2") & str_detect(source_table, "FSM") ~ 11,
        str_detect(replicate, "Typha_3") & str_detect(source_table, "FSM") ~ 12,
        str_detect(replicate, "C_1") & str_detect(source_table, "LM") ~ 13,
        str_detect(replicate, "C_2") & str_detect(source_table, "LM") ~ 14,
        str_detect(replicate, "C_3") & str_detect(source_table, "LM") ~ 15,
        str_detect(replicate, "P_1") & str_detect(source_table, "LM") ~ 16,
        str_detect(replicate, "P_2") & str_detect(source_table, "LM") ~ 17,
        str_detect(replicate, "P_3") & str_detect(source_table, "LM") ~ 18,
        str_detect(replicate, "T_1") & str_detect(source_table, "LM") ~ 19,
        str_detect(replicate, "T_2") & str_detect(source_table, "LM") ~ 20,
        str_detect(replicate, "T_3") & str_detect(source_table, "LM") ~ 21,
        str_detect(replicate, "C_1") & str_detect(source_table, "RH") ~ 22,
        str_detect(replicate, "C_2") & str_detect(source_table, "RH") ~ 23,
        str_detect(replicate, "C_3") & str_detect(source_table, "RH") ~ 24,
        str_detect(replicate, "P_1") & str_detect(source_table, "RH") ~ 25,
        str_detect(replicate, "P_2") & str_detect(source_table, "RH") ~ 26,
        str_detect(replicate, "P_3") & str_detect(source_table, "RH") ~ 27,
        TRUE ~ NA_real_
      ))
#-------------------------------------------------------------------------------    
    
### Convert plant type to Number
#-------------------------------------------------------------------------------
  # Helper function
    add_plant_type <- function(df) {
      df %>%
        mutate(
          plant_type = case_when(
            str_detect(source_table, "Carex") ~ 1,
            str_detect(source_table, "Phalaris") ~ 2,
            str_detect(source_table, "Phragmites") ~ 3,
            str_detect(source_table, "LM_Typha") ~ 5,
            str_detect(source_table, "Typha") ~ 4,
            TRUE ~ NA_real_
      ))}
    
  # Apply to all tables
    CH4      <- add_plant_type(CH4)
    GPP  <- add_plant_type(GPP_all)
    RECO <- add_plant_type(RECO_all)
    NEE  <- add_plant_type(NEE_all)
#-------------------------------------------------------------------------------

### Remove Rows with NA
#-------------------------------------------------------------------------------
  CH4<- na.omit(CH4)
  GPP<- na.omit(GPP)
  NEE<- na.omit(NEE)
  RECO<- na.omit(RECO)
#-------------------------------------------------------------------------------
  
### Remove ROws where flux is 0
#-------------------------------------------------------------------------------
  CH4 <- CH4 %>%
    filter(flux_CH4 != 0)
  GPP <- GPP %>%
    filter(GPP_flux != 0)
  NEE <- NEE %>%
    filter(CO2.flux_mg != 0)
  RECO <- RECO %>%
    filter(CO2.flux_mg != 0)
#-------------------------------------------------------------------------------

### Keep only Relevant Columns and add uniformity
#-------------------------------------------------------------------------------
  # All CH4 Tables
    CH4 <- CH4 %>%
      rename (
        WT_P = WT_Pressure_mean,
        WT_T = WT_T_mean,
        WT = WT_mean,
        WT2 = WT_mean_2,
        ST2 = ST_2_mean,
        ST5 =  ST_5_mean,
        ST10 = ST_10_mean,
        Tair = AT_20_mean,
        RH = RH_mean,
        PAR = PAR_mean,
      ) %>%
      select(Date, flux_CH4, Tair, ST2, ST5, ST10, WT, RH, PAR, plant_type, Site)
  # All GPP Tables
    GPP <- GPP %>%
      rename (
        Date = datetime,
        WT_P = WT_pressure,
        WT_T = WT_temperature,
        WT = WT_depth_cm,
        RH = RH_percent
      ) %>%
      select(Date, GPP_flux, Tair, ST2, ST5, ST10, WT, RH, PAR, plant_type)
  # All NEE Tables
    NEE <- NEE %>%
      rename (
        Date = datetime,
        WT_P = WT_pressure,
        WT_T = WT_temperature,
        WT = WT_depth_cm,
        RH = RH_percent
      ) %>%
      select(Date, CO2.flux_mg, Tair, ST2, ST5, ST10, WT, RH, PAR, plant_type)
  # All RECO Tables
    RECO <- RECO %>%
      rename (
        Date = datetime,
        WT_P = WT_pressure,
        WT_T = WT_temperature,
        WT = WT_depth_cm,
        RH = RH_percent
      ) %>%
      select(Date, CO2.flux_mg, Tair, ST2, ST5, ST10, WT, RH, PAR, plant_type, Site)
#-------------------------------------------------------------------------------

### Removing Outliers
#-------------------------------------------------------------------------------
  # Response Variables
    GPP <- GPP %>%
      filter(GPP_flux >= -70)
    NEE <- NEE %>%
      filter(CO2.flux_mg >= -10000)
    RECO <- RECO %>%
      filter(CO2.flux_mg <= 4500)
  # Covariates
    # CH4
      CH4 <- CH4 %>%
        filter(WT <= 80)
    # GPP
      GPP <- GPP %>%
         filter(RH >= 0)
    # NEE
      NEE <- NEE %>%
        filter(RH >= 0)
    # RECO
      RECO <- RECO %>%
        filter(RH >= 0)
      RECO <- RECO[!(RECO$Date %in% as.POSIXct(c(
        "2019-06-14 15:36:34",
        "2019-06-14 15:41:23",
        "2019-06-14 15:47:15"
      ), tz = "UTC")), ]
#-------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Export new CSVs
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # CH4
    write.csv(CH4, "CH4.csv", row.names = TRUE)
  # GPP
    write.csv(GPP, "GPP.csv", row.names = TRUE)
  # NEE
    write.csv(NEE, "NEE.csv", row.names = TRUE)
  # RECO
    write.csv(RECO, "RECO.csv", row.names = TRUE)
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx