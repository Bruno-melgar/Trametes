rm(list = ls())     # clear objects  
graphics.off() 
#######################################
###### Trameles mushroom   ############
#######################################


# Packages ----------------------------------------------------------------
inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","cluster", "readxl", "magrittr","hrbrthemes",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","writexl","gtools","ggbiplot","ggrepel","pheatmap", 
              "ggcorrplot", "CCA", "lubridate")
inst(packages)
theme_set(theme_minimal())


# -------------------------- #
#    Importing Dataset       #
# -------------------------- #
(df_screening <- read_excel("Screening.xlsx"))
glimpse(df_screening)


# -------------------------- #
#    Wrangling               #
# -------------------------- #
# Count the number of unique 'Log' identifiers used in the dataset
(n_unique_logs <- length(unique(df_screening$Log)))


# Percentage of Growth
# Find the first sampling event (Number) where growth was detected
(first_growth_number <- df_screening %>%
    filter(Growth == 1) %>%
    summarise(min_number = min(Number)) %>%
    pull(min_number))

# Filter the data from that point onwards
df_post_growth <- df_screening %>%
  filter(Number >= first_growth_number)

# Identify which Logs had growth after the first detection
logs_with_growth <- df_post_growth %>%
  group_by(Log) %>%
  summarise(growth_detected = any(Growth == 1), .groups = "drop")

# Count how many Logs had growth
(n_logs_with_growth <- sum(logs_with_growth$growth_detected))

# Calculate the percentage of Logs with Growth
(perc_logs_with_growth <- (n_logs_with_growth / n_unique_logs) * 100)


# Growth time for Logs
# Step 1: Logs that showed growth at some point
logs_positive <- df_screening %>%
  group_by(Log) %>%
  filter(any(Growth == 1)) %>%
  ungroup()

# Step 2: For each Log, find first Day with Growth == 0 and first Day with Growth == 1
(growth_times <- logs_positive %>%
  group_by(Log) %>%
  summarise(
    first_day_0 = min(Day[Growth == 0]),
    first_day_1 = min(Day[Growth == 1]),
    .groups = "drop"
  ) %>%
  mutate(
    days_to_growth = as.numeric(difftime(first_day_1, first_day_0, units = "days"))
  ))

# Calculate mean and standard deviation of days to fructification
(mean_growth_days <- mean(growth_times$days_to_growth, na.rm = TRUE))
(sd_growth_days <- sd(growth_times$days_to_growth, na.rm = TRUE))


# Extra days of monitoring
dates <- df_screening %>%
  filter(Number %in% c(4, 5)) %>%
  group_by(Number) %>%
  summarise(Day = max(as.Date(Day, "%d/%m/%y"))) %>%
  ungroup()

(diff_days <- as.numeric(dates$Day[dates$Number == 5] - dates$Day[dates$Number == 4]))




