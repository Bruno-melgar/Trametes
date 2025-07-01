rm(list = ls())     # clear objects  
graphics.off() 
#######################################
###### Trameles micro   ############
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
              "fpc","cluster", "readxl", "magrittr","hrbrthemes", "forcats",
              "multipanelfigure","klaR","psych","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","writexl","gtools","ggbiplot","ggrepel","pheatmap", 
              "ggcorrplot", "CCA", "ggstatsplot", "paletteer", "scales",
              "broom", "drc", "patchwork", "ggalt", "ggtext")
inst(packages)
theme_set(theme_minimal())


# -------------------------- #
#    Importing Dataset       #
# -------------------------- ## Datos en formato tidy para actividad antibacterial
antibacterial <- data.frame(
  Activity = "Antibacterial",
  Microorganism = rep(c("Staphylococcus aureus", "Bacillus cereus", 
                        "Listeria monocytogenes", "Escherichia coli", 
                        "Enterobacter cloacae"), each = 4),
  Extract = rep(c("CEA", "Wild", "E211", "E224"), times = 5),
  MIC = c(1, 1, 4, 1,
          2, 1, 0.5, 2,
          8, 1, 1, 0.5,
          8, 1, 1, 1,
          8, 1, 1, 0.5),
  MBC_MFC = c(2, 2, 4, 1,
          4, 2, 0.5, 4,
          16, 2, 2, 1,
          16, 2, 2, 1,
          16, 2, 2, 1)
)

# Datos en formato tidy para actividad antifúngica
antifungal <- data.frame(
  Activity = "Antifungal",
  Microorganism = rep(c("Aspergillus versicolor", "Aspergillus niger", 
                        "Penicillium funiculosum", "Penicillium verrucosum var. cyclopium", 
                        "Trichoderma viride"), each = 4),
  Extract = rep(c("CEA", "Wild", "E211", "E224"), times = 5),
  MIC = c(2, 0.5, 2, 1,
          4, 0.5, 1, 1,
          2, 0.12, 1, 0.5,
          4, 16, 2, 1,
          2, 1, 1, 0.5),
  MBC_MFC = c(4, 1, 4, 1,
          8, 1, 2, 1,
          4, 0.25, 2, 0.5,
          8, 16, 4, 1,
          4, 2, 2, 0.5)
)

# Unir en un solo dataframe si lo deseas
full_df <- rbind(antibacterial, antifungal)


# -------------------------- #
#       Wrangling            #
# -------------------------- #
# Sample type nesting
full_df$Type <- ifelse(full_df$Extract %in% c("E211", "E224"), "Control", "Sample")

# Re- order of DataFrame
full_df <- full_df[, c("Type", setdiff(names(full_df), "Type"))]

# Subsets
df_control <- subset(full_df, Type == "Control")
df_sample  <- subset(full_df, Type == "Sample")


# Ratio MBC or MFC / MIC per strain
df_sample$Ratio_MBC_MFC_MIC <- df_sample$MBC_MFC / df_sample$MIC

# Naming effect type
df_sample$Effect_Type <- ifelse(
  df_sample$Ratio_MBC_MFC_MIC >= 4,
  ifelse(df_sample$Activity == "Antibacterial", "Bacteriostatic", "Fungistatic"),
  ifelse(df_sample$Activity == "Antibacterial", "Bactericide", "Fungicide")
)

# Factor conversion
df_sample$Effect_Type <- factor(df_sample$Effect_Type, 
                              levels = c("Bactericide", "Bacteriostatic", "Fungicide", "Fungistatic"))


# Confronting controls vs samples
# Reformatear df_control para tener columnas separadas por extracto
df_control_wide <- df_control %>%
  pivot_wider(
    id_cols = c(Microorganism, Activity),
    names_from = Extract,
    values_from = c(MIC, MBC_MFC),
    names_glue = "{.value}_{Extract}"
  )

merged_df <- df_sample %>%
  left_join(df_control_wide, by = c("Microorganism", "Activity"))

merged_df <- merged_df %>%
  mutate(
    Ratio_E211 = case_when(
      Effect_Type %in% c("Bactericide", "Fungicide") ~ MBC_MFC_E211 / MBC_MFC,
      Effect_Type %in% c("Bacteriostatic", "Fungistatic") ~ MIC_E211 / MIC,
      TRUE ~ NA_real_
    ),
    Ratio_E224 = case_when(
      Effect_Type %in% c("Bactericide", "Fungicide") ~ MBC_MFC_E224 / MBC_MFC,
      Effect_Type %in% c("Bacteriostatic", "Fungistatic") ~ MIC_E224 / MIC,
      TRUE ~ NA_real_
    )
  )


# -------------------------- #
#    Plot   #
# -------------------------- #
merged_df$Microorganism <- factor(merged_df$Microorganism)

# Pivot and celaning
df_long <- merged_df %>%
  pivot_longer(
    cols = c(Ratio_E211, Ratio_E224),
    names_to = "Ratio",
    values_to = "Value"
  ) %>%
  mutate(
    Ratio = recode(Ratio,
                   "Ratio_E211" = "E211",
                   "Ratio_E224" = "E224"),
    Extract = factor(Extract, levels = c("CEA", "Wild"))
  ) %>%
  filter(!is.na(Value))

# set original order
micro_levels <- df_long %>%
  pull(Microorganism) %>%
  unique() %>%
  sort(decreasing = TRUE)

# Markdown style and re-order
df_long <- df_long %>%
  mutate(Microorganism = paste0("*", Microorganism, "*")) %>%
  mutate(Microorganism = factor(Microorganism, levels = paste0("*", micro_levels, "*")))

# Plotting
ggplot(df_long, aes(x = Extract, y = Microorganism, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "red", mid = "white", high = "lightgreen", midpoint = 1, name = "Ratio") +
  facet_grid(Activity ~ Ratio, scales = "free_y", space = "free_y", drop = TRUE) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = ggtext::element_markdown()
  )

