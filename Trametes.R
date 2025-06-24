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
              "multipanelfigure","klaR","psych","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","writexl","gtools","ggbiplot","ggrepel","pheatmap", 
              "ggcorrplot", "CCA", "ggstatsplot", "paletteer", "scales",
              "broom", "drc", "patchwork", "ggalt", "randomForest")
inst(packages)
theme_set(theme_minimal())


# -------------------------- #
#    Importing Dataset       #
# -------------------------- #
(df <- read_excel("Trametes.xlsx", skip = 1))

# Data glimpse
str(df)

col_removed <- df %>%
  select(where(~ all(is.na(.)) || all(. == 0)))

df <- df %>%
  select(where(~ !all(is.na(.)) && !all(. == 0)))

# -------------------------- #
#       Wrangling            #
# -------------------------- #
sapply(df, class) # Spot class to standardize them
# Standardize data class
df <- df %>%
  mutate(across(where(is.logical), as.numeric))

# Rename repeated terms function
names(df)[grepl("Total\\.\\.\\.", names(df))]

# Rename columns
df <- df %>%
  rename(
    TotalSugars = `Total...18`,
    TotalOrganicAcids = `Total...22`,
    TotalTocopherols = `Total...25`,
    TotalFattyAcids = `Total...60`
  )

# Rename of Samples
df <- df %>%
  mutate(
    Treatment = case_when(
      grepl("^TramS", Sample) ~ "Wild",
      grepl("^TramN", Sample) ~ "CEA"
    ),
    Replicate = str_extract(Sample, "\\d+$") %>% as.factor()
  ) %>%
  dplyr::select(Sample, Treatment, Replicate, everything())


# -------------------------- #
#       Summary              #
# -------------------------- #  
df_summary_general <- df %>%
  group_by(Treatment) %>%
  summarise(across(
    where(is.numeric),
    list(mean = mean, 
         sd = sd, 
         cv = ~sd(.) / mean(.) * 100),
    .names = "{.col}_{.fn}"
  ), 
  .groups = "drop")


# Pivoting
df_summary_tidy <- df_summary_general %>%
  pivot_longer(
    cols = -Treatment,
    names_to = c("Variable", "Measurement"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = value
  ) %>%
  dplyr::select(Treatment, Measurement, everything())


df_summary_tidy_list <- df_summary_tidy %>%
  pivot_longer(
    cols = -c(Treatment, Measurement),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Group = case_when(
      Variable %in% c("Moisture", "Protein", "Ash", "Lipids", "Fiber", "Carbohydrates", "Energy") ~ "Proximate composition",
      Variable %in% c("Fructose", "Glucose", "Manitol", "Sacarose", "Rafinose", "Trealose", "TotalSugars") ~ "Sugars",
      Variable %in% c("Oxalic acid", "Malic acid", "Fumaric acid", "TotalOrganicAcids") ~ "Organic acids",
      Variable %in% c("a-Tocopherol", "d-Tocopherol", "TotalTocopherols" ) ~ "Tocopherols",
      Variable %in% c("DPPH", "OxHLIA 60 min", "TBARS") ~ "Antioxidants",
      Variable %in% c("C6:0", "C11:0", "C12:0", "C14:0", "C15:0", "C16:0", "C16:1", "C17:0", "C18:0", 
                      "C18:1n9t", "C18:1n9c", "C18:2n6t", "C18:2n6c", "C18:3n3", "C20:0", "C20:3n6", 
                      "C20:5n3", "C22:0", "SFA", "MUFA", "PUFA", "TotalFattyAcids") ~ "Fatty acids",
      TRUE ~ "Other"
    ),
    Value = round(Value, 3)  # rounded to 3 decimal points
  ) %>%
  relocate(Treatment, Group, Variable, Measurement, Value)



df_summary_wide <- df_summary_tidy_list %>%
  pivot_wider(
    names_from = Treatment,
    values_from = Value
  ) %>%
  relocate(Group, Variable, Measurement, CEA, Wild)


# ---------------------------- #
# Comparisons and descriptions #
# ---------------------------- #
df_long <- df %>%
  select(Treatment, where(is.numeric)) %>%  # Removing Sample y Replicate
  pivot_longer(
    cols = -Treatment,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Group = case_when(
      Variable %in% c("Moisture", "Protein", "Ash", "Lipids", "Fiber", "Carbohydrates", "Energy") ~ "Proximate composition",
      Variable %in% c("Rhamnose", "Fucose", "Arabinose", "Xylose", "Mannose", "Galactose", "Glucose", "Uronic acid","TotalSugars") ~ "Sugars",
      Variable %in% c("Oxalic acid", "Malic acid", "Fumaric acid", "TotalOrganicAcids") ~ "Organic acids",
      Variable %in% c("a-Tocopherol", "d-Tocopherol", "TotalTocopherols" ) ~ "Tocopherols",
      Variable %in% c("DPPH", "OxHLIA 60 min", "TBARS") ~ "Antioxidants",
      Variable %in% c("C6:0", "C11:0", "C12:0", "C14:0", "C15:0", "C16:0", "C16:1", "C17:0", "C18:0", 
                      "C18:1n9t", "C18:1n9c", "C18:2n6t", "C18:2n6c", "C18:3n3", "C20:0", "C20:3n6", 
                      "C20:5n3", "C22:0", "SFA", "MUFA", "PUFA", "TotalFattyAcids") ~ "Fatty acids",
      TRUE ~ "Other"
    ),
    Value = round(Value, 3)  # rounded to 3 decimal points
  ) %>%
  relocate(Treatment, Group, Variable, Value)



# -------------------------- #
#    Proximate composition   #
# -------------------------- #
# Create Proximate composition subset without energy to achive 100%
df_proximate <- df_long %>%
  filter(Group == "Proximate composition" & Variable != "Energy")

# Stack bars version ggstatplot
df_summ <- df_proximate %>%
  group_by(Treatment, Variable) %>%
  summarise(Mean_Value = mean(Value), .groups = "drop") %>%
  group_by(Treatment) %>%
  mutate(Percentage = Mean_Value/sum(Mean_Value)*100)

(p_macro <- ggplot(df_summ, aes(x = Treatment, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0)
  ) +
  scale_fill_paletteer_d(
    "beyonce::X12",  
    labels = c("Ash", "Carbohydrates", "Fiber", "Lipids", "Moisture", "Protein")
  ) +
  labs(
    title = "Proximal Composition of the Treatments",
    x = "Treatment",
    y = "Composition percentage",
    fill = "Component"
  ) +
  theme_ggstatsplot() +  # Usa el tema de ggstatsplot
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(reverse = FALSE))) 


# Stats
# Proximal composition
df_stat_prox <- df_long %>%
  filter(Group == "Proximate composition")

results_ttest_prox <- df_stat_prox %>%
  group_by(Variable) %>%
  summarise(
    t_test = list(t.test(Value ~ Treatment)),
    .groups = "drop"
  ) %>%
  mutate(tidy_result = map(t_test, tidy)) %>%
  unnest(tidy_result) %>%
  select(Variable, estimate, estimate1, estimate2, statistic, p.value, conf.low, conf.high)




# -------------------------- #
#    Sugars-acids  #
# -------------------------- #
# Stats
# Sugars prep
df_stat_sug <- df_long %>%
  filter(Group == "Sugars")

# Sugars t-test
results_ttest_sug <- df_stat_sug %>%
  group_by(Variable) %>%
  summarise(
    t_test = list(t.test(Value ~ Treatment)),
    .groups = "drop"
  ) %>%
  mutate(tidy_result = map(t_test, tidy)) %>%
  unnest(tidy_result) %>%
  select(Variable, estimate, estimate1, estimate2, statistic, p.value, conf.low, conf.high)

# Organic Acids
# OA prep
df_stat_oa <- df_long %>%
  filter(Group == "Organic acids")

# OA t-test
results_ttest_oa <- df_stat_oa %>%
  group_by(Variable) %>%
  summarise(
    t_test = list(t.test(Value ~ Treatment)),
    .groups = "drop"
  ) %>%
  mutate(tidy_result = map(t_test, tidy)) %>%
  unnest(tidy_result) %>%
  select(Variable, estimate, estimate1, estimate2, statistic, p.value, conf.low, conf.high)

# Total Removal
df_sug_oa_clean <- df_long %>%
  filter(Group %in% c("Sugars", "Organic acids"),
         !str_starts(Variable, "Total"))


# Create Sugars and Organic acids subset
df_sugars <- df_sug_oa_clean %>% filter(Group == "Sugars")
df_acids  <- df_sug_oa_clean %>% filter(Group == "Organic acids")

# Building stat-dataframe for plotting in sugars
df_pvals_sugars <- results_ttest_sug %>%
  #filter(p.value < 0.05) %>%  # solo significativos
  mutate(
    group1 = "Wild",
    group2 = "CEA",
    y.position = pmax(estimate1, estimate2) + 0.1,
    p.adj.signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05  ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(Variable != "TotalSugars") %>% 
  select(Variable, group1, group2, y.position, p.adj.signif)

# Building stat-dataframe for plotting in sugars
df_pvals_acids <- results_ttest_oa %>%
  filter(p.value < 0.05) %>%  # solo significativos
  mutate(
    group1 = "Wild",
    group2 = "CEA",
    y.position = pmax(estimate1, estimate2) + 0.04,  # ajustar separación visual
    p.adj.signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05  ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(Variable != "TotalOrganicAcids") %>%  # quitar total
  select(Variable, group1, group2, y.position, p.adj.signif)


# Sugars plot
p1 <- ggplot(df_sugars, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Variable, nrow = 1) +
  #ylim(0, 1.8) +
  labs(
    title = "Distribution of Sugars and Organic Acids by Treatment",
    subtitle = "Sugars",
    x = NULL,
    y = "g/100g"
  ) +
  scale_fill_paletteer_d("beyonce::X12") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) 
# Sugars plot + stat annotations 
p1 <- p1 +
  stat_pvalue_manual(
    data = df_pvals_sugars,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 5,
    inherit.aes = FALSE,
    mapping = aes(x = group1, xend = group2, y = y.position, yend = y.position, label = p.adj.signif),
    facets = vars(Variable)
  )


df_sugars_no_glu <- df_sugars %>% filter(Variable != "Glucose")
df_pvals_sugars_no_glu <- df_pvals_sugars %>% filter(Variable != "Glucose")

df_pvals_sugars_no_glu <- df_pvals_sugars_no_glu %>%
  mutate(y.position = y.position + 3)

(p1 <- ggplot(df_sugars_no_glu, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Variable, nrow = 1) +
  labs(
    title = "Distribution of Sugars by Treatment",
    x = NULL,
    y = "g/100g"
  ) +
  scale_fill_paletteer_d("beyonce::X12") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  stat_pvalue_manual(
    data = df_pvals_sugars_no_glu,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 5,
    inherit.aes = FALSE,
    mapping = aes(x = group1, xend = group2, y = y.position, yend = y.position, label = p.adj.signif),
    facets = vars(Variable)
  ))


df_sugars_glu <- df_sugars %>% filter(Variable == "Glucose")
df_pvals_sugars_glu <- df_pvals_sugars %>% filter(Variable == "Glucose")

df_pvals_sugars_glu <- df_pvals_sugars_glu %>%
  mutate(y.position = y.position + 45)

(p3 <- ggplot(df_sugars_glu, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  labs(
    subtitle = "Glucose",
    x = NULL,
    y = "g/100g"
  ) +
  scale_fill_paletteer_d("beyonce::X12") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  stat_pvalue_manual(
    data = df_pvals_sugars_glu,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 5,
    inherit.aes = FALSE,
    mapping = aes(x = group1, xend = group2, y = y.position, yend = y.position, label = p.adj.signif)
  ))


(summary_table_sug <- df_sugars %>%
  group_by(Treatment, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Summary = sprintf("%.2f ± %.2f", Mean, SD)
  ) %>%
  select(Treatment, Variable, Summary) %>%
  pivot_wider(names_from = Treatment, values_from = Summary))


# AO plot
p2 <- ggplot(df_acids, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Variable, nrow = 1) +
  ylim(0, 0.48) +
  labs(
    subtitle = "Organic Acids",
    x = "Treatment",
    y = "g/100g"
  ) +
  scale_fill_paletteer_d("beyonce::X12") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) 
# OA plot + stat annotations
p2 <- p2 + 
  stat_pvalue_manual(
    data = df_pvals_acids,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 5,
    inherit.aes = FALSE,
    mapping = aes(x = group1, xend = group2, y = y.position, yend = y.position, label = p.adj.signif),
    facets = vars(Variable)
  )


(summary_table_oa <- df_acids %>%
    group_by(Treatment, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Summary = sprintf("%.3f ± %.3f", Mean, SD)
    ) %>%
    select(Treatment, Variable, Summary) %>%
    pivot_wider(names_from = Treatment, values_from = Summary))

# Final plot
p.mix <- p1 + p3 +
  plot_layout(widths = c(2.7, 0.3))  

# Join plots Sugars-OA plot + stat annotations
(p_sug_ao <- p.mix / p2 + plot_layout(heights = c(1, 1)))

# -------------------------- #
#    Antiox  #
# -------------------------- #
# Tocopherols, DPPH, OxHLIA, TBARS
# Filtrar compuestos de interés
compounds <- c("a-Tocopherol", "d-Tocopherol", "TotalTocopherols", 
               "DPPH", "OxHLIA 60 min", "TBARS")

df_antiox <- df_long %>%
  filter(Variable %in% compounds) %>%
  group_by(Variable, Treatment) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")

df_antiox <- df_antiox %>%
  mutate(Variable = factor(Variable, levels = c(
    "a-Tocopherol",
    "d-Tocopherol",
    "TotalTocopherols",
    "DPPH",
    "OxHLIA 60 min",
    "TBARS"
  )))

df_scaled <- df_antiox %>%
  group_by(Variable) %>%
  mutate(scaled_value = scale(mean_value)[,1]) %>%
  ungroup()

# Graficar heatmap
ggplot(df_scaled, aes(x = Treatment, y = Variable, fill = scaled_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_value, 2)), color = "white", size = 4.5) +
  scale_fill_gradient2(low = "#A1C0C1", mid = "white", high = "#451E07", midpoint = 0) +
  scale_y_discrete(limits = rev(levels(df_antiox$Variable))) +
  labs(
    title = "Standardized Antioxidant and Tocopherol Levels by Treatment",
    subtitle = "Values shown are actual means, colors indicate z-score per compound",
    x = NULL,
    y = NULL,
    fill = "Z-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )




# -------------------------- #
#    Fatty acids  #
# -------------------------- #
# Filtering df for fatty acids
df_fatty <- df_long %>% 
  filter(Group == "Fatty acids", Variable != "TotalFattyAcids") %>%
  mutate(Value = ifelse(is.na(Value), 0, Value))

# Wranging for dumbell plot
means_fatty <- df_fatty %>%
  group_by(Variable, Treatment) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Treatment, values_from = mean_value)

(summary_table_fatty <- df_fatty %>%
    group_by(Treatment, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Summary = sprintf("%.2f ± %.2f", Mean, SD)
    ) %>%
    select(Treatment, Variable, Summary) %>%
    pivot_wider(names_from = Treatment, values_from = Summary))

# Stats between treatments
results_ttest_fatty <- df_fatty %>%
  group_by(Variable) %>%
  summarise(
    p.value = t.test(Value ~ Treatment)$p.value,
    estimate1 = mean(Value[Treatment == "Wild"]),
    estimate2 = mean(Value[Treatment == "CEA"]),
    .groups = "drop"
  ) %>%
  mutate(
    p.adj.signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

## Join frames
plot_data_fatty <- means_fatty %>%
  left_join(results_ttest_fatty, by = "Variable")


plot_data_fatty <- plot_data_fatty %>%
  mutate(mean_value = (Wild + CEA) / 2) %>%
  arrange(mean_value) %>%
  mutate(Variable = factor(Variable, levels = unique(Variable)))

# Order of fatty acids for presentation
new_order <- c("C6:0", "C11:0", "C12:0", "C14:0", "C15:0", "C16:0", "C17:0", "C18:0", "C20:0", "C22:0", "SFA",
               "C16:1", "C18:1n9t", "C18:1n9c", "MUFA", "C18:2n6t", "C18:2n6c", "C18:3n3", "C20:3n6", "C20:5n3", "PUFA")
# Descend arrange
new_order_reverse <- rev(new_order)
# Assigning
plot_data_fatty$Variable <- factor(plot_data_fatty$Variable, levels = new_order_reverse)



# Dumbell plot
## Degradation line for plot prep
n_segments <- 100
## Degradation line built
plot_segments <- plot_data_fatty %>%
  rowwise() %>%
  mutate(
    x_seq = list(seq(Wild, CEA, length.out = n_segments)),
    color_seq = list(colorRampPalette(c("#451E07", "#A1C0C1"))(n_segments))
  ) %>%
  unnest(c(x_seq, color_seq)) %>%
  group_by(Variable) %>%
  mutate(
    xend_seq = lead(x_seq),
    color_end = lead(color_seq)
  ) %>%
  filter(!is.na(xend_seq)) %>%
  ungroup()

# Ensambling plot
(p_fatty <- ggplot() +
  geom_segment(
    data = plot_segments,
    aes(
      x = x_seq,
      xend = xend_seq,
      y = Variable,
      yend = Variable,
      color = color_seq
    ),
    size = 2   # Degradation line layer mapping
  ) +
  geom_point(
    data = plot_data_fatty,
    aes(x = Wild, y = Variable),
    color = "#451E07", size = 4 # Dumbell Wild point mapping
  ) +
  geom_point(
    data = plot_data_fatty,
    aes(x = CEA, y = Variable),
    color = "#A1C0C1", size = 4 # Dumbell CEA point mapping
  ) +
  geom_text(
    data = plot_data_fatty,
    aes(x = pmax(Wild, CEA) + 1.5, y = Variable, label = p.adj.signif),
    size = 5 # Stats info mapping
  ) +
  scale_color_identity() +
  labs(
    title = "Distribution of Fatty Acids by Treatment",
    x = "Relative %",
    y = NULL # Title and axis framing
  ) +
  theme_minimal(base_size = 13) +
  annotate(
    "rect",
    xmin = 40, xmax = 50,  # Ajusta según tu escala
    ymin = 18, ymax = 22,  # Ajusta según la posición en el eje Y
    fill = "white",
    color = NA
  ) +
  annotate("text", x = 42, y = 21, label = "Treatment", color = "black", size = 5, hjust = 0) +
  annotate("point", x = 44, y = 20, color = "#451E07", size = 4) + 
  annotate("text", x = 45, y = 20, label = "Wild", color = "black", size = 5, hjust = 0) +
  annotate("point", x = 44, y = 19, color = "#A1C0C1", size = 4) + 
  annotate("text", x = 45, y = 19, label = "CEA", color = "black", size = 5, hjust = 0)) # Custom label to save space 



# -------------------------- #
#    Multivarite  #
# -------------------------- #
## PCA
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
  select(-starts_with("Total"))

# Asegúrate de que solo las columnas numéricas estén incluidas
df_pca <- df %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(.) != 0))

pca_res <- prcomp(df_pca, scale. = TRUE)

# Biplot clásico con colores por tratamiento
fviz_pca_biplot(pca_res,
                geom.ind = "point",
                habillage = df$Treatment,
                addEllipses = TRUE,
                label = "var",
                select.var = list(contrib = 46),
                repel = TRUE,
                arrowsize = 0.1,
                mean.point = FALSE,
                pointsize = 3,
                palette = c("#A1C0C1", "#451E07"),
                col.var = "black",
                col.var.labels = "black")


loadings <- as.data.frame(pca_res$rotation)

# Agregar una columna con el nombre de la variable
loadings$Variable <- rownames(loadings)

# Ordenar por la contribución al PC1 (mayores pesos absolutos)
(loadings_ordered <- loadings %>%
  arrange(desc(abs(PC1))))


(M1 <- ggplot(loadings_ordered, aes(x = reorder(Variable, PC1), y = PC1)) +
  geom_col(fill = ifelse(loadings_ordered$PC1 > 0, "#A1C0C1", "#451E07")) +
  coord_flip() +
  labs(title = "Contribution to Principal\nComponent 1 (84.7%)", x = NULL, y = "Loading") +
  theme_minimal(base_size = 11) +
  annotate(
    "rect",
    xmin = 2, xmax = 8,  # Ajusta según tu escala
    ymin = 0.09, ymax = 0.14,  # Ajusta según la posición en el eje Y
    fill = "white",
    color = NA
  ) +
  annotate("text", x = 7, y = 0.1, label = "Treatment", color = "black", size = 4, hjust = 0) +
  annotate("point", x = 5, y = 0.1, color = "#451E07", size = 3) + 
  annotate("text", x = 5, y = 0.11, label = "Wild", color = "black", size = 4, hjust = 0) +
  annotate("point", x = 3, y = 0.1, color = "#A1C0C1", size = 3) + 
  annotate("text", x = 3, y = 0.11, label = "CEA", color = "black", size = 4, hjust = 0)) # Custom label to save space 



## Random forest
# Treatment as factor
df$Treatment <- as.factor(df$Treatment)

names(df) <- make.names(names(df))

# Remove non numeric variables
vars_numericas <- df %>%
  select(where(is.numeric)) %>%
  names()

# Formula for RF
form <- as.formula(paste("Treatment ~", paste(vars_numericas, collapse = " + ")))

# Random forest model adjustment
set.seed(123)  # for replication
rf_model <- randomForest(form, data = df, importance = TRUE, ntree = 2000)

# Creating df of variables according to PC1
loadings_df <- data.frame(Variable = make.names(rownames(pca_res$rotation)),
                          PC1 = pca_res$rotation[, 1]) %>%
  mutate(Color = ifelse(PC1 > 0, "#A1C0C1", "#451E07"))


# assigning colors acording to PC1
loadings_df <- loadings_df %>%
  mutate(Color = ifelse(PC1 > 0, "#A1C0C1", "#451E07"))


# Variables importance
importancia <- importance(rf_model, type = 1)  # MeanDecreaseAccuracy
importancia_df <- data.frame(Variable = rownames(importancia),
                             Importancia = importancia[,1]) %>%
  arrange(desc(Importancia))

# Left joint prep
importancia_df_colored <- importancia_df %>%
  left_join(loadings_df, by = "Variable")


# Color according to PC1
loadings_df <- loadings_df %>%
  mutate(Color = ifelse(PC1 > 0, "#A1C0C1", "#451E07"))

# Importance plot
(ggplot(importancia_df_colored[1:46,], 
       aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_col(fill = "#ABA3B6") +
  coord_flip() +
  labs(title = "Variables importance based on mean decrease in impurity (Random Forest)",
       x = "Variable", y = "Importance (Mean Decrease Accuracy)") +
  theme_minimal())

(M2 <- ggplot(importancia_df_colored[1:51, ], 
       aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_col(aes(fill = Color)) +
  scale_fill_identity() + 
  coord_flip() +
  labs(title = "Variable Importance by Random Forest\n(colored by PC1 loading sign)",
       x = "Variable", y = "Importance (Mean Decrease Accuracy)") +
  theme_minimal(base_size = 11) +
  annotate(
    "rect",
    xmin = 2, xmax = 8,  
    ymin = 3.5, ymax = 4.5, 
    fill = "white",
    color = NA
  ) +
  annotate("text", x = 7, y = 3.65, label = "Treatment", color = "black", size = 4, hjust = 0) +
  annotate("point", x = 5, y = 3.65, color = "#451E07", size = 3) + 
  annotate("text", x = 5, y = 3.85, label = "Wild", color = "black", size = 4, hjust = 0) +
  annotate("point", x = 3, y = 3.65, color = "#A1C0C1", size = 3) + 
  annotate("text", x = 3, y = 3.85, label = "CEA", color = "black", size = 4, hjust = 0)) # Custom label to save space 


patchwork <- M1 | M2 
patchwork + plot_annotation(
  title = 'Variables with Maximum Contribution on Treatment Characterization',
  tag_levels = 'A')

# Confusion Matrix and metrics
predicciones <- predict(rf_model, type = "response")
install.packages("caret")
library(caret)
confusionMatrix(predicciones, df$Treatment)


# -------------------------- #
#    Final Plots  #
# -------------------------- #
# Nutritional
# Right
(macro_right <- p_sug_ao / p_fatty)

