# Load necessary packages
library(tidyverse)
library(readxl) 
library(writexl)# For reading Excel files

setwd("C:/Users/ritar/Ambiente de Trabalho/Project")

#Upload files for final data set, and bird activity, species richness and community composition tables

data <- read_excel("final.xlsx")  
frequency <- read_excel("frequency_per_site_week.csv")
richness <- read_excel("richness_per_site_week.csv")
presence_absence <- read_excel("presence_absence.csv")

#Metadata table
metadata <- read_excel("metadata.xlsx")



#Plots
library(dplyr)
library(ggplot2)



#Bird Activity Figures

# Count recordings per week and street type
summary_table <- final %>%
  group_by(Week, street_type) %>%
  summarise(n_recordings = n(), .groups = "drop")
View(summary_table)


#compare wooded and non wooded (t test)
t.test(n_recordings ~ street_type, summary_table)


#Descritpive statistical analysis
summary_stats <- summary_table %>%
  group_by(street_type) %>%
  summarise(
    mean_recordings = mean(n_recordings),
    sd_recordings   = sd(n_recordings),
    median_recordings = median(n_recordings),
    min_recordings  = min(n_recordings),
    max_recordings  = max(n_recordings),
    .groups = "drop"
  )




#Figure 2
summary_table_plot <- summary_table %>%
  group_by(Week, street_type) %>%
  summarise(total_obs = sum(n_recordings), .groups = "drop")

# Define colors
bar_colors <- c("non-wooded" = "#D2B48C", "wooded" = "#4682B4")

# Create the bar plot
ggplot(summary_table_plot, aes(x = factor(Week), y = total_obs, fill = street_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = bar_colors) +
  labs(
    x = "Week",
    y = "Number of Observations",
    fill = "Street Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85"),
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "top"
  )




#Figure 3

library(dplyr)

#Table with counts for each street and species
streettypes <- read_excel("streets.xlsx")

freq_table <- frequency %>%
  left_join(streettypes, by = "street_id")

# Calculate weekly mean frequency, grouped by species and street type
avg_freq_week <- freq_table %>%
  group_by(species, Week) %>%
  summarise(mean_freq = mean(frequency, na.rm = TRUE)) %>%
  ungroup()


# Extract baseline (week 1) frequencies for each species and street type
baseline <- avg_freq_week %>%
  filter(Week == 1) %>%
  select(species, baseline_freq = mean_freq)

# Join the baseline data with the original dataset and calculate the percentage change
freq_pct_change <- avg_freq_week %>%
  left_join(baseline, by = c("species")) %>%
  mutate(pct_change = ((mean_freq - baseline_freq) / baseline_freq) * 100)

#Plot
library(RColorBrewer)

soft9 <- c(
  "#66c2a5",  # teal
  "#fc8d62",  # salmon
  "#8da0cb",  # blue-purple
  "#e78ac3",  # pink
  "#a6d854",  # light green
  "#ffd92f",  # yellow
  "#e5c494",  # beige
  "#b3b3b3",  # gray
  "#a6cee3"   # sky blue
)


ggplot(freq_pct_change, aes(x = as.factor(Week), y = pct_change, group = species, color = species)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = soft9) +
  labs(
    x = "Week",
    y = "Change in Average Observations (%) from week 1",
    color = "Species Common Name"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.line = element_line(color = "black")
  )



#Figure 4
#First merge street types table with frequency table

streettypes <- read_excel("streets.xlsx")

freq_table <- frequency %>%
  left_join(streettypes, by = "street_id")


library(dplyr)
library(ggplot2)

# Calculate mean recordings per week by street type
mean_freq_by_week_type <- freq_table %>%
  group_by(street_type.x, Week) %>%
  summarise(mean_recordings = mean(frequency), .groups = "drop")

# Calculate % change from week 1, by street type
mean_freq_by_week_type <- mean_freq_by_week_type %>%
  group_by(street_type.x) %>%
  mutate(pct_change = 100 * (mean_recordings - first(mean_recordings)) / first(mean_recordings))

ggplot(mean_freq_by_week_type, 
       aes(x = as.factor(Week), y = pct_change, 
           color = street_type.x, group = street_type.x)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Week",
    y = "Change in Average Observations (%) from week 1",
    color = "Street Type"
  ) +
  scale_color_manual(values = c("wooded" = "#1b9e77", "non-wooded" = "#d95f02")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.key = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(family = "sans", size = 12)
  )


#Species richness 

# Define wooded and non-wooded street names
wooded_streets <- c("Angel lane", "College street", "Huggin hill", "Friday street", 
                    "Little Britain", "Aldermanbury street", "Basinghall avenue")

non_wooded_streets <- c("Cloak lane", "Garlick hill", "Distaff lane", "Swan lane", 
                        "Noble street", "Wood street", "Paternoster row", "Gutter lane")

# Add StreetType column
richnessstreets <- richness %>%
  mutate(street_type = case_when(
    street_id %in% wooded_streets ~ "Wooded",
    street_id %in% non_wooded_streets ~ "Non-wooded",
    TRUE ~ NA_character_
  ))

#Comparison of species richness between street types
t.test(richness ~ street_type, richnessstreets)


#Descriptive statistical analysis
summary_stats_richness <- richnessstreets %>%
  group_by(street_type) %>%
  summarise(
    mean_richness = mean(richness),
    sd_richness   = sd(richness),
    median_richness = median(richness),
    min_richness  = min(richness),
    max_richness  = max(richness),
    .groups = "drop"
  )

View(summary_stats_richness)



#Figure 5

ggplot(richnessstreets, aes(x = factor(Week), y = richness, fill = street_type)) +
  geom_boxplot(position = position_dodge(0.8), na.rm = TRUE, outlier.size = 1.5, width = 0.6) +
  labs(
    title = "Species Richness by Week and Tree Cover",
    y = "Species Richness",
    x = "Week",
    fill = "Street Type"
  ) +
  scale_fill_manual(values = c("Wooded" = "#4C9F70", "Non-wooded" = "#D9B44A")) +  # Softer greens/golds
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )




#Community composition plot
#Figure 7

soft9 <- c(
  "#66c2a5",  # teal
  "#fc8d62",  # salmon
  "#8da0cb",  # blue-purple
  "#e78ac3",  # pink
  "#a6d854",  # light green
  "#ffd92f",  # yellow
  "#e5c494",  # beige
  "#b3b3b3",  # gray
  "#a6cee3"   # sky blue
)

ggplot(species_week_pct, aes(x = as.factor(Week), y = percentage, fill = species)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ street_type.x) +
  labs(
    x = "Week",
    y = "Percentage of observations (%)",
    fill = "Species"
  ) +
  scale_fill_manual(values = soft9) + # Softer colors
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.key = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(family = "sans", size = 12)
  )






