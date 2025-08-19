# Load necessary packages
library(tidyverse)
library(readxl) 
library(writexl)
library(dplyr)
library(tidyr)

setwd("C:/Users/ritar/Ambiente de Trabalho/Project")

# 1. Upload your Excel dataset
# Replace with the actual file name and sheet name if needed
data <- read_excel("final.xlsx")  


#Create table with Bird frequencies per site per week
frequency_table <- data %>%
  group_by(street_id, Week, species) %>%
  summarise(frequency = n(), .groups = "drop")

write_xlsx(frequency_table, "frequency_per_site_week.csv")


#Species richness per site per week

richness_table_siteWeek <- data %>%
  group_by(street_id, Week) %>%
  summarise(richness = n_distinct(species), .groups = "drop")
View(richness_table_siteWeek)

write_xlsx(richness_table_siteWeek, "richness_per_site_week.csv")



#Community composition (presence/absence matrix)


# Create a presence/absence matrix
presence_absence <- data %>%
  group_by(street_id, Week, species) %>%
  summarise(present = 1, .groups = "drop") %>%  # mark all species detections as present
  pivot_wider(names_from = species, values_from = present, values_fill = 0)

write_xlsx(presence_absence, "presence_absence.csv")


#Upload files for final data set, and bird activity, species richness and community composition tables

data <- read_excel("final.xlsx")  
frequency <- read_excel("frequency_per_site_week.csv")
richness <- read_excel("richness_per_site_week.csv")
presence_absence <- read_excel("presence_absence.csv")


#Metadata table
metadata <- read_excel("metadata.xlsx")
View(metadata)






# Hypothesis 1: Does Streetlight Intensity Affect Bird Communities?

library(lme4)
library(lmerTest)  # for p-values
library(ggplot2)
library(glmmTMB)
library(DHARMa)


#GLMM on bird Activity (frequency)

model_activity_nb <- glmmTMB(
  frequency ~ Week + (1 | street_id),
  data = frequency,
  family = nbinom2
)
summary(model_activity_nb)


#Three models: selection

# Poisson GLMM
model_activity_h1_poisson <- glmmTMB(frequency ~ Week + (1 | street_id), family = poisson, data=frequency)

# Negative Binomial (quadratic mean-variance: nbinom2) - SELECTED
model_activity_nb <- glmmTMB(frequency ~ Week + (1 | street_id), family = nbinom2, data=frequency)

# Negative Binomial (linear mean-variance: nbinom1)
model_activity_h1_nb1 <- glmmTMB(frequency ~ Week + (1 | street_id), family = nbinom1, data=frequency)

#Compare the 3 models
AIC(model_activity_h1_poisson, model_activity_h1_nb2, model_activity_h1_nb1)

#Overdispersion test wit DHarma
overdispersionactivity <- simulateResiduals(model_activity_nb)
testDispersion(overdispersionactivity)






#pairwise comparison 
library(multcomp)

# Tukey post-hoc comparisons for Week
week_comparisons <- glht(model_activity_nb, linfct = mcp(Week = "Tukey"))

# Summary with adjusted p-values
summary(week_comparisons)

# Optional: compact letter display to see which weeks differ
library(multcompView)
cld(week_comparisons)






#Species richness
#GLMM on species richness (frequency)
model_richness_h1_pois <- glmer(richness ~ Week + (1|street_id), 
                                data = richness, 
                                family = poisson)
summary(model_richness_h1_pois)


#Three models: selection
# Poisson Model (selected)
model_richness_h1_pois <- glmer(richness ~ Week + (1|street_id), 
                                data = richness, 
                                family = poisson)
summary(model_richness_h1_pois)
# Negative Binomial Model 1 (default dispersion)
model_richness_h1_nb2 <- glmer.nb(richness ~ Week + (1|street_id), 
                                 data=richness, family = nbinom2, )

# Negative Binomial Model 2 (specific dispersion parameter for each observation)
model_richness_h1_nb1 <- glmer.nb(richness ~ Week + (1|street_id), 
                                  data = richness, family=nbinom1)

# Compare AIC values
AIC(model_richness_h1_pois,model_richness_h1_nb1, model_richness_h1_nb2)

#Overdispersion test wit DHarma
sim_res <- simulateResiduals(model_richness_h1_pois)
testDispersion(sim_res)





#Community composition - creating abundance matrix

library(dplyr)
library(tidyr)

frequencynew <- frequency %>%
  mutate(sample_id = paste(street_id, Week, sep = "_"))

abundance_matrix <- frequencynew %>%
  dplyr::select(sample_id, species, frequency) %>%
  tidyr::pivot_wider(names_from = species, values_from = frequency, values_fill = 0)

View(abundance_matrix)

# Create sample_id - apagar?
frequencynew <- frequency %>%
  mutate(sample_id = paste(street_id, Week, sep = "_"))



#PERMANOVA for community composition
library(vegan)

# Remove specific columns by name
abundance_matrixx <- abundance_matrix[, !(colnames(abundance_matrix) %in% c("sample_id"))]
View(abundance_matrixx)


# Light intensity as group
adonis2(abundance_matrixx ~ light_intensity, metadata, method = "bray", permutations = 999)






#Hypothesis 2

library(lmerTest)
library(lme4)


# GLMM for bird activity

model_activity_2 <- glmmTMB(
  frequency ~ Week * street_type + 
    (1 | species) + (1 | street_id),
  data = frequency,
  family = nbinom2
)
summary(model_activity_2)


#three models: selection

# Poisson GLMM
model_activity_h2_poisson <- glmmTMB(frequency ~ Week * street_type + 
                                       (1 | species) + (1 | street_id), family = poisson, data=frequency)

# Negative Binomial (quadratic mean-variance: nbinom2) (Selected)
model_activity_2 <- glmmTMB(frequency ~ Week * street_type + 
                                   (1 | species) + (1 | street_id), family = nbinom2, data=frequency)

# Negative Binomial (linear mean-variance: nbinom1)
model_activity_h2_nb1 <- glmmTMB(frequency ~ Week * street_type + 
                                   (1 | species) + (1 | street_id), family = nbinom1, data=frequency)

#Compare the 3 models
AIC(model_activity_h2_poisson, model_activity_2, model_activity_h2_nb1)

#Overdispersion test wit DHarma
overdispersionactivity2 <- simulateResiduals(model_activity_2)
testDispersion(overdispersionactivity2)




#GLMM for species richness (H2)
model_richness_h2_pois <- glmmTMB(
  richness ~ Week * street_type + (1 | street_id),
  family = poisson, data=richness
)
summary(model_richness_h2_pois)


#three models: selection

# Poisson GLMM (Selected)
model_richness_h2_pois <- glmmTMB(
  richness ~ Week * street_type + (1 | street_id),
  family = poisson, data=richness
)

# Negative Binomial 2 GLMM
model_richness_h2_nb2 <- glmmTMB(
  richness ~ Week * street_type + (1 | street_id),
  family = nbinom2, data=richness
)

# Negative Binomial 1 GLMM
model_richness_h2_nb1 <- glmmTMB(
  richness ~ Week * street_type + (1 | street_id),
  family = nbinom1, data=richness
)

AIC(model_richness_h2_pois, model_richness_h2_nb2, model_richness_h2_nb1)

#Overdispersion test wit DHarma
overdispersionrichness2 <- simulateResiduals(model_richness_h2_pois)
testDispersion(overdispersionrichness2)



#Community composition


#Permanova - creating abundance matrix
frequencynew <- frequency %>%
  mutate(sample_id = paste(street_id, Week, sep = "_"))


abundance_matrix <- frequencynew %>%
  dplyr::select(sample_id, species, frequency) %>%
  tidyr::pivot_wider(names_from = species, values_from = frequency, values_fill = 0)


# Remove specific columns by name
species_matrixx <- abundance_matrix[, !(colnames(abundance_matrix) %in% c("sample_id"))]

# Compute Bray-Curtis distances
dist_matrix <- vegdist(species_matrixx, method = "bray")

#Permanova individual effects
adonis2(dist_matrix ~ Week + street_type + Week:street_type, data = metadata, by = "term")



# PCoA on the Bray–Curtis distance matrix - Figure 6

pcoa_result <- cmdscale(dist_matrix, eig = TRUE, k = 2)  # k = number of axes

# Eigenvalues from cmdscale
eig_vals <- pcoa_result$eig

# Percent variation for each axis
var_expl <- round(100 * eig_vals / sum(eig_vals), 1)



library(ggplot2)
library(patchwork)

# Common minimal theme tweak for both plots
my_minimal_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),     # remove major grid lines
    panel.grid.minor = element_blank(),     # remove minor grid lines
    axis.ticks = element_blank(),            # remove axis ticks
    axis.text = element_text(color = "grey30"),  # soften axis text color
    legend.background = element_blank(),    # clean legend background
    legend.key = element_blank(),           # remove legend keys background
    legend.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# Light intensity plot (p1)
p1 <- ggplot(pcoa_df, aes(x = Axis1, y = Axis2)) +
  stat_ellipse(aes(group = LightGroup, fill = LightGroup),
               geom = "polygon", alpha = 0.15, colour = NA) +   # fainter fill
  geom_point(aes(colour = LightGroup), size = 3, alpha = 0.7) +
  scale_colour_manual(values = light_colors) +
  scale_fill_manual(values = light_colors) +
  coord_equal() +
  labs(
    title = "A",
    colour = "Week",
    fill = "Week",
    x = paste0("PCoA1 (", var_expl[1], "%)"),
    y = paste0("PCoA2 (", var_expl[2], "%)")
  ) +
  my_minimal_theme

# Street type plot (p2)
p2 <- ggplot(pcoa_df, aes(x = Axis1, y = Axis2)) +
  geom_point(aes(colour = StreetType, shape = StreetType), size = 3, alpha = 0.7) +
  stat_ellipse(aes(group = StreetType, fill = StreetType), 
               geom = "polygon", alpha = 0.15, colour = NA) +    # fainter fill
  scale_colour_manual(values = street_colors) +
  scale_fill_manual(values = street_colors) +
  scale_shape_manual(values = c(16, 17)) +
  coord_equal() +
  labs(
    title = "B",
    colour = "Street type",
    fill = "Street type",
    shape = "Street type",
    x = paste0("PCoA1 (", var_expl[1], "%)"),
    y = paste0("PCoA2 (", var_expl[2], "%)")
  ) +
  my_minimal_theme

# Combine plots side by side
p1 + p2 + plot_layout(ncol = 2)




#Ensuring Little Britain's data used for analysis is not significantly different than the same data excluding week4
  
frequencylb <- read.csv("frequency_per_site_week_lb.csv")
richnesslb <- read.csv("richness_per_site_week_lb.csv")

View(richnesslb)

#Comparison of both models on Activity
model_full <- model_activity_nb <- glmmTMB(
  frequency ~ Week + (1 | street_id),
  data = frequency,
  family = nbinom2
)

model_restricted <- model_activity_nb <- glmmTMB(
  frequency ~ Week + (1 | street_id),
  data = frequencylb,
  family = nbinom2
)

summary(model_full)
summary(model_restricted)

confint(model_full)
confint(model_restricted)



#Comparison of both models on species richness
model_full<-model_richness_h1_pois <- glmer(richness ~ Week + (1|street_id), 
                                data = richness, 
                                family = poisson)

model_restricted <- glmer(richness ~ Week + (1|street_id), 
                                data = richnesslb, 
                                family = poisson)

summary(model_full)
summary(model_restricted)

confint(model_full)
confint(model_restricted)

