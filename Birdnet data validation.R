
library(readxl)
library(dplyr)
library(purrr)
library(writexl)

# Set working directory
setwd("C:/Users/ritar/Ambiente de Trabalho/Project")

# Replace with your actual folder path
file_list <- list.files("C:/Users/ritar/Ambiente de Trabalho/Project/Data", pattern = "\\.xlsx$", full.names = TRUE)

# Combine all CSVs into one dataframe
birdnet_data <- map_dfr(file_list, ~ read_excel(.x))

# Rename columns for easier coding
birdnet_data <- birdnet_data %>%
  rename(
    species = "Common Name",
    confidence = Confidence
  )

View(birdnet_data)

#only count one individual of the same species per recording
unique_calls <- birdnet_data %>%
  distinct("Begin Path", species)

num_unique_species <- length(unique(unique_calls$species))

print(num_unique_species)

#bird net data only count one individual of the same species per recording
birdnetdata <- birdnet_data %>%
  distinct(`Begin Path`, species, .keep_all = TRUE)

View(birdnetdata)



#Random selection for validation

sample_species <- function(df, species_name) {
  species_df <- df %>% filter(species == species_name)
  n_obs <- nrow(species_df)
  
  # Create strata
  high <- species_df %>% filter(confidence >= 0.9)
  mid  <- species_df %>% filter(confidence >= 0.85 & confidence < 0.9)
  low  <- species_df %>% filter(confidence >= 0.8 & confidence < 0.85)
  
  # Sample from each stratum
  set.seed(42)  # For reproducibility
  high_sample <- high %>% slice_sample(n = min(10, nrow(high)))
  mid_sample  <- mid  %>% slice_sample(n = min(6, nrow(mid)))
  low_sample  <- low  %>% slice_sample(n = min(4, nrow(low)))
  
  # Combine samples
  bind_rows(high_sample, mid_sample, low_sample) %>%
    mutate(stratum = case_when(
      confidence >= 0.9 ~ "high",
      confidence >= 0.85  ~ "mid",
      TRUE          ~ "low"
    ))
}

validation_samples <- birdnetdata %>%
  group_by(species) %>%
  filter(n() >= 15) %>%
  group_split(species) %>%
  map_dfr(~ sample_species(.x, unique(.x$species)))


View(validation_samples)

#Saving the excel table with the selected recordings
write_xlsx(validation_samples, "validatingeBirdData.xlsx")


# after validation, saving excel table with validation scores (True positive or False positive)

validated_data <- read_excel("C:/Users/ritar/Ambiente de Trabalho/Project/validatingeBirdData.xlsx")


#calculating precision

library(dplyr)

validated_data <- validated_data %>%
  mutate(
    TP = ifelse(Correct == "y", 1, 0),
    FP = ifelse(Correct != "y", 1, 0)
  )

precision_by_species <- validated_data %>%
  group_by(species) %>%
  summarise(
    n = n(),
    TP = sum(TP),
    FP = sum(FP),
    precision = round(TP / (TP + FP), 3)
  ) %>%
  arrange(desc(precision))

View(precision_by_species)


#filter species with more than 0,90 precision

species_over_90 <- precision_by_species %>%
  filter(precision >= 0.90) %>%
  pull(species)


#Keep all recordings from the most accurate species
validated_species<- c("Black Redstart", "Eurasian Wren", "Gray Wagtail", "Lesser Black-backed Gull", "Herring Gull", "Eurasian Blue Tit", "Carrion Crow", "Eurasian Blackbird", "European Robin")


finaldataset <- birdnetdata[birdnetdata$species %in% validated_species, ]

View(finaldataset)



#Manually adding street id and types to final data set

final <- read_excel("C:/Users/ritar/Ambiente de Trabalho/Project/final.xlsx")

View(final)

