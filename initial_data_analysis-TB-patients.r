
setwd("C:/Users/migle/Desktop/data_mining")

library(tidyverse)
library(ggplot2)

data <- read.csv(
  "C:/Users/migle/Desktop/data_mining/TubPacientoZalingasIprotis.csv",
  stringsAsFactors = FALSE
)

# Check that the data was imported correctly
dim(data)
colnames(data)



# -------------------- DATA CLEANING --------------------

# Inspect validation status
table(data$vda_ak_validacija)

# Keep only valid patient samples
valid_data <- data %>%
  filter(vda_ak_validacija == "Tinkamas")

# Inspect the filtered dataset
dim(valid_data)
glimpse(valid_data)
summary(valid_data)

# Count unique patient identifiers
n_distinct(valid_data$paciento_asmens_kodas, na.rm = TRUE)


# -------------------- CONVERT VARIABLES TO FACTORS --------------------

valid_data <- valid_data %>%
  mutate(
    rukymas_id = factor(rukymas_id),
    alkoholis_id = factor(alkoholis_id),
    rukymo_stazas_id = factor(rukymo_stazas_id)
  )

# Set reference categories

valid_data$rukymas_id <- factor(valid_data$rukymas_id, levels = c("4", "1", "2", "3", "5"))

valid_data$alkoholis_id <- factor(valid_data$alkoholis_id, levels = c("1", "2", "3", "4"))

valid_data$rukymo_stazas_id <- factor(valid_data$rukymo_stazas_id, levels = c("0", "1", "2", "3", "4"))

valid_data <- valid_data %>%
  mutate(
    paciento_lytis = str_trim(as.character(paciento_lytis)),
    
    # Map Lithuanian codes to English codes for later visualisation
    paciento_lytis = case_when(
      paciento_lytis == "M" ~ "F", # Moteris -> Female
      paciento_lytis == "V" ~ "M", # Vyras -> Male
      TRUE ~ NA_character_
    ),
    
    #Convert to factor and set "F" as the reference baseline
    paciento_lytis = factor(paciento_lytis, levels = c("F", "M"))
  )

# Inspect categories
table(valid_data$rukymas_id, useNA = "ifany")
table(valid_data$alkoholis_id, useNA = "ifany")
table(valid_data$rukymo_stazas_id, useNA = "ifany")
table(valid_data$paciento_lytis, useNA = "ifany")


# -------------------- HANDLE MISSING VALUES --------------------


# Inspect missing values in every column
colSums(is.na(valid_data))

#converting the column ar_mire_nuo_tub into logical values and adding NA instead of empty cells
valid_data <- valid_data %>%
  mutate(
    ar_mire_nuo_tub = na_if(ar_mire_nuo_tub, ""),
    ar_mire_nuo_tub = as.logical(ar_mire_nuo_tub)
  )

# Check the converted variable
str(valid_data$ar_mire_nuo_tub)
table(valid_data$ar_mire_nuo_tub, useNA = "ifany")

# Check missing values again
colSums(is.na(valid_data))


# -------------------- CHECK DEATH DATE CONSISTENCY --------------------
# Standardize blank death dates as missing.
#If the column is a Date or POSIX date, preserve its date type.
if (is.character(valid_data$mirties_nuo_tub_d) ||
    is.factor(valid_data$mirties_nuo_tub_d)) {
  
  valid_data$mirties_nuo_tub_d <- as.character(
    valid_data$mirties_nuo_tub_d
  )
  
  valid_data$mirties_nuo_tub_d[
    trimws(valid_data$mirties_nuo_tub_d) == ""
  ] <- NA
}



# Find records marked TRUE but without a death date
missing_death_dates <- valid_data %>%
  filter(
    ar_mire_nuo_tub == TRUE &
      is.na(mirties_nuo_tub_d)
  )

nrow(missing_death_dates)

# Find records marked FALSE but with a death date
contradictions <- valid_data %>%
  filter(
    ar_mire_nuo_tub == FALSE &
      !is.na(mirties_nuo_tub_d)
  )

# Number of contradictions
nrow(contradictions)


# Exclude TRUE records with missing death dates.
# Preserve records with unknown mortality status.
data_without_missing_death_dates <- valid_data %>%
  filter(
    is.na(ar_mire_nuo_tub) |
      !(ar_mire_nuo_tub == TRUE &
          is.na(mirties_nuo_tub_d))
  )
summary(data_without_missing_death_dates)

# Change FALSE to TRUE when a death date is present.
changed_data <- data_without_missing_death_dates %>%
  mutate(
    ar_mire_nuo_tub = case_when(
      ar_mire_nuo_tub == FALSE &
        !is.na(mirties_nuo_tub_d) ~ TRUE,
      TRUE ~ ar_mire_nuo_tub
    )
  )

summary(changed_data)
table(changed_data$ar_mire_nuo_tub, useNA = "ifany")


# -------------------- KEEP UNIQUE PATIENTS --------------------

# Remove patients with multiple records.
# Patients with missing identifiers are excluded because their
# uniqueness cannot be established.
data_with_unique_patient_entries <- changed_data %>%
  filter(!is.na(paciento_asmens_kodas)) %>%
  group_by(paciento_asmens_kodas) %>%
  filter(n() == 1) %>%
  ungroup()

dim(data_with_unique_patient_entries)
summary(data_with_unique_patient_entries)

# Inspect disease codes
table(data_with_unique_patient_entries$ligos_kodas)


# -------------------- GROUP TUBERCULOSIS DISEASE CODES --------------------

data_with_unique_patient_entries <- data_with_unique_patient_entries %>%
  mutate(
    tuberculosis_group = case_when(
      str_detect(
        toupper(trimws(as.character(ligos_kodas))),
        "^A15"
      ) ~ "A15",

      str_detect(
        toupper(trimws(as.character(ligos_kodas))),
        "^A16"
      ) ~ "A16",

      str_detect(
        toupper(trimws(as.character(ligos_kodas))),
        "^A17"
      ) ~ "A17",

      str_detect(
        toupper(trimws(as.character(ligos_kodas))),
        "^A18"
      ) ~ "A18",

      str_detect(
        toupper(trimws(as.character(ligos_kodas))),
        "^A19"
      ) ~ "A19",

      TRUE ~ NA_character_
    )
  )

table(data_with_unique_patient_entries$tuberculosis_group)

# Set A15 as the reference category
data_with_unique_patient_entries <- data_with_unique_patient_entries %>%
  mutate(
    tuberculosis_group = factor(
      tuberculosis_group,
      levels = c("A15", "A16", "A17", "A18", "A19")
    )
  )


# -------------------- VISUALIZATIONS --------------------

# 1. Distribution of tuberculosis diagnose codes
ggplot(
  data_with_unique_patient_entries,
  aes(x = tuberculosis_group)
) +
  geom_bar(fill = "purple") +
  geom_text(
    stat = "count", 
    aes(label = after_stat(count)), 
    vjust = -0.5,   
    size = 3  
  ) +
  labs(
    #title = str_wrap("Distribution of Patients across ICD-10 TB Diagnosis Categories", width = 45),
    x = "ICD-10 diagnosis code for TB",
    y = "Number of patients"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))





# 2. Cigarette consumption scores

ggplot(
  data_with_unique_patient_entries %>%
    filter(!is.na(rukymas_id), rukymas_id != "0"),
  aes(x = rukymas_id)
) +
  geom_bar(fill = "steelblue") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,    
    size = 3             
  ) +
  labs(
    #title = str_wrap("Distribution of Patients across Daily Cigarette Consumption Categories", width = 45),
    x = "Daily cigarette consumption category",
    y = "Number of patients"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# 3. Smoking duration scores

ggplot(
  data_with_unique_patient_entries %>%
    filter(!is.na(rukymo_stazas_id)),
  aes(x = rukymo_stazas_id)
) +
  geom_bar(fill = "pink") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5, 
    size = 3               
  ) + 
  labs(
    #title = str_wrap("Distribution of Patients across Smoking Duration Categories", width = 45),
    x = "Smoking duration category",
    y = "Number of patients"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



# 4. Alcohol consumption scores

ggplot(
  data_with_unique_patient_entries %>%
    filter(!is.na(alkoholis_id), alkoholis_id != "0"),
  aes(x = alkoholis_id)
) +
  geom_bar(fill = "violet") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,            
    size = 3                 
  ) +
  labs(
    #title = str_wrap("Distribution of Patients across Alcohol Consumption Categories", width = 45),
    x = "Alcohol consumption category",
    y = "Number of patients"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))




# 5. Tuberculosis-attributed mortality count

ggplot(
  data_with_unique_patient_entries %>%
    filter(!is.na(ar_mire_nuo_tub)),
  aes(x = ar_mire_nuo_tub, fill = ar_mire_nuo_tub)
) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    #title = str_wrap("Distribution of Patients by Tuberculosis-attributed Mortality", width = 50),
    x = "Tuberculosis-attributed mortality",
    y = "Number of patients"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



# 6. Mortality proportion by cigarette consumption

ggplot(
  data_with_unique_patient_entries %>%
    filter(
      !is.na(ar_mire_nuo_tub),
      !is.na(rukymas_id),
      rukymas_id != "0"
    ),
  aes(x = rukymas_id, fill = ar_mire_nuo_tub)
) +
  geom_bar(position = "fill") +
  labs(
    #title = str_wrap("Tuberculosis Mortality Proportion by Daily Cigarette Consumption", width = 45),
    x = "Daily cigarette consumption category",
    y = "Proportion",
    fill = "Died from tuberculosis"
  ) +
  theme_minimal()


# 7. Mortality proportion by smoking duration

ggplot(
  data_with_unique_patient_entries %>%
    filter(
      !is.na(ar_mire_nuo_tub),
      !is.na(rukymo_stazas_id)
    ),
  aes(x = rukymo_stazas_id, fill = ar_mire_nuo_tub)
) +
  geom_bar(position = "fill") +
  labs(
    #title = str_wrap("Tuberculosis Mortality Proportion by Smoking Duration", width = 42),
    x = "Smoking duration category",
    y = "Proportion",
    fill = "Died from tuberculosis"
  ) +
  theme_minimal()


# 8. Mortality proportion by alcohol consumption

ggplot(
  data_with_unique_patient_entries %>%
    filter(
      !is.na(ar_mire_nuo_tub),
      !is.na(alkoholis_id),
      alkoholis_id != "0"
    ),
  aes(x = alkoholis_id, fill = ar_mire_nuo_tub)
) +
  geom_bar(position = "fill") +
  labs(
    #title = str_wrap("Tuberculosis Mortality Proportion by Alcohol Consumption", width = 45),
    x = "Alcohol consumption category",
    y = "Proportion",
    fill = "Died from tuberculosis"
  ) +
  theme_minimal()


# 9. Mortality proportion by TB diagnosis code

ggplot(
  data_with_unique_patient_entries %>%
    filter(
      !is.na(ar_mire_nuo_tub),
      !is.na(tuberculosis_group)
    ),
  aes(x = tuberculosis_group, fill = ar_mire_nuo_tub)
) +
  geom_bar(position = "fill") +
  labs(
    #title = str_wrap("Tuberculosis Mortality Proportion by ICD-10 TB Diagnosis Category", width = 45),
    x = "ICD-10 TB diagnosis category",
    y = "Proportion",
    fill = "Died from tuberculosis"
  ) +
  theme_minimal()

#10. Distribution of genders
ggplot(
  data_with_unique_patient_entries,
  aes(x = paciento_lytis)
) +
  geom_bar(fill = "darkblue") +
  geom_text(
    stat = "count", 
    aes(label = after_stat(count)), 
    vjust = -0.5,   
    size = 3  
  ) +
  labs(
    #title = str_wrap("Distribution of Patient genders", width = 45),
    x = "Gender",
    y = "Number of patients"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#11.  Mortality proportion by gender

ggplot(
  data_with_unique_patient_entries %>%
    filter(
      !is.na(ar_mire_nuo_tub)
    ),
  aes(x = paciento_lytis, fill = ar_mire_nuo_tub)
) +
  geom_bar(position = "fill") +
  labs(
    #title = str_wrap("Tuberculosis Mortality Proportion accross patient genders", width = 45),
    x = "Gender",
    y = "Proportion",
    fill = "Died from tuberculosis"
  ) +
  theme_minimal()


