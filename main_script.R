setwd("~/Desktop/githubbahubba/blood-type")

###### IS: cleaning column names ######
##########################################
source("blood_work_sccript_colname_cleaning.R")
##########################################
##########################################

colnames(data)

# Load cleaned data
df <- read.csv("blood_types.csv", header = TRUE)

# Reshape from wide to long format
df_long <- df %>%
  select(Region, State, N, 
         `A..Rh.D..`, `A..Rh.d..`, 
         `B..Rh.D..`, `B..Rh.d..`, 
         `AB..Rh.D..`, 
         `O..Rh.D..`, `O..Rh.d..`) %>%
  pivot_longer(
    cols = c(`A..Rh.D..`, `A..Rh.d..`, `B..Rh.D..`, `B..Rh.d..`, 
             `AB..Rh.D..`, `O..Rh.D..`, `O..Rh.d..`),
    names_to = "BloodType",
    values_to = "Percentage"
  ) %>%
  mutate(
    # Calculate actual counts from percentages
    Count = round(Percentage * N / 100),
    # Clean up blood type names - Rh.D. = positive, Rh.d. = negative
    BloodType = case_when(
      str_detect(BloodType, "A\\.\\.Rh\\.D\\.") ~ "A+",
      str_detect(BloodType, "A\\.\\.Rh\\.d\\.") ~ "A-",
      str_detect(BloodType, "B\\.\\.Rh\\.D\\.") ~ "B+",
      str_detect(BloodType, "B\\.\\.Rh\\.d\\.") ~ "B-",
      str_detect(BloodType, "AB\\.\\.Rh\\.D\\.") ~ "AB+",
      str_detect(BloodType, "O\\.\\.Rh\\.D\\.") ~ "O+",
      str_detect(BloodType, "O\\.\\.Rh\\.d\\.") ~ "O-",
      TRUE ~ BloodType
    ),
    # Extract base blood type (A, B, AB, O) for aggregation option
    BaseBloodType = str_replace(BloodType, "[+-]", "")
  )

## make new data set with out the Confidence Interval columns
df_blood <- data[grep("CI",colnames(data),invert = T)]

## Summon dplyr
library(dplyr)
for(i in 4:12){plot((density(df_blood[,i])),main = colnames(df_blood)[i])
}
