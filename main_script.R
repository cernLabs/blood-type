setwd("~/Desktop/githubbahubba/blood-type")


# read the data
blood = read.csv("raw_info.csv", head = T)
# raw info was extracted directly from the main research article Canizalez-Román
# , et al. with the assistance of AI chats

# create a dataframe without the confidence intervals
df <- blood %>% select(
  Region,
  State,
  N,
  A_rh_Percent,
  A_Percent,
  B_rh_Percent,
  B_Percent,
  AB_rh_Percent,
  AB_Percent,
  O_rh_Percent,
  O_Percent
)

# Reshape from wide to long format
df_long <- df %>%
  pivot_longer(
    cols = c(  A_rh_Percent,
               A_Percent,
               B_rh_Percent,
               B_Percent,
               AB_rh_Percent,
               AB_Percent,
               O_rh_Percent,
               O_Percent
             ),
    names_to = "BloodType",
    values_to = "Percentage"
  ) %>%
  mutate(
    # Calculate actual counts from percentages
    Count = round(Percentage * N / 100),
    # Clean up blood type names - rh = positive, otherwise negative
    BloodType = gsub("A_rh_Percent", "A+", BloodType),
    BloodType = gsub("A_Percent", "A-", BloodType),
    BloodType = gsub("B_rh_Percent", "B+", BloodType),
    BloodType = gsub("B_Percent", "B-", BloodType),
    
    BloodType = gsub("AB_rh_Percent", "AB+", BloodType),
    BloodType = gsub("AB_Percent", "AB-", BloodType),
    BloodType = gsub("O_rh_Percent", "O+", BloodType),
    BloodType = gsub("O_Percent", "O-", BloodType),
    
    # Extract base blood type (A, B, AB, O) for aggregation option
    BaseBloodType = str_replace(BloodType, "[+-]", "")
  )
    
    
    
    # Clean up blood type names - rh = positive, otherwise negative
    
    BloodType = case_when(
      str_detect(BloodType, "A_rh_Percent") ~ "A+",
      str_detect(BloodType, "A_Percent") ~ "A-",
      str_detect(BloodType, "B_rh_Percent") ~ "B+",
      str_detect(BloodType, "B_Percent") ~ "B-",
      str_detect(BloodType, "AB_rh_Percent") ~ "AB+",
      str_detect(BloodType, "AB_Percent") ~ "AB-",
      str_detect(BloodType, "O_rh_Percent") ~ "O+",
      str_detect(BloodType, "O_Percent") ~ "O-",
      TRUE ~ BloodType
    ),
    # Extract base blood type (A, B, AB, O) for aggregation option
    BaseBloodType = str_replace(BloodType, "[+-]", "")
  )

