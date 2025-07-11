###### IS: cleaning column names ######
##########################################
source("blood_work_sccript_colname_cleaning.R")
##########################################
##########################################

colnames(data)


## make new data set with out the Confidence Interval columns
df_blood <- data[grep("CI",colnames(data),invert = T)]

## Summon dplyr
library(dplyr)
for(i in 4:12){plot((density(df_blood[,i])),main = colnames(df_blood)[i])
}
