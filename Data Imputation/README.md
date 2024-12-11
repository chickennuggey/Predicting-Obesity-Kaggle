Data imputation was performed 5 different ways in order to produce two datasets each, one for training and one for testing: 
* amelia.csv: imputed using Amelia library
* mean_mode.csv: numerics were imputed with mean, categoricals were imputed with mode
* mean_prop.csv: numerics were imputed with mean, categoricals were imputed randomly with existing class proportions
* median_prop.csv: numerics were imputed with median, categoricals were imputed randomly with existing class proportions
* mice_prop.csv: numerics were imputed with mice library, categoricals were imputed randomly with existing class proportions
