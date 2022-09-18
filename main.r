source('preprocess_table.r')
source('clean_table.r')

library("readxl")
cyclism_base <- read_excel("cyclismePSY6019_B.xlsx")

print('RAW DATASET:')
cyclism_base
colnames(cyclism_base)

cyclism_preprocess <- preprocess_df(cyclism_base)

print('PROCESSED DATASET:')
cyclism_preprocess
colnames(cyclism_preprocess)

cyclism_clean <- clean_df(cyclism_preprocess)

print('CLEANED DATASET:')
cyclism_clean
colnames(cyclism_clean)