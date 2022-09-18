# load data
library("readxl")
cyclism_base <- read_excel("cyclismePSY6019_B.xlsx") 
cyclism_base

colnames(cyclism_base)

## Step 0: check variable type
class(cyclism_base$sexe) # already numeric
class(cyclism_base$aut1)


## Step 1: freq tables
library("sjmisc")
frq(cyclism_base, sexe)
frq(cyclism_base, scolar) # 0 but no 1 => invalid
frq(cyclism_base, coutvelo)

# modifying of variables
library('dplyr')
cyclism_preprocess <- mutate(cyclism_base, scolar = ifelse(scolar==0, 1, scolar))
frq(cyclism_preprocess, scolar)


## Step 2: create variables
class(cyclism_base$expert)
# 2.a:
# lapply(cyclism_base, class) # shows that all variable cols are already numeric

# 2.b:
recoded_var <- function(var, scale_max) {
	return((scale_max+1)-var)
}

scale_max <- 5
cyclism_preprocess$opti3 <- recoded_var(cyclism_preprocess$ropti3, scale_max)

frq(cyclism_preprocess, ropti3)
frq(cyclism_preprocess, opti3)

# `cyclism_preprocess$opti3` equiv to `cyclism_preprocess[['opti3']]`
# `frq(cyclism_preprocess, opti3)` equiv to `frq(cyclism_preprocess, 'opti3')`

# 2.c:
var_sub_df <- function(var_base, dataset, scale_max) {
	var_base_subset <- grep(var_base, colnames(dataset), value=TRUE)
	sub_table <- dataset[c(var_base_subset)]

	for(var in var_base_subset) {
		var_no_num <- substring(var,1,nchar(var)-1)
		var_base_with_r <- paste('r',var_base,sep="")
		if(var_no_num==var_base_with_r) {
			sub_table[[substring(var,2)]] <- recoded_var(dataset[[var]], scale_max)
			sub_table <- sub_table %>% select(-contains(var))
		}
	}

	return(sub_table)
}

base_variables <- c('sexe','scolar')
cyclism_clean <- cyclism_preprocess[base_variables]

cyclism_clean

# repeat following operations with various var_base eg. opti, psotvi, etc. to get all sub columns (and recoded if necessary) 
var_base <- 'opti'
opti_df <- var_sub_df(var_base, cyclism_base, scale_max)

cyclism_clean[[var_base]] <- row_means(opti_df,n=.75,var=var_base,append=FALSE)
cyclism_clean[[var_base]] <- as.numeric(unlist(cyclism_clean[[var_base]]))

cyclism_clean

# 2.d:
# library(labelled)
# var_label(cyclism_clean[['var1']]) <- 'new_var_name'

## Step 3: Retrieve missing data %
na_perc <- function(dataframe, var_base) {
	f <- frq(dataframe, var_base)
	# print(f[[1]]) # gets dataframe of frq table
	var_base_na_perc <- tail(f[[1]]$raw.prc, n=1)
	return(var_base_na_perc)
}

var_base <- 'coutvelo'
var_base_na_perc <- na_perc(cyclism_base, var_base)
var_base_na_perc

# frq(cyclism_base, 'abc')

print('CHECKING DATASET FOR MISSING VALUES')
missing_threshold <- 5
for (var in colnames(cyclism_base)) {
	var_base_na_perc <- na_perc(cyclism_base, var)
	if (var_base_na_perc > missing_threshold){
		print('following variable with higher NA % than threshold')
		print(var)
		print(var_base_na_perc)
	}
}

## Step 4: Retrieve variable distribution

var_dist <- function(dataframe, var_base) {
	f <- frq(dataframe, var_base)
	var_dist_list <- head(f[[1]]$valid.prc, -1)
	return(var_dist_list)
}

var_dist(cyclism_base, 'sexe')