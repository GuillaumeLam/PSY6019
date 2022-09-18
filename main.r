# load data
library("readxl")
cyclism_base <- read_excel("cyclismePSY6019_B.xlsx") 
cyclism_base


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
cyclism_cleaned <- mutate(cyclism_base, scolar = ifelse(scolar==0, 1, scolar))
frq(cyclism_cleaned, scolar)


## Step 2: create variables
class(cyclism_base$expert)
# 2.a:
# lapply(cyclism_base, class) # shows that all variable cols are already numeric

# 2.b:
recoded_var <- function(var, scale_max) {
	return((scale_max+1)-var)
}

scale_max <- 5
cyclism_cleaned$opti3 <- recoded_var(cyclism_cleaned$ropti3, scale_max)

frq(cyclism_cleaned, ropti3)
frq(cyclism_cleaned, opti3)

# `cyclism_cleaned$opti3` equiv to `cyclism_cleaned[['opti3']]`
# `frq(cyclism_cleaned, opti3)` equiv to `frq(cyclism_cleaned, 'opti3')`

# 2.c:
var_base <- 'opti'

op_var <- function(var_base, dataset, scale_max, op) {
	temp_table <- dataset

	var_base_subset <- grep(var_base, colnames(dataset), value=TRUE)

	for(var in var_base_subset) {
		var_no_num <- substring(var,1,nchar(var)-1)
		var_base_with_r <- paste('r',var_base,sep="")
		print(var_no_num) 
		print(var_base_with_r)
		if(var_no_num==var_base_with_r) {
			temp_table[[substring(var,2)]] <- recoded_var(dataset[[var]], scale_max)
		}
	}

	all_recoded <- grep(var_base, colnames(temp_table), value=TRUE)
	all_recoded
	# keep only cols in which the first letter matches with var_base
}
print('HERE')
op_var(var_base, cyclism_base, scale_max, 4)