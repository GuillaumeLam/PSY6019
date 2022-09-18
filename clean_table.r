library('dplyr')
library("sjmisc")

# file for producing cleaned table from preprocessed table
# input: (variables wanted, operations on multi-variables)

# todo: 
# - scan all variables
# - split variables into base & multi
# - add base variables 
# - cleanup multi variables & find scale max

# dict of scale max's?

## EXPORTED FUNCTIONS

# base_variables <- c('id','sexe','scolar')
multi_variables_scale <- c(
	'opti'=5, 
	'postvi'=7,
	'defi'=7,
	'pilot'=7,
	'buts'=7,
	'retro'=7,
	'conc'=7,
	'contro'=7,
	'fusion'=7,
	'temps'=7,
	'auto'=7
)

clean_df <- function(preprocessed_df) {
	base_variables <- grep("^[a-zA-Z]+$", colnames(preprocessed_df), value=TRUE)

	cleaned_df <- preprocessed_df[c(base_variables)]

	for (mv in names(multi_variables_scale)) {
		mv_df <- var_sub_df(mv, preprocessed_df, multi_variables_scale[mv])

		mv_m <- paste(mv,'_mean',sep="")
		cleaned_df[[mv_m]] <- row_means(mv_df,n=.75,var=mv_m,append=FALSE)
		cleaned_df[[mv_m]] <- as.numeric(unlist(cleaned_df[[mv_m]]))

	}

	return(cleaned_df)
} 

## INTERNAL FUNC

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

recoded_var <- function(var, scale_max) {
	return((scale_max+1)-var)
}