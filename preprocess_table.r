library("sjmisc")

# produce preprocessed table from raw data
# -> hard-code changes per variable

## EXPORTED FUNCTIONS

preprocess_df <- function(raw_df) {
	print('CHECKING DATASET FOR MISSING VALUES')
	missing_threshold <- 5
	for (var in colnames(raw_df)) {
		var_base_na_perc <- na_prc(raw_df, var)
		if (var_base_na_perc > missing_threshold){
			print('The following variable with higher NA % than threshold')
			print(var)
			print(var_base_na_perc)
			# decide to remove is hit => need to justify!
		}
	}

	# # modifying of variables
	# library('dplyr')
	# cyclism_preprocess <- mutate(cyclism_base, scolar = ifelse(scolar==0, 1, scolar))
	# frq(cyclism_preprocess, scolar)
	# # uncomment to hard-code variable fixes

	return (raw_df)
}

## INTERNAL FUNC

na_prc <- function(dataframe, var_base) {
	f <- frq(dataframe, var_base)
	# print(f[[1]]) # gets dataframe of frq table
	var_base_na_perc <- tail(f[[1]]$raw.prc, n=1)
	return(var_base_na_perc)
}