table1_make  <-  function (dest, bites_data, diet_data, intestine_data) {
	x  <-  bites_data %>% 
		   dplyr::group_by(local) %>%
		   dplyr::summarise(sp = unique(spp), depth = max(depth), temp = mean(temperature), bite = n())
	y  <-  diet_data %>%
		   dplyr::group_by(local) %>%
		   dplyr::summarise(diet = n())
	z  <-  intestine_data %>%
	       dplyr::group_by(local) %>%
	       dplyr::summarise(intestine = n())
	table1  <-  dplyr::left_join(dplyr::left_join(x, y), z)
	write.csv(table1, dest, row.names = TRUE)
	table1
}

soi_table_make  <-  function (dest, model) {
	coefs  <-  brms::posterior_summary(model)
	coefs  <-  coefs[1:(which(rownames(coefs) == 'sigma') - 1), c('Estimate', 'Q2.5', 'Q97.5')] %>%
			   round(2)
	write.csv(coefs, dest, row.names = TRUE)
	coefs
}
