make_S_table  <-  function (dest, model) {
	coefs  <-  brms::posterior_summary(model)
	coefs  <-  coefs[1:(which(rownames(coefs) == 'sigma') - 1), c('Estimate', 'Q2.5', 'Q97.5')] %>%
			   round(2)
	write.csv(coefs, dest, row.names = TRUE)
	coefs
}
