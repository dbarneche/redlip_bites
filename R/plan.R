plan  <-  drake::drake_plan(
	cols = c('ophioblennius_spST' = 'tomato', 'ophioblennius_trinitatis' = 'dodgerblue2', 'ophioblennius_sp' = 'darkseagreen3', 'ophioblennius_macclurei' = 'goldenrod2'),
    shps = c('ophioblennius_spST' = 21, 'ophioblennius_trinitatis' = 24, 'ophioblennius_sp' = 22, 'ophioblennius_macclurei' = 23),
	bites_data = bite_data_make(cols, shps, file_name = file_in('data/bites_data.csv')),
	bites_model = bites_model_run(bites_data),
	mouth_data = mouth_data_make(cols, shps, file_name = 'data/diet_data.csv'),
	mouth_model = mouth_model_run(mouth_data),
	diet_data = diet_data_make(cols, shps, bites_data, file_name = file_in('data/diet_data.csv')),
	gut_content_data = gut_content_data_make(diet_data),
	logratios_data = logratios_data_make(diet_data, bites_data, gut_content_data),
	logratios_model = logratios_model_run(logratios_data),
	intestine_data = intestine_data_make(cols, shps, file_name = file_in('data/diet_data.csv')),
	intestine_model = stats::kruskal.test(intestine_data$local ~ intestine_data$QI),
    # Figures ----------------------------------------------
    fig_out_folder = dir.create('output/figures/', recursive = TRUE, showWarnings = FALSE),
	fig1_pdf = {
		fig_out_folder
		fig1_make(file_out('output/figures/fig1.pdf'), bites_data, bites_model$best)
	},
    fig1_png = pngs_generate(file_in('output/figures/fig1.pdf'), file_out('output/figures/fig1.png')),
	fig2_pdf = {
		fig_out_folder
		fig2_make(file_out('output/figures/fig2.pdf'), mouth_data, mouth_model)
	},
    fig2_png = pngs_generate(file_in('output/figures/fig2.pdf'), file_out('output/figures/fig2.png')),
	fig3_pdf = {
		fig_out_folder
		fig3_make(file_out('output/figures/fig3.pdf'), gut_content_data, diet_data)
	},
    fig3_png = pngs_generate(file_in('output/figures/fig3.pdf'), file_out('output/figures/fig3.png')),
	fig4_pdf = {
		fig_out_folder
		fig4_make(file_out('output/figures/fig4.pdf'), logratios_model$best, bites_model$best, diet_data)
	},
    fig4_png = pngs_generate(file_in('output/figures/fig4.pdf'), file_out('output/figures/fig4.png')),    
    figS1_pdf = {
		fig_out_folder
		figS1to3_make(file_out('output/figures/figS1.pdf'), logratios = FALSE, bites_data, bites_model$best, 'ln_mass_g')
	},
    figS1_png = pngs_generate(file_in('output/figures/figS1.pdf'), file_out('output/figures/figS1.png')),
    figS2_pdf = {
		fig_out_folder
		figS1to3_make(file_out('output/figures/figS2.pdf'), logratios = FALSE, mouth_data, mouth_model, 'ln_mass_g')
	},
    figS2_png = pngs_generate(file_in('output/figures/figS2.pdf'), file_out('output/figures/figS2.png')),
    figS3_pdf = {
		fig_out_folder
		figS1to3_make(file_out('output/figures/figS3.pdf'), logratios = TRUE, logratios_data, logratios_model$best)
	},
    figS3_png = pngs_generate(file_in('output/figures/figS3.pdf'), file_out('output/figures/figS3.png')),
	figS4_pdf = {
		fig_out_folder
		figS4_make(file_out('output/figures/figS4.pdf'), bites_data)
	},
    figS4_png = pngs_generate(file_in('output/figures/figS4.pdf'), file_out('output/figures/figS4.png')),
	figS5_pdf = {
		fig_out_folder
		figS5_make(file_out('output/figures/figS5.pdf'), intestine_data)
	},
    figS5_png = pngs_generate(file_in('output/figures/figS5.pdf'), file_out('output/figures/figS5.png'))
)
