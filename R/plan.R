plan  <-  drake::drake_plan(
	cols = c('ophioblennius_spST' = 'tomato', 'ophioblennius_trinitatis' = 'dodgerblue2', 'ophioblennius_sp' = 'darkseagreen3', 'ophioblennius_macclurei' = 'goldenrod2'),
    shps = c('ophioblennius_spST' = 21, 'ophioblennius_trinitatis' = 24, 'ophioblennius_sp' = 22, 'ophioblennius_macclurei' = 23),
	bites_data = bite_data_clean(cols, shps, file_name = file_in('data/bites_data.csv')),
	diet_data = diet_data_clean(cols, shps, bites_data, file_name = file_in('data/diet_data.csv')),
	intestine_data = intestine_data_clean(cols, shps, file_name = file_in('data/diet_data.csv')),
	gut_content_data = gut_content_data_make(diet_data),
	mouth_data = mouth_data_clean(cols, shps, file_name = 'data/diet_data.csv'),
	bites_model = bites_model_run(bites_data),
	diet_model = diet_model_run(diet_data),
	intestine_model = stats::kruskal.test(intestine_data$local ~ intestine_data$QI),
	mouth_model = mouth_model_run(mouth_data),
    # Figures ----------------------------------------------
    fig_out_folder = dir.create('output/figures/', recursive = TRUE, showWarnings = FALSE),
	fig1_pdf = {
		fig_out_folder
		fig1_make(file_out('output/figures/fig1.pdf'), bites_data)
	},
    fig1_png = pngs_generate(file_in('output/figures/fig1.pdf'), file_out('output/figures/fig1.png')),
	fig2_pdf = {
		fig_out_folder
		fig2_make(file_out('output/figures/fig2.pdf'), intestine_data)
	},
    fig2_png = pngs_generate(file_in('output/figures/fig2.pdf'), file_out('output/figures/fig2.png')),
	fig3_pdf = {
		fig_out_folder
		fig3_make(file_out('output/figures/fig3.pdf'), gut_content_data, diet_data)
	},
    fig3_png = pngs_generate(file_in('output/figures/fig3.pdf'), file_out('output/figures/fig3.png')),
	fig4_pdf = {
		fig_out_folder
		fig4_make(file_out('output/figures/fig4.pdf'), bites_data, bites_model$best)
	},
    fig4_png = pngs_generate(file_in('output/figures/fig4.pdf'), file_out('output/figures/fig4.png')),
	fig5_pdf = {
		fig_out_folder
		fig5_make(file_out('output/figures/fig5.pdf'), diet_model, bites_model$best, diet_data)
	},
    fig5_png = pngs_generate(file_in('output/figures/fig5.pdf'), file_out('output/figures/fig5.png')),    
	figS1_pdf = {
		fig_out_folder
		figS1_make(file_out('output/figures/figS1.pdf'), mouth_data, mouth_model)
	},
    figS1_png = pngs_generate(file_in('output/figures/figS1.pdf'), file_out('output/figures/figS1.png'))
)
