plan  <-  drake::drake_plan(
  # Data -------------------------------------------------
  cols = c("ophioblennius_spST" = "tomato",
           "ophioblennius_trinitatis" = "dodgerblue2",
           "ophioblennius_sp" = "darkseagreen3",
           "ophioblennius_macclurei" = "goldenrod2"),
  shps = c("ophioblennius_spST" = 21,
           "ophioblennius_trinitatis" = 24,
           "ophioblennius_sp" = 22,
           "ophioblennius_macclurei" = 23),
  bites_data = make_bite_data(cols, shps,
                              file_name = file_in("data/bites_data.csv")),
  mouth_data = make_mouth_data(cols, shps, file_name = "data/diet_data.csv"),
  diet_data = make_diet_data(cols, shps, bites_data,
                             file_name = file_in("data/diet_data.csv")),
  gut_content_data = make_gut_content_data(diet_data),
  id_data = make_id_data(gut_content_data),
  logratios_data = make_logratios_data(diet_data, bites_data, gut_content_data),
  intest_data = make_intestine_data(cols, shps,
                                    file_name = file_in("data/diet_data.csv")),

  # Analyses ---------------------------------------------
  bites_model = run_bites_model(bites_data),
  mouth_model = run_mouth_model(mouth_data),
  logratios_model = run_logratios_model(logratios_data),
  intestine_model = intest_data %>%
    stats::kruskal.test(local ~ QI, data = .),
  correlation_data = make_correlation_data(diet_data, bites_model$best,
                                           logratios_model$best),
  logratios_correlation = run_logratios_correlation(correlation_data),
  ms_numbers = make_ms_numbers(bites_data, bites_model$best,
                               mouth_model, id_data, logratios_model$best),

  # Figures ----------------------------------------------
  fig_out_folder = dir.create(file_out("output/figures/"),
                              recursive = TRUE,
                              showWarnings = FALSE),
  fig_1_pdf = {
    fig_out_folder
    make_fig_1(file_out("output/figures/fig_1.pdf"), bites_data,
               bites_model$best)
  },
  fig_1_png = make_png(file_in("output/figures/fig_1.pdf"),
                       file_out("output/figures/fig_1.png")),
  fig_2_pdf = {
    fig_out_folder
    make_fig_2(file_out("output/figures/fig_2.pdf"), mouth_data, mouth_model)
  },
  fig_2_png = make_png(file_in("output/figures/fig_2.pdf"),
                       file_out("output/figures/fig_2.png")),
  fig_3_pdf = {
    fig_out_folder
    make_fig_3(file_out("output/figures/fig_3.pdf"),
               gut_content_data, diet_data, id_data)
  },
  fig_3_png = make_png(file_in("output/figures/fig_3.pdf"),
                       file_out("output/figures/fig_3.png")),
  fig_4_pdf = {
    fig_out_folder
    make_fig_4(file_out("output/figures/fig_4.pdf"),
               correlation_data, logratios_correlation$estimate)
  },
  fig_4_png = make_png(file_in("output/figures/fig_4.pdf"),
                       file_out("output/figures/fig_4.png")),
  fig_s_1_pdf = {
    fig_out_folder
    make_fig_s_1to3(file_out("output/figures/fig_s_1.pdf"),
                    logratios = FALSE, adjust = FALSE,
                    bites_data, bites_model$best, "ln_mass_g")
  },
  fig_s_1_png = make_png(file_in("output/figures/fig_s_1.pdf"),
                         file_out("output/figures/fig_s_1.png")),
  fig_s_2_pdf = {
    fig_out_folder
    make_fig_s_1to3(file_out("output/figures/fig_s_2.pdf"),
                    logratios = FALSE, adjust = TRUE, mouth_data,
                    mouth_model, "ln_mass_g")
  },
  fig_s_2_png = make_png(file_in("output/figures/fig_s_2.pdf"),
                         file_out("output/figures/fig_s_2.png")),
  fig_s_3_pdf = {
    fig_out_folder
    make_fig_s_1to3(file_out("output/figures/fig_s_3.pdf"),
                    logratios = TRUE, adjust = FALSE,
                    logratios_data, logratios_model$best)
  },
  fig_s_3_png = make_png(file_in("output/figures/fig_s_3.pdf"),
                         file_out("output/figures/fig_s_3.png")),
  fig_s_4_pdf = {
    fig_out_folder
    make_fig_s_4(file_out("output/figures/fig_s_4.pdf"), bites_data)
  },
  fig_s_4_png = make_png(file_in("output/figures/fig_s_4.pdf"),
                         file_out("output/figures/fig_s_4.png")),
  fig_s_5_pdf = {
    fig_out_folder
    make_fig_s_5(file_out("output/figures/fig_s_5.pdf"), intest_data)
  },
  fig_s_5_png = make_png(file_in("output/figures/fig_s_5.pdf"),
                         file_out("output/figures/fig_s_5.png")),

  # Tables -----------------------------------------------
  tab_out_folder = dir.create("output/tables/", recursive = TRUE,
                              showWarnings = FALSE),
  table_1 = {
    tab_out_folder
    make_table_1(file_out("output/tables/table_1.csv"),
                 bites_data, diet_data, intest_data)
  },
  table_s_1 = {
    tab_out_folder
    make_soi_table(file_out("output/tables/table_s_1.csv"), bites_model$best)
  },
  table_s_2 = {
    tab_out_folder
    make_soi_table(file_out("output/tables/table_s_2.csv"), mouth_model)
  },
  table_s_3 = {
    tab_out_folder
    make_soi_table(file_out("output/tables/table_s_3.csv"),
                   logratios_model$best)
  }
)
