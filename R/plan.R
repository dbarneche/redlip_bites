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
  intest_data = make_intestine_data(cols, shps,
                                    file_name = file_in("data/diet_data.csv")),

  # Analyses ---------------------------------------------
  bites_model = run_bites_model(bites_data),
  mouth_model = run_mouth_model(mouth_data),
  intestine_model = intest_data %>%
    stats::kruskal.test(local ~ QI, data = .),
  ms_numbers = make_ms_numbers(bites_data, bites_model$best,
                               mouth_model, id_data),

  # Figures ----------------------------------------------
  fig_out_folder = dir.create(file_out("output/figures/"),
                              recursive = TRUE,
                              showWarnings = FALSE),
  ophio_png = make_grob_png("pics/ophio.png"),
  fig_2_pdf = {
    fig_out_folder
    make_fig_2(file_out("output/figures/fig_2.pdf"), bites_data,
               bites_model$best)
  },
  fig_2_png = make_png(file_in("output/figures/fig_2.pdf"),
                       file_out("output/figures/fig_2.png")),
  fig_3_pdf = {
    fig_out_folder
    make_fig_3(file_out("output/figures/fig_3.pdf"), mouth_data, mouth_model)
  },
  fig_3_png = make_png(file_in("output/figures/fig_3.pdf"),
                       file_out("output/figures/fig_3.png")),
  fig_4_pdf = {
    fig_out_folder
    make_fig_4(file_out("output/figures/fig_4.pdf"),
               gut_content_data, diet_data, id_data)
  },
  fig_4_png = make_png(file_in("output/figures/fig_4.pdf"),
                       file_out("output/figures/fig_4.png")),
  fig_5_pdf = {
    fig_out_folder
    make_fig_5(file_out("output/figures/fig_5.pdf"),
               bites_model, mouth_model, bites_data, ophio_png)
  },
  fig_5_png = make_png(file_in("output/figures/fig_5.pdf"),
                       file_out("output/figures/fig_5.png")),
  fig_s_1_pdf = {
    fig_out_folder
    make_fig_s_1to2(file_out("output/figures/fig_s_1.pdf"),
                    adjust = FALSE, bites_data,
                    bites_model$best, "ln_mass_g")
  },
  fig_s_1_png = make_png(file_in("output/figures/fig_s_1.pdf"),
                         file_out("output/figures/fig_s_1.png")),
  fig_s_2_pdf = {
    fig_out_folder
    make_fig_s_1to2(file_out("output/figures/fig_s_2.pdf"),
                    adjust = TRUE, mouth_data,
                    mouth_model, "ln_mass_g")
  },
  fig_s_2_png = make_png(file_in("output/figures/fig_s_2.pdf"),
                         file_out("output/figures/fig_s_2.png")),
  fig_s_3_pdf = {
    fig_out_folder
    make_fig_s_3(file_out("output/figures/fig_s_3.pdf"), intest_data)
  },
  fig_s_3_png = make_png(file_in("output/figures/fig_s_3.pdf"),
                         file_out("output/figures/fig_s_3.png")),

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
  }
)
