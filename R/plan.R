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
  coords = make_coords(bites_data),
  bites_data_sat = make_satellite_bites_data(bites_data, coords),
  mouth_data = make_mouth_data(cols, shps, file_name = "data/diet_data.csv"),
  diet_data = make_diet_data(cols, shps, bites_data,
                             file_name = file_in("data/diet_data.csv")),
  gut_content_data = make_gut_content_data(diet_data),
  id_data = make_id_data(gut_content_data),
  intest_data = make_intestine_data(cols, shps,
                                    file_name = file_in("data/diet_data.csv")),
  osp1_shp = sf::st_read("data/shps/Ophioblenniusp1.shp"),
  osp2_shp = sf::st_read("data/shps/Ophioblenniusp2.shp"),
  oatl_shp = sf::st_read("data/shps/Ophioblenniusatlanticus.shp"),
  omac_shp = sf::st_read("data/shps/Ophmacclurei.shp"),
  otri_shp = sf::st_read("data/shps/Ophtrinitatis.shp"),
  world1 = rnaturalearth::ne_countries(scale = "medium",
                                        returnclass = "sf"),
  world2 = rnaturalearth::ne_coastline(scale = "medium",
                                        returnclass = "sf"),

  # Analyses ---------------------------------------------
  bites_model = run_bites_model(bites_data),
  bites_model_sat = run_bites_model(bites_data_sat),
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
  fig_1_pdf = make_fig_1(fig_out_folder, file_out("output/figures/fig_1.pdf"),
                         osp1_shp, osp2_shp, oatl_shp, omac_shp, otri_shp,
                         world1, world2),
  fig_2_pdf = make_fig_2(fig_out_folder, file_out("output/figures/fig_2.pdf"),
                         bites_data, bites_model$best),
  fig_3_pdf = make_fig_3(fig_out_folder, file_out("output/figures/fig_3.pdf"),
                         mouth_data, mouth_model),
  fig_4_pdf = make_fig_4(fig_out_folder, file_out("output/figures/fig_4.pdf"),
                         gut_content_data, diet_data, id_data),
  fig_5_pdf = make_fig_5(fig_out_folder, file_out("output/figures/fig_5.pdf"),
                         bites_model, mouth_model, bites_data, ophio_png),
  fig_s1_pdf = make_fig_s1_2(fig_out_folder,
                             file_out("output/figures/fig_s1.pdf"),
                             adjust = FALSE, bites_data, bites_model$best,
                             "ln_mass_g"),
  fig_s2_pdf = make_fig_s1_2(fig_out_folder,
                             file_out("output/figures/fig_s2.pdf"),
                             adjust = TRUE, mouth_data, mouth_model,
                             "ln_mass_g"),
  fig_s3_pdf = make_fig_s3(fig_out_folder,
                           file_out("output/figures/fig_s3.pdf"),
                           bites_data_sat, bites_model_sat$best,
                           bites_model$best, coords),
  fig_s4_pdf = make_fig_s4(fig_out_folder,
                           file_out("output/figures/fig_s4.pdf"), intest_data),

  # Tables -----------------------------------------------
  tab_out_folder = dir.create("output/tables/", recursive = TRUE,
                              showWarnings = FALSE),
  table_1 = {
    tab_out_folder
    make_table_1(file_out("output/tables/table_1.csv"),
                 bites_data, diet_data, intest_data, mouth_data)
  },
  table_s1 = {
    tab_out_folder
    make_soi_table(file_out("output/tables/table_s1.csv"), bites_model$best)
  },
  table_s2 = {
    tab_out_folder
    make_soi_table(file_out("output/tables/table_s2.csv"), mouth_model)
  }
)
