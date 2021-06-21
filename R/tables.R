make_table_1 <- function(dest, bites_data, diet_data, intestine_data,
                         mouth_data) {
  x <- bites_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(sp = unique(spp),
                     depth = max(depth),
                     temp = mean(temperature),
                     bite = dplyr::n())
  y <- diet_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(diet = dplyr::n())
  z <- intestine_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(intestine = dplyr::n())
  w <- mouth_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(mouth = length(unique(ind))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      local = dplyr::recode(local, noronha = "fernando_de_noronha",
                            rocas_atoll = "atol_das_rocas",
                            salvador = "bahia", sc = "santa_catarina")
    )
  table_1 <- dplyr::left_join(x, y) %>%
    dplyr::left_join(z) %>%
    dplyr::left_join(w, by = "local")
  write.csv(table_1, dest, row.names = TRUE)
  table_1
}

make_soi_table <- function(dest, model) {
  coefs <- brms::posterior_summary(model)
  coefs <- coefs[1:(which(rownames(coefs) == "sigma") - 1),
                 c("Estimate", "Q2.5", "Q97.5")] %>%
    round(2)
  write.csv(coefs, dest, row.names = TRUE)
  coefs
}
