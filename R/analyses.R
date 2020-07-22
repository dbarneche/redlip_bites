##############
# STAN OPTIONS
##############
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#########
# GENERAL
#########
read_file <- function(file_name, ...) {
  read.csv(file_name, header = TRUE, stringsAsFactors = FALSE, ...)
}

convert_lw <- function(x) {
  # parameters from O. atlanticus (FishBase), size is in cm
  0.01023 * x^3.01
}

bite_rate <- function(mass, ...) {
  3.2 * mass^-0.18 * exp(1.16 * inv_kt(...))
}

consumption_rate <- function(mass, ...) {
  3.2 * mass^1.06 * exp(1.16 * inv_kt(...))
}

inv_kt <- function(temp, mean_temp_k = 300.87) {
  temp_k <- temp + 273.15
  1 / 8.62e-5 * (1 / mean_temp_k - 1 / temp_k)
}

###############
# DATA CLEANING
# AND MAKING
###############
make_bite_data <- function(cols, shps, ...) {
  data <- read_file(...) %>%
    dplyr::filter(!(site %in% c("tide_pool", "ancoras")),
                  bites_original > 0) %>%
    dplyr::mutate(temp_k = temperature + 273.15,
                  mean_temp_k = mean(temp_k),
                  inv_kt = 1 / 8.62e-5 * (1 / mean_temp_k - 1 / temp_k),
                  mass_g = convert_lw(size),
                  ln_mass_g = log(mass_g),
                  ln_obs_time = log(obs_time),
                  ln_bite_rates = log(bites_original),
                  colors = cols[spp],
                  shapes = shps[spp],
                  local_original = local,
                  local = gsub("santa_catarina.*", "santa_catarina", local))
}

make_mouth_data <- function(cols, shps, ...) {
  read_file(..., na.strings = c("", "na"), sep = ";", dec = ",") %>%
    dplyr::filter(!is.na(mouth_volume) & !is.na(TL_mm)) %>%
    dplyr::mutate(mass_g = convert_lw((TL_mm / 10)),
                  ln_mass_g = log(mass_g),
                  ln_vol_mm3 = log(mouth_volume),
                  spp = gsub("sp1", "spST", spp),
                  colors = cols[spp],
                  shapes = shps[spp])
}

make_diet_data <- function(cols, shps, bites_data, ...) {
  new_temp <- bites_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(mean_temp = mean(temperature))
  locals_str <- c("principe" = "principe_island",
                  "rocas_atoll" = "atol_das_rocas",
                  "salvador" = "bahia",
                  "sc" = "santa_catarina",
                  "spspa" = "aspsp")
  read_file(..., na.strings = c("", "na"), sep = ";", dec = ",") %>%
    dplyr::filter(!is.na(total)) %>%
    dplyr::mutate(local = locals_str[local],
                  spp = gsub("sp1", "spST", spp),
                  colors = cols[spp],
                  shapes = shps[spp],
                  mean_temp = new_temp$mean_temp[match(local, new_temp$local)],
                  temp_k = mean_temp + 273.15,
                  inv_kt = 1 / 8.62e-5 *
                    (1 / unique(bites_data$mean_temp_k) -
                       1 / temp_k),
                  mass_g = convert_lw((TL_mm / 10)),
                  ln_mass_g = log(mass_g),
                  ln_gut_vol_mm3 = log(total))
}

make_gut_content_data <- function(diet_data) {
  diet_data <- diet_data %>%
    dplyr::mutate(Other_animals = hexacorallia + insect +
                    fish_scale + polychaeta,
                  Mollusca = bivalve + gastropoda,
                  Chlorophyta = ulvales + cladophorales,
                  Rhodophyta = gelidiales + ceramiales + corallinales,
                  Heterokontophyta = dictyotales + ectocarpales,
                  Copepoda = copepoda,
                  Eggs = eggs,
                  Uniden_algae = ni_algae,
                  Organic_detritus = organic_detritus,
                  Plastic = plastic,
                  Sediment = sediment,
                  Total = total)

  content_data <- cbind(diet_data, {
    diet_data %>%
      dplyr::select(Other_animals:Total) %>%
      dplyr::rename_all(function(x) paste0(x, "_per"))
  }) %>%
    dplyr::mutate_at(dplyr::vars(Other_animals_per:Total_per),
                     function(x, total) x / total * 100, total = .$Total)

  dplyr::left_join({
    content_data %>%
      dplyr::select(spp, local, ind, Other_animals:Total) %>%
      tidyr::gather(key = "item", value = "vol_mm", Other_animals:Total)
  }, {
    content_data %>%
      dplyr::select(spp, local, ind, Other_animals_per:Total_per) %>%
      tidyr::gather(key = "item", value = "vol_per",
                    Other_animals_per:Total_per) %>%
      dplyr::mutate(item = gsub("_per", "", item))
  }) %>%
    dplyr::mutate_at(dplyr::vars(local, item), as.factor) %>%
    dplyr::mutate(local = dplyr::recode(local, aspsp = "SPSPA",
                                        atol_das_rocas = "Rocas",
                                        bahia = "Salvador",
                                        principe_island = "Principe",
                                        santa_catarina = "St Catarina",
                                        .default = levels(local)),
                  item = dplyr::recode(item,
                                       Other_animals = "Other animals",
                                       Uniden_algae = "Uniden. algae",
                                       Organic_detritus = "Organic detritus",
                                       .default = levels(item)))
}

make_logratios_data <- function(diet_data, bites_data, gut_content_data) {
  match_data <- diet_data  %>%
    dplyr::mutate(local = dplyr::recode(local,
                                        aspsp = "SPSPA",
                                        atol_das_rocas = "Rocas",
                                        bahia = "Salvador",
                                        principe_island = "Principe",
                                        santa_catarina = "St Catarina",
                                        .default = levels(local)),
                  ln_fullness = log(fullness * 0.25),
                  temp_eff = unique(bites_data$mean_temp_k - 273.15) -
                    mean_temp) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(spp, local, ind, ln_fullness, ln_mass_g, mean_temp,
                  temp_eff, colors, shapes)

  plyr::ddply(gut_content_data, .(local, ind), function(x) {
    animals <- c("Copepoda", "Eggs", "Mollusca", "Other animals")
    others <- c("Chlorophyta", "Rhodophyta", "Heterokontophyta",
                "Uniden. algae", "Organic detritus")
    x %>%
      dplyr::mutate(log_ratio = log(sum(vol_mm[item %in% animals]) /
                                      sum(vol_mm[item %in% others])),
                    item = "All animals") %>%
      dplyr::filter(log_ratio != -Inf) %>%
      dplyr::slice(1)
  }) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::left_join(match_data)
}

make_intestine_data <- function(cols, shps, ...) {
  locals_str <- c("principe" = "principe_island",
                  "rocas_atoll" = "atol_das_rocas",
                  "salvador" = "bahia",
                  "sc" = "santa_catarina",
                  "spspa" = "aspsp")
  read_file(..., na.strings = c("", "na"), sep = ";", dec = ",") %>%
    dplyr::filter(!is.na(QI)) %>%
    dplyr::mutate(local = locals_str[local],
                  spp = gsub("sp1", "spST", spp),
                  colors = cols[spp],
                  shapes = shps[spp])
}

########
# MODELS
########
run_model_comparison <- function(full_model, nested_model) {
  full_model <- brms::add_criterion(full_model, "loo")
  nested_model <- brms::add_criterion(nested_model, "loo")
  lood <- brms::LOO(full_model, nested_model, pointwise = FALSE, cores = 4)
  pval <- 2 * pnorm(-abs((lood$diffs[2, "elpd_diff"] - 0) /
                           lood$diffs[2, "se_diff"]))
  list("best" = get(ifelse(pval < 0.05,
                           rownames(lood$diffs)[1],
                           rownames(lood$diffs)[2])),
       "loo" = lood,
       "pval" = pval)
}

run_bites_model <- function(data) {
  set.seed(10)
  full <- brms::brm(ln_bite_rates ~ ln_obs_time +
                      ln_mass_g + inv_kt + (1 + ln_mass_g | local),
                    data = data,
                    family = gaussian(),
                    prior = c(prior(normal(1, 2), "b"),
                              prior(normal(3, 3), "Intercept"),
                              prior(cauchy(0, 5), "sd"),
                              prior(cauchy(0, 5), "sigma")),
                    sample_prior = TRUE, chains = 4, cores = 4,
                    iter = 5e3, warmup = 2.5e3,
                    control = list(adapt_delta = 0.99,
                                   max_treedepth = 18))
  set.seed(10)
  nest <- brms::brm(ln_bite_rates ~ ln_obs_time + ln_mass_g +
                      inv_kt + (1 | local),
                    data = data,
                    family = gaussian(),
                    prior = c(prior(normal(1, 2), "b"),
                              prior(normal(3, 3), "Intercept"),
                              prior(cauchy(0, 5), "sd"),
                              prior(cauchy(0, 5), "sigma")),
                    sample_prior = TRUE, chains = 4, cores = 4,
                    iter = 5e3, warmup = 2.5e3,
                    control = list(adapt_delta = 0.99,
                                   max_treedepth = 18))
  run_model_comparison(full, nest)
}

run_mouth_model <- function(data) {
  set.seed(10)
  brms::brm(ln_vol_mm3 ~ ln_mass_g, data = data,
            family = gaussian(), sample_prior = TRUE,
            chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3)
}

run_logratios_model <- function(data) {
  set.seed(10)
  full <- brms::brm(log_ratio ~ ln_fullness + ln_mass_g +
                      temp_eff + (1 | local),
                    data = data,
                    family = gaussian(),
                    prior = c(prior(cauchy(0, 5), "sigma")),
                    sample_prior = TRUE, chains = 4, cores = 4,
                    iter = 5e3, warmup = 2.5e3,
                    control = list(adapt_delta = 0.99,
                                   max_treedepth = 18))
  set.seed(10)
  nest <- brms::brm(log_ratio ~ 1 + (1 | local),
                    data = data,
                    family = gaussian(),
                    prior = c(prior(cauchy(0, 5), "sigma")),
                    sample_prior = TRUE, chains = 4, cores = 4,
                    iter = 5e3, warmup = 2.5e3,
                    control = list(adapt_delta = 0.99,
                                   max_treedepth = 18))
  run_model_comparison(full, nest)
}

make_ms_numbers <- function(data) {
  rocas <- data %>%
    dplyr::filter(local == "atol_das_rocas")
  temp_1 <- 30
  mass_1 <- mean(rocas$mass_g)
  temp_2 <- 31
  mass_2 <- mean(rocas$mass_g) * 0.6
  list(bites_fold_change = bite_rate(mass_2, temp_2) /
         bite_rate(mass_1, temp_1),
       cons_fold_change = consumption_rate(mass_2, temp_2) /
         consumption_rate(mass_1, temp_1),
       range_temp = range(data$temperature),
       range_mass = range(data$mass_g))
}
