##############
# STAN OPTIONS
##############
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#########
# GENERAL
#########
file_read  <-  function (file_name, ...) {
    read.csv(file_name, header = TRUE, stringsAsFactors = FALSE, ...)
}

mass_calculate  <-  function (x) {
    # parameters from O. atlanticus (FishBase), size is in cm
    0.01023 * x^3.01
}

###############
# DATA CLEANING
# AND MAKING
###############
bite_data_make  <-  function (cols, shps, ...) {
    data    <-   file_read(...) %>%
                 dplyr::filter(!(site %in% c('tide_pool', 'ancoras')), bites_original > 0) %>% # remove bad data
                 dplyr::mutate(temp_K = temperature + 273.15) # temperature converted to kelvin
    data %>%
    dplyr::mutate(mean_temp_K = mean(temp_K),       
                  invKT = 1 / 8.62e-5 * (1 / mean_temp_K - 1 / temp_K), # Boltzmann relationship
                  mass_g = mass_calculate(size), 
                  ln_mass_g = log(mass_g), # log individual mass in grams
                  ln_obs_time = log(obs_time), # observation time in minutes
                  ln_bite_rates = log(bites_original), # bites rates
                  colors = cols[spp],
                  shapes = shps[spp],
                  local_original = local,
                  local = gsub('santa_catarina.*', 'santa_catarina', local)
                 )
}

mouth_data_make  <-  function (cols, shps, ...) {
    file_read(..., na.strings = c('', 'na'), sep = ';', dec = ',') %>%
        dplyr::filter(!is.na(mouth_volume) & !is.na(TL_mm)) %>%
        dplyr::mutate(mass_g = mass_calculate((TL_mm / 10)),
                      ln_mass_g = log(mass_g), # log individual mass in grams 
                      ln_vol_mm3 = log(mouth_volume), # log individual mouth volume in mm3
                      spp = gsub('sp1', 'spST', spp),
                      colors = cols[spp],
                      shapes = shps[spp]
                     )
}

diet_data_make  <-  function (cols, shps, bites_data, ...) {
    locals_str  <-  c('principe' = 'principe_island', 'rocas_atoll' = 'atol_das_rocas', 'salvador' = 'bahia', 'sc' = 'santa_catarina', 'spspa' = 'aspsp')
    file_read(..., na.strings = c('', 'na'), sep = ';', dec = ',') %>%
    dplyr::filter(!is.na(total)) %>%
    dplyr::mutate(local = locals_str[local],
                  spp = gsub('sp1', 'spST', spp),
                  colors = cols[spp],
                  shapes = shps[spp],
                  temp_K = mean_temp + 273.15, # temperature converted to kelvin
                  invKT = 1 / 8.62e-5 * (1 / unique(bites_data$mean_temp_K) - 1 / temp_K), # use same standardising temperature as bites data
                  mass_g = mass_calculate((TL_mm / 10)),
                  ln_mass_g = log(mass_g), # log individual mass in grams
                  ln_gut_vol_mm3 = log(total) # log individual gut volume in mm3
                 )
}

gut_content_data_make  <-  function (diet_data) {
    diet_data  <-  diet_data %>%
                   dplyr::mutate(Other_animals = hexacorallia + insect + fish_scale + polychaeta,
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
                                 Total = total
                                )

    content_data  <-  cbind(diet_data, 
                            {diet_data %>% 
                             dplyr::select(Other_animals:Total) %>% 
                             rename_all(function (x) paste0(x, '_per'))
                            }) %>%
                      dplyr::mutate_at(vars(Other_animals_per:Total_per), function (x, total) x / total * 100, total = .$Total)

    dplyr::left_join({content_data %>% 
                      dplyr::select(spp, local, ind, Other_animals:Total) %>% 
                      tidyr::gather(key = 'item', value = 'vol_mm', Other_animals:Total)
                     },
                     {content_data %>% 
                      dplyr::select(spp, local, ind, Other_animals_per:Total_per) %>% 
                      tidyr::gather(key = 'item', value = 'vol_per', Other_animals_per:Total_per) %>% 
                      dplyr::mutate(item = gsub('_per', '', item))
                     }
                    ) %>%
    dplyr::mutate_at(vars(local, item), as.factor) %>% 
    dplyr::mutate(local = dplyr::recode(local, aspsp = 'SPSPA', atol_das_rocas = 'Rocas', bahia = 'Salvador', principe_island = 'Principe', santa_catarina = 'St Catarina', .default = levels(local)),
                  item = dplyr::recode(item, Other_animals = 'Other animals', Uniden_algae = 'Uniden. algae', Organic_detritus = 'Organic detritus', .default = levels(item))
                 )
}

logratios_data_make  <-  function (diet_data, bites_data, gut_content_data) {
    match_data  <-  diet_data  %>% 
                    dplyr::mutate(local = dplyr::recode(local, aspsp = 'SPSPA', atol_das_rocas = 'Rocas', bahia = 'Salvador', principe_island = 'Principe', santa_catarina = 'St Catarina', .default = levels(local)),
                                  ln_fullness = log(fullness * 0.25),
                                  temp_eff = unique(bites_data$mean_temp_K - 273.15) - mean_temp) %>%
                    dplyr::mutate_if(is.factor, as.character) %>%
                    dplyr::select(spp, local, ind, ln_fullness, ln_mass_g, mean_temp, temp_eff, colors, shapes)

    plyr::ddply(gut_content_data, .(local, ind), function (x) {
        x %>% 
        dplyr::mutate(log_ratio = log(sum(vol_mm[item %in% c('Copepoda', 'Eggs', 'Mollusca', 'Other animals')]) / sum(vol_mm[item %in% c('Chlorophyta', 'Rhodophyta', 'Heterokontophyta', 'Uniden. algae', 'Organic detritus')])),
                     item = 'All animals') %>%
        dplyr::filter(log_ratio != -Inf) %>%
        dplyr::slice(1)
    }) %>% 
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::left_join(match_data)
}

intestine_data_make  <-  function (cols, shps, ...) {
    locals_str  <-  c('principe' = 'principe_island', 'rocas_atoll' = 'atol_das_rocas', 'salvador' = 'bahia', 'sc' = 'santa_catarina', 'spspa' = 'aspsp')
    file_read(..., na.strings = c('', 'na'), sep = ';', dec = ',') %>%
    dplyr::filter(!is.na(QI)) %>%
    dplyr::mutate(local = locals_str[local],
                  spp = gsub('sp1', 'spST', spp),
                  colors = cols[spp],
                  shapes = shps[spp]
                 )
}

########
# MODELS
########
model_comparison_run  <-  function (full_model, nested_model) {
    full_model    <-  brms::add_criterion(full_model, 'loo')
    nested_model  <-  brms::add_criterion(nested_model, 'loo')
    lood  <-  brms::LOO(full_model, nested_model, pointwise = FALSE, cores = 4)
    pval  <-  2 * pnorm(-abs((lood$diffs[2, 'elpd_diff'] - 0) / lood$diffs[2, 'se_diff']))
    list('best' = get(ifelse(pval < 0.05, rownames(lood$diffs)[1], rownames(lood$diffs)[2])),
        'loo' = lood,
        'pval' = pval)
}

bites_model_run  <-  function (data) {
    set.seed(10)
    full  <-  brms::brm(ln_bite_rates ~ ln_obs_time + ln_mass_g + invKT + (1 + ln_mass_g | local), data = data, family = gaussian(), prior = c(prior(normal(1, 2), 'b'), prior(normal(3, 3), 'Intercept'), prior(cauchy(0, 5), 'sd'), prior(cauchy(0, 5), 'sigma')), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    set.seed(10)
    nest  <-  brms::brm(ln_bite_rates ~ ln_obs_time + ln_mass_g + invKT + (1 | local), data = data, family = gaussian(), prior = c(prior(normal(1, 2), 'b'), prior(normal(3, 3), 'Intercept'), prior(cauchy(0, 5), 'sd'), prior(cauchy(0, 5), 'sigma')), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    model_comparison_run(full, nest)
}

mouth_model_run  <-  function (data) {
    set.seed(10)
    brms::brm(ln_vol_mm3 ~ ln_mass_g, data = data, family = gaussian(), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3)
}

logratios_model_run  <-  function (data) {
    set.seed(10)
    full  <-  brms::brm(log_ratio ~ ln_fullness + ln_mass_g + temp_eff + (1 | local), data = data, family = gaussian(), prior = prior(cauchy(0, 5), 'sigma'), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    set.seed(10)
    nest  <-  brms::brm(log_ratio ~ 1 + (1 | local), data = data, family = gaussian(), prior = prior(cauchy(0, 5), 'sigma'), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    model_comparison_run(full, nest)
}
