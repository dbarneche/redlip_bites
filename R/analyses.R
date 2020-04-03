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

bite_data_clean  <-  function (cols, shps, ...) {
    data    <-   file_read(...) %>%
                     dplyr::filter(!(site %in% c('tide_pool', 'ancoras')), bites_original > 0) %>% # remove bad data
                     dplyr::mutate(temp_K = temperature + 273.15) # temperature converted to kelvin
    temps_K  <-  unique(data[, c('site', 'temp_K')])$temp_K # temperature is originally in Celsius
    data %>%
        dplyr::mutate(mean_temp_K = mean(temps_K),       
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

diet_data_clean  <-  function (cols, shps, bites_data, ...) {
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

intestine_data_clean  <-  function (cols, shps, ...) {
    locals_str  <-  c('principe' = 'principe_island', 'rocas_atoll' = 'atol_das_rocas', 'salvador' = 'bahia', 'sc' = 'santa_catarina', 'spspa' = 'aspsp')
    file_read(..., na.strings = c('', 'na'), sep = ';', dec = ',') %>%
    dplyr::filter(!is.na(QI)) %>%
    dplyr::mutate(local = locals_str[local],
                  spp = gsub('sp1', 'spST', spp),
                  colors = cols[spp],
                  shapes = shps[spp]
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

mouth_data_clean  <-  function (cols, shps, ...) {
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

bites_model_run  <-  function (data) {
    set.seed(10)
    full  <-  brms::brm(ln_bite_rates ~ ln_obs_time + ln_mass_g + invKT + (1 + ln_mass_g | local), data = data, family = gaussian(), prior = c(prior(normal(1, 2), 'b'), prior(normal(3, 3), 'Intercept'), prior(cauchy(0, 5), 'sd'), prior(cauchy(0, 5), 'sigma')), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    nest  <-  brms::brm(ln_bite_rates ~ ln_obs_time + ln_mass_g + invKT + (1 | local), data = data, family = gaussian(), prior = c(prior(normal(1, 2), 'b'), prior(normal(3, 3), 'Intercept'), prior(cauchy(0, 5), 'sd'), prior(cauchy(0, 5), 'sigma')), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
    full  <-  brms::add_criterion(full, 'loo')
    nest  <-  brms::add_criterion(nest, 'loo')
    lood  <-  brms::LOO(full, nest, pointwise = FALSE, cores = 4)
    pval  <-  2 * pnorm(-abs((lood$diffs[2, 'elpd_diff'] - 0) / lood$diffs[2, 'se_diff']))
    list('best' = get(ifelse(pval < 0.05, rownames(lood$diffs)[1], rownames(lood$diffs)[2])),
        'loo' = lood,
        'pval' = pval)
}

diet_model_run  <-  function (data) {
    set.seed(10)
    brms::brm(ln_gut_vol_mm3 ~ ln_mass_g + invKT + (1 | local), data = data, family = gaussian(), prior = c(prior(normal(1, 2), 'b'), prior(normal(3, 3), 'Intercept'), prior(cauchy(0, 5), 'sd'), prior(cauchy(0, 5), 'sigma')), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3, control = list(adapt_delta = 0.99, max_treedepth = 18))
}

mouth_model_run  <-  function (data) {
    set.seed(10)
    brms::brm(ln_vol_mm3 ~ ln_mass_g, data = data, family = gaussian(), sample_prior = TRUE, chains = 4, cores = 4, iter = 5e3, warmup = 2.5e3)
}
