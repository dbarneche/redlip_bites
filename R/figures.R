######################
# AUXILLIARY FUNCTIONS
######################
make_png <- function(origin_file_name, output_file_name) {
  system(paste0("sips -s formatOptions best -s format png ",
                origin_file_name,
                " --out ",
                output_file_name))
  file.info(output_file_name)
}

to_dev <- function(expr, dev, filename, ..., verbose = TRUE) {
  if (verbose) {
    cat(sprintf("Creating %s\n", filename))
  }
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to_pdf <- function(expr, filename, ...) {
  to_dev(expr, pdf, filename, ...)
}

make_model_ggplot_data <- function(raw_data, model_list, resp) {
  model_data <- model_list[[resp]]
  list("output" = model_data,
       "polygons" = with(model_data,
                         data.frame(x = c(effect1__,
                                          rev(effect1__)),
                                    y_cred = c(lower__,
                                               rev(upper__)))),
       "lines" = with(model_data, data.frame(x = effect1__, y = estimate__)),
       "points" = cbind(attr(model_data, "points"),
                        (raw_data %>% dplyr::select(c("colors",
                                                      "shapes",
                                                      "local"))))
  )
}

create_aes_vector <- function(target_aes_col, data, ref = "local") {
  out <- unique(data[, c(ref, target_aes_col)])
  vec <- out[[target_aes_col]]
  names(vec) <- out[[ref]]
  vec
}

aes_vec_list_create <- function(targets, ...) {
  sapply(targets, create_aes_vector, ..., USE.NAMES = TRUE, simplify = FALSE)
}

temp_to_inv_kt <- function(temps, mean_t) {
  1 / 8.62e-5 * (1 / mean_t - 1 / (temps + 273.15))
}

temp_from_inv_kt <- function(inv_kt, mean_t) {
  (1 / mean_t - inv_kt / (1 / 8.62e-5))^-1 - 273.15
}

my_theme <- function() {
  theme_bw() +
    theme(axis.title.y = element_text(size = 14,
                                      margin = margin(t = 0,
                                                      r = 10,
                                                      b = 0,
                                                      l = 10)),
          axis.title.x = element_text(size = 14,
                                      margin = margin(t = 10,
                                                      r = 0,
                                                      b = 10,
                                                      l = 0)),
          axis.text.x = element_text(size = 12,
                                     margin = margin(t = 4,
                                                     r = 0,
                                                     b = 0,
                                                     l = 0)),
          axis.text.y = element_text(size = 12,
                                     margin = margin(t = 0,
                                                     r = 4,
                                                     b = 0,
                                                     l = 0)),
          axis.ticks.length = unit(5, "pt"),
          strip.background = element_blank())
}

bites_fig_base <- function(data_list, aes_vecs, my_ylab, my_xlab) {
  ggplot() +
    geom_polygon(data = data_list$polygons,
                 mapping = aes(x = x, y = y_cred),
                 fill = "grey60", alpha = 0.3) +
    geom_line(data = data_list$lines,
              mapping = aes(x = x, y = y),
              col = "black", lty = 2, size = 0.5) +
    geom_point(data = data_list$points,
               mapping = aes(x = effect1__, y = resp__,
                             fill = local, col = local,
                             shape = local),
               size = 2.5, alpha = 0.8,
               position = position_jitterdodge(jitter.width = 0.03,
                                               dodge.width = 0,
                                               seed = 1),
               show.legend = FALSE) +
    scale_colour_manual(values = aes_vecs$colors,
                        aesthetics = c("colour", "fill")) +
    scale_shape_manual(values = aes_vecs$shapes) +
    ylab(my_ylab) +
    xlab(my_xlab) +
    my_theme()
}

gg_relative_text <- function(ggobject, px, py, lab, ...) {
  usr <- ggplot2::ggplot_build(ggobject)$layout$panel_params[[1]]
  usr <- c(usr$x.range, usr$y.range)
  x_p <- usr[1] + px * (usr[2] - usr[1])
  y_p <- usr[3] + py * (usr[4] - usr[3])
  ggplot2::annotate("text", x_p, y_p, label = lab, ...)
}

make_aes_vec <- function(data, col) {
  data <- data %>%
    dplyr::select(c("local", tidyselect::all_of(col))) %>%
    dplyr::distinct(.keep_all = TRUE)
  x <- data[[col]]
  names(x) <- data$local
  x
}

###############
# PAPER FIGURES
###############
make_fig_1 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_1(...), device = "pdf", width = 8.8,
                  height = 3.8, units = "in", onefile = FALSE,
                  useDingbats = FALSE)
}

fig_1 <- function(bites_data, bites_model) {
  fixefs <- brms::fixef(bites_model)[, "Estimate"]
  alpha <- fixefs["ln_mass_g"]
  er <- fixefs["inv_kt"]
  b1 <- fixefs["ln_obs_time"]
  ln_b0 <- fixefs["Intercept"]
  r2 <- brms::bayes_R2(bites_model) %>%
    data.frame() %>%
    dplyr::select(Estimate) %>%
    LoLinR::rounded(2)
  for_figs <- brms::conditional_effects(bites_model, points = TRUE)
  mass_ggplot <- make_model_ggplot_data(bites_data, for_figs, "ln_mass_g")
  aes_vecs_a <- aes_vec_list_create(c("colors", "shapes"),
                                    data = mass_ggplot$points)
  k_a <- mass_ggplot$output %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::summarise(ln_b0 + ln_obs_time * b1 + inv_kt * er) %>%
    unlist %>%
    unname %>%
    exp %>%
    LoLinR::rounded(2)
  temp_ggplot <- make_model_ggplot_data(bites_data, for_figs, "inv_kt")
  aes_vecs_b <- aes_vec_list_create(c("colors", "shapes"),
                                    data = temp_ggplot$points)
  k_b <- temp_ggplot$output %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::summarise(ln_b0 + ln_obs_time * b1 + ln_mass_g * alpha) %>%
    unlist %>%
    unname %>%
    exp %>%
    LoLinR::rounded(2)
  axis_labs_b <- temp_to_inv_kt(seq(18, 30, 4), bites_data$mean_temp_k[1])
  z <- temp_from_inv_kt(inv_kt = mass_ggplot$output$inv_kt[1],
                        bites_data$mean_temp_k[1]) %>%
    LoLinR::rounded(1)
  a <- bites_fig_base(mass_ggplot, aes_vecs_a,
                      my_ylab = substitute("Bite rate @ " * z * degree * "C",
                                           list(z = z)),
                      my_xlab = "Body mass (g)") +
    scale_x_continuous(breaks = 0:4, labels = LoLinR::rounded(exp(0:4), 1)) +
    scale_y_continuous(breaks = 0:4, labels = LoLinR::rounded(exp(0:4), 1))
  a <- a +
    gg_relative_text(a, px = 0.03, py = 0.95,
                     "a", fontface = "bold", size = 5) +
    gg_relative_text(a, px = 0.03, py = 0.05,
                     deparse(substitute("Bayesian " * italic("R")^2 == z,
                                        list(z = r2))),
                     fontface = "bold", size = 4, hjust = 0,
                     parse = TRUE) +
    gg_relative_text(a, px = 0.95, py = 0.95,
                     deparse(substitute(y == k %.% x^z,
                                        list(k = k_a,
                                             z = LoLinR::rounded(alpha, 2)))),
                     fontface = "bold", size = 4,
                     hjust = 1, parse = TRUE)
  my_ylab <- exp(temp_ggplot$output$ln_mass_g[1]) %>%
    LoLinR::rounded(1)
  my_xlab <- expression(paste("Temperature (" * degree, "C)",
                              sep = ""))
  b <- bites_fig_base(temp_ggplot, aes_vecs_b,
                      my_ylab = paste0("Bite rate @ ", my_ylab, " g"),
                      my_xlab = my_xlab) +
    scale_x_continuous(breaks = axis_labs_b,
                       labels = temp_from_inv_kt(inv_kt = axis_labs_b,
                                                 bites_data$mean_temp_k[1])) +
    scale_y_continuous(breaks = 0:4, labels = LoLinR::rounded(exp(0:4), 1))
    my_lab <- deparse(substitute(y == k %.% italic("e") ^ z,
                                   list(k = k_b,
                                        z = substitute(a %.%
                                                         italic("f") *
                                                           "(x)",
                                                       list(a = LoLinR::rounded(er, 2))))))
  b <- b +
    gg_relative_text(b, px = 0.03, py = 0.95, "b",
                     fontface = "bold", size = 5) +
    gg_relative_text(a, px = 0.02,
                     py = 0.85, my_lab, fontface = "bold", size = 4,
                     hjust = 1, parse = TRUE)
  gridExtra::grid.arrange(a, b, nrow = 1)
}

make_fig_2 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_2(...), device = "pdf", width = 6,
                  height = 6, units = "in", onefile = FALSE,
                  useDingbats = FALSE)
}

fig_2 <- function(mouth_data, mouth_model) {
  mouth_data <- mouth_data %>%
    dplyr::mutate_at(dplyr::vars(local, colors, shapes), unname) %>%
    dplyr::select(spp, local, mass_g, mouth_volume, colors, shapes)
  coefs <- brms::fixef(mouth_model)[, "Estimate"]
  x_range <- seq(min(mouth_data$mass_g), max(mouth_data$mass_g),
                 length.out = 30)
  fit_d <- data.frame(x = x_range, y = exp(coefs[1]) * x_range ^ coefs[2])
  r2 <- brms::bayes_R2(mouth_model) %>%
    data.frame() %>%
    dplyr::select(Estimate) %>%
    LoLinR::rounded(2)
  my_cols <- make_aes_vec(mouth_data, "colors")
  my_shps <- make_aes_vec(mouth_data, "shapes")
  g1 <- ggplot() +
    geom_point(data = mouth_data,
               mapping = aes(x = mass_g,
                             y = mouth_volume,
                             fill = local,
                             colour = local,
                             shape = local),
               size = 2.5, alpha = 0.8,
               show.legend = FALSE) +
    geom_line(data = fit_d,
              mapping = aes(x = x, y = y),
              col = "black", lty = 2,
              size = 0.5) +
    scale_colour_manual(values = my_cols) +
    scale_fill_manual(values = my_cols) +
    scale_shape_manual(values = my_shps) +
    xlab("Body mass (g)") +
    ylab(substitute("Mouth volume (mm"^3 * ")")) +
    my_theme()
  my_lab_1 <- deparse(substitute(y == k %.% x^z,
                                 list(k = LoLinR::rounded(exp(coefs[1])),
                                      z = LoLinR::rounded(coefs[2], 2))))
  my_lab_2 <- deparse(substitute("Bayesian " * italic("R")^2 == z,
                                 list(z = r2)))
  g1 +
    gg_relative_text(g1, px = 0.03, py = 0.95,
                     my_lab_1, fontface = "bold",
                     size = 5, hjust = 0,
                     parse = TRUE) +
    gg_relative_text(g1, px = 0.03, py = 0.85,
                     my_lab_2, fontface = "bold",
                     size = 5, hjust = 0,
                     parse = TRUE)
}

make_fig_3 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_3(...), device = "pdf", width = 12,
                  height = 5, units = "in", onefile = FALSE,
                  useDingbats = FALSE)
}

fig_3 <- function(gut_content_data, diet_data, id_data) {
  gut_content_data <- gut_content_data %>%
    dplyr::filter(item != "Total") %>%
    droplevels
  mean_cont <- gut_content_data %>%
    dplyr::group_by(local, item) %>%
    dplyr::summarise("mean" = mean(vol_per),
                     "error" = sd(vol_per) / sqrt(dplyr::n())) %>%
    as.data.frame %>%
    dplyr::filter(mean > 0)
  gut_content_data <- left_join(gut_content_data, {
    diet_data %>%
      dplyr::select(spp, colors, shapes) %>%
      dplyr::distinct(.keep_all = TRUE)
  }) %>%
    dplyr::group_by(local, item) %>%
    dplyr::mutate(sum_item = sum(vol_per)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(sum_item > 0)

  ggplot(data = gut_content_data) +
    facet_grid(.~ local, switch = "both") +
    geom_point(mapping = aes(x = vol_per, y = item),
               shape = gut_content_data$shapes,
               size = 3, colour = gut_content_data$colors,
               fill = alpha(gut_content_data$colors, 0.7),
               stroke = 0.3) +
    scale_x_discrete(limits = c("SPSPA", "Rocas",
                                "Salvador", "St Catarina",
                                "Principe")) +
    scale_y_discrete(limits = c("Plastic", "Other animals", "Eggs",
                                "Copepoda", "Mollusca", "Uniden. algae",
                                "Rhodophyta", "Chlorophyta",
                                "Heterokontophyta", "Sediment",
                                "Organic detritus")) +
    scale_x_sqrt(breaks = c(0, 1, 10, 30, 60, 100), position = "top") +
    xlab(label = "Volume (%)") +
    ylab(label = NULL) +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 10,
                                     angle = 0,
                                     vjust = 0.5,
                                     hjust = 0.5,
                                     face = "plain"),
          axis.text.y = element_text(colour = "black",
                                     size = 10,
                                     angle = 0,
                                     vjust = 0.5,
                                     face = "plain"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, color = "black"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.line = element_line(colour = "black",
                                   size = 0.5,
                                   linetype = "solid"),
          strip.background.x = element_rect(color = "black"),
          strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
          axis.ticks.x = element_line(size = 0.5, linetype = "solid",
                                      color = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          axis.ticks.y = element_line(size = 0.5, linetype = "solid",
                                      color = "black"),
          legend.position = "") +
    geom_errorbarh(data = mean_cont,
                   mapping = aes(xmax = mean + error,
                                 xmin = mean - error, y = item),
                   size = 1,
                   height = 0,
                   color = "black") +
    geom_point(data = mean_cont,
               mapping = aes(x = mean, y = item),
               shape = 21, size = 2,
               colour = "black",
               fill = "white", stroke = 1.5) +
    geom_point(data = id_data,
               mapping = aes(x = iai_per, y = item),
               shape = 18, size = 3,
               colour = "black",
               position = position_nudge(y = -0.2))
}

make_fig_4 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_4(...), device = "pdf", width = 6,
                  height = 6, units = "in", onefile = FALSE,
                  useDingbats = FALSE)
}

fig_4 <- function(correlation_data, logratios_correlation) {
  g1 <- ggplot(data = correlation_data,
               mapping = aes(x = x, y = y, fill = local,
                             colour = local, shape = local,
                             label = local)) +
    geom_point(size = 4, alpha = 0.8, show.legend = FALSE) +
    scale_colour_manual(values = correlation_data$colors) +
    scale_fill_manual(values = correlation_data$colors) +
    scale_shape_manual(values = correlation_data$shapes) +
    scale_x_continuous(limits = c(-5.15, -3.75)) +
    xlab("Diet log-ratio") +
    ylab("Normalised bite rate (natural log)") +
    my_theme() +
    geom_text(check_overlap = TRUE, hjust = 0,
              nudge_x = 0.05, show.legend = FALSE,
              colour = "black")
  my_lab <- deparse(substitute(italic("r") == a * " (N.S.)",
                               list(a = unname(round(logratios_correlation,
                                                     2)))))
  g1 +
    gg_relative_text(g1, px = 0.95, py = 0.95,
                     my_lab, size = 4, hjust = 1,
                     parse = TRUE)
}

make_fig_s_1to3 <- function(dest, logratios = FALSE, adjust = FALSE, ...) {
  if (logratios) {
    ggplot2::ggsave(dest, fig_s_1to3(..., logratios = logratios),
                    device = "pdf", width = (14.37 / 4) * 3,
                    height = 3.67, units = "in", onefile = FALSE,
                    useDingbats = FALSE)
  } else {
    ggplot2::ggsave(dest, fig_s_1to3(..., adjust = adjust),
                    device = "pdf", width = 14.37,
                    height = 3.67, units = "in",
                    onefile = FALSE, useDingbats = FALSE)
  }
}

fig_s_1to3 <- function(data, model, x, logratios = FALSE, adjust = FALSE) {
  my_theme <- function() {
    theme_bw() +
      theme(plot.margin = unit(c(0.2, 0.1, 0.4, 0.2), "in"),
            panel.grid = element_blank(),
            axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
            axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
            axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
            axis.text.y = element_text(size = 10))
  }

  get_response <- function(model) {
    strsplit(as.character(model$formula)[1], " ~ ", fixed  = TRUE)[[1]][1]
  }

  y_rep <- brms::posterior_predict(model)
  resp <- get_response(model)
  y <- model$data[[resp]]
  bayesplot::color_scheme_set("gray")
  # for p_c
  my_colors <- make_aes_vec(data, "colors")
  my_shapes <- make_aes_vec(data, "shapes")
  s_dat <- data.frame(y = y, y_rep = colMeans(y_rep),
                      group = data$local,
                      stringsAsFactors = FALSE)

  p_a <- bayesplot::ppc_dens_overlay(y, y_rep[1:200, ]) +
    my_theme() +
    theme(legend.text = element_blank(),
          legend.position = c(0.08, 0.93),
          legend.background = element_blank(),
          legend.key.height = unit(0.05, "npc")) +
    labs(y = "Density", x = "Response")

  if (adjust) {
    p_a <- p_a +
      scale_y_continuous(limits = c(0, 0.35))
  }
  p_a <- p_a +
    gg_relative_text(p_a, px = 0.12, py = 0.92,
                     "Observed", size = 3, hjust = 0) +
    gg_relative_text(p_a, px = 0.12, py = 0.86,
                     "Predicted", size = 3, hjust = 0)

  p_b <- ggplot(data = s_dat, mapping = aes(x = y_rep)) +
    geom_histogram(colour = "black", fill = "grey60", size = 0.2) +
    geom_vline(mapping = aes(xintercept = mean(y)),
               color = "black", size = 0.5,
               lty = 2) +
    labs(x = "Posterior predicted response", y = "Frequency") +
    my_theme() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 18, hjust = 0),
          legend.position = c(0.8, 0.8),
          legend.background = element_blank())

  p_c <- ggplot(data = s_dat) +
    geom_smooth(mapping = aes(x = y, y = y),
                method = "lm", se = FALSE, lty = 2,
                colour = "black", size = 0.5) +
    geom_point(mapping = aes(x = y, y = y_rep,
                             fill = group, shape = group,
                             colour = group),
               size = 2) +
    scale_fill_manual(values = alpha(my_colors, 0.8)) +
    scale_colour_manual(values = my_colors) +
    scale_shape_manual(values = my_shapes) +
    labs(y = "Predicted", x = "Observed") +
    my_theme() +
    theme(legend.position = "none")

  if (logratios) {
    gridExtra::grid.arrange(p_a, p_b, p_c, ncol = 3)
  } else {
    x <- data[[x]]
    p_d <- bayesplot::ppc_intervals(y = y, yrep = y_rep, x = x, prob = 0.5) +
      labs(x = "ln Body mass (g)", y = "Response") +
      my_theme() +
      theme(legend.text = element_blank(),
            legend.position = c(0.75, 0.93),
            legend.background = element_blank(),
            legend.key.height = unit(0.05, "npc"))
    p_d <- p_d +
      gg_relative_text(p_d, px = 0.78, py = 0.925,
                       "Observed", size = 3, hjust = 0) +
      gg_relative_text(p_d, px = 0.78, py = 0.855,
                       "Predicted", size = 3, hjust = 0)
    gridExtra::grid.arrange(p_a, p_b, p_c, p_d, ncol = 4)
  }
}

make_fig_s_4 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_s_4(...), device = "pdf",
                  width = 10, height = 5, units = "in",
                  onefile = FALSE, useDingbats = FALSE)
}

fig_s_4 <- function(bites_data) {
  bites_data <- bites_data %>%
    dplyr::mutate(bites_min = bites_original / obs_time,
                  local_original = as.factor(local_original),
                  local_original = dplyr::recode(local_original,
                                                 ascension_island = "Ascension",
                                                 aspsp = "SPSPA",
                                                 atol_das_rocas = "Rocas",
                                                 bahia = "Salvador",
                                                 bocas_del_toro = "Bocas",
                                                 fernando_de_noronha = "Noronha",
                                                 principe_island = "Principe",
                                                 santa_catarina.sum = "SC summer",
                                                 santa_catarina.win = "SC winter",
                                                 .default = levels(local_original)))
  mean_bites <- bites_data %>%
    dplyr::group_by(local_original) %>%
    dplyr::summarise("mean" = mean(bites_min),
                     "error" = sd(bites_min) / sqrt(dplyr::n())) %>%
    as.data.frame()

  ggplot(data = bites_data) +
    geom_boxplot(mapping = aes(x = local_original,
                               y = bites_min),
                 outlier.shape = NA,
                 notch = TRUE) +
    geom_jitter(mapping = aes(x = local_original,
                              y = bites_min),
                fill = alpha(bites_data$colors, 0.8),
                colour = bites_data$colors,
                shape = bites_data$shapes,
                size = 4,
                position = position_jitter(0.1, 0),
                stroke = 0.3) +
    scale_x_discrete(limits = c("Noronha", "Rocas", "Salvador",
                                "SPSPA", "Principe", "SC summer",
                                "SC winter", "Bocas", "Ascension")) +
    xlab(label = NULL) +
    ylab(label = "Bites / min") +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 12, angle = 0,
                                     vjust = 0.5, hjust = 0.5,
                                     face = "plain"),
          axis.text.y = element_text(colour = "black",
                                     size = 12, angle = 0,
                                     vjust = 0.5, face = "plain"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, color = "black"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.line = element_line(colour = "black", size = 0.5,
                                   linetype = "solid"),
          strip.background.x = element_rect(color = "black"),
          strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
          axis.ticks.x = element_line(size = 0.5, linetype = "solid",
                                      color = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          axis.ticks.y = element_line(size = 0.5, linetype = "solid",
                                      color = "black"),
          legend.position = "") +
    geom_errorbar(data = mean_bites,
                  mapping = aes(x = local_original,
                                ymax = mean + error,
                                ymin = mean - error),
                  width = 0, size = 1.2,
                  color = "black") +
    geom_point(data = mean_bites,
               mapping = aes(x = local_original, y = mean),
               shape = 21, size = 2.5,
               colour = "black",
               fill = "white",
               stroke = 1.5)
}

make_fig_s_5 <- function(dest, ...) {
  ggplot2::ggsave(dest, fig_s_5(...), device = "pdf", width = 5,
                  height = 5, units = "in", onefile = FALSE,
                  useDingbats = FALSE)
}

fig_s_5 <- function(intestine_data) {
  intestine_data <- intestine_data %>%
    dplyr::mutate(local = as.factor(local),
                  local = dplyr::recode(local,
                                        aspsp = "SPSPA",
                                        atol_das_rocas = "Rocas",
                                        bahia = "Salvador",
                                        principe_island = "Principe",
                                        santa_catarina = "St Catarina",
                                        .default = levels(local)))
  mean_ints <- intestine_data %>%
    dplyr::group_by(local) %>%
    dplyr::summarise("mean" = mean(QI),
                     "error" = sd(QI) / sqrt(n())) %>%
    as.data.frame()
  ggplot(data = intestine_data) +
    geom_hline(yintercept = c(1, 3), linetype = "dashed") +
    geom_boxplot(mapping = aes(x = local, y = QI),
                 outlier.shape = NA, notch = FALSE) +
    geom_jitter(mapping = aes(x = local, y = QI),
                fill = alpha(intestine_data$colors, 0.8),
                colour = intestine_data$colors,
                shape = intestine_data$shapes,
                size = 4,
                position = position_jitter(0.1, 0),
                stroke = 0.3) +
    scale_x_discrete(limits = c("SPSPA", "Rocas",
                                "Salvador", "St Catarina",
                                "Principe")) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4)) +
    xlab(label = NULL) +
    ylab(label = "Intestinal Coefficient") +
    theme(axis.text.x = element_text(colour = "black",
                                     size = 12, angle = 0,
                                     vjust = 0.5, hjust = 0.5,
                                     face = "plain"),
          axis.text.y = element_text(colour = "black",
                                     size = 12, angle = 0,
                                     vjust = 0.5,
                                     face = "plain"),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = NA, color = "black"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.line = element_line(colour = "black",
                                   size = 0.5,
                                   linetype = "solid"),
          strip.background.x = element_rect(color = "black"),
          strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
          axis.ticks.x = element_line(size = 0.5,
                                      linetype = "solid",
                                      color = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          axis.ticks.y = element_line(size = 0.5,
                                      linetype = "solid",
                                      color = "black"),
          legend.position = "") +
    geom_errorbar(data = mean_ints,
                  mapping = aes(x = local,
                                ymax = mean + error,
                                ymin = mean - error),
                  width = 0, size = 1.2,
                  color = "black") +
    geom_point(data = mean_ints,
               mapping = aes(x = local, y = mean),
               shape = 21, size = 3,
               colour = "black",
               fill = "white",
               stroke = 1.5)
}
