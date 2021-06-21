# Predicting the effects of body size, temperature and diet on animal feeding rates

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
\!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/dbarneche/redlip_bites/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Nunes LT<sup>†</sup>, Barneche DR<sup>†</sup>, Lastrucci NS, Fraga AA, Nunes JACC, Ferreira CEL, Floeter SR** (2021) Predicting the effects of body size, temperature and diet on animal feeding rates. *Functional Ecology* (accepted).

**When using the data or code from this project, please cite it as:**

**Nunes LT<sup>†</sup>, Barneche DR<sup>†</sup>, Lastrucci NS, Fraga AA, Nunes JACC, Ferreira CEL, Floeter SR** (2021) dbarneche/redlip_bites: Accepted version of paper data and code of manuscript: Predicting the effects of body size, temperature and diet on animal feeding rates (Functional Ecology). *Zenodo*. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5003045.svg)](https://doi.org/10.5281/zenodo.5003045.svg)

<sup>†</sup> Equal authorship

## Instructions

All processing was done in `R`. This routine uses the [drake R package](https://github.com/ropensci/drake) to compile the output time table. First install `drake`:

```r
install.packages("drake")
```

Next you need to open an R session with working directory set to the root of the project.

This routine loads multiple packages which are found in `R/packages.R`, so make sure to successfully install and load them before running drake.

Then, to reproduce the data analyses and figures, do:

```r
source("make.R")
drake::loadd()
```

All output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

Also notice that all the combined Bayesian models in this paper may take 20--30 minutes to run on a regular computer.

### This paper was produced using the following software and associated packages:
```
R version 4.1.0 (2021-05-18)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] LoLinR_0.0.0.9000       png_0.1-7               gridExtra_2.3           bayesplot_1.8.0         rerddap_0.7.4          
 [6] rnaturalearthdata_0.1.0 rnaturalearth_0.1.0     sf_0.9-8                rgeos_0.5-5             sp_1.4-5               
[11] patchwork_1.1.1         forcats_0.5.1           stringr_1.4.0           dplyr_1.0.6             purrr_0.3.4            
[16] readr_1.4.0             tidyr_1.1.3             tibble_3.1.2            ggplot2_3.3.3           tidyverse_1.3.1        
[21] plyr_1.8.6              drake_7.13.2            brms_2.15.0             Rcpp_1.0.6             

loaded via a namespace (and not attached):
  [1] readxl_1.3.1         backports_1.2.1      igraph_1.2.6         splines_4.1.0        storr_1.2.5          crosstalk_1.1.1     
  [7] TH.data_1.0-10       rstantools_2.1.1     inline_0.3.18        digest_0.6.27        htmltools_0.5.1.1    rsconnect_0.8.18    
 [13] fansi_0.5.0          magrittr_2.0.1       base64url_1.4        modelr_0.1.8         RcppParallel_5.1.4   matrixStats_0.58.0  
 [19] xts_0.12.1           sandwich_3.0-1       prettyunits_1.1.1    colorspace_2.0-1     rappdirs_0.3.3       rvest_1.0.0         
 [25] hoardr_0.5.2         haven_2.4.1          callr_3.7.0          crayon_1.4.1         jsonlite_1.7.2       lme4_1.1-27         
 [31] survival_3.2-11      zoo_1.8-9            glue_1.4.2           gtable_0.3.0         emmeans_1.6.0        V8_3.4.2            
 [37] pkgbuild_1.2.0       rstan_2.21.3         abind_1.4-5          scales_1.1.1         mvtnorm_1.1-1        DBI_1.1.1           
 [43] miniUI_0.1.1.1       xtable_1.8-4         progress_1.2.2       units_0.7-1          proxy_0.4-25         txtq_0.2.4          
 [49] stats4_4.1.0         StanHeaders_2.21.0-7 DT_0.18              htmlwidgets_1.5.3    httr_1.4.2           threejs_0.3.3       
 [55] ellipsis_0.3.2       pkgconfig_2.0.3      loo_2.4.1            dbplyr_2.1.1         crul_1.1.0           utf8_1.2.1          
 [61] tidyselect_1.1.1     rlang_0.4.11         reshape2_1.4.4       later_1.2.0          munsell_0.5.0        cellranger_1.1.0    
 [67] tools_4.1.0          cli_2.5.0            generics_0.1.0       broom_0.7.6          ggridges_0.5.3       fastmap_1.1.0       
 [73] processx_3.5.2       fs_1.5.0             ncdf4_1.17           nlme_3.1-152         mime_0.10            projpred_2.0.2      
 [79] xml2_1.3.2           compiler_4.1.0       shinythemes_1.2.0    rstudioapi_0.13      filelock_1.0.2       curl_4.3.1          
 [85] gamm4_0.2-6          e1071_1.7-7          reprex_2.0.0         stringi_1.6.2        ps_1.6.0             Brobdingnag_1.2-6   
 [91] lattice_0.20-44      Matrix_1.3-3         classInt_0.4-3       nloptr_1.2.2.2       markdown_1.1         shinyjs_2.0.0       
 [97] vctrs_0.3.8          pillar_1.6.1         lifecycle_1.0.0      lmtest_0.9-38        bridgesampling_1.1-2 estimability_1.3    
[103] data.table_1.14.0    httpuv_1.6.1         R6_2.5.0             promises_1.2.0.1     KernSmooth_2.23-20   codetools_0.2-18    
[109] boot_1.3-28          colourpicker_1.1.0   MASS_7.3-54          gtools_3.8.2         assertthat_0.2.1     httpcode_0.3.0      
[115] withr_2.4.2          shinystan_2.5.0      multcomp_1.4-17      mgcv_1.8-35          parallel_4.1.0       hms_1.1.0           
[121] class_7.3-19         coda_0.19-4          minqa_1.2.4          shiny_1.6.0          lubridate_1.7.10     base64enc_0.1-3     
[127] dygraphs_1.1.1.6    
```

## License

This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please [report any issues or bugs](https://github.com/dbarneche/redlip_bites/issues).
