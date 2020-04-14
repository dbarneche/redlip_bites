# Title TBD

This repository contains code and data needed to reproduce the article:

**Nunes LT, Barneche DR, Lastrucci NS, Fraga AA, Nunes JACC, Ferreira CEL, Floeter SR**, Title TBD (in prep)

## Instructions

All processing was done in `R`. This routine uses the [drake R package](https://github.com/ropensci/drake) to compile the output time table. First install `drake`:

```r
install.packages('drake')
```

Next you need to open an R session with working directory set to the root of the project.

This routine loads multiple packages which are found in `R/packages.R`, so make sure to successfully install and load them before running drake.

Then, to reproduce the data analyses and figures, do:

```r
source('make.R')
drake::loadd()
```

All output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

Also notice that all the combined Bayesian models in this paper may take 20--30 minutes to run on a regular computer.

### This paper was produced using the following software and associated packages:
```
R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tidyselect_1.0.0  gridExtra_2.3     bayesplot_1.7.1   ggplot2_3.3.0     tidyr_1.0.2       dplyr_0.8.4      
 [7] plyr_1.8.6        LoLinR_0.0.0.9000 brms_2.12.0       Rcpp_1.0.3        drake_7.11.0     

loaded via a namespace (and not attached):
 [1] Brobdingnag_1.2-6    gtools_3.8.1         StanHeaders_2.21.0-1 threejs_0.3.3        shiny_1.4.0         
 [6] assertthat_0.2.1     stats4_3.6.1         base64url_1.4        progress_1.2.2       pillar_1.4.3        
[11] backports_1.1.5      lattice_0.20-40      glue_1.3.1           digest_0.6.25        promises_1.1.0      
[16] colorspace_1.4-1     htmltools_0.4.0      httpuv_1.5.2         Matrix_1.2-18        dygraphs_1.1.1.6    
[21] pkgconfig_2.0.3      rstan_2.19.3         purrr_0.3.3          xtable_1.8-4         mvtnorm_1.1-0       
[26] scales_1.1.0         processx_3.4.2       later_1.0.0          tibble_2.1.3         txtq_0.2.0          
[31] farver_2.0.3         DT_0.12              withr_2.1.2          shinyjs_1.1          cli_2.0.2           
[36] magrittr_1.5         crayon_1.3.4         mime_0.9             ps_1.3.2             storr_1.2.1         
[41] fansi_0.4.1          nlme_3.1-145         xts_0.12-0           pkgbuild_1.0.6       colourpicker_1.0    
[46] rsconnect_0.8.16     tools_3.6.1          loo_2.2.0            prettyunits_1.1.1    hms_0.5.3           
[51] lifecycle_0.1.0      matrixStats_0.55.0   stringr_1.4.0        munsell_0.5.0        callr_3.4.2         
[56] compiler_3.6.1       rlang_0.4.5          ggridges_0.5.2       htmlwidgets_1.5.1    crosstalk_1.0.0     
[61] igraph_1.2.4.2       miniUI_0.1.1.1       labeling_0.3         base64enc_0.1-3      gtable_0.3.0        
[66] inline_0.3.15        abind_1.4-5          markdown_1.1         reshape2_1.4.3       R6_2.4.1            
[71] rstantools_2.0.0     zoo_1.8-7            bridgesampling_1.0-0 utf8_1.1.4           fastmap_1.0.1       
[76] shinystan_2.5.0      filelock_1.0.2       shinythemes_1.1.2    stringi_1.4.6        parallel_3.6.1      
[81] vctrs_0.2.3          lmtest_0.9-37        coda_0.19-3         
```

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please [report any issues or bugs](https://github.com/dbarneche/redlip_bites/issues).
