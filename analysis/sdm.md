
<!-- # Obesity model estimation -->

Create a spatial weights matrix. It’s showing that we have three regions
with no links, so we need to ensure that these are estimated in the
model.

``` r
library(tidyverse)
library(geojsonio)
library(tidyverse)
library(maptools)
library(rgdal)
library(rgeos)
library(spdep)
library(texreg)
library(leaflet)
library(sf)
library(VGAM)
```

``` r
# tracts %>% class
# neighbors <- poly2nb(tracts, queen = TRUE)
# neighbors %>% write_rds("../output/neighbors.rds")
neighbors <- read_rds("../output/neighbors.rds")
neighbors %>% class
```

    ## [1] "nb"

``` r
W <- nb2listw(neighbours = neighbors, zero.policy = TRUE)
print(W, zero.policy = TRUE)
```

    ## Characteristics of weights list object:
    ## Neighbour list object:
    ## Number of regions: 2102 
    ## Number of nonzero links: 12558 
    ## Percentage nonzero weights: 0.2842203 
    ## Average number of links: 5.97431 
    ## 3 regions with no links:
    ## 392 553 1602
    ## 
    ## Weights style: W 
    ## Weights constants summary:
    ##      n      nn   S0       S1       S2
    ## W 2099 4405801 2099 738.2911 8573.701

``` r
trMC <- trW(as(W, "CsparseMatrix"), type="MC") # trace used in montecarlo impacts
```

    ## Warning: Function trW moved to the spatialreg package

    ## Registered S3 methods overwritten by 'spatialreg':
    ##   method                   from 
    ##   residuals.stsls          spdep
    ##   deviance.stsls           spdep
    ##   coef.stsls               spdep
    ##   print.stsls              spdep
    ##   summary.stsls            spdep
    ##   print.summary.stsls      spdep
    ##   residuals.gmsar          spdep
    ##   deviance.gmsar           spdep
    ##   coef.gmsar               spdep
    ##   fitted.gmsar             spdep
    ##   print.gmsar              spdep
    ##   summary.gmsar            spdep
    ##   print.summary.gmsar      spdep
    ##   print.lagmess            spdep
    ##   summary.lagmess          spdep
    ##   print.summary.lagmess    spdep
    ##   residuals.lagmess        spdep
    ##   deviance.lagmess         spdep
    ##   coef.lagmess             spdep
    ##   fitted.lagmess           spdep
    ##   logLik.lagmess           spdep
    ##   fitted.SFResult          spdep
    ##   print.SFResult           spdep
    ##   fitted.ME_res            spdep
    ##   print.ME_res             spdep
    ##   print.lagImpact          spdep
    ##   plot.lagImpact           spdep
    ##   summary.lagImpact        spdep
    ##   HPDinterval.lagImpact    spdep
    ##   print.summary.lagImpact  spdep
    ##   print.sarlm              spdep
    ##   summary.sarlm            spdep
    ##   residuals.sarlm          spdep
    ##   deviance.sarlm           spdep
    ##   coef.sarlm               spdep
    ##   vcov.sarlm               spdep
    ##   fitted.sarlm             spdep
    ##   logLik.sarlm             spdep
    ##   anova.sarlm              spdep
    ##   predict.sarlm            spdep
    ##   print.summary.sarlm      spdep
    ##   print.sarlm.pred         spdep
    ##   as.data.frame.sarlm.pred spdep
    ##   residuals.spautolm       spdep
    ##   deviance.spautolm        spdep
    ##   coef.spautolm            spdep
    ##   fitted.spautolm          spdep
    ##   print.spautolm           spdep
    ##   summary.spautolm         spdep
    ##   logLik.spautolm          spdep
    ##   print.summary.spautolm   spdep
    ##   print.WXImpact           spdep
    ##   summary.WXImpact         spdep
    ##   print.summary.WXImpact   spdep
    ##   predict.SLX              spdep

First, let’s estimate the base model as an LM, SDM, and SEM model

``` r
# tracts@data %>% write_rds("../output/tracts_data.rds")
data <- read_rds("../output/tracts_data.rds")
data %>% head()
```

    ## # A tibble: 6 x 34
    ##   GEOID OBESITY Park_Percent Phys_Act MENTAL Income1 Income2 Income3
    ##   <chr>   <dbl>        <dbl>    <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 3608~    25              0     27.9   11.6     7.7     1.4     8.6
    ## 2 3608~    32.9            0     36.6   16.5     8.2     6.5    15.1
    ## 3 3608~    23.8            0     26.4   10.7     1.1     0       6.6
    ## 4 3608~    28.9            0     34.3   14.8    12.3     5.7    13.6
    ## 5 3608~    25.6            0     29.4   13       4.9     4.9    15.6
    ## 6 3608~    25              0     33.4   12.9    10.1     2.8     9.6
    ## # ... with 26 more variables: Income4 <dbl>, Income5 <dbl>, Income6 <dbl>,
    ## #   Income7 <dbl>, Income8 <dbl>, Income9 <dbl>, Income10 <dbl>,
    ## #   Pop_Density <dbl>, FulltimeWork <dbl>, CollegeDeg <dbl>,
    ## #   Pct0to17 <dbl>, Pct18to29 <dbl>, Pct30to64 <dbl>, Pct65plus <dbl>,
    ## #   Single_Percent <dbl>, PctWhite <dbl>, PctBlack <dbl>, PctNative <dbl>,
    ## #   PctAsian <dbl>, PctPacific <dbl>, PctOther <dbl>, PctHispanic <dbl>,
    ## #   park_ls1 <dbl>, park_ls2 <dbl>, park_ls4 <dbl>, park_ls5 <dbl>

``` r
# Time consuming.
obese_base_lm <- lm(update(base_formula, OBESITY ~ .), data = data)
obese_base_sar <- lagsarlm(update(base_formula, OBESITY ~ .), 
                           data = data, listw = W, zero.policy = TRUE)
```

    ## Warning: Function lagsarlm moved to the spatialreg package

``` r
obese_base_sem <- errorsarlm(update(base_formula, OBESITY ~ .), 
                             data = data, listw = W, zero.policy = TRUE)
```

    ## Warning: Function errorsarlm moved to the spatialreg package

``` r
obese_base_sdm <- lagsarlm(update(base_formula, OBESITY ~ .), 
                           data = data, listw = W, zero.policy = TRUE, 
                           type = "mixed")
```

    ## Warning: Function lagsarlm moved to the spatialreg package

A likelihood ratio test reveals that the SEM is not preferred, so we use
the SDM only going forward.

``` r
lmtest::lrtest(obese_base_sem, obese_base_sdm)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: function (x, ...) 
    ## UseMethod("formula")
    ## Model 2: function (x, ...) 
    ## UseMethod("formula")
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1  14 -4034.9                         
    ## 2  25 -4009.1 11 51.634  3.175e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimate spatial models for the `Phys_Act` variable. Model 1 does not
include park access. Models 2 & 3 each use a different park access
variable with a different combination of parameters:

``` r
screenreg(obesity_models)
```

    ## 
    ## ==================================================================================================
    ##                       Base          Short, no tweets  Short, tweets  Long, no tweets  Long, tweets
    ## --------------------------------------------------------------------------------------------------
    ## (Intercept)              19.29 ***     19.51 ***         19.62 ***      19.08 ***        19.27 ***
    ##                          (0.99)        (0.98)            (0.99)         (0.99)           (0.99)   
    ## log(Pop_Density)          0.20 ***      0.21 ***          0.19 **        0.21 ***         0.19 ** 
    ##                          (0.06)        (0.06)            (0.06)         (0.06)           (0.06)   
    ## FulltimeWork             -0.05 ***     -0.05 ***         -0.05 ***      -0.05 ***        -0.05 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## CollegeDeg               -0.09 ***     -0.09 ***         -0.09 ***      -0.09 ***        -0.09 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## Single_Percent            0.07 ***      0.07 ***          0.07 ***       0.07 ***         0.07 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## Pct0to17                  0.10 ***      0.10 ***          0.10 ***       0.10 ***         0.10 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## Pct18to29                -0.07 ***     -0.07 ***         -0.07 ***      -0.07 ***        -0.07 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## Pct65plus                -0.01         -0.01             -0.01          -0.01            -0.01    
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## PctBlack                  0.08 ***      0.08 ***          0.08 ***       0.08 ***         0.08 ***
    ##                          (0.00)        (0.00)            (0.00)         (0.00)           (0.00)   
    ## PctAsian                 -0.05 ***     -0.05 ***         -0.05 ***      -0.05 ***        -0.05 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## PctOther                  0.02 **       0.02 **           0.02 **        0.02 **          0.02 ** 
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## PctHispanic               0.04 ***      0.04 ***          0.04 ***       0.04 ***         0.04 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.log(Pop_Density)     -0.72 ***     -0.65 ***         -0.79 ***      -0.70 ***        -0.71 ***
    ##                          (0.08)        (0.08)            (0.09)         (0.08)           (0.08)   
    ## lag.FulltimeWork         -0.03 ***     -0.04 ***         -0.03 **       -0.03 ***        -0.03 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.CollegeDeg            0.06 ***      0.06 ***          0.05 ***       0.06 ***         0.05 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.Single_Percent       -0.05 ***     -0.03 **          -0.05 ***      -0.04 ***        -0.05 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.Pct0to17             -0.15 ***     -0.14 ***         -0.15 ***      -0.14 ***        -0.15 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.Pct18to29            -0.06 ***     -0.07 ***         -0.05 ***      -0.06 ***        -0.06 ***
    ##                          (0.02)        (0.02)            (0.02)         (0.02)           (0.02)   
    ## lag.Pct65plus            -0.09 ***     -0.08 ***         -0.09 ***      -0.09 ***        -0.09 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.PctBlack             -0.05 ***     -0.05 ***         -0.05 ***      -0.05 ***        -0.05 ***
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.PctAsian             -0.00          0.00             -0.00          -0.00            -0.00    
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.PctOther              0.00          0.00             -0.00          -0.00            -0.00    
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## lag.PctHispanic          -0.02 **      -0.02 **          -0.02 **       -0.02 **         -0.02 ** 
    ##                          (0.01)        (0.01)            (0.01)         (0.01)           (0.01)   
    ## rho                       0.66 ***      0.64 ***          0.65 ***       0.65 ***         0.66 ***
    ##                          (0.02)        (0.02)            (0.02)         (0.02)           (0.02)   
    ## park_ls1                               -0.01                                                      
    ##                                        (0.13)                                                     
    ## lag.park_ls1                           -0.38 *                                                    
    ##                                        (0.16)                                                     
    ## park_ls2                                                 -0.03                                    
    ##                                                          (0.07)                                   
    ## lag.park_ls2                                              0.08                                    
    ##                                                          (0.08)                                   
    ## park_ls4                                                                -0.06                     
    ##                                                                         (0.03)                    
    ## lag.park_ls4                                                            -0.06                     
    ##                                                                         (0.06)                    
    ## park_ls5                                                                                 -0.03    
    ##                                                                                          (0.03)   
    ## lag.park_ls5                                                                              0.04    
    ##                                                                                          (0.03)   
    ## --------------------------------------------------------------------------------------------------
    ## Num. obs.              2102          2102              2102           2102             2102       
    ## Parameters               25            27                27             27               27       
    ## Log Likelihood        -4009.07      -3999.50          -4004.96       -4005.25         -4008.24    
    ## AIC (Linear model)     9063.87       8976.13           9060.00        8999.23          9061.00    
    ## AIC (Spatial model)    8068.13       8053.00           8063.92        8064.51          8070.49    
    ## LR test: statistic      997.74        925.13            998.09         936.72           992.51    
    ## LR test: p-value          0.00          0.00              0.00           0.00             0.00    
    ## ==================================================================================================
    ## *** p < 0.001, ** p < 0.01, * p < 0.05

Estimate the impacts for `obese_sdm_age`.

``` r
obesity_models %>% class
```

    ## [1] "list"

``` r
obesity_models
```

    ## $Base
    ## 
    ## Call:
    ## spatialreg::lagsarlm(formula = formula, data = data, listw = listw, 
    ##     na.action = na.action, Durbin = Durbin, type = type, method = method, 
    ##     quiet = quiet, zero.policy = zero.policy, interval = interval, 
    ##     tol.solve = tol.solve, trs = trs, control = control)
    ## Type: mixed 
    ## 
    ## Coefficients:
    ##                  rho          (Intercept)     log(Pop_Density) 
    ##         0.6554583386        19.2889572919         0.2031122246 
    ##         FulltimeWork           CollegeDeg       Single_Percent 
    ##        -0.0493573166        -0.0897726065         0.0712261916 
    ##             Pct0to17            Pct18to29            Pct65plus 
    ##         0.0965306553        -0.0712042124        -0.0125423634 
    ##             PctBlack             PctAsian             PctOther 
    ##         0.0793955679        -0.0469038965         0.0233942227 
    ##          PctHispanic lag.log(Pop_Density)     lag.FulltimeWork 
    ##         0.0391293199        -0.7181393514        -0.0305700075 
    ##       lag.CollegeDeg   lag.Single_Percent         lag.Pct0to17 
    ##         0.0552364873        -0.0465670704        -0.1469988875 
    ##        lag.Pct18to29        lag.Pct65plus         lag.PctBlack 
    ##        -0.0577753977        -0.0856822389        -0.0530527973 
    ##         lag.PctAsian         lag.PctOther      lag.PctHispanic 
    ##        -0.0015842098         0.0001383207        -0.0227248613 
    ## 
    ## Log likelihood: -4009.067 
    ## 
    ## $`Short, no tweets`
    ## 
    ## Call:
    ## spatialreg::lagsarlm(formula = formula, data = data, listw = listw, 
    ##     na.action = na.action, Durbin = Durbin, type = type, method = method, 
    ##     quiet = quiet, zero.policy = zero.policy, interval = interval, 
    ##     tol.solve = tol.solve, trs = trs, control = control)
    ## Type: mixed 
    ## 
    ## Coefficients:
    ##                  rho          (Intercept)     log(Pop_Density) 
    ##         0.6407314650        19.5057141686         0.2131392130 
    ##         FulltimeWork           CollegeDeg       Single_Percent 
    ##        -0.0519742022        -0.0895046501         0.0729982684 
    ##             Pct0to17            Pct18to29            Pct65plus 
    ##         0.0964642712        -0.0739784922        -0.0134285030 
    ##             PctBlack             PctAsian             PctOther 
    ##         0.0785115520        -0.0476054555         0.0229649295 
    ##          PctHispanic             park_ls1 lag.log(Pop_Density) 
    ##         0.0389553516        -0.0071394519        -0.6502855407 
    ##     lag.FulltimeWork       lag.CollegeDeg   lag.Single_Percent 
    ##        -0.0379844637         0.0596225361        -0.0315379491 
    ##         lag.Pct0to17        lag.Pct18to29        lag.Pct65plus 
    ##        -0.1359245134        -0.0678337767        -0.0802090789 
    ##         lag.PctBlack         lag.PctAsian         lag.PctOther 
    ##        -0.0514300235         0.0002045054         0.0006934537 
    ##      lag.PctHispanic         lag.park_ls1 
    ##        -0.0202565275        -0.3792594028 
    ## 
    ## Log likelihood: -3999.501 
    ## 
    ## $`Short, tweets`
    ## 
    ## Call:
    ## spatialreg::lagsarlm(formula = formula, data = data, listw = listw, 
    ##     na.action = na.action, Durbin = Durbin, type = type, method = method, 
    ##     quiet = quiet, zero.policy = zero.policy, interval = interval, 
    ##     tol.solve = tol.solve, trs = trs, control = control)
    ## Type: mixed 
    ## 
    ## Coefficients:
    ##                  rho          (Intercept)     log(Pop_Density) 
    ##         0.6548281441        19.6162049673         0.1886102228 
    ##         FulltimeWork           CollegeDeg       Single_Percent 
    ##        -0.0491637566        -0.0901717108         0.0699257023 
    ##             Pct0to17            Pct18to29            Pct65plus 
    ##         0.0952522433        -0.0704174326        -0.0129599613 
    ##             PctBlack             PctAsian             PctOther 
    ##         0.0797785159        -0.0468126977         0.0234290324 
    ##          PctHispanic             park_ls2 lag.log(Pop_Density) 
    ##         0.0392338227        -0.0289330752        -0.7936788612 
    ##     lag.FulltimeWork       lag.CollegeDeg   lag.Single_Percent 
    ##        -0.0262685881         0.0502507449        -0.0526866614 
    ##         lag.Pct0to17        lag.Pct18to29        lag.Pct65plus 
    ##        -0.1459855257        -0.0509607831        -0.0866998863 
    ##         lag.PctBlack         lag.PctAsian         lag.PctOther 
    ##        -0.0519264912        -0.0029031311        -0.0006310116 
    ##      lag.PctHispanic         lag.park_ls2 
    ##        -0.0236945989         0.0824512393 
    ## 
    ## Log likelihood: -4004.958 
    ## 
    ## $`Long, no tweets`
    ## 
    ## Call:
    ## spatialreg::lagsarlm(formula = formula, data = data, listw = listw, 
    ##     na.action = na.action, Durbin = Durbin, type = type, method = method, 
    ##     quiet = quiet, zero.policy = zero.policy, interval = interval, 
    ##     tol.solve = tol.solve, trs = trs, control = control)
    ## Type: mixed 
    ## 
    ## Coefficients:
    ##                  rho          (Intercept)     log(Pop_Density) 
    ##          0.648145597         19.078768531          0.209819276 
    ##         FulltimeWork           CollegeDeg       Single_Percent 
    ##         -0.050900771         -0.089597737          0.072202814 
    ##             Pct0to17            Pct18to29            Pct65plus 
    ##          0.096898455         -0.072663666         -0.011963160 
    ##             PctBlack             PctAsian             PctOther 
    ##          0.079192074         -0.046940310          0.023313707 
    ##          PctHispanic             park_ls4 lag.log(Pop_Density) 
    ##          0.039177876         -0.058803068         -0.696809056 
    ##     lag.FulltimeWork       lag.CollegeDeg   lag.Single_Percent 
    ##         -0.034128248          0.057004986         -0.039595423 
    ##         lag.Pct0to17        lag.Pct18to29        lag.Pct65plus 
    ##         -0.143296722         -0.063741355         -0.085340966 
    ##         lag.PctBlack         lag.PctAsian         lag.PctOther 
    ##         -0.052750242         -0.001564359         -0.000128001 
    ##      lag.PctHispanic         lag.park_ls4 
    ##         -0.021510680         -0.056510711 
    ## 
    ## Log likelihood: -4005.254 
    ## 
    ## $`Long, tweets`
    ## 
    ## Call:
    ## spatialreg::lagsarlm(formula = formula, data = data, listw = listw, 
    ##     na.action = na.action, Durbin = Durbin, type = type, method = method, 
    ##     quiet = quiet, zero.policy = zero.policy, interval = interval, 
    ##     tol.solve = tol.solve, trs = trs, control = control)
    ## Type: mixed 
    ## 
    ## Coefficients:
    ##                  rho          (Intercept)     log(Pop_Density) 
    ##         0.6571241390        19.2692649876         0.1943455221 
    ##         FulltimeWork           CollegeDeg       Single_Percent 
    ##        -0.0497087399        -0.0897148138         0.0712045420 
    ##             Pct0to17            Pct18to29            Pct65plus 
    ##         0.0962315723        -0.0712251773        -0.0121853620 
    ##             PctBlack             PctAsian             PctOther 
    ##         0.0796177698        -0.0467289905         0.0233381038 
    ##          PctHispanic             park_ls5 lag.log(Pop_Density) 
    ##         0.0392121925        -0.0312329091        -0.7142955311 
    ##     lag.FulltimeWork       lag.CollegeDeg   lag.Single_Percent 
    ##        -0.0292423474         0.0545468200        -0.0481299680 
    ##         lag.Pct0to17        lag.Pct18to29        lag.Pct65plus 
    ##        -0.1465934326        -0.0563915631        -0.0854946678 
    ##         lag.PctBlack         lag.PctAsian         lag.PctOther 
    ##        -0.0531102131        -0.0015208407        -0.0002107148 
    ##      lag.PctHispanic         lag.park_ls5 
    ##        -0.0227150108         0.0414808295 
    ## 
    ## Log likelihood: -4008.243

``` r
# add2pkg::create_desc()
devtools::load_all()
```

    ## Loading park_access_new

    ## Invalid DESCRIPTION:
    ## Malformed package name
    ## 
    ## See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    ## manual.

``` r
obesity_models %>%
  lapply(impacts_extractor) %>%
  bind_rows(.id = "model") %>%
  transmute(model, var, effect,
            output = paste(round(impact, 5),
                           gtools::stars.pval(`p-val`))) %>%
  spread(model, output)
```

    ## Warning: Method impacts.sarlm moved to the spatialreg package
    
    ## Warning: Method impacts.sarlm moved to the spatialreg package
    
    ## Warning: Method impacts.sarlm moved to the spatialreg package
    
    ## Warning: Method impacts.sarlm moved to the spatialreg package
    
    ## Warning: Method impacts.sarlm moved to the spatialreg package

    ## # A tibble: 45 x 7
    ##    var   effect Base  `Long, no tweet~ `Long, tweets` `Short, no twee~
    ##    <chr> <chr>  <chr> <chr>            <chr>          <chr>           
    ##  1 Coll~ direct -0.0~ -0.08999 ***     -0.09049 ***   -0.08931 ***    
    ##  2 Coll~ indir~ "-0.~ "-0.00215  "     "-0.01248  "   "0.00648  "     
    ##  3 Coll~ total  -0.1~ -0.09215 ***     -0.10297 ***   -0.08282 ***    
    ##  4 Full~ direct -0.0~ -0.06271 ***     -0.06116 ***   -0.06423 ***    
    ##  5 Full~ indir~ -0.1~ -0.1802 ***      -0.17027 ***   -0.18691 ***    
    ##  6 Full~ total  -0.2~ -0.2429 ***      -0.23143 ***   -0.25114 ***    
    ##  7 log(~ direct "0.0~ 0.10933 .        "0.08755  "    0.12417 *       
    ##  8 log(~ indir~ -1.5~ -1.49327 ***     -1.60768 ***   -1.34674 ***    
    ##  9 log(~ total  -1.4~ -1.38394 ***     -1.52013 ***   -1.22258 ***    
    ## 10 park~ direct <NA>  <NA>             <NA>           "-0.07402  "    
    ## # ... with 35 more rows, and 1 more variable: `Short, tweets` <chr>

<!-- # Estimate Physical Activity Models -->
