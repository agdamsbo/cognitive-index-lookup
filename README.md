
# cognitive.index.lookup

<!-- badges: start -->
[![pages-build-deployment](https://github.com/agdamsbo/cognitive.index.lookup/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/agdamsbo/cognitive.index.lookup/actions/workflows/pages/pages-build-deployment)
<!-- badges: end -->

Cognitive index score for specific assessment tool.

The tool has been implemented with [shinylive for `R`](https://posit-dev.github.io/r-shinylive/) to run live in the browser without any remote data storage. It is available here: [https://agdamsbo.github.io/cognitive.index.lookup/](https://agdamsbo.github.io/cognitive.index.lookup/). Please note that the app runs with a little delay as it runs purely in the browser including all the computations and plotting.

## Installation an example use

To use the package and run the shiny-app locally, you can install the development version of cognitive.index.lookup with:

``` r
# install.packages("devtools")
devtools::install_github("agdamsbo/cognitive-index-lookup")
```

Launch the included shiny app with the following code:

``` r
library(cognitive.index.lookup)
shiny_index()
```

## Code of Conduct

Please note that the cognitive.index.lookup project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
