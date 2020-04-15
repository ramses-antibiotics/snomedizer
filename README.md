
<!-- README.md is generated from README.Rmd. Please edit that file -->

<h1>

<code>snomedizer</code>: R Interface to the SNOMED CT Terminology Server
REST API

</h1>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ramses-antibiotics/snomedizer.svg?branch=master)](https://travis-ci.org/ramses-antibiotics/snomedizer)
[![Codecov test
coverage](https://codecov.io/gh/ramses-antibiotics/snomedizer/branch/master/graph/badge.svg)](https://codecov.io/gh/ramses-antibiotics/snomedizer?branch=master)
<!-- badges: end -->

<p class="lead">

<code>snomedizer</code> is an R package to manipulate the SNOMED
clinical ontology using the SNOMED CT Terminology Server REST API
<https://github.com/IHTSDO/snowstorm>.

</p>

## Installation

<!-- 
You can install the released version of snomedizer from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("snomedizer")
```
-->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ramses-antibiotics/snomedizer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(snomedizer)
api_find_concepts(term = "pneumonia", activeFilter = TRUE)
#> Response [https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/MAIN/concepts?term=pneumonia]
#>   Date: 2020-04-15 16:06
#>   Status: 200
#>   Content-Type: application/json;charset=UTF-8
#>   Size: 18.4 kB
#> {
#>   "items" : [ {
#>     "conceptId" : "60363000",
#>     "active" : false,
#>     "definitionStatus" : "PRIMITIVE",
#>     "moduleId" : "900000000000207008",
#>     "effectiveTime" : "20020131",
#>     "fsn" : {
#>       "term" : "Pneumonia (disorder)",
#>       "lang" : "en"
#> ...
```

## Code of conduct

Please note that the ‘snomedizer’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
