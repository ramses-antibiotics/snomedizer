
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

You can install the development version from
[GitHub](https://github.com/ramses-antibiotics/snomedizer) with:

``` r
install.packages("devtools")
devtools::install_github("ramses-antibiotics/snomedizer")
```

## Example

`snomedizer` provides a direct interface to the SNOMED CT Terminology
Server REST API.

By default, the package uses the official [IHTSDO API
endpoint](https://browser.ihtsdotools.org/snowstorm/snomed-ct/), subject
to the [SNOMED International SNOMED CT Browser License
Agreement](https://browser.ihtsdotools.org/) (restricted to reference
purposes).

For example,  is implemented in `api_concepts()`:

``` r
library(snomedizer)
api_concepts(term = "pneumonia", activeFilter = TRUE)
#> Response [https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct/MAIN/concepts?term=pneumonia&limit=50&offset=0&activeFilter=TRUE]
#>   Date: 2020-07-19 17:05
#>   Status: 200
#>   Content-Type: application/json;charset=UTF-8
#>   Size: 18.4 kB
#> {
#>   "items" : [ {
#>     "conceptId" : "233604007",
#>     "active" : true,
#>     "definitionStatus" : "FULLY_DEFINED",
#>     "moduleId" : "900000000000207008",
#>     "effectiveTime" : "20150131",
#>     "fsn" : {
#>       "term" : "Pneumonia (disorder)",
#>       "lang" : "en"
#> ...
```

Simpler wrapper functions are available for common operations, which
provide results as data frames:

``` r
concepts_find(term = "pneumonia", limit = 5) %>% 
  dplyr::select(conceptId, fsn.term, pt.term) 
#> Warning: 
#> This server request returned just 5 of a total 562 results.
#> Please increase the server `limit` to fetch all results.
#>   conceptId                         fsn.term            pt.term
#> 1 233604007             Pneumonia (disorder)          Pneumonia
#> 2 161525004 History of pneumonia (situation)     H/O: pneumonia
#> 3 416916004    Lipoid pneumonitis (disorder) Lipoid pneumonitis
#> 4 300999006       Basal pneumonia (disorder)    Basal pneumonia
#> 5 278516003       Lobar pneumonia (disorder)    Lobar pneumonia
```

## Code of conduct

The `snomedizer` project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
