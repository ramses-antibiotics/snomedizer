#!/usr/bin/Rscript
library(pkgdown)

override_data_authors <- function (pkg = ".") {
  pkg <- as_pkgdown(pkg)
  author_info <- data_author_info(pkg)
  all <- pkg %>% pkg_authors() %>% purrr::map(author_list,
                                              author_info)
  main <- all
  needs_page <- FALSE
  print_yaml(list(all = all, main = main, needs_page = needs_page))
}

override_build_authors <- function (pkg = ".") {
  pkg <- as_pkgdown(pkg)
  data <- list(pagetitle = "Authors", authors = data_authors(pkg)$all)
  render_page(pkg, "authors", data, "authors.html")
}

envpkgd <- getNamespace("pkgdown")
R.utils::reassignInPackage("data_authors", "pkgdown", override_data_authors, keepOld=F)

swap_render_fun <- function() {
  # adapted from https://github.com/crew102/slowraker/blob/146f442085f652d824177e91fd1c38b29802b621/inst/site/build-site.R

  # Alter pkgdown:::build_rmarkdown_format
  build_rmarkdown_format2 <- function(pkg = ".",
                                      name,
                                      depth = 1L,
                                      data = list(),
                                      toc = TRUE) {
    template <- pkgdown:::rmarkdown_template(pkg, name, depth = depth, data = data)

    out <- rmarkdown::html_document(
      toc = toc,
      toc_depth = rlang::`%||%`(pkg$meta$toc$depth, 2),
      self_contained = FALSE,
      theme = NULL,
      template = template$path,
      df_print = "tibble"
    )
    out$knitr$opts_chunk <- pkgdown:::fig_opts_chunk(pkg$figures, out$knitr$opts_chunk)

    attr(out, "__cleanup") <- template$cleanup

    out
  }
  assignInNamespace(
    "build_rmarkdown_format", build_rmarkdown_format2, ns = "pkgdown"
  )
}

build_site <- function(...) {

  # Change render function in pkgdown so it uses paged df printing
  swap_render_fun()

  # Build site
  pkgdown::build_site(..., new_process = FALSE)
}

build_site()
