

# concepts_find -----------------------------------------------------------

test_that("concepts_find", {

  expect_warning(asthma_concepts <- concepts_find(term = "asthma"))
  expect_true("195967001" %in% asthma_concepts$conceptId)

  asthma_concepts <- concepts_find(conceptIds = "195967001")
  expect_true(dim(asthma_concepts)[1] == 1)
  expect_equal(asthma_concepts$fsn.term, "Asthma (disorder)")

  infection_concepts <- concepts_find(conceptIds =  c("233604007", "68566005"))
  expect_setequal(infection_concepts$conceptId, c("233604007", "68566005"))

  ecl_query_concept <- concepts_find(
    ecl = paste(
      "<! 68566005 | Urinary tract infectious disease (disorder) |",
      "AND",
      "< 87628006 | Bacterial infectious disease (disorder) |"
    )
  )
  expect_equal(ecl_query_concept$conceptId, "312124009")

  # test when both ecl and conceptIds are provided
  disjoint <- concepts_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("233604007", # Pneumonia
                   "40733004")
  )
  expect_equal(disjoint$conceptId, "40733004")

  included <- concepts_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("312342009", # INFECTIVE pneumonia
                   "40733004")
  )
  expect_equal(sort(included$conceptId),
               c("312342009", "40733004"))
})

test_that("concepts_find (batch)", {
  concepts <- concepts_find(ecl = "<233604007", limit = 300)
  concepts_batch <- concepts_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = TRUE
  )
  concepts_batch <- concepts_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = FALSE
  )
  expect_equal(sort(concepts$conceptId),
               sort(concepts_batch$conceptId))
})

# concepts_descendants ----------------------------------------------------

test_that("concepts_descendants", {

  infections <- concepts_descendants(conceptIds = c("233604007", "68566005"),
                                     direct_descendants = TRUE,
                                     activeFilter = TRUE,
                                     silent = TRUE)
  infections <- concepts_descendants(conceptIds = c("233604007", "68566005"),
                                     direct_descendants = TRUE,
                                     activeFilter = TRUE,
                                     silent = FALSE)
  expect_false("882784691000119100" %in% infections$`233604007`$conceptId)
  expect_false("1469007" %in% infections$`68566005`$conceptId)
  expect_true("422747000" %in% infections$`68566005`$conceptId)
  expect_warning(concepts_descendants(conceptIds = c("233604007", "68566005"), limit = 2))
  infections <- concepts_descendants(conceptIds = c("233604007", "68566005"), limit = 300)
  expect_true("882784691000119100" %in% infections$`233604007`$conceptId)

  expect_warning(concepts_descendants(conceptIds = "blurgh"))
})

# concepts_descriptions ---------------------------------------------------

test_that("concepts_descriptions", {
  infection_descriptions <- concepts_descriptions(
    conceptIds =  c("68566005", "233604007")
  )
  expect_equal(
    names(infection_descriptions),
    c("233604007", "68566005")
  )
  expect_true("Pneumonia" %in% infection_descriptions[["233604007"]]$term)
  expect_true("Urinary tract infectious disease" %in% infection_descriptions[["68566005"]]$term)
  expect_error(concepts_descriptions(""))
})

test_that("concepts_descriptions (batch)", {

  concepts <- concepts_find(ecl = "<233604007", limit = 300)

  infection_descriptions <- concepts_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = TRUE
  )
  infection_descriptions <- concepts_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = FALSE
  )
  expect_equal(
    sort(names(infection_descriptions)),
    sort(concepts$conceptId)
  )

})

# release_version ---------------------------------------------------------

test_that("release_version", {
  ct_version <- release_version(branch = "MAIN/2021-07-31")
  expect_equal(ct_version$rf2_date, "20210731")
  expect_equal(ct_version$rf2_month_year, "July 2021")
})

