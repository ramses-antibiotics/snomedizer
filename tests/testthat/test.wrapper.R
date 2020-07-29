

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
})



# concepts_descendants ----------------------------------------------------

test_that("concepts_descendants", {

  infections <- concepts_descendants(conceptIds = c("233604007", "68566005"),
                                     direct_descendants = TRUE, activeFilter = TRUE)
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
  infection_descriptions <- concepts_descriptions(conceptIds =  c("233604007",
                                                                  "68566005"))
  expect_true("Pneumonia" %in% infection_descriptions$`233604007`$term)
  expect_true("Urinary tract infectious disease" %in% infection_descriptions$`68566005`$term)
  expect_null(concepts_descriptions("")[[1]])
  expect_warning(concepts_descriptions(conceptIds = "233604007", limit = 1))
})

# release_version ---------------------------------------------------------

test_that("release_version", {
  ct_version <- release_version()
  expect_false(is.na(ct_version$rf2_date))
  expect_false(is.na(ct_version$rf2_month_year))
})
