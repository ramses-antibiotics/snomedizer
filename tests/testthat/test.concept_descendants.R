test_that("concept_descendants", {

  infections <- concept_descendants(conceptIds = c("68566005", "233604007"),
                                     silent = TRUE,
                                     limit = 250)
  expect_true("882784691000119100" %in% infections$`233604007`$conceptId)
  expect_true("1469007" %in% infections$`68566005`$conceptId)
  expect_true("422747000" %in% infections$`68566005`$conceptId)
  expect_warning(concept_descendants(conceptIds = c("233604007", "68566005"), limit = 2))
  infections <- concept_descendants(conceptIds = c("233604007", "68566005"), limit = 300)
  expect_true("882784691000119100" %in% infections$`233604007`$conceptId)

  expect_warning(concept_descendants(conceptIds = "blurgh"))

  expect_false("68566005" %in% infections$`68566005`$conceptId)
  expect_false("233604007" %in% infections$`233604007`$conceptId)
})

test_that("concept_descendants (include_self)", {

  infections <- concept_descendants(conceptIds = c("68566005", "233604007"),
                                     include_self = TRUE,
                                     silent = TRUE,
                                     limit = 250)
  expect_true("68566005" %in% infections$`68566005`$conceptId)
  expect_true("233604007" %in% infections$`233604007`$conceptId)
})
