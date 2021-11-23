
test_that("concepts_ancestors (order preserving)", {
  infections <- concepts_ancestors(conceptIds = c("68566005", "233604007"),
                                   silent = TRUE)
  expect_equal(names(infections),
               c("68566005", "233604007"))
  infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"),
                                   silent = TRUE)
  expect_equal(names(infections),
               c("233604007", "68566005"))
})

test_that("concepts_ancestors ", {
  infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"),
                                   silent = FALSE)
  expect_true(
    "205237003" %in% #pneumonitis
      infections[[1]]$conceptId
  )
  expect_false(
    "53084003" %in% #bacterial pneumonia
      infections[[1]]$conceptId
  )
  expect_false(
    "40733004" %in% #Infectious disease
      infections[[1]]$conceptId
  )
  expect_true(
    "40733004" %in% #Infectious disease
      infections[[2]]$conceptId
  )
  expect_warning(concepts_ancestors(conceptIds = c("233604007", "68566005"), limit = 2))
  expect_is(
    infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"), limit = 300),
    "list"
  )

  expect_false("68566005" %in% infections$`68566005`$conceptId)
  expect_false("233604007" %in% infections$`233604007`$conceptId)
})


test_that("concepts_ancestors (include_self)", {
  infections <- concepts_ancestors(conceptIds = c("68566005", "233604007"),
                                   include_self = TRUE,
                                   silent = TRUE)
  expect_true("68566005" %in% infections$`68566005`$conceptId)
  expect_true("233604007" %in% infections$`233604007`$conceptId)
})
