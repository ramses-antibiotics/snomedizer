test_that("concepts_included_in", {

  concepts <- dplyr::tibble(
    concept_id = c("407671000",
                   "422747000",
                   "27658006",
                   "39732311000001104", # unknown to MAIN branch (International Edition)
                   NA_character_),
    concep_fsn = c("Bilateral pneumonia (disorder)",
                   "Upper urinary tract infection (disorder)",
                   "Product containing amoxicillin (medicinal product)",
                   "Amoxicillin 250mg capsules (product)", # in UK edition only
                   NA_character_),
    expect_pneumonia = c(T, F, F, NA, NA),
    expect_uti = c(F, T, F, NA, NA),
    expect_medicine = c(F, F, T, NA, NA)
  )

  concepts[["is_pneumonia"]] <- concepts_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "233604007" # Pneumonia (disorder)
  )
  expect_equal(
    concepts[["is_pneumonia"]],
    concepts[["expect_pneumonia"]]
  )
  concepts[["is_uti"]] <- concepts_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "68566005" # Urinary tract infectious disease (disorder)
  )
  expect_equal(
    concepts[["is_uti"]],
    concepts[["expect_uti"]]
  )
  concepts[["is_medicine"]] <- concepts_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "763158003", # Medicinal product (product)
    branch = "MAIN"
  )
  expect_equal(
    concepts[["is_medicine"]],
    concepts[["expect_medicine"]]
  )

  # When none is valid
  expect_equal(
    concepts_included_in(
      concept_ids = concepts$concept_id[4],
      target_ecl = "763158003",
      branch = "MAIN"
    ),
    NA
  )
  expect_equal(
    concepts_included_in(
      concept_ids = NA,
      target_ecl = "763158003",
      branch = "MAIN"
    ),
    NA
  )
  expect_equal(
    concepts_included_in(
      concept_ids = "",
      target_ecl = "763158003",
      branch = "MAIN"
    ),
    NA
  )

  # check when none belongs to target field
  expect_equal(
    concepts_included_in(
      concept_ids = concepts$concept_id[1],
      target_ecl = "763158003"
    ),
    FALSE
  )

  # Trigger REST 400 BAD_REQUEST
  expect_warning(
    expect_equal(
      concepts_included_in(
        concept_ids = c(
          "407671000",
          "xxab" # (SNOMED codes should not have letters)
        ),
        target_ecl = "763158003"
      ),
      c(FALSE, NA)
    )
  )

})
