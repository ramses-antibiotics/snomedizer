
test_that("concept_included_in (batch)", {
  concepts_pneumo <- concept_find(ecl = "<233604007", limit = 300)
  concepts_batch <- concept_included_in(
    concept_ids = concepts_pneumo$conceptId,
    target_ecl = "763158003", # Medicinal products
    silent = TRUE
  )
  expect_false(any(concepts_batch))
})

test_that("concept_included_in", {

  concepts <- dplyr::tibble(
    concept_id = c("407671000",
                   "422747000",
                   "27658006",

                   # unknown to MAIN branch (International Edition)
                   "39732311000001104",

                   "10625071000119104",
                   NA_character_),
    concept_fsn = c("Bilateral pneumonia (disorder)",
                    "Upper urinary tract infection (disorder)",
                    "Product containing amoxicillin (medicinal product)",

                    # in UK edition only
                    "Amoxicillin 250mg capsules (product)",

                    "Bronchopneumonia caused by bacteria",
                    NA_character_),
    expect_pneumonia = c(TRUE, FALSE, FALSE, NA, TRUE, NA),
    expect_uti = c(FALSE, TRUE, FALSE, NA, FALSE, NA),
    expect_medicine = c(FALSE, FALSE, TRUE, NA, FALSE, NA),
    expect_nonbact_pneumonia = c(TRUE, FALSE, FALSE, NA, FALSE, NA)
  )

  concepts[["is_pneumonia"]] <- concept_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "<<233604007" # Pneumonia (disorder)
  )
  expect_equal(
    concepts[["is_pneumonia"]],
    concepts[["expect_pneumonia"]]
  )
  concepts[["is_uti"]] <- concept_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "<<68566005" # Urinary tract infectious disease (disorder)
  )
  expect_equal(
    concepts[["is_uti"]],
    concepts[["expect_uti"]]
  )
  concepts[["is_medicine"]] <- concept_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "<<763158003", # Medicinal product (product)
    branch = "MAIN"
  )
  expect_equal(
    concepts[["is_medicine"]],
    concepts[["expect_medicine"]]
  )
  concepts[["is_nonbact_pneumonia"]] <- concept_included_in(
    concept_ids = concepts$concept_id,
    target_ecl = "<<233604007 MINUS <<53084003"
    # Pneumonia excluding bacterial pneumonia
  )
  expect_equal(
    concepts[["is_nonbact_pneumonia"]],
    concepts[["expect_nonbact_pneumonia"]]
  )

  # When none is valid
  expect_equal(
    concept_included_in(
      concept_ids = concepts$concept_id[4],
      target_ecl = "<<763158003",
      branch = "MAIN"
    ),
    NA
  )
  expect_equal(
    concept_included_in(
      concept_ids = NA,
      target_ecl = "<<763158003",
      branch = "MAIN"
    ),
    NA
  )
  expect_equal(
    concept_included_in(
      concept_ids = "",
      target_ecl = "<<763158003",
      branch = "MAIN"
    ),
    NA
  )

  # check when none belongs to target field
  expect_equal(
    concept_included_in(
      concept_ids = concepts$concept_id[1],
      target_ecl = "<<763158003"
    ),
    FALSE
  )

  # Trigger REST 400 BAD_REQUEST
  expect_warning(
    expect_equal(
      concept_included_in(
        concept_ids = c(
          "407671000",
          "xxab" # (SNOMED codes should not have letters)
        ),
        target_ecl = "<<763158003"
      ),
      c(FALSE, NA)
    )
  )

})
