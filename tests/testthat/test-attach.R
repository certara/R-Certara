test_that("certara_attach returns a named logical vector", {
  res <- certara_attach()
  expect_type(res, "logical")
  expect_named(res)
  expect_setequal(names(res), certara_core_packages())
})

test_that("certara_attach attaches the hard Depends Certara.RsNLME", {
  skip_if_not_installed("Certara.RsNLME")
  res <- certara_attach()
  # When run from source without the hard Depends installed, skip rather than
  # fail spuriously; R CMD check installs Depends so this assertion runs there.
  expect_true(res[["Certara.RsNLME"]])
})
