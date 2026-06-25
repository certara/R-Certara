test_that("certara_attach returns a named logical vector", {
  res <- certara_attach()
  expect_type(res, "logical")
  expect_named(res)
  expect_setequal(names(res), certara_core_packages())
})

test_that("certara_attach attaches the hard Depends Certara.RsNLME", {
  res <- certara_attach()
  # Certara.RsNLME is a hard Depends, so it must always attach.
  expect_true(res[["Certara.RsNLME"]])
})
