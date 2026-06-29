test_that("certara_packages returns the ecosystem manifest", {
  pkgs <- certara_packages()
  expect_type(pkgs, "character")
  expect_true("Certara.RsNLME" %in% pkgs)
  expect_true("tidyvpc" %in% pkgs)
  # GitHub-only packages are included by default
  expect_true("ggcertara" %in% pkgs)
})

test_that("include_self and include_JFROG_deps toggle membership", {
  expect_false("Certara.R" %in% certara_packages())
  expect_true("Certara.R" %in% certara_packages(include_self = TRUE))

  expect_false("Certara.NLME8" %in% certara_packages())
  expect_true("Certara.NLME8" %in% certara_packages(include_JFROG_deps = TRUE))
})

test_that("certara_core_packages excludes GitHub-only packages", {
  core <- certara_core_packages()
  expect_false("ggcertara" %in% core)
  expect_false("table1c" %in% core)
  expect_true("Certara.RsNLME" %in% core)
})
