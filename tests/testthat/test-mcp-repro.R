# Core reproducible-script recorder (mcp_repro.R).

test_that("mcp_repro_call renders literals, symbols, and assignment", {
  expect_equal(
    mcp_repro_call("f", list(a = "x", b = mcp_repro_sym("xpdb1"), c = 2)),
    'f(a = "x", b = xpdb1, c = 2)'
  )
  expect_equal(
    mcp_repro_call("g", list(a = 1, b = NULL), var = "out"),
    "out <- g(a = 1)"
  )
})

test_that("mcp_repro_call validates fn", {
  expect_error(mcp_repro_call(1), "single non-empty")
})

test_that("recording builds a parseable script with a single library block", {
  mcp_repro_reset()
  path <- file.path(tempdir(), "repro_core_test.R")
  mcp_repro_path(path)
  mcp_repro_record(
    mcp_repro_call("xposeNlme", list(dir = "C:/x"), var = "xpdb1"),
    libraries = c("Certara.Xpose.NLME", "xpose")
  )
  mcp_repro_record(
    mcp_repro_call("get_overallNlme", list(mcp_repro_sym("xpdb1")),
                   var = "overall"),
    libraries = c("Certara.Xpose.NLME", "xpose")
  )
  txt <- mcp_repro_read()
  expect_equal(lengths(regmatches(txt, gregexpr("library\\(xpose\\)", txt)))[1], 1L)
  expect_match(txt, "xpdb1 <- xposeNlme(dir = \"C:/x\")", fixed = TRUE)
  expect_true(file.exists(path))
  expect_silent(parse(text = txt))
})

test_that("setting a new path resets the script", {
  mcp_repro_reset()
  mcp_repro_path(file.path(tempdir(), "repro_a.R"))
  mcp_repro_record("x <- 1")
  expect_true(nzchar(mcp_repro_read()))
  mcp_repro_path(file.path(tempdir(), "repro_b.R"))
  expect_equal(mcp_repro_read(), "")
})

test_that("mcp_repro_info returns path and contents", {
  mcp_repro_reset()
  mcp_repro_path(file.path(tempdir(), "repro_info.R"))
  mcp_repro_record("y <- 2")
  info <- mcp_repro_info()
  expect_equal(info$path, file.path(tempdir(), "repro_info.R"))
  expect_match(info$contents, "y <- 2", fixed = TRUE)
})

test_that("empty recorder reads as empty string", {
  mcp_repro_reset()
  expect_equal(mcp_repro_read(), "")
})

test_that("repro_script is advertised as a host capability rule", {
  cap <- certara_mcp_capabilities()
  expect_true("repro_script" %in% names(cap$rules))
})
