test_that("test is_mac()", {
  expect_equal(
    is_mac(c("02:42:b1:bb:61:c4", NA, "3C:6A:A7:39:14:B3",
             "02:42:b1:bb:61", "02:42:b1:bb:61:c4:04", "02:42:b1:bb:61:c4:",
             "02:42:g1:bb:61:c4", "02:42:b1:bb0:61:c4")),
    c(TRUE, NA, TRUE, rep(FALSE, 5))
  )
  expect_equal(is_mac(1:5), rep(FALSE, 5))
  expect_equal(is_mac(c()), logical(0))
  expect_equal(is_mac(rep(NA_character_, 5)), rep(NA, 5))
})


test_that("test is_ip()", {
  expect_equal(
    is_ip(c("192.168.178.38", NA, "1.0.1.0",
             "192.168.178", "192.168.178.38.4", "192.168.178.38.",
             "192.168.178.a45", "192.168.178.256", "...")),
    c(TRUE, NA, TRUE, rep(FALSE, 6))
  )
  expect_equal(is_ip(1:5), rep(FALSE, 5))
  expect_equal(is_ip(c()), logical(0))
  expect_equal(is_ip(rep(NA_character_, 5)), rep(NA, 5))
})


test_that("test file_name_with_ext()", {
  expect_equal(
    file_name_with_ext(c("file", "file.csv"), ".csv"),
    rep("file.csv", 2)
  )
  expect_error(
    file_name_with_ext(c("file", "file.csv"), ".txt"),
    "file\\.csv has the wrong extension. Expected: \\.txt"
  )
  expect_error(
    file_name_with_ext(c("file.dat", "file.csv"), ".rds"),
    "file\\.dat, file.csv have the wrong extension. Expected: \\.rds"
  )
  expect_error(
    file_name_with_ext(c("file", "file.csv"), c(".csv", ".txt")),
    "ext must have length one."
  )
})
