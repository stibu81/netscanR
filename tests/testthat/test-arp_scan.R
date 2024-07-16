test_that("test find_arp_scan", {
  expect_vector(find_arp_scan(), ptype = character(0), size = 1)
  skip_on_cran()
  skip_on_os("mac")
  expect_equal(find_arp_scan(), "/usr/sbin/arp-scan")
})


test_that("test arp_scan_version()", {
  expect_s3_class(arp_scan_version(), "numeric_version")
  skip_on_cran()
  skip_on_os("mac")
  expect_true(arp_scan_version() >= as.numeric_version("1.9.7"))
})


test_that("test can_run_arpscan()", {
  expect_vector(can_run_arp_scan(verbose = FALSE), ptype = logical(0), size = 1) %>%
    expect_silent()
  skip_on_os("mac")
  skip_on_cran()
  suppressMessages(
    expect_true(can_run_arp_scan()) %>%
      expect_message("running as root")
  )
  expect_true(can_run_arp_scan(verbose = FALSE))
})
