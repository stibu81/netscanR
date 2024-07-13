library(dplyr, warn.conflicts = FALSE)


test_that("test find_ifconfig()", {
  expect_vector(find_ifconfig(), ptype = character(0), size = 1)
  skip_on_cran()
  skip_on_os("mac")
  expect_equal(find_ifconfig(), "/usr/sbin/ifconfig")
})


test_that("test list_interfaces()", {
  #skip_on_ci()
  #skip_on_cran()
  expect_s3_class(list_interfaces(), "tbl_df") %>%
    expect_named(c("name", "mtu", "mac", "ipv4", "netmask"))
})


test_that("test list_interfaces()", {
  expect_equal(parse_ifconfig(get_ifconfig_test_output()), get_ifconfig_ref())
})

