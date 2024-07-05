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
    expect_named(c("name", "mtu", "mac", "ipv4"))
})


ifconfig_ref <- tibble(
  name = c("docker0", "enp0s31f6", "lo", "wlp6s0"),
  mtu = c(1500L, 1500L, 65536L, 1500L),
  mac = c("03:28:b4:cb:32:d9", "5c:48:2a:d7:4d:9f", NA, "2c:6c:a4:a9:4d:a3"),
  ipv4 = c("172.17.0.1", "192.168.1.57", "127.0.0.1", "192.168.1.132")
)

test_that("test list_interfaces()", {
  expect_equal(parse_ifconfig(get_ifconfig_test_output()), ifconfig_ref)
})

