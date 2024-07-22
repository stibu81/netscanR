test_that("test ping()", {
  skip_on_cran()
  skip_on_os("mac")
  expect_equal(
    ping(c("127.0.0.1", "127.0.0.2")),
    setNames(c(TRUE, TRUE), c("127.0.0.1", "127.0.0.2"))
  )
  skip_on_ci()
  expect_equal(
    ping(c("192.168.1.1", "192.168.255.1")),
    setNames(c(TRUE, FALSE), c("192.168.1.1", "192.168.255.1"))
  )
})


test_that("test ping() with error", {
  expect_error(
    ping(c("192.168.1.1", "192.168.1.257", "300.1.1.1")),
    "Some hosts are not valid IP addresses: 192\\.168\\.1\\.257, 300\\.1\\.1\\.1"
  )
})


test_that("test find_ping()", {
  skip_on_cran()
  skip_on_os("mac")
  expect_equal(find_ping(), "/usr/bin/ping")
})


test_that("test get_ping_command()", {
  expect_equal(
    get_ping_command("192.168.1.1", count = 3, interval = 0.1, timeout = 1),
    "ping -c 3 -i 0.1 -W 1 192.168.1.1"
  )
  expect_equal(
    get_ping_command("192.168.1.100", count = 10, interval = 2, timeout = 0.5),
    "ping -c 10 -i 2 -W 0.5 192.168.1.100"
  )
})


test_that("test parse_ping()", {
  expect_equal(parse_ping(get_ping_test_output()), TRUE)
  expect_equal(parse_ping(get_ping_test_output(failure = TRUE)), FALSE)
})
