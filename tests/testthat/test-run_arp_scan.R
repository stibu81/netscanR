library(dplyr, warn.conflicts = FALSE)

device_list_file <- system.file("extdata", "device_list.csv", package = "netscanR")

test_that("test get_arp_scan_command()", {
  expect_equal(get_arp_scan_command(),
               "arp-scan --localnet --retry=2 --interval=2000u 2>&1")
  expect_equal(get_arp_scan_command(hosts = "192.168.1.234"),
               "arp-scan --retry=2 --interval=2000u 192.168.1.234 2>&1")
  expect_equal(
    get_arp_scan_command(interface = "wlp6s0"),
    "arp-scan --localnet --interface wlp6s0 --retry=2 --interval=2000u 2>&1"
  )
  expect_equal(
    get_arp_scan_command(hosts = "192.168.1.23", interface = "wlp6s0"),
    "arp-scan --interface wlp6s0 --retry=2 --interval=2000u 192.168.1.23 2>&1"
  )
  expect_equal(get_arp_scan_command(retry = 5),
               "arp-scan --localnet --retry=5 --interval=2000u 2>&1")
  expect_equal(get_arp_scan_command(interval = 100),
               "arp-scan --localnet --retry=2 --interval=100u 2>&1")
  expect_equal(
    get_arp_scan_command(retry = 3.2, interval = 49.8,
                         interface = "eth0", hosts = "192.168.1.234"),
    "arp-scan --interface eth0 --retry=3 --interval=50u 192.168.1.234 2>&1")
  expect_error(get_arp_scan_command(localnet = FALSE),
               "Either provide hosts or set localnet to TRUE.")
})


test_that("test run_arp_scan()", {
  skip_on_cran()
  skip_on_os("mac")
  expect_s3_class(run_arp_scan(retry = 0), "tbl_df") %>%
    expect_named(c("interface", "ip", "mac", "vendor"))
  expect_s3_class(
      run_arp_scan(retry = 0, device_list = device_list_file),
      "tbl_df"
    ) %>%
    expect_named(c("interface", "ip", "mac", "vendor", "description",
                   "expected_ip", "known_device"))
  expect_s3_class(
      run_arp_scan(retry = 0, device_list = device_list_file, require_ping = 5),
      "tbl_df"
    ) %>%
    expect_named(c("interface", "ip", "mac", "vendor", "description",
                   "expected_ip", "known_device"))
  expect_error(run_arp_scan(interface = "doesnotexist"),
               "doesnotexist")

  # running arp-scan on github actions works, but it aborts after scanning about
  # 200 addresses, so we skip the test with retry > 0.
  skip_on_ci()
  expect_s3_class(run_arp_scan(retry = 1, interval = 10), "tbl_df") %>%
    expect_named(c("interface", "ip", "mac", "vendor"))
  expect_s3_class(
      run_arp_scan(retry = 1, interval = 10, device_list = device_list_file),
      "tbl_df"
    ) %>%
    expect_named(c("interface", "ip", "mac", "vendor", "description",
                   "expected_ip", "known_device"))
  expect_s3_class(
      run_arp_scan(retry = 1, interval = 10,
                   device_list = device_list_file,
                   require_ping = 5),
      "tbl_df"
    ) %>%
    expect_named(c("interface", "ip", "mac", "vendor", "description",
                   "expected_ip", "known_device"))
})


test_that("test parse_arp_scan()", {
  expect_equal(parse_arp_scan(get_arp_scan_test_output()),
               get_arp_scan_ref(with_device_list = FALSE))
  parse_arp_scan(get_arp_scan_test_output(), verbose = TRUE) %>%
    expect_message("arp-scan was successful") %>%
    expect_message("Interface: wlp6s0") %>%
    expect_message("MAC: 2c:6c:a4:a9:4d:a3") %>%
    expect_message("IPv4: 192.168.1.132") %>%
    expect_message("Number of scanned hosts: 256") %>%
    expect_message("Scan time: 2.122 seconds")
})


test_that("test apply_device_list()", {
  arp_scan_table <- parse_arp_scan(get_arp_scan_test_output())
  device_list <- read_device_list(device_list_file)
  expect_equal(apply_device_list(arp_scan_table, device_list), get_arp_scan_ref())
})


test_that("test parse_arp_scan() with error", {
  expect_error(parse_arp_scan(get_arp_scan_test_output(error = TRUE)),
               "ERROR: No hosts to process.")
})


test_that("test rm_patenthesis()", {
  expect_equal(
    rm_parenthesis(c("ACME", "(ACME)", "(The) ACME", "The (ACME)",
                     "(The ACME)", "A(CM)E")),
    c("ACME", "ACME", "(The) ACME", "The (ACME)", "The ACME", "A(CM)E")
  )
})


test_that("test format_arp_scan_errors()", {
  expect_equal(
    format_arp_scan_errors(get_arp_scan_test_output(error = TRUE)),
    c("i" = "Interface: wlp6s0, type: EN10MB, MAC: 2c:6c:a4:a9:4d:a3, IPv4: 192.168.1.132",
      "!" = "WARNING: get_host_address failed for \"badhost\": Name or service not known - target ignored",
      "x" = "ERROR: No hosts to process.")
  )
})


test_that("test filter_by_ping()", {
  skip_on_cran()
  skip_on_os("mac")
  arp_scan_table <- get_arp_scan_ref() %>%
    mutate(ip = paste0("127.0.0.", 1:8))
  expect_equal(
    filter_by_ping(arp_scan_table, require_ping = 2),
    arp_scan_table
  )
  # set three IPs to addresses that do not exist in my local network
  arp_scan_table$ip[c(3, 5, 7)] <- paste0("192.168.1.", 2:4)
  expect_equal(
    filter_by_ping(arp_scan_table, require_ping = 2, timeout = 0.1),
    arp_scan_table[-c(5, 7), ]
  )
  arp_scan_table$known_device <- FALSE
  expect_equal(
    filter_by_ping(arp_scan_table, require_ping = 2),
    arp_scan_table
  )
  arp_scan_table <- get_arp_scan_ref(with_device_list = FALSE)
  expect_equal(
      filter_by_ping(arp_scan_table, require_ping = 2),
      arp_scan_table
    ) %>%
    expect_message("Cannot filter by ping")
})
