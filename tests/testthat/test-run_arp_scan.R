library(dplyr, warn.conflicts = FALSE)

device_list_file <- system.file("extdata", "device_list.csv", package = "netscanR")
arp_scan_ref <- tibble(
  interface = "wlp6s0",
  ip = paste0("192.168.1.", c("1", "23", "27", "113", "178", "83", "111", "239")),
  mac = c("b1:5b:92:b5:32:d8", "3d:3d:22:38:4d:ae", "31:3a:fa:32:b3:d3",
          "cc:c1:79:a5:f9:f1", "ee:44:eb:bf:01:9a", "72:73:b3:17:d4:ac",
          "da:13:54:95:ab:63", "f4:b5:d1:36:5e:32"),
  vendor = c("Unknown", "ACME, Inc.", "Unknown: locally administered",
             "Some Manufacturing Co., Ltd.",
             rep("Unknown: locally administered", 3), "Unknown"),
  description = c("Router", "Tablet Peter", NA, "Phone Peter", "Laptop Anna",
                  "Phone Anna", "Laptop Frank", "Printer"),
  expected_ip = c(TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, FALSE),
  known_device = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
)


test_that("test get_arp_scan_command()", {
  expect_equal(get_arp_scan_command(), "arp-scan --localnet 2>&1")
  expect_equal(get_arp_scan_command(hosts = "192.168.1.234"),
               "arp-scan 192.168.1.234 2>&1")
  expect_equal(get_arp_scan_command(interface = "wlp6s0"),
               "arp-scan --localnet --interface wlp6s0 2>&1")
  expect_equal(get_arp_scan_command(hosts = "192.168.1.23",
                                    interface = "wlp6s0"),
               "arp-scan --interface wlp6s0 192.168.1.23 2>&1")
  expect_error(get_arp_scan_command(localnet = FALSE),
               "Either provide hosts or set localnet to TRUE.")
})


test_that("test run_arp_scan()", {
  skip_on_ci()
  skip_on_cran()
  expect_s3_class(run_arp_scan(), "tbl_df") %>%
    expect_named(c("interface", "ip", "mac", "vendor"))
  expect_s3_class(run_arp_scan(device_list = device_list_file), "tbl_df") %>%
    expect_named(c("interface", "ip", "mac", "vendor", "description",
                   "expected_ip", "known_device"))
  expect_error(run_arp_scan(interface = "doesnotexist"),
               "doesnotexist")
})


test_that("test parse_arp_scan()", {
  expect_equal(parse_arp_scan(get_arp_scan_test_output()),
               select(arp_scan_ref, interface:vendor))
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
  expect_equal(apply_device_list(arp_scan_table, device_list), arp_scan_ref)
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
