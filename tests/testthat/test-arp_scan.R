library(dplyr, warn.conflicts = FALSE)

test_that("test run_arp_scan()", {
  skip_on_ci()
  skip_on_cran()
  expect_s3_class(run_arp_scan(), "tbl_df") %>%
    expect_named(c("interface", "ip", "mac", "vendor"))
  expect_error(run_arp_scan(interface = "doesnotexist"),
               "doesnotexist")
})


arp_scan_output <- c(
  "Interface: wlp6s0, type: EN10MB, MAC: 2c:6c:a4:a9:4d:a3, IPv4: 192.168.1.132",
  "Starting arp-scan 1.9.7 with 256 hosts (https://github.com/royhills/arp-scan)",
  "192.168.1.1\tb1:5b:92:b5:32:d8\t(Unknown)",
  "192.168.1.23\t3d:3d:22:38:4d:ae\tACME, Inc.",
  "192.168.1.27\t31:3a:fa:32:b3:d3\t(Unknown: locally administered)",
  "192.168.1.113\tcc:c1:79:a5:f9:f1\tSome Manufacturing Co., Ltd.",
  "192.168.1.178\tee:44:eb:bf:01:9a\t(Unknown: locally administered)",
  "192.168.1.83\t72:73:b3:17:d4:ac\t(Unknown: locally administered)",
  "192.168.1.111\tda:13:54:95:ab:63\t(Unknown: locally administered)",
  "192.168.1.239\tf4:b5:d1:36:5e:32\t(Unknown)", "",
  "8 packets received by filter, 0 packets dropped by kernel",
  "Ending arp-scan 1.9.7: 256 hosts scanned in 2.122 seconds (120.64 hosts/sec). 8 responded"
)
arp_scan_ref <- tibble(
  interface = "wlp6s0",
  ip = paste0("192.168.1.", c("1", "23", "27", "113", "178", "83", "111", "239")),
  mac = c("b1:5b:92:b5:32:d8", "3d:3d:22:38:4d:ae", "31:3a:fa:32:b3:d3",
          "cc:c1:79:a5:f9:f1", "ee:44:eb:bf:01:9a", "72:73:b3:17:d4:ac",
          "da:13:54:95:ab:63", "f4:b5:d1:36:5e:32"),
  vendor = c("Unknown", "ACME, Inc.", "Unknown: locally administered",
             "Some Manufacturing Co., Ltd.",
             rep("Unknown: locally administered", 3), "Unknown")
)

test_that("test parse_arp_scan()", {
  expect_equal(parse_arp_scan(arp_scan_output, verbose = FALSE),
               arp_scan_ref)
  parse_arp_scan(arp_scan_output, verbose = TRUE) %>%
    expect_message("arp-scan was successful") %>%
    expect_message("Interface: wlp6s0") %>%
    expect_message("MAC: 2c:6c:a4:a9:4d:a3") %>%
    expect_message("IPv4: 192.168.1.132") %>%
    expect_message("Number of scanned hosts: 256") %>%
    expect_message("Scan time: 2.122 seconds")
})


arp_scan_output <- "some error"
attr(arp_scan_output, "status") <- 1L

test_that("test parse_arp_scan() with error", {
  expect_error(parse_arp_scan(arp_scan_output, verbose = FALSE),
               "some error")
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
    format_arp_scan_errors(c("message", "WARNING: message", "ERROR: message")),
    c("i" = "message", "!" = "WARNING: message", "x" = "ERROR: message")
  )
})
