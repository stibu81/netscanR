# get sample output for tests
get_arp_scan_test_output <- function(error = FALSE) {
  header <- "Interface: wlp6s0, type: EN10MB, MAC: 2c:6c:a4:a9:4d:a3, IPv4: 192.168.1.132"
  if (error) {
    out <- c(header,
             "WARNING: get_host_address failed for \"badhost\": Name or service not known - target ignored",
             "ERROR: No hosts to process.")
    attr(out, "status") <- 1L
    attr(out, "errormsg") <- "Resource temporarily unavailable"
  } else {
    out <- c(
      header,
      "Starting arp-scan 1.9.7 with 256 hosts (https://github.com/royhills/arp-scan)",
      "192.168.1.1\tb1:5b:92:b5:32:d8\t(Unknown)",
      "192.168.1.23\t3d:3d:22:38:4d:ae\tACME, Inc.",
      "192.168.1.27\t31:3a:fa:32:b3:d3\t(Unknown: locally administered)",
      "192.168.1.113\tcc:c1:79:a5:f9:f1\tSome Manufacturing Co., Ltd.",
      "192.168.1.178\tee:44:eb:bf:01:9a\t(Unknown: locally administered)",
      "192.168.1.83\t72:73:b3:17:d4:ac\t(Unknown: locally administered)",
      "192.168.1.111\tda:13:54:95:ab:63\t(Unknown: locally administered)",
      "192.168.1.239\tf4:b5:d1:36:5e:32\t(Unknown)",
      "192.168.1.23\t3d:3d:22:38:4d:ae\tACME, Inc. (DUP: 2)", "",
      "8 packets received by filter, 0 packets dropped by kernel",
      "Ending arp-scan 1.9.7: 256 hosts scanned in 2.122 seconds (120.64 hosts/sec). 8 responded"
    )
  }

  out
}
