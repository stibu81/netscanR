# get sample output for tests
get_arp_scan_test_output <- function(error = FALSE, with_repeater = FALSE) {
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

    # if with_repeater is TRUE, hide some devices behind the repeater
    if (with_repeater) {
      repeater_mac <- "2a:53:59:e4:3f:80"
      out[c(4, 7, 9, 10, 11)] <- out[c(4, 7, 9, 10, 11)] %>%
        stringr::str_replace(mac_pattern, repeater_mac)
    }
  }

  out
}


get_ifconfig_test_output <- function() {
  c("docker0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500",
      "        inet 172.17.0.1  netmask 255.255.0.0  broadcast 172.17.255.255",
      "        inet6 fe80::42:b1ff:febb:61c4  prefixlen 64  scopeid 0x20<link>",
      "        ether 03:28:b4:cb:32:d9  txqueuelen 0  (Ethernet)",
      "        RX packets 54097  bytes 2049215 (2.0 MB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 35978  bytes 114352859 (114.3 MB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "",
      "enp0s31f6: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500",
      "        inet 192.168.1.57  netmask 255.255.255.0  broadcast 192.168.1.255",
      "        inet6 2b43:aa32:e561:9c82:ca3f:52af:e4e3:9d6a  prefixlen 64  scopeid 0x0<global>",
      "        inet6 2b43:aa32:e561:9c82:eff9:b5ab:7056:d429  prefixlen 64  scopeid 0x0<global>",
      "        inet6 a4c4::e094:a244:b08c:a21d  prefixlen 64  scopeid 0x20<link>",
      "        ether 5c:48:2a:d7:4d:9f  txqueuelen 1000  (Ethernet)",
      "        RX packets 571  bytes 120680 (120.6 KB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 726  bytes 131909 (131.9 KB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "        device interrupt 16  memory 0xe1300000-e1320000  ",
      "",
      "lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536",
      "        inet 127.0.0.1  netmask 255.0.0.0",
      "        inet6 ::1  prefixlen 128  scopeid 0x10<host>",
      "        loop  txqueuelen 1000  (Local Loopback)",
      "        RX packets 979150  bytes 1213455947 (1.2 GB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 979150  bytes 1213455947 (1.2 GB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "",
      "", "wlp6s0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500",
      "        inet 192.168.1.132  netmask 255.255.255.0  broadcast 192.168.1.255",
      "        inet6 2b02:a310:e472:db82:bb1c:41f5:6abd:b8d9  prefixlen 64  scopeid 0x0<global>",
      "        inet6 b02:a310:e472:db82:45de:2520:c327:4af4  prefixlen 64  scopeid 0x0<global>",
      "        inet6 fa83::4f33:52fa:b438:b51f  prefixlen 64  scopeid 0x20<link>",
      "        ether 2c:6c:a4:a9:4d:a3  txqueuelen 1000  (Ethernet)",
      "        RX packets 21657652  bytes 3114683092 (3.1 GB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 6510317  bytes 1108485001 (1.1 GB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "")
}


get_ping_test_output <- function(failure = FALSE) {
  if (failure) {
    out <- c("PING 192.168.1.157 (192.168.1.134) 56(84) bytes of data.",
             "From 192.168.1.132 icmp_seq=1 Destination Host Unreachable",
             "From 192.168.1.132 icmp_seq=2 Destination Host Unreachable",
             "From 192.168.1.132 icmp_seq=3 Destination Host Unreachable", "",
             "--- 192.168.1.157 ping statistics ---",
             "3 packets transmitted, 0 received, +3 errors, 100% packet loss, time 2044ms",
             "pipe 3")

    attr(out, "status") <- 1L
    attr(out, "errmsg") <- "Resource temporarily unavailable"
  } else {
    out <- c("PING 192.168.1.1 (192.168.1.1) 56(84) bytes of data.",
             "64 bytes from 192.168.1.1: icmp_seq=1 ttl=1 time=4.62 ms",
             "64 bytes from 192.168.1.1: icmp_seq=2 ttl=64 time=4.92 ms",
             "64 bytes from 192.168.1.1: icmp_seq=3 ttl=64 time=5.32 ms", "",
             "--- 192.168.1.1 ping statistics ---",
             "3 packets transmitted, 3 received, 0% packet loss, time 2003ms",
             "rtt min/avg/max/mdev = 4.623/4.952/5.315/0.283 ms")
  }

  out
}


# get reference data for tests
get_arp_scan_ref <- function(with_device_list = TRUE) {
  arp_scan_ref <- dplyr::tibble(
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

  if (!with_device_list) {
    arp_scan_ref <- arp_scan_ref %>%
      dplyr::select("interface":"vendor")
  }

  arp_scan_ref
}


get_ifconfig_ref <- function() {
  dplyr::tibble(
    name = c("docker0", "enp0s31f6", "lo", "wlp6s0"),
    mtu = c(1500L, 1500L, 65536L, 1500L),
    mac = c("03:28:b4:cb:32:d9", "5c:48:2a:d7:4d:9f", NA, "2c:6c:a4:a9:4d:a3"),
    ipv4 = c("172.17.0.1", "192.168.1.57", "127.0.0.1", "192.168.1.132"),
    netmask = c("255.255.0.0", "255.255.255.0", "255.0.0.0", "255.255.255.0")
  )
}


get_device_list_ref <- function(with_ip = TRUE) {
  dev_list_ref <- dplyr::tibble(
    mac = c(
      "b1:5b:92:b5:32:d8", "da:13:54:95:ab:63", "cc:c1:79:a5:f9:f1",
      "ee:44:eb:bf:01:9a", "3d:3d:22:38:4d:ae", "f4:b5:d1:36:5e:32",
      "72:73:b3:17:d4:ac"
    ),
    description = c(
      "Router", "Laptop Frank", "Phone Peter", "Laptop Anna", "Tablet Peter",
      "Printer", "Phone Anna"
    ),
    ip = c(
      "192.168.1.1", "192.168.1.111", "192.168.1.113", "192.168.1.178",
      NA, "192.168.1.240", "192.168.1.83"
    )
  )

  if (!with_ip) dev_list_ref$ip <- NA_character_

  dev_list_ref
}
