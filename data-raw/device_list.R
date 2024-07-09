## prepare list of known devices from the test output for arp-scan
library(netscanR)
library(dplyr)
library(readr)

device_list <- get_arp_scan_test_output() %>%
  netscanR:::parse_arp_scan() %>%
  arrange(ip) %>%
  mutate(
    description = c("Router", "Laptop Frank", "Phone Peter", "Laptop Anna",
                    "Tablet Peter", "Printer", "Laptop Peter", "Phone Anna")
  ) %>%
  select(mac, description, ip) %>%
  # remove one line such that we have an unexpected mac address
  filter(description != "Laptop Peter") %>%
  # change one ip address
  mutate(ip = ifelse(description == "Printer", "192.168.1.240", ip)) %>%
  # make one ip address missing
  mutate(ip = ifelse(description == "Tablet Peter", NA_character_, ip))

# write the table with all three columns
write_csv(device_list, "inst/extdata/device_list.csv")
