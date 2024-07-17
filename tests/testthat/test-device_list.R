library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(stringr)
library(withr)

dev_list_file <- system.file("extdata", "device_list.csv", package = "netscanR")
dev_list_ref <- get_device_list_ref()

test_that("test read_device_list()", {
  expect_equal(read_device_list(dev_list_file), dev_list_ref)
})


test_that("test read_device_list() without ip column", {
  with_tempfile("dev_list_no_ip_file", {
    dev_list_ref %>%
      select(-ip) %>%
      write_csv(dev_list_no_ip_file)
    expect_equal(read_device_list(dev_list_no_ip_file),
                 get_device_list_ref(with_ip = FALSE))
  })
})


test_that("test read_device_list() with missing columns", {
  with_tempfile("dev_list_missing_columns_file", {
    dev_list_ref %>%
      select(-description) %>%
      write_csv(dev_list_missing_columns_file)
    expect_error(
      read_device_list(dev_list_missing_columns_file),
      "Columns \"mac\" and/or \"description\" missing"
    )
    dev_list_ref %>%
      select(-mac) %>%
      write_csv(dev_list_missing_columns_file)
    expect_error(
      read_device_list(dev_list_missing_columns_file),
      "Columns \"mac\" and/or \"description\" missing"
    )
  })
})


test_that("test read_device_list() with different column order", {
  with_tempfile("dev_list_bad_order_file", {
    dev_list_ref %>%
      select(ip, description, mac) %>%
      write_csv(dev_list_bad_order_file)
    expect_equal(read_device_list(dev_list_bad_order_file), dev_list_ref)
  })
})


test_that("test read_device_list() with additional column", {
  with_tempfile("dev_list_add_col_file", {
    dev_list_ref %>%
      select(ip, description, mac) %>%
      mutate(foo = 1:n()) %>%
      write_csv(dev_list_add_col_file)
    expect_equal(read_device_list(dev_list_add_col_file), dev_list_ref)
  })
})


test_that("test read_device_list() with wrong delimiter", {
  with_tempfile("dev_list_bad_delim_file", {
    dev_list_ref %>%
      write_delim(dev_list_bad_delim_file, delim = ";")
    expect_error(
      read_device_list(dev_list_bad_delim_file),
      "has only one column. Ensure that \",\" is used as the delimiter."
    )
  })
})


test_that("test read_device_list() with erroneous files", {
  with_tempfile("dev_list_erroneous_file", {
    # missing mac address
    dev_list_ref %>%
      mutate(mac = ifelse(row_number() == 1, NA_character_, mac)) %>%
      write_csv(dev_list_erroneous_file)
    expect_equal(
        read_device_list(dev_list_erroneous_file),
        dev_list_ref[-1, ]
      ) %>%
      expect_warning("Some entries in the device list have no mac address")
    # invalid mac address
    dev_list_ref %>%
      mutate(mac = ifelse(row_number() == 1, "b1", mac)) %>%
      write_csv(dev_list_erroneous_file)
    expect_equal(
        read_device_list(dev_list_erroneous_file),
        dev_list_ref[-1, ]
      ) %>%
      expect_warning(
        "Some entries in the device list have an invalid mac address."
      )
    # duplicated mac address
    dev_list_ref %>%
      mutate(mac = ifelse(row_number() == 2, dev_list_ref$mac[1], mac)) %>%
      write_csv(dev_list_erroneous_file)
    expect_equal(
        read_device_list(dev_list_erroneous_file),
        dev_list_ref[-2, ]
      ) %>%
      expect_warning(
        "Some entries in the device list have duplicated mac address"
      )
    # invalid ip address
    dev_list_ref %>%
      mutate(ip = ifelse(row_number() == 1, "192", ip)) %>%
      write_csv(dev_list_erroneous_file)
    expect_equal(
        read_device_list(dev_list_erroneous_file),
        dev_list_ref %>%
          mutate(ip = ifelse(row_number() == 1, NA_character_, ip))
      ) %>%
      expect_warning(
        "Some entries in the device list have an invalid ip address."
      )

  })
})


arp_scan_table <- get_arp_scan_ref()

test_that("test update_device_list() to create a new file with description", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    dev_list_ref_l <- arp_scan_table %>%
      select("mac", description = "vendor", "ip")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})


test_that("test update_device_list() to create a new file without description", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    dev_list_ref_l <- arp_scan_table %>%
      select("mac", "ip") %>%
      mutate(description = NA_character_, .after = "mac")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l,
                         vendor_to_description = FALSE),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})


test_that("test update_device_list() to create a new file with ip update", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    dev_list_ref_l <- arp_scan_table %>%
      select("mac", description = "vendor", "ip")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l, update_ip = TRUE),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})


test_that("test update_device_list() to update a file with description", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    file.copy(dev_list_file, dev_list_file_l)
    dev_list_ref_l <- dev_list_ref %>%
      add_row(mac = "31:3a:fa:32:b3:d3",
              description = "Unknown: locally administered",
              ip = "192.168.1.27")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})


test_that("test update_device_list() to update a file without description", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    file.copy(dev_list_file, dev_list_file_l)
    dev_list_ref_l <- dev_list_ref %>%
      add_row(mac = "31:3a:fa:32:b3:d3",
              ip = "192.168.1.27")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l,
                         vendor_to_description = FALSE),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})


test_that("test update_device_list() to update a file with ip update", {
  with_tempfile("dev_list_file_l", fileext = ".csv", {
    file.copy(dev_list_file, dev_list_file_l)
    dev_list_ref_l <- dev_list_ref %>%
      add_row(mac = "31:3a:fa:32:b3:d3",
              description = "Unknown: locally administered",
              ip = "192.168.1.27")
    dev_list_ref_l$ip[c(5, 6)] <- c("192.168.1.23", "192.168.1.239")
    expect_equal(
      update_device_list(arp_scan_table, dev_list_file_l, update_ip = TRUE),
      dev_list_ref_l
    )
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
    # writing again should not change anything
    update_device_list(arp_scan_table, dev_list_file_l, update_ip = TRUE)
    expect_equal(read_device_list(dev_list_file_l), dev_list_ref_l)
  })
})
