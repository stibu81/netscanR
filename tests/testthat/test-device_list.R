library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(stringr)
library(withr)

dev_list_file <- system.file("extdata", "device_list.csv", package = "netscanR")
dev_list_ref <- tibble(
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

test_that("test read_device_list()", {
  expect_equal(read_device_list(dev_list_file), dev_list_ref)
})


test_that("test read_device_list() without ip column", {
  with_tempfile("dev_list_no_ip_file", {
    dev_list_ref %>%
      select(-ip) %>%
      write_csv(dev_list_no_ip_file)
    dev_list_no_ip_ref <- dev_list_ref %>% mutate(ip = NA_character_)
    expect_equal(read_device_list(dev_list_no_ip_file), dev_list_no_ip_ref)
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
