library(withr)
library(RSQLite)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(dbplyr, warn.conflicts = FALSE)


test_that("test create_netscanr_db()", {
  with_tempfile("netscanr_db", fileext = ".sqlite", {
    # test creation of new netscanr db
    expect_true(create_netscanr_db(netscanr_db))
    expect_true(file.exists(netscanr_db))
    with_db_connection(list(con = dbConnect(SQLite(), netscanr_db)), {
      expect_equal(dbListTables(con), "netscanr")
      expect_equal(
        dbListFields(con, "netscanr"),
        c("timestamp", "interface", "ip", "mac", "vendor", "description",
          "expected_ip", "known_device")
      )
    })

    # test error when db and table netscanr already exist
    expect_error(create_netscanr_db(netscanr_db),
                 "Table \"netscanr\" already exists in database")

    # test creation of table netscanr in existing db
    with_db_connection(
      list(con = dbConnect(SQLite(), netscanr_db)),
      dbExecute(con, "DROP TABLE netscanr")
    )
    expect_true(create_netscanr_db(netscanr_db))
    expect_true(file.exists(netscanr_db))
    with_db_connection(list(con = dbConnect(SQLite(), netscanr_db)), {
      expect_equal(dbListTables(con), "netscanr")
      expect_equal(
        dbListFields(con, "netscanr"),
        c("timestamp", "interface", "ip", "mac", "vendor", "description",
          "expected_ip", "known_device")
      )
    })

    # test overwriting of existing database. Check that the database has been
    # overwritten by comparing the modification time stamp
    mod_before <- file.info(netscanr_db)$mtime
    expect_true(create_netscanr_db(netscanr_db, overwrite = TRUE))
    expect_gt(file.info(netscanr_db)$mtime, mod_before)
  })
})


test_that("test create_netscanr_db() with errors", {

  # test applying create_netscanr_db() on a file that is not a SQLite db
  with_tempfile("not_a_db", fileext = ".sqlite", {
    writeLines("not a database", not_a_db)
    expect_error(create_netscanr_db(not_a_db),
                 "The file is not a database")
  })

  # test creating a database in a directory that does not exist
  with_tempfile("non_existent_dir", {
    expect_error(
      create_netscanr_db(file.path(non_existent_dir, "netscanr.sqlite")),
      "Connecting to database .*/netscanr\\.sqlite failed."
    )
  })

  # test creating a database with a wrong file extension
  expect_error(
    create_netscanr_db("netscanr.txt"),
    "netscanr\\.txt has the wrong extension"
  )
})


test_that("test reading and writing database", {
  with_tempfile("netscanr_db", fileext = ".sqlite", {
    arp_scan_table <- get_arp_scan_ref()
    create_netscanr_db(netscanr_db)
    ts <- ymd_hms(c("2024-03-07 10:00:00", "2024-06-12 07:00:00"),
                  tz = Sys.timezone())
    # write database
    expect_equal(
      write_netscanr_db(arp_scan_table, netscanr_db, ts[1]),
      arp_scan_table %>% mutate(timestamp = ts[1], .before = 1)
    )
    expect_equal(
      write_netscanr_db(arp_scan_table, netscanr_db, ts[2]),
      arp_scan_table %>% mutate(timestamp = ts[2], .before = 1)
    )
    # read database with standard time zone
    expect_equal(
      read_netscanr_db(netscanr_db),
      bind_rows(
        arp_scan_table %>% mutate(timestamp = ts[1], .before = 1),
        arp_scan_table %>% mutate(timestamp = ts[2], .before = 1)
      )
    )
    # read database with different time zone
    ts_la <- with_tz(ts, "America/Los_Angeles")
    expect_equal(
      read_netscanr_db(netscanr_db, tz = "America/Los_Angeles"),
      bind_rows(
        arp_scan_table %>% mutate(timestamp = ts_la[1], .before = 1),
        arp_scan_table %>% mutate(timestamp = ts_la[2], .before = 1)
      )
    )
    # read data with time filter
    expect_equal(
      read_netscanr_db(netscanr_db, start = ts[2] - days(1)),
      bind_rows(
        arp_scan_table %>% mutate(timestamp = ts[2], .before = 1)
      )
    )
    expect_equal(
      read_netscanr_db(netscanr_db, end = ts[1] + days(1)),
      bind_rows(
        arp_scan_table %>% mutate(timestamp = ts[1], .before = 1)
      )
    )
    expect_equal(
      read_netscanr_db(netscanr_db, start = ts[1] - days(1), end = ts[2] + days(1)),
      bind_rows(
        arp_scan_table %>% mutate(timestamp = ts[1], .before = 1),
        arp_scan_table %>% mutate(timestamp = ts[2], .before = 1)
      )
    )
    expect_equal(
      read_netscanr_db(netscanr_db, start = ts[1] + days(1), end = ts[2] - days(1)),
      arp_scan_table %>%
        mutate(timestamp = ts[1], .before = 1) %>%
        slice(integer(0))
    )
  })
})


test_that("test reading and writing database", {
  with_tempfile("netscanr_db", fileext = ".sqlite", {
    create_netscanr_db(netscanr_db)
    # bad data type for start/end time
    expect_error(
      read_netscanr_db(netscanr_db, start = "2024-03-07 10:00:00"),
      "start must be a POSIXt object"
    )
    expect_error(
      read_netscanr_db(netscanr_db, end = "2024-03-07 10:00:00"),
      "end must be a POSIXt object"
    )
    with_db_connection(list(con = dbConnect(SQLite(), netscanr_db)), {
      # bad table name
      dbExecute(con, "DROP TABLE netscanr")
      expect_error(
        read_netscanr_db(netscanr_db),
        "Table \"netscanr\" does not exist in database"
      )
    })
  })
})
