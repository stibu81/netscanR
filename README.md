
<!-- README.md is generated from README.Rmd. Please edit that file -->

# netscanR

<!-- badges: start -->

[![R-CMD-check](https://github.com/stibu81/netscanR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stibu81/netscanR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stibu81/netscanR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stibu81/netscanR?branch=main)
<!-- badges: end -->

## About

netscanR is an R package that allows you to scan your local network
using `arp-scan`. Output from `arp-scan` is parsed into a tibble that
can be analysed using R.

## Installation

You can install the development version of netscanR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stibu81/netscanR")
```

## Setting up `arp-scan`

`arp-scan` must be installed in order for netscanR to work. You can
check that the command can be found by running

``` r
library(netscanR)
find_arp_scan()
#> [1] "/usr/sbin/arp-scan"
```

If `arp-scan` is not found, an empty string is returned. On Linux, you
can usually install `arp-scan` from the official package repositories.
For example, on Debian, Ubuntu and derivatives, you can run

``` bash
sudo apt install arp-scan
```

`arp-scan` should also run on any other Linux distribution, macOS,
FreeBDS and others (see the [arp-scan GitHub
readme](https://github.com/royhills/arp-scan?tab=readme-ov-file#building-and-installing-from-source)),
and once it is installed, netscanR might be able to run. However, this
has only been tested on Ubuntu.

The version of `arp-scan` can be obtained from R using

``` r
arp_scan_version()
#> [1] '1.9.7'
```

If `arp-scan` is not found, the returned version is 0.

netscanR has only been tested with `arp-scan` version 1.9.7.

## Usage

Once `arp-scan` is installed, you can scan the local network from R
using

``` r
run_arp_scan()
```

<!-- in order not to disclose any private information, the test data is used
here instead of an actual call of run_arp_scan() -->

    #> # A tibble: 8 × 4
    #>   interface ip            mac               vendor                       
    #>   <chr>     <chr>         <chr>             <chr>                        
    #> 1 wlp6s0    192.168.1.1   b1:5b:92:b5:32:d8 Unknown                      
    #> 2 wlp6s0    192.168.1.23  3d:3d:22:38:4d:ae ACME, Inc.                   
    #> 3 wlp6s0    192.168.1.27  31:3a:fa:32:b3:d3 Unknown: locally administered
    #> 4 wlp6s0    192.168.1.113 cc:c1:79:a5:f9:f1 Some Manufacturing Co., Ltd. 
    #> 5 wlp6s0    192.168.1.178 ee:44:eb:bf:01:9a Unknown: locally administered
    #> 6 wlp6s0    192.168.1.83  72:73:b3:17:d4:ac Unknown: locally administered
    #> 7 wlp6s0    192.168.1.111 da:13:54:95:ab:63 Unknown: locally administered
    #> 8 wlp6s0    192.168.1.239 f4:b5:d1:36:5e:32 Unknown
