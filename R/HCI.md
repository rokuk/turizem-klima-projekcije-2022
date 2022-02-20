HCI
================

This code reads data from netcdf files (downloaded from
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-climate-suitability-indicators?tab=overview))
and makes plots for selected stations showing HCI values under different
RCP scenarios.

## Importing libraries and data

``` r
source("common.R", local = knitr::knit_global())
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

Read and process all data for HCI:

``` r
all_hci_data <- assembledata("hci")
```

    ## [1] "historical 1986_2005 mean fair"
    ## [1] "historical 1986_2005 mean good"
    ## [1] "historical 1986_2005 mean unf"
    ## [1] "historical 1986_2005 10prct fair"
    ## [1] "historical 1986_2005 10prct good"
    ## [1] "historical 1986_2005 10prct unf"
    ## [1] "historical 1986_2005 90prct fair"
    ## [1] "historical 1986_2005 90prct good"
    ## [1] "historical 1986_2005 90prct unf"
    ## [1] "RCP2.6 2021_2040 mean fair"
    ## [1] "RCP2.6 2021_2040 mean good"
    ## [1] "RCP2.6 2021_2040 mean unf"
    ## [1] "RCP2.6 2021_2040 10prct fair"
    ## [1] "RCP2.6 2021_2040 10prct good"
    ## [1] "RCP2.6 2021_2040 10prct unf"
    ## [1] "RCP2.6 2021_2040 90prct fair"
    ## [1] "RCP2.6 2021_2040 90prct good"
    ## [1] "RCP2.6 2021_2040 90prct unf"
    ## [1] "RCP2.6 2041_2060 mean fair"
    ## [1] "RCP2.6 2041_2060 mean good"
    ## [1] "RCP2.6 2041_2060 mean unf"
    ## [1] "RCP2.6 2041_2060 10prct fair"
    ## [1] "RCP2.6 2041_2060 10prct good"
    ## [1] "RCP2.6 2041_2060 10prct unf"
    ## [1] "RCP2.6 2041_2060 90prct fair"
    ## [1] "RCP2.6 2041_2060 90prct good"
    ## [1] "RCP2.6 2041_2060 90prct unf"
    ## [1] "RCP2.6 2081_2100 mean fair"
    ## [1] "RCP2.6 2081_2100 mean good"
    ## [1] "RCP2.6 2081_2100 mean unf"
    ## [1] "RCP2.6 2081_2100 10prct fair"
    ## [1] "RCP2.6 2081_2100 10prct good"
    ## [1] "RCP2.6 2081_2100 10prct unf"
    ## [1] "RCP2.6 2081_2100 90prct fair"
    ## [1] "RCP2.6 2081_2100 90prct good"
    ## [1] "RCP2.6 2081_2100 90prct unf"
    ## [1] "RCP4.5 2021_2040 mean fair"
    ## [1] "RCP4.5 2021_2040 mean good"
    ## [1] "RCP4.5 2021_2040 mean unf"
    ## [1] "RCP4.5 2021_2040 10prct fair"
    ## [1] "RCP4.5 2021_2040 10prct good"
    ## [1] "RCP4.5 2021_2040 10prct unf"
    ## [1] "RCP4.5 2021_2040 90prct fair"
    ## [1] "RCP4.5 2021_2040 90prct good"
    ## [1] "RCP4.5 2021_2040 90prct unf"
    ## [1] "RCP4.5 2041_2060 mean fair"
    ## [1] "RCP4.5 2041_2060 mean good"
    ## [1] "RCP4.5 2041_2060 mean unf"
    ## [1] "RCP4.5 2041_2060 10prct fair"
    ## [1] "RCP4.5 2041_2060 10prct good"
    ## [1] "RCP4.5 2041_2060 10prct unf"
    ## [1] "RCP4.5 2041_2060 90prct fair"
    ## [1] "RCP4.5 2041_2060 90prct good"
    ## [1] "RCP4.5 2041_2060 90prct unf"
    ## [1] "RCP4.5 2081_2100 mean fair"
    ## [1] "RCP4.5 2081_2100 mean good"
    ## [1] "RCP4.5 2081_2100 mean unf"
    ## [1] "RCP4.5 2081_2100 10prct fair"
    ## [1] "RCP4.5 2081_2100 10prct good"
    ## [1] "RCP4.5 2081_2100 10prct unf"
    ## [1] "RCP4.5 2081_2100 90prct fair"
    ## [1] "RCP4.5 2081_2100 90prct good"
    ## [1] "RCP4.5 2081_2100 90prct unf"
    ## [1] "RCP8.5 2021_2040 mean fair"
    ## [1] "RCP8.5 2021_2040 mean good"
    ## [1] "RCP8.5 2021_2040 mean unf"
    ## [1] "RCP8.5 2021_2040 10prct fair"
    ## [1] "RCP8.5 2021_2040 10prct good"
    ## [1] "RCP8.5 2021_2040 10prct unf"
    ## [1] "RCP8.5 2021_2040 90prct fair"
    ## [1] "RCP8.5 2021_2040 90prct good"
    ## [1] "RCP8.5 2021_2040 90prct unf"
    ## [1] "RCP8.5 2041_2060 mean fair"
    ## [1] "RCP8.5 2041_2060 mean good"
    ## [1] "RCP8.5 2041_2060 mean unf"
    ## [1] "RCP8.5 2041_2060 10prct fair"
    ## [1] "RCP8.5 2041_2060 10prct good"
    ## [1] "RCP8.5 2041_2060 10prct unf"
    ## [1] "RCP8.5 2041_2060 90prct fair"
    ## [1] "RCP8.5 2041_2060 90prct good"
    ## [1] "RCP8.5 2041_2060 90prct unf"
    ## [1] "RCP8.5 2081_2100 mean fair"
    ## [1] "RCP8.5 2081_2100 mean good"
    ## [1] "RCP8.5 2081_2100 mean unf"
    ## [1] "RCP8.5 2081_2100 10prct fair"
    ## [1] "RCP8.5 2081_2100 10prct good"
    ## [1] "RCP8.5 2081_2100 10prct unf"
    ## [1] "RCP8.5 2081_2100 90prct fair"
    ## [1] "RCP8.5 2081_2100 90prct good"
    ## [1] "RCP8.5 2081_2100 90prct unf"

All relevant data is in `alldata`, where column `stationid` is the index
of the grid point in the raw imported data, `scenario` is either
“historical”, “RCP2.6”, “RCP4.5” or “RCP8.5”, `time_period` is either
“1986-2005”, “2021-2040”, “2041-2060” or “2081-2100”, `metric` is the
mean, 10th percentile or 90th percentile, `day_cat` is either “fair”,
“good” or “unf” (category determined based on HCI value), `datapoint` is
the count of days in `day_cat` category for a specific month and metric,
`month` is the month for which the datapoint was calculated.

``` r
head(all_hci_data)
```

    ##   stationid   scenario time_period metric day_cat month datapoint
    ## 1       661 historical   1986-2005   mean    fair   jan 16.941667
    ## 2       661 historical   1986-2005   mean    fair   feb 13.875000
    ## 3       661 historical   1986-2005   mean    fair   mar 11.141667
    ## 4       661 historical   1986-2005   mean    fair   apr  7.383333
    ## 5       661 historical   1986-2005   mean    fair   may  4.583333
    ## 6       661 historical   1986-2005   mean    fair   jun  2.091667

## Plot

Plot data for all stations and scenarios:

``` r
for (stat_id in gridpoint_indexes) {
    for (scen in scenarios) {
        p <- plotdata(stat_id, scen, all_hci_data)
        print(p)
    }
}
```

![](HCI_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-25.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-26.png)<!-- -->![](HCI_files/figure-gfm/unnamed-chunk-4-27.png)<!-- -->

Save all the plots:

``` r
for (stat_id in gridpoint_indexes) {
    for (scen in scenarios) {
        stat_name <- names[match(stat_id, gridpoint_indexes)]
        print(paste(stat_name, scen))

        p <- plotdata(stat_id, scen, all_hci_data)
        
        ggsave(paste(gsub(" ", "_", stat_name), "_", scen, ".pdf", sep=""), p, width=8, height=4, units="in", path="../output", device=cairo_pdf)
        ggsave(paste(gsub(" ", "_", stat_name), "_", scen, ".eps", sep=""), p, width=8, height=4, units="in", path="../output", device=cairo_ps)
        ggsave(paste(gsub(" ", "_", stat_name), "_", scen, ".png", sep=""), p, width=8, height=4, units="in", path="../output", dpi=500)
    }
}
```
