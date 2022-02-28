Weather stations
================

This code reads data from Excel files (received from ARSO) and makes
plots for selected stations.

## Importing libraries and data

``` r
library(readxl)
library(dplyr)
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
library(purrr)
library(forcats)
library(ggplot2)
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.4.0, PROJ 8.1.1; sf_use_s2() is TRUE

``` r
library(rnaturalearth)
```

Define variable values:

``` r
filenames <- c("rr1", "rr2", "snow", "warm", "hot", "tropical") # excel file names
variablenames <- c("Days with at least 1 mm of precipitation", "Days with at least 20 mm of precipitation", "Days with snow", expression("Warm days (T"["max"]*" \u2265 25\u00B0C)"), expression("Hot days (T"["max"]*" \u2265 30\u00B0C)"), expression("Tropical nights (T"["min"]*" \u2265 20\u00B0C)")) # display variable names
axislabels <- c("number of days per month", "number of days per month", "number of days per month", "number of days per month", "number of days per month", "number of nights per month")
projnames <- c("RCP45", "RCP85", "RCP26") # excel sheet names
scenarionames <- c("RCP4.5", "RCP8.5", "RCP2.6") # display scenario names
monthnames <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
```

Read data from Excel files:

``` r
refdata <- data.frame(matrix(nrow=0, ncol=6))
projdata <- data.frame(matrix(nrow=0, ncol=8))

for (file in filenames) {
    # read reference data
    ref <- read_excel(paste("../data/arso/", file, ".xlsx", sep=""), sheet=1)
    colnames <- names(ref)
    ref$month <- fct_relabel(factor(ref$month), function(x) monthnames[as.numeric(x)])
    
    for (statid in 2:length(colnames)) {
        refdata <- rbind(refdata, data.frame(
            var = file,
            period = "1981-2010",
            scenario = "ref",
            month = ref$month,
            station = colnames[statid],
            refvalue = pull(ref, colnames[statid])
        ))
    }
    
    # read RCP4.5, RCP8.5 and RCP2.6 data
    for (projid in 1:3) {
        proj <- read_excel(paste("../data/arso/", file, ".xlsx", sep=""), sheet = projid+1)
        proj$month <- fct_relabel(factor(proj$month), function(x) monthnames[as.numeric(x)])
        projdata <- rbind(projdata, data.frame(
            var = file,
            period = proj$period,
            scenario = scenarionames[projid],
            month = proj$month,
            station = proj$station,
            median = proj$median,
            min = proj$min,
            max = proj$max
        ))
    }
}

head(refdata)
```

    ##   var    period scenario month    station  refvalue
    ## 1 rr1 1981-2010      ref   jan NOVO MESTO  7.600000
    ## 2 rr1 1981-2010      ref   feb NOVO MESTO  6.794444
    ## 3 rr1 1981-2010      ref   mar NOVO MESTO  8.611111
    ## 4 rr1 1981-2010      ref   apr NOVO MESTO 10.405556
    ## 5 rr1 1981-2010      ref   may NOVO MESTO 11.911111
    ## 6 rr1 1981-2010      ref   jun NOVO MESTO 11.783333

``` r
head(projdata)
```

    ##   var    period scenario month                           station    median
    ## 1 rr1 2011-2040   RCP4.5   jan                        NOVO MESTO 0.3000000
    ## 2 rr1 2011-2040   RCP4.5   jan                    CELJE - MEDLOG 0.4166667
    ## 3 rr1 2011-2040   RCP4.5   jan           MURSKA SOBOTA - RAKIČAN 0.8333333
    ## 4 rr1 2011-2040   RCP4.5   jan                            RATEČE 0.5333333
    ## 5 rr1 2011-2040   RCP4.5   jan LETALIŠČE EDVARDA RUSJANA MARIBOR 0.6000000
    ## 6 rr1 2011-2040   RCP4.5   jan              LJUBLJANA - BEŽIGRAD 0.3166667
    ##          min      max
    ## 1 -0.8000000 2.400000
    ## 2 -1.0333333 2.633333
    ## 3 -1.1000000 2.600000
    ## 4 -1.3333333 1.933333
    ## 5 -0.9333333 2.266667
    ## 6 -0.7333333 2.166667

## Data wrangling

Projection data is given as a deviation from the reference value. Get
reference values for each projection datapoint and add them to median,
min and max values to obtain actual projection values:

``` r
refvals <- c()
for (i in 1:nrow(projdata)) {
    refvals <- rbind(refvals, refdata[refdata$month==projdata[i,4] & refdata$station==projdata[i,5] & refdata$var==projdata[i,1], "refvalue"])
}

projdata$median <- projdata$median + refvals
projdata$min <- projdata$min + refvals
projdata$max <- projdata$max + refvals

head(projdata)
```

    ##   var    period scenario month                           station   median
    ## 1 rr1 2011-2040   RCP4.5   jan                        NOVO MESTO 7.900000
    ## 2 rr1 2011-2040   RCP4.5   jan                    CELJE - MEDLOG 7.983333
    ## 3 rr1 2011-2040   RCP4.5   jan           MURSKA SOBOTA - RAKIČAN 6.105556
    ## 4 rr1 2011-2040   RCP4.5   jan                            RATEČE 9.144444
    ## 5 rr1 2011-2040   RCP4.5   jan LETALIŠČE EDVARDA RUSJANA MARIBOR 6.827778
    ## 6 rr1 2011-2040   RCP4.5   jan              LJUBLJANA - BEŽIGRAD 8.277778
    ##        min       max
    ## 1 6.800000 10.000000
    ## 2 6.533333 10.200000
    ## 3 4.172222  7.872222
    ## 4 7.277778 10.544444
    ## 5 5.294444  8.494444
    ## 6 7.227778 10.127778

Combine reference and projection data frames for plotting, also rename
stations:

``` r
alldata <- data.frame(matrix(nrow=0, ncol=8))
alldata <- rbind(alldata, projdata)

names(refdata)[names(refdata) == "refvalue"] <- "median"
refdata$min <- NA # reference data error bar range is set to 0
refdata$max <- NA

# add reference data to alldata frame for each scenario
refdata$scenario <- "RCP4.5"
alldata <- rbind(alldata, refdata)
refdata$scenario <- "RCP8.5"
alldata <- rbind(alldata, refdata)
refdata$scenario <- "RCP2.6"
alldata <- rbind(alldata, refdata)

alldata[alldata$station=="BILJE", "station"] <- "Bilje"
alldata[alldata$station=="CELJE - MEDLOG", "station"] <- "Celje Medlog"
alldata[alldata$station=="LETALIŠČE EDVARDA RUSJANA MARIBOR", "station"] <- "Letališče Edvarda Rusjana Maribor"
alldata[alldata$station=="LJUBLJANA - BEŽIGRAD", "station"] <- "Ljubljana Bežigrad"
alldata[alldata$station=="MURSKA SOBOTA - RAKIČAN", "station"] <- "Murska Sobota Rakičan"
alldata[alldata$station=="NOVO MESTO", "station"] <- "Novo mesto"
alldata[alldata$station=="PORTOROŽ - LETALIŠČE", "station"] <- "Portorož letališče"
alldata[alldata$station=="RATEČE", "station"] <- "Rateče"
alldata[alldata$station=="ŠMARTNO PRI SLOVENJ GRADCU", "station"] <- "Šmartno pri Slovenj Gradcu"
```

## Plots

``` r
plotdata <- function(alldata, variable, stat) {
    subset <- filter(alldata, var==variable & station==stat)

    p <- ggplot(data = subset, 
                mapping = aes(x = period, y = median, fill = period)) + 
        geom_col() +
        facet_grid(scenario~month) + 
        scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9", "#D55E00")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
        ylab(axislabels[match(variable, filenames)]) + 
        labs(title = variablenames[match(variable, filenames)], subtitle = stat, fill="period") +
        geom_errorbar(mapping = aes(ymax=max, ymin=min), stat="identity", size=0.3, width=0.9) +
        theme(panel.grid.major.x = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank())
    
    return (p)
}

plotdata2 <- function(alldata, variable, scen) {
    subset <- filter(alldata, var==variable & scenario==scen)

    p <- ggplot(data = subset, 
                mapping = aes(x = period, y = median, fill = period)) + 
        geom_col() +
        facet_grid(station~month, labeller = label_wrap_gen(width=10)) + 
        scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9", "#D55E00")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
        ylab(axislabels[match(variable, filenames)]) + 
        labs(title = variablenames[match(variable, filenames)], subtitle = scen, fill="period") +
        geom_errorbar(mapping = aes(ymax=max, ymin=min), stat="identity", size=0.3, width=0.9) +
        theme(panel.grid.major.x = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              strip.text.y.right = element_text(angle = 0))
    
    return (p)
}

#plotdata(alldata, "rr1", "Rateče")
#plotdata2(alldata, "snow", "RCP4.5")
```

``` r
for (var in filenames) {
    for (stat in distinct(alldata, station)$station) {
        p <- plotdata(alldata, var, stat)
        print(p)
    }
}
```

![](WeatherStations_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-16.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-17.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-18.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-19.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-20.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-21.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-22.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-23.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-24.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-25.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-26.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-27.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-28.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-29.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-30.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-31.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-32.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-33.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-34.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-35.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-36.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-37.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-38.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-39.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-40.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-41.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-42.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-43.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-44.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-45.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-46.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-47.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-48.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-49.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-50.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-51.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-52.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-53.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-7-54.png)<!-- -->

``` r
for (var in filenames) {
    for (scen in c("RCP2.6", "RCP4.5", "RCP8.5")) {
        p <- plotdata2(alldata, var, scen)
        print(p)
    }
}
```

![](WeatherStations_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->![](WeatherStations_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->

Save all the plots:

``` r
for (var in filenames) {
    for (stat in distinct(alldata, station)$station) {
        print(paste(var, stat))

        p <- plotdata(alldata, var, stat)
        
        ggsave(paste("stations_", gsub(" ", "_", stat_name), "_", scen, ".pdf", sep=""), p, width=9, height=4, units="in", path="../output", device=cairo_pdf)
        ggsave(paste("stations_", gsub(" ", "_", stat_name), "_", scen, ".eps", sep=""), p, width=9, height=4, units="in", path="../output", device=cairo_ps)
        ggsave(paste("stations_", gsub(" ", "_", stat_name), "_", scen, ".png", sep=""), p, width=9, height=4, units="in", path="../output", dpi=500)
    }
}
```
