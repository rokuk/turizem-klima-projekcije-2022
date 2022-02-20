# this script defines some commonly used functions

library(ncdf4)
library(dplyr)

month_names = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "okt", "nov", "dec")
scenarios <- c("RCP2.6", "RCP4.5", "RCP8.5")
time_periods <- c("2021_2040", "2041_2060", "2081_2100")
day_categories <- c("fair", "good", "unf")
metrics <- c("mean", "10prct", "90prct")

names <- c("Rateče", "Bilje", "Koper", "Ljubljana", "Novo mesto", "Celje", "Slovenj Gradec", "Maribor", "Murska Sobota")
gridpoint_indexes <- c(706, 661, 698, 902, 1060, 1064, 1066, 1186, 1347) # these points of the grid are manualy chosen, see CIT-HCI-gridpoints file in plotting folder

"The datasets downloaded from CDS are saved in netcdf files. We define a 
function that opens the file, reads longitude, latitude, rotated longitude, 
rotated latitude, the number of days (saved under `datavarname` variable in the 
file). Only the data near Slovenia is read to conserve memory."
readcdf <- function(filepath, datavarname) {
    nc_data <- nc_open(filepath)
    
    rlonstart = 215
    rlatstart = 150
    loncount = 45
    latcount = 40
    
    lon <- ncvar_get(nc_data, "lon", start=c(rlonstart, rlatstart), count=c(loncount, latcount))
    lat <- ncvar_get(nc_data, "lat", start=c(rlonstart, rlatstart), count=c(loncount, latcount))
    rlon <- ncvar_get(nc_data, "rlon", start=c(rlonstart), count=c(loncount))
    rlat <- ncvar_get(nc_data, "rlat", start=c(rlatstart), count=c(latcount))
    
    data.array <- ncvar_get(nc_data, datavarname, start=c(rlonstart, rlatstart, 1), count=c(loncount, latcount, -1))
    
    nc_close(nc_data)
    
    returnlist <- list("lon" = lon, "lat" = lat, "rlon" = rlon, "rlat" = rlat, "data"=data.array)
    
    return (returnlist)
}

"Datapoints in the netcdf file use rotated lon and lat coordinates. We need 
regular longitude and latitude. The file also contains a table of lon(rlon, rlat) 
and a table of lat(rlon, rlat) coordinates for conversion. We compose a data 
frame with longitude, latitude and number of days."
transform_coords <- function(netcdfdata) {
    lons = c()
    lats = c()
    datapoints = c()
    months = c()
    ids = c()
    count = 0
    
    for (i in 1:length(netcdfdata$rlon)) {
        for (j in 1:length(netcdfdata$rlat)) {
            count <- count + 1
            ids <- c(ids, rep(count, 12))
            lons <- c(lons, rep(netcdfdata$lon[i,j], 12))
            lats <- c(lats, rep(netcdfdata$lat[i,j], 12))
            months <- c(months, month_names)
            datapoints <- c(datapoints, netcdfdata$data[i, j,])
        }
    }
    
    return (data.frame(
        id=ids,
        lon=lons,
        lat=lats,
        month=months,
        datapoint=datapoints
    ))
}

"Read data from all netcdf files at selected grid points and assemble it into a 
single dataframe (`alldata`) for plotting. Grid points are the same for all datasets.
Parameter `quantity` is either 'cit' or 'hci'."
assembledata <- function(quantity) {
    alldata <- data.frame(matrix(ncol = 6, nrow = 0)) # create empty dataframe

    # read and extract historical data
    for (metric in metrics) {
        for (day_cat in day_categories) {
            
            print(paste("historical", "1986_2005", metric, day_cat))
            
            filepath <- paste("../data/", quantity, "/historical/", metric, "/C3S422Lot2TEC_day-" , day_cat, "-", quantity, "-month-proj_", metric, "_monthly_1986_2005_v1.nc", sep = "")
            datavarname <- paste("day-", day_cat, "-", quantity, "-month-proj", sep="")
            
            dataset <- readcdf(filepath, datavarname) %>% transform_coords() %>% filter(id %in% gridpoint_indexes)
            
            alldata <- rbind(alldata, data.frame(
                stationid=dataset$id, 
                scenario="historical", 
                time_period="1986-2005", 
                metric=metric, 
                day_cat=day_cat, 
                month=dataset$month, 
                datapoint=dataset$datapoint))
        }
    }

    # read and extract RCP2.6, RCP4.5 and RCP8.5 data
    for (scenario in scenarios) {
        for (time_period in time_periods) {
            for (metric in metrics) {
                for (day_cat in day_categories) {
                    
                    print(paste(scenario, time_period, metric, day_cat))
                    
                    filepath <- paste("../data/", quantity, "/", scenario, "/", metric, "/C3S422Lot2TEC_day-" , day_cat, "-", quantity, "-month-proj_", metric, "_monthly_", time_period, "_v1.nc", sep = "")
                    datavarname <- paste("day-", day_cat, "-", quantity, "-month-proj", sep="")
                    
                    dataset <- readcdf(filepath, datavarname) %>% transform_coords %>% filter(id %in% gridpoint_indexes)
                    
                    alldata <- rbind(alldata, data.frame(
                        stationid=dataset$id, 
                        scenario=scenario, 
                        time_period=sub("_", "-", time_period), 
                        metric=metric, 
                        day_cat=day_cat, 
                        month=dataset$month, 
                    datapoint=dataset$datapoint))
                }
            }
        }
    }
    
    return (alldata)
}

"A function to calculate min and max y position for errorbars in a stacked barplot"
adderrorbars <- function(subset) {
    means <- subset[subset$metric=="mean",]
    prct10 <- subset[subset$metric=="10prct",]
    prct90 <- subset[subset$metric=="90prct",]
    means$prct10 <- prct10$datapoint
    means$prct90 <- prct90$datapoint
    
    means_fair.mean <- filter(means, day_cat=="fair")$datapoint
    means_good.mean <- filter(means, day_cat=="good")$datapoint
    
    means_fair <- filter(means, day_cat == "fair") %>%
        mutate(lower = prct10,
               upper = prct90)
    
    means_good <- filter(means, day_cat == "good") %>%
        mutate(lower = means_fair.mean + prct10,
               upper = means_fair.mean + prct90)
    
    means_unf <- filter(means, day_cat == "unf") %>%
        mutate(lower = means_fair.mean + means_good.mean + prct10,
               upper = means_fair.mean + means_good.mean + prct90)
    
    return (rbind(means_fair, means_good, means_unf))
}

"A function to plot data for specific point (`stat_id`) and scenario (`scen`). 
To produce figures without errorbars, comment the line with `geom_errorbar()`."
plotdata <- function(stat_id, scen, alldata) {
    subset <- filter(alldata, stationid == stat_id & (scenario == scen | scenario == "historical")) %>% adderrorbars()
    
    p <- ggplot(data=subset,
                mapping=aes(x=time_period, y=datapoint, fill=factor(day_cat, levels=c("unf", "good", "fair")))) +
        geom_col() +
        geom_errorbar(mapping=aes(ymax=upper, ymin=lower, color=factor(day_cat, levels=c("unf", "good", "fair"))), stat="identity", width=0.5) + # to disable errorbars comment this line
        scale_colour_manual(values = c("#1C678D", "#AA5A00", "#006147"), guide="none") + # errorbar color
        facet_grid(~factor(month, levels=month_names)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02)), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
        guides(x = guide_axis(angle = 90)) +
        labs(title=names[match(stat_id, gridpoint_indexes)],
             subtitle = scen, fill="CIT category") +
        xlab("period") +
        ylab("days per month") +
        scale_fill_manual(values = c("#4CACDC", "#EAAA00", "#009E73")) +
        theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))
    
    return (p)
}