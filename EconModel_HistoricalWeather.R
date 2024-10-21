########################################
#
# Swedish sugar beets economic model, Historical weather data
# Will English, 2021-03-18
# Version 2023-10-19 R v4.3.1, snapshot 2023-08-01
#
########################################


###############################################
#
# Setup
#
###############################################

{
  # -------------------------------------------
  snapshot_date = "2023-08-01"
  options("repos" = paste0("https://packagemanager.posit.co/cran/", snapshot_date))
  # -------------------------------------------
  
  # -------------------------------------------
  # sink options
  options(width = 150)
  # rJava memory option
  options(java.parameters = "-Xmx8000m")
  # -------------------------------------------
  
  # R packages
  # -------------------------------------------
  Rpackages_version = c("data.table_1.14.2",
                        "dplyr_1.0.7",
                        "ggplot2_3.3.5",
                        "tidyr_1.1.4",
                        "writexl_1.4.0",
                        "lubridate_1.8.0",
                        "httr_1.4.2",
                        "stringr_1.4.0",
                        "vctrs_0.3.8",
                        "png_0.1-7",
                        "readxl_1.3.1",
                        "jsonlite_1.7.2"
  )
  path_Rpackages = "C:/R packages_431"
  # -------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.3.1 (2023-06-16 ucrt)") stop("R.version must be 4.3.1 (2023-06-16 ucrt)")
  
  # install packages
  Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
  Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
  if(!all(Rpack %in% list.files(path_Rpackages))){
    loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
    for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
  }
  
  # load packages
  for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
  
  # Grid is a base package, so shouldn't be in the above system
  library("grid")
  
  rm(list=ls())  
}

############################################
############################################
# Definitions
## Data to collect from LantMet and Meteometics
### TIME AND TIME ZONES
time_zone <- "Europe/Copenhagen"
startdate <- ISOdatetime(year = as.integer("2022",'%Y'),
                         month = as.integer("09",'%m'),
                         day = as.integer("10",'%d'),
                         hour = 00, min = 00, sec = 00, tz = "UTC") #DK format
startDate <- date(startdate)                                        #SE format
enddate <- ISOdatetime(year = as.integer("2023",'%Y'),
                       month = as.integer("02",'%m'),
                       day = as.integer("28",'%d'),
                       hour = 00, min = 00, sec = 00, tz = "UTC")   #DK format
endDate <- date(enddate)                                            #SE format
# enddate <- ISOdatetime(year = as.integer(strftime(today(),'%Y')),
#                        month = as.integer(strftime(today(),'%m')),
#                        day = as.integer(strftime(today(),'%d')),
#                        hour = 00, min = 00, sec = 00, tz = "UTC")   #DK format
# endDate <- date(enddate) - 1                                        #SE format

### LOCATIONS
station_id <- c(20947, 36010, 40007, 40141, 40143, 40144, 40145, 41105) # NBRs stations = 40141, 40142, 40143, 40144, 40145
station_names <-c("Borgeby","Hammenhög","Sandby gård","Gretelund","Lovisero","Tofta","Hviderup", "Jonstorp")
station_land <- c("SE", "SE","SE","SE","SE","SE","SE","SE")
station_lat <- c(55.7530, 55.48809, 55.4395, 55.8864, 55.3772, 55.8935, 55.7779, 56.2295)
station_long <- c(13.0396, 14.19498, 14.1827, 14.0355, 13.4048, 12.9146, 13.3217, 12.6535)
stations <- data.frame("WSTNID" = station_id, "WSTN_namn" = station_names, "land" = station_land,"lat" = station_lat, "long" = station_long)


### API-RELEVANT PARAMETERS
#### SE / LANTMET
logInterval <- 2                                # 1= hourly, 2 = daily
elementMeasurement <- "TM"

############################################
# Get data 
## SWEDEN
### create matrix of all urls = startDate x stations x elements
len_startDate <- length(startDate)
len_stations <- nrow(stations[which(stations$land == "SE"),])
len_ele <- length(elementMeasurement)
len_matrix <- len_startDate*len_stations*len_ele
startDate_c <- rep(startDate, len_matrix/len_startDate)
endDate_c <- rep(endDate, len_matrix)
stations_c <- rep(stations$WSTNID[which(stations$land == "SE")], each=len_matrix/len_stations)
logInterval_c <- rep(logInterval, len_matrix)
elementMeasurement_c <- rep(elementMeasurement, each=len_matrix/len_ele)
urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"

### create url
for (i in 1:len_matrix){
  urlStation <- paste0("weatherStationID=",stations_c[i])
  urlStart <- paste0("startDate=",startDate_c[i])
  urlEnd <- paste0("endDate=",endDate_c[i])
  urlLog <- paste0("LogIntervalID=",logInterval_c[i])
  urlEle <- paste0("elementMeasurementTypeList=",elementMeasurement_c[i])
  url <- paste(urlBase,urlStation,urlStart,urlEnd,urlLog,urlEle,sep="&")

  # Import from Lantmet. Need to add a time-out and re-run to this...
  dat_in_SE_i <- data.frame(fread(url))
  dat_in_SE_i <- dat_in_SE_i %>%
    mutate_at(c(-1,-ncol(dat_in_SE_i)), as.numeric)

  if(i==1L) dat_in_SE <- dat_in_SE_i else dat_in_SE <- rbind(dat_in_SE, dat_in_SE_i)
}

rm(dat_in_SE_i)

# visual checks
ggplot(data=dat_in_SE[which(dat_in_SE$WSTNID == 20947),], aes(x=DAY, y=TM, group=1)) +
  geom_line()+
  geom_point()

dat_in_SE <- dat_in_SE %>%
  left_join(stations, by = "WSTNID") %>%
  mutate(source = "lantmet")

dat_sum <- dat_in_SE %>% 
  group_by(DAY) %>% 
  summarise(mean = mean(TM)) %>%
  ungroup()

write_xlsx(list(summary = dat_sum, full_data = dat_in_SE), "SE_2023.xlsx")

dat_sum_vec <- unlist(dat_sum$mean)
dat_sum_vec <- paste(round(dat_sum_vec,digits=1), collapse = ",")
dat_sum_vec
