# update_data.R
# Pulls USGS water quality data daily through GitHub Actions and saves as CSV daily for Shiny App to read
# author: H. Short
# 2026-08-26 08:34:53

# -------------------------------------------------------------------------------
# load packages
library(dataRetrieval)
library(tidyverse)

# -------------------------------------------------------------------------------
# figure out the start of the water year using "today's" sys.date
get_wy_start <- function(date = Sys.Date()) {
  # pulls sys.date as.Date
  date <- as.Date(date)
  #if the current month is Oct, Nov, Dec, then the water year is the calendar year
  if (month(date) >= 10) {
    as.Date(paste0(year(date), "-10-01"))
  } 
  # if the current month is Jan-Sep, then the water year is the previous (-1) calendar year
  else {
    as.Date(paste0(year(date) - 1, "-10-01"))
  }
}

# use function to get start of water year
start_date <- get_wy_start(Sys.Date())
# end will always be the date the code is run
end_date   <- Sys.Date()                  

# -------------------------------------------------------------------------------
# using the date of interest, calculates the appropriate water year
calc_water_year <- function(date) {
  date <- as.Date(date)
  yr <- year(date)
  if_else(month(date) >= 10, yr + 1, yr)
}

# -------------------------------------------------------------------------------
# lookup tables for stations and parameters of interest
stations_parameters <- tibble::tribble(~station_id,     ~p_code,
                                       "USGS-11447650", "00010",
                                       "USGS-11447650", "00060",
                                       "USGS-11447650", "72137",
                                       "USGS-11447650", "63680",
                                       "USGS-11447650", "00300",

                                       "USGS-11447905", "72137",
                                       "USGS-11447905", "00060")

parameter_lookup <- tibble::tribble(~p_code, ~value_type,                    ~units,
                                    "00010", "Temperature",                  "deg C",
                                    "00060", "Discharge",                    "cfs",
                                    "72137", "Discharge, tidally filtered",  "cfs",
                                    "63680", "Turbidity",                    "FNU",
                                    "00300", "Dissolved Oxygen",             " mg/l")

station_lookup <- tibble::tribble(~station_id,      ~location,
                                  "USGS-11447650",  "Freeport",
                                  "USGS-11447905",  "Sacramento River below Georgiana Slough")


# -------------------------------------------------------------------------------
# function to pull data from one station/parameter combo
get_USGS_data <- function(monitoring_location_id, parameter_code, start_date, end_date) {

  df <- read_waterdata_continuous(monitoring_location_id = monitoring_location_id,
                                  parameter_code = parameter_code,
                                  time = c(start_date, end_date)) %>%
            mutate(station_id = monitoring_location_id,
                   p_code = parameter_code)

  return(df)

} # end function

# -------------------------------------------------------------------------------
updated_wq_data <- stations_parameters |>
  mutate(data = map2(station_id, p_code,
                    ~ possibly(get_USGS_data, otherwise = NULL)(.x, .y, start_date, end_date))) |>
  select(-station_id, -p_code) |>
  unnest(data) |>
  left_join(parameter_lookup, by = "p_code") |>
  left_join(station_lookup, by = "station_id") |>
  rename(datetime = time) |>
  mutate(downloaded_from = "USGS",
         water_year = calc_water_year(datetime)) |>
  select(downloaded_from, station_id, location, water_year, datetime, value, value_type, units)

# -------------------------------------------------------------------------------
# double check before overwriting the CSV

# if the pull failed or came back empty, stop here rather than overwriting a good CSV with an empty/broken one
if (nrow(test_df) == 0) {
  stop("No data returned from USGS pull — stopping before overwriting CSV.")
}

# -------------------------------------------------------------------------------
# write the new and updated water quality csv
write_csv(updated_wq_data, "data/current_wq_data.csv")

message("Data updated successfully: ", nrow(test_df), " rows written at ", Sys.time())