library(dplyr)
newFlights <- select(flights, "carrier", "origin", "dep_time", "arr_time", "dep_delay", "arr_delay")
newFlights <- na.omit(newFlights)
colSums(is.na(newFlights))

