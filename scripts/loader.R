# Load in the clean_data/
options(stringsAsFactors=FALSE)
library(data.table)

full2014 <- as.data.table(read.csv("clean_data/cleaned.csv"))
funding2014 <- as.data.table(read.csv("clean_data/funding-by-year.csv"))
geoLocations <- as.data.table(read.csv("clean_data/InstitutionNMHRC_geocoded.csv"))

# fix column types
full2014[, TotalAmount := as.numeric(TotalAmount)]

setkey(full2014, ApplicationId)
setkey(funding2014, ApplicationId)
setnames(geoLocations, "original.address", "Institution")
setkey(geoLocations, "Institution")
