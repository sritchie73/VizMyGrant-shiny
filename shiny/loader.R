# Load in the data
options(stringsAsFactors=FALSE)
library(data.table)

full2014 <- as.data.table(read.csv("data/cleaned.csv"))
funding2014 <- as.data.table(read.csv("data/funding-by-year.csv"))
geoLocations <- as.data.table(read.csv("data/InstitutionNMHRC_geocoded.csv"))

setkey(full2014, ApplicationId)
setkey(funding2014, ApplicationId)
setnames(geoLocations, "original.address", "Institution")
setkey(geoLocations, "Institution")

# Create a lookup table of UI queries to variable type
lookup <- list(
  "Number Funded"=list(
    var.type="discrete, positive, point",
    var.query=".N",
    query.loc="j",
    table="full2014"
  ),
  "Total Amount Awarded"=list(
    var.type="continuous, positive, point",
    var.query="sum(TotalAmount)",
    table="full2014"
  ),
  "Gender"=list(
    var.type="categorical",
    var.query="Gender",
    table="full2014",
    colors=list(
      female="#e9a3c9",
      male="#91bfdb"
    )
  ),
  "Career Stage"=list(
    var.type="categorical",
    var.query="CareerStage",
    query.loc="j",
    table="full2014",
    order=c(
      "Early Career", "Mid Career", "Senior"  
    )
  ),
  "Salary Grants"=list(
    var.type="categorical",
    var.query="!is.na(isSalaryGrant) & isSalaryGrant == TRUE",
    table="full2014"
  ),
  "Non Salary Grants"=list(
    var.type="categorical",
    var.query="!is.na(isSalaryGrant) & isSalaryGrant == FALSE",
    table="full2014"
  ),
  "None"=list(
    var.type=NA,
    var.query=NA,
    table=NA
  )
)

getType <- function(ui.query) {
  lookup[[ui.query]][["var.type"]]  
}

# get the data.table query
getDTQ <- function(ui.query) {
  lookup[[ui.query]][["var.query"]]
}

getTable <- function(ui.query) {
  lookup[[ui.query]][["table"]]
}
