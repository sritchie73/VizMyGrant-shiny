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

# Create a lookup table of UI queries to variable type and data.table calls
lookup <- list(
  "Number Funded"=list(
    var.type="discrete, positive, point",
    var.query=".N",
    query.loc="j",
    table="full2014"
  ),
  "Total Amount Awarded"=list(
    var.type="continuous, positive, point",
    var.query="sum(TotalAmount, na.rm=TRUE)",
    table="full2014"
  ),
  "Median Grant Length"=list(
    var.type="point, positive, discrete",
    var.query="cleanMedian(GrantLength, 'integer')",
    table="full2014"
  ),
  "Median Amount Awarded"=list(
    var.type="point, positive, continuous",
    var.query="cleanMedian(TotalAmount, 'double')",
    table="full2014"
  ),
  "Sex"=list(
    var.type="categorical",
    var.query="Gender",
    table="full2014",
    colors=list(
      female="#f1a340",
      male="#998ec3"
    ),
    order=c("female", "male")
  ),
  "Career Stage"=list(
    var.type="categorical",
    var.query="CareerStage",
    table="full2014",
    order=c(
      "Early Career", "Mid Career", "Senior"  
    )
  ),
  "Grant Type"=list(
    var.type="categorical",
    var.query="GrantType",
    table="full2014"
  ),
  "Grant Sub Type"=list(
    var.type="categorical",
    var.query="GrantSubType",
    table="full2014"
  ),
  "Fellowship Grants"=list(
    var.type="categorical",
    var.query="!is.na(isSalaryGrant) & isSalaryGrant == TRUE",
    table="full2014"
  ),
  "Non Fellowship Grants"=list(
    var.type="categorical",
    var.query="!is.na(isSalaryGrant) & isSalaryGrant == FALSE",
    table="full2014"
  ),
  "State"=list(
    var.type="categorical",
    var.query="State",
    table="full2014"
  ),
  "Institution"=list(
    var.type="categorical",
    var.query="Institution",
    table="full2014"
  ),
  "Broad Research Area"=list(
    var.type="categorical",
    var.query="BroadArea",
    table="full2014"
  ),
  "Field of Research Category"=list(
    var.type="categorical",
    var.query="Category",
    table="full2014"
  ),
  "Field of Research"=list(
    var.type="categorical",
    var.query="FoR",
    table="full2014"
  ),
  "Individual's Title"=list(
    var.type="categorical",
    var.query="Title",
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

# get the clean_data/.table query
getDTQ <- function(ui.query) {
  lookup[[ui.query]][["var.query"]]
}

getTable <- function(ui.query) {
  lookup[[ui.query]][["table"]]
}

# Get the ordering of labels by value size or implicit ordering
# Table needs to be provided because we may be looking at the subset
getOrder <- function(cat.var, val.query, table) {
  implicit <- lookup[[cat.var]][["order"]]
  cat.query <- getDTQ(cat.var)
  
  if (is.null(implicit)) {
    eval(parse(
      text=paste0(
        "cats <- table[!is.na(", cat.query, "), unique(", cat.query, ")]"
      )  
    ))
    vals <- sapply(cats, function(cat) {
      eval(parse(
        text=paste0(
          "table[!is.na(", cat.query, ") & ", cat.query, ' == "', cat, '", ',
          val.query, "]"
        )
      ))
    })
    names(vals) <- cats
    return(rev(names(sort(vals))))
  } else {
    return(implicit)
  }
}
