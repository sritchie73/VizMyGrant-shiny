 library(xlsx)
library(data.table)
library(stringr)
library(gender)

options(stringsAsFactors=FALSE)

oct2014 <- read.xlsx(
  file="raw_data/summary_data_17_october_2014_announcement_141020.xlsx", 
  sheetName="Grants Data"
)
oct2014 <- as.data.table(oct2014)

aug2014 <- read.xlsx(
  file="raw_data/summary_data_august_2014_announcement_140919.xlsx",
  sheetName="Data"
)
aug2014 <- as.data.table(aug2014)

# Strip out titles from names
titles <- c(
  sapply(str_split(oct2014[,CIA], " "), `[[`, 1),
  sapply(str_split(aug2014[,CIA_NAME], " "), `[[`, 1)
)
titles[titles == "Research"] <- NA

names <- gsub(
    pattern=paste(paste0("(", unique(titles)[!is.na(unique(titles))], " )"), collapse="|"),
    "", c(oct2014[,CIA], aug2014[,CIA_NAME])
  )

# build cleaned dataset
clean <- data.table(
    ApplicationId = c(
      oct2014[,APP.ID], 
      aug2014[,APP.ID]
    ),
    Title = factor(
      titles, levels=c("Mr", "Ms", "Miss", "Mrs", "Dr", "A/Pr", "E/Pr,", "Prof")
    ),
    Name = names,
    Gender = NA_character_,
    CareerStage = NA_character_,
    GrantType = c(
      oct2014[,Grant.Type], 
      aug2014[,GRANT_TYPE]
    ),
    isSalaryGrant = c(FALSE),
    GrantSubType = c(
      oct2014[,Sub.Type], 
      aug2014[,SUB_TYPE]
    ),
    Institution = c(
      oct2014[,Admin.Institution], 
      aug2014[,ADMIN_.INSTITUTION]
    ),
    State = c(
      oct2014[,State], 
      aug2014[,STATE]
    ),
    BroadArea = c(
      oct2014[, Broad.Research.Area], 
      aug2014[,BROAD_RESEARCH_AREA]
    ),
    Category = c(
      oct2014[,FOR.Category],
      aug2014[,FOR_.CATEGORY]
    ),
    FoR = c(
      gsub(",", "", oct2014[,Field.of.Research]),
      gsub(",", "", aug2014[,FIELD_OF_..RESEARCH])
    ),
    TotalAmount = c(
      oct2014[,TOTAL],
      aug2014[,TOTAL]
    )
  )

# determine if the grant is a salary-based grant or not
clean[
  grepl("(Fellowships)", GrantType) |
  grepl("(ECF$)|(CDF$)|(Fellowship)", GrantSubType),
  isSalaryGrant := TRUE
]

# determine gender
first_names <- sapply(str_split(clean[,names], " "), `[[`, 1)
gender <- sapply(gender(first_names), `[[`, "gender")
clean[,Gender := gender]

clean[
  Title %in% c("Mrs", "Miss", "Ms"),
  Gender := "female"
]

clean[
  Title %in% c("Mr"),
  Gender := "male"
]

# determine career stage
clean[
  Title == "A/Pr",
  CareerStage := "Mid Career"
]
 
clean[
  Title == "Dr",
  CareerStage := "Early Career"
]
 
clean[
  grepl("(Early Career Fellowship)", GrantType) |
  grepl("(ECF$)", GrantSubType),
  CareerStage := "Early Career"
]

clean[
  grepl("(Career Development Fellowship)", GrantType) |
  grepl("(CDF$)", GrantSubType),
  CareerStage := "Mid Career"
]

clean[
  grepl("(Research Fellowship)", GrantSubType),
  CareerStage := "Senior"
]

clean[
  grepl("(NHMRC Partnerships)", GrantType),
  CareerStage := "Senior"
]

clean[
  Title %in% c("Mr", "Ms", "Mrs", "Miss"),
  CareerStage := "Early Career"
]

clean[
  Title %in% c("E/Pr", "Prof"),
  CareerStage := "Senior"
]



# Hack together long dataset
fundOct2014 <- oct2014[,      
  c(
    match("APP.ID", colnames(oct2014)), 
    grep("X[0-9][0-9][0-9][0-9]", colnames(oct2014))
  ),
  with=FALSE
]

fundAug2014 <- aug2014[,
  c(
    match("APP.ID", colnames(aug2014)), 
    grep("X[0-9][0-9][0-9][0-9]", colnames(aug2014))
  ),
  with=FALSE
]

fundAug2014[,X2014 := NA_real_]

fundTable <- rbind(fundOct2014, fundAug2014)

longTable <- data.table(
  ApplicationId = rep(fundTable[,APP.ID], 6),
  Year = as.vector(sapply(c(2014:2019), rep, fundTable[,.N])),
  Amount = c(
    fundTable[,X2014],
    fundTable[,X2015],
    fundTable[,X2016],
    fundTable[,X2017],
    fundTable[,X2018],
    fundTable[,X2019]
  )
)
longTable <- longTable[!is.na(Amount)]

# Calculate grant length and add to cleaned dataset
grantLength <- longTable[,.N, by=ApplicationId]
setnames(grantLength, "N", "GrantLength")
setkey(grantLength, "ApplicationId")
setkey(clean, "ApplicationId")

clean <- merge(clean, grantLength)

# Write out tables
write.table(clean, file="cleaned.csv", sep=",", row.names=FALSE, quote=FALSE)
write.table(longTable, file="funding-by-year.csv", sep=",", row.names=FALSE, quote=FALSE)

  
