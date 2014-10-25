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
      oct2014[,Field.of.Research],
      aug2014[,FIELD_OF_..RESEARCH]
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
gender <- sapply(sapply(first_names, gender), `[[`, "gender")
clean[,gender := gender]

# determine career stage


  
