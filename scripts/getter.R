# Pull down the NHRMC funding data
library(xlsx)
library(data.table)

# Pull in 2014 data
oct2014 <- read.xlsx(
    file="https://www.nhmrc.gov.au/_files_nhmrc/file/grants/funding/2014/summary_data_17_october_2014_announcement_141020.xlsx",
    sheetIndex=1
  )

aug2014 <- read.xlsx(
    file="https://www.nhmrc.gov.au/_files_nhmrc/file/grants/funding/2014/summary_data_august_2014_announcement_140919.xlsx",
    sheetIndex=1
  )

oct2014 <- as.data.table(oct2014)
aug2014 <- as.data.table(aug2014)

# save in plain text (but raw) format
write.csv(oct2014, file="october_2014.csv", quote=FALSE)
write.csv(aug2014, file="august_2014.csv", quote=FALSE)

