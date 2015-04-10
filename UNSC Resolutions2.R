# Get a list of all UN Security Council resolutions 
# AUTHOR: Nicolas Klotz (mail <at> nicolasklotz.de)
# DATE: 23-02-2015

library(XML)
library(stringr)

setwd("/Users/nklotz/Documents/IT/R/UNSC Resolutions/")
startYear <- 1946
endYear <- as.numeric(format(Sys.Date(), "%Y")) # current year
years <- c(startYear:endYear) # vector with all years from which resolutions shall be scrapped
baseURL <- "http://www.un.org/en/sc/documents/resolutions/" # where to get resolutions from
suffix <- ".shtml" # suffix of each year's resolution overview page
urls <- paste0(baseURL, years, suffix) # list of all years' resolution overview page
resolutions <- data.frame() # initialize data.frame

for(theurl in urls) {
  # code based on answer by user225056 on StackOverflow
  # SOURCE-URL: https://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package
  
  tables <- readHTMLTable(theurl) # get all tables from the page (there should be only one)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1])) # get length of each table (there should be only one)
  tmp <- tables[[which.max(n.rows)]] # extract data from the longest table (there should be only one)

  # up to 2014 the concrete date was not included; thus, a third (empty) column needs to be added to compensate
  if (ncol(tmp) <= 2) {
    tmp <- data.frame(V1=tmp[,1], V2=NA, V3=tmp[,2], stringsAsFactors=FALSE)
    }
  
  # get URLs
  site <- htmlParse(theurl)
  links <- getNodeSet(site, "//td/a") # extract all html anchors
  link.list <- c() # initialize empty list
  for(i in links) {
    link <- xmlGetAttr(i, name = "href") # extract link from anchor
    link.list <- c(link.list, link) # append list
  }
  
  # work around for 1985: wrong link for S/RES/571
  if(theurl == "http://www.un.org/en/sc/documents/resolutions/1985.shtml") {    
    link.list[10] <- '/en/ga/search/view_doc.asp?symbol=S/RES/571(1985)'
  }

  # work around for 2003: contains other links
  if(theurl == "http://www.un.org/en/sc/documents/resolutions/2003.shtml") {    
    link.list <- c(link.list[1:16], link.list[25:length(link.list)])
  }

  # work around for 2013: contains wrong line
  if(theurl == "http://www.un.org/en/sc/documents/resolutions/2013.shtml") {    
    tmp <- subset(tmp, V1 != "Document Symbol") # delete this row
  }
    
  # remove duplicates
  link.list <- unique(link.list)
  tmp <- unique(tmp)
  
  tmp <- cbind.data.frame(tmp, data.frame(link.list))
  resolutions <- rbind.data.frame(resolutions, tmp) # combine with dataset
} # end for-loop

resolutions$V1 <- as.character(resolutions$V1)
resolutions$V1[resolutions$V1=="S/RES/1331 2000)"] <- "S/RES/1331 (2000)" # correct error
resolutions <- cbind.data.frame(resolutions, str_split_fixed(resolutions$V1, '[[:space:]]\\(', 2)) # split resolution name and year
resolutions$"1" <- gsub(" ", "", resolutions$"1", fixed = TRUE) # get rid of remaining spaces
resolutions$"2" <- gsub(")", "", resolutions$"2", fixed = TRUE) # get rid of remaining )s

# rename variables
resolutions <- data.frame(Resolution=resolutions[,"1"],
                          Number=as.numeric(gsub("S/RES/", "", resolutions[,"1"], fixed = TRUE)), # extract RES number
                          Year=resolutions[,"2"],
                          Date=resolutions[,"V2"],
                          Topic=resolutions[,"V3"],
                          URL=resolutions[,"link.list"],
                          stringsAsFactors = FALSE)

# correct incomplete URLs and clean them up
resolutions$URL <- as.character(resolutions$URL)
resolutions$URL[substring(resolutions$URL, 1, 1)=='/'] <- paste0('http://www.un.org', resolutions$URL[substring(resolutions$URL, 1, 1)=='/'])
resolutions$URL <- gsub("%20", "", resolutions$URL, fixed = TRUE)
resolutions$URL <- gsub("[[:space:]]", "", resolutions$URL, fixed = F)

# delete white spaces in other text fields
resolutions$Topic <- as.character(resolutions$Topic)
resolutions$Topic <- gsub("\n", "", resolutions$Topic, fixed = T)
resolutions$Resolution <- gsub("[[:space:]]", "", resolutions$Resolution, fixed = F)

# order data frame
resolutions <- resolutions[order(resolutions$Number),]

# Transform topic string to upper case
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
resolutions$Topic <- as.character(resolutions$Topic)
resolutions$Topic <- as.character(lapply(resolutions$Topic, .simpleCap))

# Save files
#filename.r <- paste0("UNSC_Resolutions_", startYear, "-", endYear, ".RData")
#filename.csv <- paste0("UNSC_Resolutions_", startYear, "-", endYear, ".csv")
write.csv2(resolutions, file = "UNSC_Resolutions.csv")
save(resolutions, file = "UNSC_Resolutions.RData")

# cleanup
rm(tmp, baseURL, endYear, startYear, i, link, link.list, links, n.rows, suffix, site, tables, theurl, urls, years)
