# Download UNSC Resolutions

setwd("/Users/nklotz/Documents/IT/R/UNSC Resolutions/PDFs/")
load("../UNSC_Resolutions.RData")

# problems with UN site: pdfs are embedded in frame; direct download link cannot be extracted
# alternative site: http://unscr.com
# following conditional statements needed to construct correct download links

for(i in resolutions$Number){
  #print(i)
  if(nchar(i) == 1){
    filename <- paste0("0000", i, ".pdf")
  } else if(nchar(i) == 2){
    filename <- paste0("000", i, ".pdf")
  } else if(nchar(i) == 3){
    filename <- paste0("00", i, ".pdf")
  } else {
      filename <- paste0("0", i, ".pdf")
  }
  alturl <- paste0("http://unscr.com/files/", resolutions$Year[i], "/", filename)
  download.file(alturl, basename(alturl))
}