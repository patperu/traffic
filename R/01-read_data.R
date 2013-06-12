##=============================================================
## Traffic accidents in Berlin - "Tabelle 3"
##
## Patrick Hausmann <patrick.hausmann@covimo.de>
##
## Quelle: http://is.gd/kGptdh
##
## 12-05-2013
##=============================================================

library(RODBC)
library(RCurl)
library(XML)
library(reshape2)
library(gplots)
library(stringr)
library(gsubfn)

############################################################################
#                              Functions
############################################################################

getLinks <- function() { 
       links = character() 
       list(a = function(node, ...) { 
                   links <<- c(links, xmlGetAttr(node, "href"))
                   node 
                }, 
            links = function()links)
     }

Get_Data <- function(x, run = TRUE) {
    if(run)
    for(i in seq_along(x)) {
        fname <- basename(x[i])
        # see http://stackoverflow.com/a/15543569
        f = CFILE(file.path(data_dir, fname), mode="wb")
        curlPerform(url = x[i], writedata = f@ref, ssl.verifypeer = FALSE)
        close(f)        
        message(paste("Datei-", sprintf("%02.0f", i), " : ", fname, " wurde heruntergeladen", sep=""))
        }
}

Read_XLS <- function(x, sheet) {
    require(XLConnect)
        id <- x
        wb <- loadWorkbook(file.path(data_dir, x))
        # Set the missing value string to 'missing'
        setMissingValue(wb, value = "x")
        w <- readWorksheet(wb,
                           sheet = sheet,
                           startRow = 7, endRow = 40,
                           startCol = 0, endCol = 11)
        w$id <- id 
        m <- data.frame( t(sapply(strsplit(w$id, "\\_"), "[")) )
        w <- data.frame(w, m)
        colnames(w) <- paste0("m", 1:ncol(w))
    print(x)    
    return(w)
   
}

Get_Year_Month <- function(x) {

  Get_Date <- function(v) {
    x_len <- nchar(v)
    switch(as.character(x_len),
      '7'= data.frame(yr = substr(v, 1, 4), mo = substr(v, 6, 7)),
      '6'= data.frame(yr = substr(v, 5, 6), mo = substr(v, 2, 3)))
  }

  m <- lapply(x, Get_Date)
  m <- do.call(rbind, m)
  m$yr <- paste("20", gsub("20", "", m$yr), sep ="")
  return(m)
}

############################################################################
#
############################################################################

options(stringsAsFactors = FALSE)  

data_source <- "https://www.statistik-berlin-brandenburg.de/statistiken/statistik_sb.asp?sageb=46002&PTyp=700&creg=B&anzwer=5&bok=1&bbok=1"
base_url <- "https://www.statistik-berlin-brandenburg.de"
sheet    <- "Tabelle 3"
data_dir <- "input/data"

f <- CFILE("input/traffic_data_urls.html", mode="wb")
curlPerform(url = data_source, writedata = f@ref, ssl.verifypeer = FALSE)
close(f) 

h1 <- getLinks()
htmlTreeParse("input/traffic_data_urls.html", handlers = h1)

v <- trim(unique(h1$links()))

# data only for Berlin ("_BE.xls")
v <- v[grep("_BE.xls", v)]

# drop "../"
v <- substr(v, 4, nchar(v))

m <- file.path(base_url, v)

# exclude files
file_ex <- c(
"SB_H01-02-00_2012j01_BE.xls",  
"SB_H01-02-00_2011j01_BE.xls",
"SB_H1-1_m01-07_BE.xls",    
"SB_H1-1_m02-07_BE.xls", 
"SB_H1-1_m03-07_BE.xls",       
"SB_H1-1_m04-07_BE.xls",
"SB_H1-1_m05-07_BE.xls", 
"SB_H1-1_m06-07_BE.xls",      
"SB_H1-1_m07-07_BE.xls", 
"SB_H1-1_m08-07_BE.xls",
"SB_H1-1_m09-07_BE.xls",
"SB_H1-1_m10-07_BE.xls",
"SB_H1-1_m11-07_BE.xls",      
"SB_H1-1_m12-07_BE.xls",
"SB_H1-2_j-08_BE.xls",
"SB_H1-2_j-09_BE.xls", 
"SB_H1-2_j01-10_BE.xls",
"SB_H1-1_m01-08_BE.xls"   # only 15 col.
)

m <- m[!basename(m) %in% file_ex]

Get_Data(m)

x1 <- lapply(dir(data_dir), FUN=function(L) Read_XLS(L, sheet))
x1 <- do.call("rbind", x1)

colnames(x1)[1] <- "wt"
x1$wt <- as.numeric(x1$wt)

x1 <- data.frame(x1, Get_Year_Month(x1$m15))

x1 <- transform(x1, Date = as.Date(paste(sprintf("%02.0f", wt), 
                          mo, yr, sep ="-"), "%d-%m-%Y"))

head(x1)
x1 <- x1[!is.na(x1$m2) & x1$m2 != "Insgesamt", ]

table(x1$yr, x1$mo)

Write_D3 <- function(x) {
  out <- x[, c("wt","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","yr","mo","Date")] 
  write.csv(out, file="output/d3/traffic_accidents.csv", quote = FALSE, row.names = FALSE)
}

Write_D3(x1)

save(x1, file="output/rdata/traffic_accidents.rdata")

#
# FINIS
#
