read.csv.url <- function(url) {
  data <- getURLContent(url, followlocation = T, binary = F, ssl.verifypeer = F)
  read.csv(textConnection(data))
}

get.f9 <- function() {
  datasets <- list()
  datasets[["2014-07-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true")
  #datasets[["2014-08-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140801.csv?raw=true")
  #datasets[["2014-09-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140901.csv?raw=true")
  #datasets[["2014-10-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20141001.csv?raw=true")
  #datasets[["2014-11-01"] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20141101.csv?raw=true")
  
  ldply(datasets)
}

show.f9 <- function(f9) {
  f9
}

get.f14 <- function(f9) {
  as.Date(f9$.id, "%Y-%m-%d")
}

get.f15 <- function(f9, f14) {
  f15<-cbind(f14, f9)
  colnames(f15)[1] <- "Extractdate"
  f15$.id<-NULL
  f15
}

get.f19 <- function(f14, f15) {
  f16<-dcast(f15, CountryNm ~ f14)
  #Pivot table ExtractDate per country, source: http://www.r-bloggers.com/pivot-tables-in-r/
  #dcast intersting URL: http://www.dummies.com/how-to/content/how-to-cast-data-to-wide-format-in-r.html
  f17<-substr(f15$GIIN, 14, 15)
  f18 <- cbind(f15, f17) # new column
  colnames(f18)[2] <- "GIINx"
  colnames(f18)[5] <- "GIIN"
  f19<-dcast(f18, CountryNm ~ Extractdate)
  f19
}

