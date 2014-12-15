read.csv.url <- function(url) {
        data <- getURLContent(url, followlocation = T, binary = F, ssl.verifypeer = F)
        read.csv(textConnection(data))
}

get.f9 <- function() {
        datasets <- list()
        datasets[["2014-07-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true")
        datasets[["2014-08-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140801.csv?raw=true")
        #datasets[["2014-09-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140901.csv?raw=true")
        #datasets[["2014-10-01"]] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20141001.csv?raw=true")
        #datasets[["2014-11-01"] <- read.csv.url("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20141101.csv?raw=true")
        
        ldply(datasets)
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

get.f16 <- function(f14, f15) {
        dcast(f15, CountryNm ~ f14)
}

get.f18 <- function (f15) {
        f17 <- substr(f15$GIIN, 14, 15)
        f18 <- cbind(f15, f17) # new column
        colnames(f18)[2] <- "GIINx"
        colnames(f18)[5] <- "GIIN"
        f18
}

get.f19 <- function(f18) {
        #Pivot table ExtractDate per country, source: http://www.r-bloggers.com/pivot-tables-in-r/
        #dcast intersting URL: http://www.dummies.com/how-to/content/how-to-cast-data-to-wide-format-in-r.html
        dcast(f18, CountryNm ~ Extractdate)
}

get.f22 <- function(f18, f19) {
        f20<-melt(f19)
        colnames(f20)[2] <- "Extractdate"
        colnames(f20)[3] <- "Frequence"
        #GIIN: creation of the graph 
        f21<-dcast(f18, GIIN ~ Extractdate)
        f22<-melt(f21)
        #check if we still have tha adequate number of recors)
        #sum(f22$value)
        #nrow(f15)
        colnames(f22)[2] <- "Extractdate"
        colnames(f22)[3] <- "Frequence"  
        f22
}

get.f23.plot <- function(f22) {
        ggplot(data=f22, aes(x=Extractdate, y=Frequence, fill=GIIN)) + geom_bar(stat="identity")
}

get.f25 <- function(f16) {
        f24<-melt(f16)
        sum(f24$value)#to check if we still have tha adequate number of recors)
        colnames(f24)[2] <- "Extractdate"
        colnames(f24)[3] <- "Frequence"
        f25<-f24[order(f24$Frequence,decreasing=TRUE), ]
}

get.f26 <- function(f25) {
        f26 <- subset(f25, f25$Frequence>=1000)
}

get.f27.plot <- function(f26) {
        f27<-ggplot(data=f26, aes(x=Extractdate, y=Frequence, fill=CountryNm)) + geom_bar(stat="identity")
        f27
}

get.f28 <- function(f25, limit) {
        subset(f25, f25$Frequence >= limit)
}

get.f29.plot <- function(f28) {
        f29<-ggplot(data=f28, aes(x=Extractdate, y=Frequence, fill=CountryNm)) +
                geom_bar(stat="identity")
        theme(axis.text.x = element_text(angle=90))
        f29
}
# f28 = slider(1000, 8000, step=50, initial = 1000)

get.dates <- function() {
        c("2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", "2014-11-01")
}

f9
