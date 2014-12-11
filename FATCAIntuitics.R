#B/RPackages################################
require(plyr)
require(reshape2)
require(data.table)
require(stringr)
require(ggplot2)
#require(ggthemes)
#require(manipulate)
library(RCurl)
#E/RPackages################################


#B/FATCA Processing################################
setwd("C:/Users/cberthillon/997.FATCA/project1")

##extract from Intuitics word cloud"
download.file.https2 <- function(httpsurl, destfile=NULL){
        data <- getURLContent(httpsurl, followlocation=T, binary=T, ssl.verifypeer = FALSE)
        if(is.null(destfile)){
                destfile = tempfile("downloaded")
        }
        to.write = file(destfile, "wb")
        writeBin(as.raw(data), to.write)
}
download.file.https2("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true", "20140701.csv")
download.file.https2("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true", "20140801.csv")
download.file.https2("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true", "20140901.csv")
download.file.https2("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true", "20141001.csv")
download.file.https2("https://github.com/charlesberthillon/001.FATCA/blob/master/project1/001.data/20140701.csv?raw=true", "20141101.csv")

f4<-'downloaded'

f5 <- list.files(path = f4, pattern = NULL, all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = FALSE)
# Source: http://stackoverflow.com/questions/5186570/when-importing-csv-into-r-how-to-generate-column-with-name-of-the-csv
# list.files() function reads into R the names of every file in that directory
# f5= filenames
# f6=read_csv_filename

f6 <- function(f5){  #read_csv_file
        f7 <- read.csv(f5)
        f7$Source <- f5 #EDIT
        f7
}

f8 <-ldply(f5,f6) # import and concatenate the source.list
f9<-data.frame(f8)

#Column "Source": subset the file name and format it in date
f10<-str_sub(f9$Source, -12, -9)
f11<-str_sub(f9$Source, -8, -7)
f12<-str_sub(f9$Source, -6, -5)
f13<-cbind(f10,"-",f11,"-",f12)
f13<-paste (f10,f11,f12, sep = "-", collapse = NULL)
f14<-as.Date(f13, "%Y-%m-%d")
f15<-cbind(f14,f9)
colnames(f15)[1] <- "Extractdate"
f15$Source<-NULL #delete column with source URL
### TO BE ANALYZED: f15$Extracdate <- strptime(f15$Extracdate, "%d/%m/%Y")# foramting date and time
#Overall check per country

f16<-dcast(f15, CountryNm ~ f14) 
#Pivot table ExtractDate per country, source: http://www.r-bloggers.com/pivot-tables-in-r/
#dcast intersting URL: http://www.dummies.com/how-to/content/how-to-cast-data-to-wide-format-in-r.html
f17<-substr(f15$GIIN, 14, 15)
f18 <- cbind(f15, f17) # new column
colnames(f18)[2] <- "GIINx"
colnames(f18)[5] <- "GIIN"
f19<-dcast(f18, CountryNm ~ Extractdate)
f20<-melt(f19)
colnames(f20)[2] <- "Extractdate"
colnames(f20)[3] <- "Frequence"
#GIIN: creation of the graph 
f21<-dcast(f18, GIIN ~ Extractdate)
f22<-melt(f21)
sum(f22$value)#to check if we still have tha adequate number of recors)
colnames(f22)[2] <- "Extractdate"
colnames(f22)[3] <- "Frequence"

#E/FATCA Processing################################

#B/Graphes##############################################


f23<-ggplot(data=f22, aes(x=Extractdate, y=Frequence, fill=GIIN)) + geom_bar(stat="identity")
f23
#E/Graphes##############################################

#B/FATCA Processing################################
f24<-melt(f16)
sum(f24$value)#to check if we still have tha adequate number of recors)
colnames(f24)[2] <- "Extractdate"
colnames(f24)[3] <- "Frequence"
f25<-f24[order(f24$Frequence,decreasing=TRUE), ]
f26 <- subset(f25, f25$Frequence>=1000)
#E/FATCA Processing################################
#B/Graphes##############################################
f27<-ggplot(data=f26, aes(x=Extractdate, y=Frequence, fill=CountryNm)) + geom_bar(stat="identity")
f27
#R Manipulate n°1
manipulate(
        {
        f28 <- subset(f25, f25$Frequence>=f28)
        f29<-ggplot(data=f28, aes(x=Extractdate, y=Frequence, fill=CountryNm)) +
              geom_bar(stat="identity")
              theme(axis.text.x = element_text(angle=90))
        f29 #some charts in http://docs.ggplot2.org/0.9.3.1/geom_bar.html
},
f28 = slider(1000, 8000, step=50, initial = 1000)
)
#R Manipulate n°2
manipulate(
{
        f30 <- subset(f25, f25$Frequence>=1000)
        f31 <- subset(f28, f28$Frequence==f33)
        f32<-ggplot(data=f28, aes(x=Extractdate, y=Frequence, fill=CountryNm)) + 
                geom_bar(stat="identity")+
                facet_grid(Drug ~ Rep) + xlab("Expression Level") + ylab("Frequency")
        f32
},
f33 = picker("2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01", "2014-11-01")
)

#B/ dedicated topic: search by text#############
nf15<-subset(f15, grepl("société",f15$FINm) |  
                     grepl("générale",f15$FINm) |  
                     grepl("societe",f15$FINm)|
                     grepl("generale",f15$FINm) 
)                 

nf16<-dcast(nf15, GIIN1 ~ Extractdate)
nf17<-melt(nf16)
colnames(nf17)[2] <- "Extractdate"
colnames(nf17)[3] <- "Frequence"
nf18<-ggplot(data=nf17, aes(x=Extractdate, y=Frequence, fill=GIIN1)) + geom_bar(stat="identity")
nf18
#N/ dedicated topic: search by text#############

#B/View##############################################
View(18)
View(24)
View(25)
#E/View##############################################
#B/PRINT##############################################
write.table(f18,file='C:/Users/cberthillon/997.FATCA/002.dataprocessed/IRSExtract.csv', col.names=T, row.names=FALSE, sep=';')
write.table(f24,file='C:/Users/cberthillon/997.FATCA/002.dataprocessed/RFATCA1.csv', col.names=T, row.names=FALSE, sep=';')
write.table(f25,file='C:/Users/cberthillon/997.FATCA/002.dataprocessed/RFATCA2.csv', col.names=T, row.names=FALSE, sep=';')
#E/PRINT ##############################################