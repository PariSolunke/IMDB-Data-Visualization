library(stringr)
library(plyr)
library(dplyr)

movie <- read.csv(file = "movies.list", quote = "", sep = '\n')
certificate <- read.csv("certificates.list", quote = "", sep = '\n')
genres <- read.csv("genres.list", quote = "", sep = '\n')
keywords <- read.csv("keywords.list", quote = "", sep = '\n')
ratings <- read.csv("ratings.list", quote = "", sep = '\n')
releaseDates <- read.csv("release-dates.list", quote = "", sep = '\n')
runTimes <- read.csv("running-times.list", quote = "", sep = '\n')

movie <- data.frame(movie[-c(1:7, nrow(movie)),])
colnames(movie) <- c(1)
pattern = "\\?\\?\\?\\?|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
movie <- data.frame(movie[-c(grep(pattern, movie$`1`,ignore.case = TRUE)),])
colnames(movie) <- c(1)
movie <- data.frame(gsub("\\{.*\\}","",movie$`1`))
colnames(movie) <- c(1)
movie <- data.frame(gsub("\t+"," ",movie$`1`))
colnames(movie) <- c(1)
movie <- data.frame(gsub(" +"," ",movie$`1`))
colnames(movie) <- c(1)
movie$Movie <- gsub(" [^ ]*$", "", movie$`1`)
movie$Year <- gsub(".* ", "", movie$`1`)
movie$`1` <- NULL
movie <- movie[!duplicated(movie[,"Movie"]),]

certificate <- data.frame(certificate[-c(1:7, nrow(certificate)),])
colnames(certificate) <- c(1)
certificate <- data.frame(certificate[c(grep("USA",certificate$`1`)),])
colnames(certificate) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|USA:X|USA:NC-17|USA:TV|USA:E|USA:C|USA:M|USA:Not Rated|USA:Unrated|\".*"
certificate <- data.frame(certificate[-c(grep(pattern,certificate$`1`,ignore.case = TRUE)),])
colnames(certificate) <- c(1)
certificate <- data.frame(gsub("\\{.*\\}","",certificate$`1`))
colnames(certificate) <- c(1)
certificate <- data.frame(gsub("\t+"," ",certificate$`1`))
colnames(certificate) <- c(1)
certificate$certificate <- gsub("^(.*?)\\(\\d{4}\\/*I*V*I*X*I*\\) +", "", certificate$`1`)
certificate <- certificate[c(grep("USA",certificate$certificate)),]
certificate$`1` <- gsub("\\) *U.*", "\\)", certificate$`1`)
colnames(certificate) <- c("Movie", "Certificate")
certificate$Certificate <- gsub("\\(.*","",certificate$Certificate)
certificate <- certificate[!duplicated(certificate),]
certificate <- aggregate(Certificate ~ Movie, data = certificate, paste, collapse = ",")

genres <- data.frame(genres[-c(1:300),])
colnames(genres) <- c(1)
genres <- data.frame(gsub("\\{.*\\}","",genres$`1`))
colnames(genres) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
genres <- data.frame(genres[-c(grep(pattern, genres$`1`,ignore.case = TRUE)),])
colnames(genres) <- c(1)
genres <- data.frame(gsub("Sci-fi","Sci-Fi",genres$`1`,ignore.case = TRUE))
colnames(genres) <- c(1)
genres$Genre <- gsub("^(.*?)\\(\\d{4}\\/*I*V*I*X*I*V*I*X*I*L*I*V*X*I*\\) *", "", genres$`1`)
pattern = "Short|Adult|Reality-TV|Talk-Show|Game-Show|News|Sex|Lifestyle|Hardcore|Experimental|Erotica|Commercial"
genres <- data.frame(genres[-c(grep(pattern,genres$Genre,ignore.case = TRUE)),])
colnames(genres) <- c(1,"Genre")
genres$`1` <- gsub("\\).*", "\\)", genres$`1`)
colnames(genres)<-c("Movie", "Genre")
genres <- genres[!duplicated(genres$Movie),]

ratings <- data.frame(ratings[-c(1:16),])
ratings <- data.frame(ratings[c(1:(nrow(ratings)-109)),])
colnames(ratings) <- c(1)
ratings <- data.frame(gsub("\\{.*\\}","",ratings$`1`))
colnames(ratings) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
ratings <- data.frame(ratings[-c(grep(pattern, ratings$`1`,ignore.case = TRUE)),])
colnames(ratings) <- c(1)
ratings <- data.frame(gsub("^ *","",ratings$`1`))
colnames(ratings) <- c(1)
ratings <- data.frame(gsub(" +"," ",ratings$`1`))
colnames(ratings) <- c(1)
ratings <- data.frame(str_split_fixed(ratings$`1`," ",4))
ratings$X1 <- NULL
colnames(ratings) <- c("Votes","Rating","Movie")

releaseDates <- data.frame(releaseDates[-c(1:7, nrow(releaseDates)),])
colnames(releaseDates) <- c(1)
releaseDates <- data.frame(gsub("\\{.*\\}","",releaseDates$`1`))
colnames(releaseDates) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
releaseDates <- data.frame(releaseDates[-c(grep(pattern, releaseDates$`1`,ignore.case = TRUE)),])
colnames(releaseDates) <- c(1)
releaseDates$Genre <- gsub("^(.*?)\\(\\d{4}\\/*I*V*I*X*I*\\) *", "", releaseDates$`1`)
releaseDates$`1` <- gsub("\\).*", "\\)", releaseDates$`1`)
colnames(releaseDates)<-c("Movie", "Release Date")
releaseDates$`Release Date` <- gsub("\\(.*","",releaseDates$`Release Date`)
releaseDates <- releaseDates[c(grep("USA",releaseDates$`Release Date`)),]
releaseDates$`Release Date` <- gsub(".*\\:","",releaseDates$`Release Date`)
releaseDates <- releaseDates[!duplicated(releaseDates$Movie),]

runTimes <- data.frame(runTimes[-c(1:7, nrow(runTimes)),])
colnames(runTimes) <- c(1)
runTimes <- data.frame(gsub("\\{.*\\}","",runTimes$`1`))
colnames(runTimes) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
runTimes <- data.frame(runTimes[-c(grep(pattern, runTimes$`1`,ignore.case = TRUE)),])
colnames(runTimes) <- c(1)
runTimes$Runtime <- gsub("^(.*?)\\(\\d{4}\\/*I*V*I*X*I*V*I*X*L*I*V*I*\\) *", "", runTimes$`1`)
runTimes$Runtime <- gsub(" *\\(.* *","",runTimes$Runtime)
runTimes$Runtime <- gsub("[^0-9]+\\: *","",runTimes$Runtime)
runTimes$Runtime <- gsub("\\:[0-9]*.*| *m.*|\\,.*|\\'.*|\\+.*|\\-.*|\\/.*|\\*.*|\\;.*| 1\\/2","",runTimes$Runtime)
runTimes$`1` <- gsub("\\).*", "\\)", runTimes$`1`)
colnames(runTimes)<-c("Movie", "Run Time")
runTimes <- runTimes[-c(grep("[[:alpha:]]", runTimes$`Run Time`)),]
runTimes$`Run Time` <- as.numeric(runTimes$`Run Time`)
runTimes <- runTimes[c(runTimes$`Run Time` > 60),]
runTimes <- runTimes[!duplicated(runTimes$Movie),]

keywords <- data.frame(keywords[-c(1:40),])

#keywordList <- data.frame(keywords[c(1:101563),])
#colnames(keywordList) <- c(1)
#keywordList <- toString(keywordList$`1`)
#keywordList <- gsub("\t","",keywordList)
#keywordList <- gsub(" *\\, *| *","",keywordList)
#keywordList <- gsub(")",") ",keywordList)
#keywordList <- data.frame(str_split(keywordList," "))
#colnames(keywordList) <- c(1)
#keywordList$Freq <- gsub(".*\\(","", keywordList$`1`)
#keywordList$Freq <- gsub("\\)","", keywordList$Freq)
#keywordList$`1` <- gsub("\\(.*\\)","", keywordList$`1`)
#keywordList <- keywordList[-c(grep("[[:alpha:]]", keywordList$Freq), nrow(keywordList)),]
#keywordList$Freq <- as.numeric(keywordList$Freq)
#keywordList <- keywordList[keywordList$Freq > 20,]
#keywordList <- toString(keywordList$`1`)
#keywordList <- gsub("\\, *","\\|",keywordList)

keywords <- data.frame(keywords[-c(1:101784),])
colnames(keywords) <- c(1)
keywords <- data.frame(gsub("\\{.*\\}","",keywords$`1`))
colnames(keywords) <- c(1)
pattern = "\\(\\?\\?\\?\\?\\/*.*\\)|\\(TV.*\\)|\\(V\\)|\\(VG\\)|\\(internet.*\\)|\\(re-release\\)|\\(Blu-ray.*\\)|\".*"
keywords <- data.frame(keywords[-c(grep(pattern, keywords$`1`,ignore.case = TRUE)),])
colnames(keywords) <- c(1)
keywords$keyword <- gsub(".*\t","",keywords$`1`)
keywordList <- data.frame(table(keywords$keyword))
keywordList <- keywordList[c(keywordList$Freq >= 20),]
keywordList <- toString(keywordList$Var1)
keywordList <- gsub("\\, *","\\|",keywordList)
keywordList <- gsub("\\|","\\$\\|\\^",keywordList)
keywordList <- paste("^",keywordList,sep="")
keywordList <- paste(keywordList,"$",sep="")
keywordList <- gsub("\\.","\\\\\\.",keywordList)

keyword <- keywords[c(grep(str_sub(keywordList,1,20009),keywords$keyword)),]
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,20011,40008),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,40010,60011),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,60013,80013),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,80015,100001),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,100003,120015),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,120017,140005),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,140007,160005),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,160007,180020),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,180022,200011),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,200013,220016),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,220018,240002),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,240004,260015),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,260017,280019),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,280021,300000),keywords$keyword)),])
keyword <- rbind(keyword,keywords[c(grep(str_sub(keywordList,300002,str_length(keywordList)),keywords$keyword)),])
keyword$`1` <- gsub("\\).*", "\\)", keyword$`1`)
keyword <- aggregate(keyword ~ `1`, data = keyword, paste, collapse = ",")
colnames(keyword)<-c("Movie", "Keywords")
keywords <- NULL

movie = merge(x=movie, y=ratings, by="Movie")
movie = merge(x=movie, y=certificate, by="Movie")
movie = merge(x=movie, y=genres, by="Movie")
movie = merge(x=movie, y=keyword, by="Movie")
movie = merge(x=movie, y=runTimes, by="Movie")
movie = merge(x=movie, y=releaseDates, by="Movie")

write.csv(movie,"imdb.csv")
