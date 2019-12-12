#Cleaning and Transformation 
DSI <-fread("https://storage.googleapis.com/edavproject/dsi1.csv")
DSI <- DSI[,2:23]
colnames(DSI)[2]<-"Name"
colnames(DSI)[6]<-"Background"
colnames(DSI)[7]<-"Institute"
register_google(key = 'AIzaSyCiEPynt-9Dbz0hCywpHJ_vR2lNA28Abc8')
api_key <- 'AIzaSyCiEPynt-9Dbz0hCywpHJ_vR2lNA28Abc8'
x <- DSI$Institute[0:122]
temp1 <- geocode(x)
temp1$Institute <- DSI$Institute
temp1[1:5,]
temp1 <- as.data.frame(temp1[,c(1,2)])

#Store co-ordinates and lat long values in another data set.
DSI$longitude <- temp1$lon
DSI$latitude <- temp1$lat
DSI$co_ord <- paste(DSI$latitude,",",DSI$longitude)
unique_institutes <- as.data.frame(table(DSI$co_ord))
colnames(unique_institutes)[1]<- 'co_ord'
tempo <- as.data.frame(DSI[,c("Institute", "co_ord")])
y <- merge( x = unique_institutes, y = tempo, by = "co_ord", all = TRUE)

df_split<- strsplit(as.character(y$co_ord), split=",")
y <- transform(y, latitude= sapply(df_split, "[[", 1),longitude= sapply(df_split, "[[", 2))
y$latitude <- as.numeric(as.character(y$latitude))
y$longitude <- as.numeric(as.character(y$longitude))

write.csv(y, "latlong.csv")

#Reverse Geo-coding to get the countries from lat long values and then provide classification for previous degree in USA/NON-USA
DSI1 <- DSI
result <- do.call(rbind,
                 lapply(1:nrow(temp1),
                        function(i)revgeocode(as.numeric(temp1[i,1:2]))))
dum1 <- cbind(temp1,result)
colnames(dum1)[3]<-"Address"
temp <- gsub(".*\\s", "", as.character(dum1$Address))
dum1$cc <- temp
colnames(dum1)[4]<-"Country Code"
dum1 <- dum1[,c(1,2,4)]
dum1$dom <- grepl("[[:digit:]]", dum1$`Country Code`)
dum1$`Country Code` = ifelse(grepl(TRUE,dum1$dom),"China",dum1$`Country Code`)
dum1$dom = ifelse(grepl("USA",dum1$`Country Code`),"USA","NON-USA")
colnames(dum1)[4]<-"Is USA?"
DSI1$Country_Code <- dum1$`Country Code`
DSI1$Is_USA <- dum1$`Is USA?`


#Bifurcate different distinct academic backgrounds into 3 groups namely Computer based, Math and Stats based and Non-Computer ad Math based

DSI1$Background <- tolower(DSI1$Background)
DSI1$Back = ifelse(grepl("(analytics|comp|data)",DSI1$Background),"Computer","Non-Comp_Math")
DSI1$Back = ifelse(grepl("(statis|math|actuarial)",DSI1$Background)&(DSI1$Back != "Computer"),"Maths and Stats",DSI1$Back)



# Grouping the response for the statement: "On a scale from 1 (I'm chillin'!) to 10 (I work/study all the time!) describe your work-life balance"" into 4 bins for simplification.

colnames(DSI1)[15] <- "Work Rate"
DSI1$`Work Rate`<- as.character(DSI1$`Work Rate`)
DSI1$Bins = ifelse(grepl("(1|2|3)",DSI1$`Work Rate`),"Chill the most",0)
DSI1$Bins = ifelse(grepl("(4|5)",DSI1$`Work Rate`),"Chill more, work less",DSI1$Bins)
DSI1$Bins = ifelse(grepl("(6|7)",DSI1$`Work Rate`),"Work more, chill less",DSI1$Bins)
DSI1$Bins = ifelse(grepl("(8|9|10)",DSI1$`Work Rate`),"Work a lot",DSI1$Bins)


#write.csv(DSI1,"DSI1.csv")
