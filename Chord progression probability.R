library(rjson)
library(reshape2)
library(googleVis)


chords <- c("1", "4", "5", "6", "2", "3", "27", "47", "16", "67", "56", "5/6", "b7", "164", "57", "37", "5/5", "464", "17", "664", "564", "46", "57/6", "642", "36", "66", "b6", "b3")

# test
# Url <- "http://www.hooktheory.com/api/trends/stats?cp=1,1"
# document <- fromJSON(file=Url, method='C')
# doc.melt <- melt(document)
# doc.df <- dcast(doc.melt, L1 ~ L2, value.var='value')


# get data ----------------------

# STEP 1 get starting probabilities:

# (For all these following sections tho... first see if the files were already
# written... no need to hit the API unneccessarily, no? :) )
if (file.exists("chord.1stL.csv")){
  all.df <- read.csv("chord.1stL.csv")
} else {
  Url <- "http://www.hooktheory.com/api/trends/stats"
  document <- fromJSON(file=Url, method='C')
  doc.melt <- melt(document)
  doc.df <- dcast(doc.melt, L1 ~ L2, value.var='value')
  
  all.df <- ""
  all.df <- rbind(all.df,doc.df)
  
  all.df[,c(2,5)]
  write.table(all.df, file="chord.1stL.csv",sep=",")
}

# STEP 2 get chord 1 -> 2

if (file.exists("chord.2ndtL.csv")){
  all2.df <- read.csv("chord.2ndtL.csv")
} else {
  all2.df <- ""
  for (i in 1:length(chords)){
    q <- chords[i]
    Url <- paste("http://www.hooktheory.com/api/trends/stats?cp=",q,sep="")
    document <- fromJSON(file=Url, method='C')
    if(length(document)!=0){
      doc.melt <- melt(document)
      doc.df <- dcast(doc.melt, L1 ~ L2, value.var='value')
      all2.df <- rbind(all2.df,doc.df)
      Sys.sleep(.1)
    }
  }
  all2.df[,c(2,5)]
  write.table(all2.df, file="chord.2ndL.csv",sep=",")
}

# STEP 3 get chord 2 -> 3

if (file.exists("chord.3rdL.csv")){
  all3.df <- read.csv("chord.3rdL.csv")
} else {
  all3.df <- ""
  for (i in 1:length(chords)){
    for (j in 1:length(chords)){
      q <- paste(chords[i],chords[j],sep=",")
      Url <- paste("http://www.hooktheory.com/api/trends/stats?cp=",q,sep="")
      document <- fromJSON(file=Url, method='C')
      if(length(document)!=0){
        doc.melt <- melt(document)
        doc.df <- dcast(doc.melt, L1 ~ L2, value.var='value')
        all3.df <- rbind(all3.df,doc.df)
        Sys.sleep(.1)
      }
    }
  }
  all3.df[,c(2,5)]
  write.table(all3.df, file="chord.3rdL.csv",sep=",")
}

# STEP 4 get chord 3 -> 4
all4.df <- ""

if (file.exists("chord.4thL.csv")){
  all4.df <- read.csv("chord.4thL.csv")
} else {
  for (i in 1:length(chords)){
    for (j in 1:length(chords)){
      for (k in 1:length(chords)){
        q <- paste(chords[i], chords[j], chords[k],sep=",")
        Url <- paste("http://www.hooktheory.com/api/trends/stats?cp=",q,sep="")
        document <- fromJSON(file=Url, method='C')
        if(length(document)!=0){
          doc.melt <- melt(document)
          doc.df <- dcast(doc.melt, L1 ~ L2, value.var='value')
          all4.df <- rbind(all4.df,doc.df)
          Sys.sleep(.1)
        }
      }
    }
  }
  all4.df[,c(2,5)]
  write.table(all4.df, file="chord.4thL.csv",sep=",")
}

### OK, Now build sankeys! =========================================

# Drop first rows, which are always empty:
all2.df <- all2.df[2:nrow(all2.df),]
all3.df <- all3.df[2:nrow(all3.df),]
all4.df <- all4.df[2:nrow(all4.df),]

# um... not sure why this is contaminated... but:
all4.df <- all4.df[9:nrow(all4.df),]

## First level
d <- data.frame(FROM=rep("start",29), TO=all.df[1:29,2], 
                PROB=as.numeric(all.df[1:29,5]))

## Second level
all2.df <- all2.df[2:nrow(all2.df),]
d2 <- as.data.frame(matrix(unlist(strsplit(all2.df[,2], ",")),
                           ncol=2,byrow=TRUE))
names(d2) <- c("FROM", "TO")
d2$PROB <- as.numeric(all2.df[,5])

#This is how many choords are at this level!
length(unique(d2[,2]))

# In order to identify different levels, keep adding stars to new 
# levels (otherwise it wouldn't even draw due to suspected cyclicity)
d2[,2] <- paste(d2[,2]," ",sep="")

# Problem! There's 128 chords at level 3! so let's be a bit picky
# d2 <- d2[d2[,3]>.06,] # where the cutoff is kinda guessed
# or simply restrict d2 to original list of chords
d2 <- d2[gsub("\\ ","",d2[,2]) %in% chords,]


## Third level
# first simplify for subsequent splitting:
all3.df.clean <- as.character(all3.df[,2])

d3 <- as.data.frame(matrix(unlist(strsplit(all3.df.clean, ",")),
                           ncol=3,byrow=TRUE))

#Drop first level chords for now since I don't care (and anyway can't use em)
d3 <- d3[,c(2,3)]
names(d3) <- c("FROM", "TO")
d3$PROB <- as.numeric(all3.df[,5])

#This is how many choords are at this level!
length(unique(d3[,2]))

d3[,1] <- paste(d3[,1]," ",sep="")
d3[,2] <- paste(d3[,2],"  ",sep="")

d3 <- d3[gsub("\\ ","",d3[,1]) %in% chords,]
d3 <- d3[gsub("\\ \\ ","",d3[,2]) %in% chords,]


## Forf level
all4.df.clean <- as.character(all4.df[,2])

d4 <- as.data.frame(matrix(unlist(strsplit(all4.df.clean, ",")),
                           ncol=4,byrow=TRUE))

#Drop first and second levels for now since I don't care
d4 <- d4[,c(3, 4)]
names(d4) <- c("FROM", "TO")
d4$PROB <- as.numeric(all4.df[,5])

#This is how many choords are at this level!
length(unique(d4[,2]))

d4[,1] <- paste(d4[,1],"  ",sep="")
d4[,2] <- paste(d4[,2],"   ",sep="")

d4 <- d4[gsub("\\ \\ ","",d4[,1]) %in% chords,]
d4 <- d4[gsub("\\ \\ \\ ","",d4[,2]) %in% chords,]


# ok, so far we're good... BUT: since we're dealing with probabilities, we 
# probably need to somehow normalize the columns... if anything, for 
# presentation purposes. So, I don't know how... propose to kinda normalize
# totals to 1

sum(d[,3], na.rm=T); sum(d2[,3], na.rm=T); sum(d3[,3], na.rm=T); sum(d4[,3], na.rm=T)

d[,3] <- d[,3] / 0.867
d2[,3] <- d2[,3] / 22.71
d3[,3] <- d3[,3] / 478.685
d4[,3] <- d4[,3] / 1308.849

d.all <- rbind(d, d2, d3, d4)
d.plot <- gvisSankey(d.all, from="FROM", to="TO", weight="PROB",
                       options=list(
                         width=1000, height=1050,
                         sankey="{link: {color: { fill: '#d799ae' } },
                         node: { width: 15, 
                                color: { fill: '#a61d4c' },
                                label: { fontName: 'Garamond',
                                         fontSize: 20,
                                         bold: true} }}"))
plot(d.plot)

##########################################################





# ==============================For song stuff ------------

# get ratings  
allScores <-""
fileUrl <-c("http://www.metrolyrics.com/rolling-stone-top500-1.html", 
            "http://www.metrolyrics.com/rolling-stone-top500-2.html", 
            "http://www.metrolyrics.com/rolling-stone-top500-3.html", 
            "http://www.metrolyrics.com/rolling-stone-top500-4.html", 
            "http://www.metrolyrics.com/rolling-stone-top500.html")

# fileUrl <- "http://www.metrolyrics.com/rolling-stone-top500-1.html"
# fileUrl <- "http://www.metrolyrics.com/rolling-stone-top500-2.html" 
# fileUrl <- "http://www.metrolyrics.com/rolling-stone-top500-3.html" 
# fileUrl <- "http://www.metrolyrics.com/rolling-stone-top500-4.html" 
# fileUrl <- "http://www.metrolyrics.com/rolling-stone-top500.html"


for (i in 1:5){
  doc <- htmlTreeParse(fileUrl[i],useInternal=TRUE)
#   doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
  scores <- xpathSApply(doc,"//li",xmlValue)
  scores <- gsub("\n","ƒ", scores)
  scores <- gsub(" Lyrics","", scores)
  scores <- gsub("\t","ƒ", scores)
  scores <- gsub("ƒƒƒ","ƒ", scores)
  scores <- scores[10:109]
  scores <- substring(scores, 2, nchar(scores)-1)
  scores.parsed <- strsplit(scores,"ƒ")
  
  scores.melt <- melt(scores.parsed)
  scores.m <- matrix(scores.melt$value,ncol=3,byrow=T)
  scores.df <- data.frame(scores.m)
  head(scores.df)
  allScores <- rbind(allScores,scores.df)  
}
  
colnames(allScores) <- c("Rank", "Song", "Author")
sort.df <- with(allScores,  allScores[order(as.numeric(allScores$Rank)) , ])
head(sort.df)

head(allScores)
