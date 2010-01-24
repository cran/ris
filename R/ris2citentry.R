ris2citentry <- function(citation){

 ####PROCESSING FOR SINGLE CITATION
 
ris2citentry.oneris <- function(citation){

 ####DEPENDENT FUNCTIONS

citentry.list <- function (entry,textVersion,header=NULL,footer=NULL,ref.list) 
{
	#SLIGHT MODIFICATION OF citEntry so that it can take list argument
    z <- ref.list
    names(z) <- tolower(names(z))
    if ("author" %in% names(z)) 
        z$author <- as.personList(z$author)
    attr(z, "entry") <- entry
    attr(z, "textVersion") <- textVersion
    attr(z, "header") <- header
    attr(z, "footer") <- footer
    class(z) <- "citation"
    z
}

authors2persons <- function(authors){

if(!any(authors=="")){

last.first <- strsplit(authors,", ?")

persons <- personList()

for(i in 1:length(last.first)){
persons[[i]] <- person(first=last.first[[i]][2],last=last.first[[i]][1])
}
return(persons)
 }
else{
return("")
}
}

get.textVersion <- function(reference,fields){
i <- which(fields %in% names(reference))
if(length(fields[i])!=0){
text <- unlist(reference[fields[i]])
text <- text[text!=""]
text <- paste(paste(text,collapse=", "),".",sep="")
}
else{
text <- ""
}
return(text)
}

get.bibtexvalue <- function(reference,possible.matches){

 #Possible.matches are in order of priority

if(any(possible.matches %in% names(reference))){
i <- which(possible.matches %in% names(reference))
i <- min(i)
return(reference[[possible.matches[i]]])
}
else{
return("")
}
}

make.book <- function(reference){

 #TAKES REFERNCE RIS OBJECT AND RETURNS BIBTEX TAGS FOR MAIN FIELD ENTRIES

bibtex.fields <- list(
 #AUTHOR
c("author.primary","author.secondary"),
 #TITLE
c("title.primary","title.secondary","journal"),
 #PUBLISHER
c("publisher"),
 #YEAR
c("year"),
 #VOLUME
c("volume"),
 #ADDRESS
c("address")
)

bibtex.reference <- mapply(get.bibtexvalue,possible=bibtex.fields,
MoreArgs=list(reference=reference))

bibtex.reference <- as.list(bibtex.reference)

names(bibtex.reference) <- c("author","title","publisher","year","volume","address")

return(bibtex.reference)
}

make.article <- function(reference){

 #TAKES REFERNCE RIS OBJECT AND RETURNS BIBTEX TAGS FOR MAIN FIELD ENTRIES

bibtex.fields <- list(
 #AUTHOR
c("author.primary","author.secondary"),
 #TITLE
c("title.primary","title.secondary"),
 #JOURNAL
c("journal.abbrev","journal"),
 #YEAR
c("year"),
 #VOLUME
c("volume"),
 #NUMBER
c("number"),
 #PAGES
c("pages")
)

bibtex.reference <- mapply(get.bibtexvalue,possible=bibtex.fields,
MoreArgs=list(reference=reference))

bibtex.reference <- as.list(bibtex.reference)

names(bibtex.reference) <- c("author","title","journal","year","volume","number","pages")

bibtex.reference[["pages"]] <- paste(bibtex.reference[["pages"]],collapse="-")

return(bibtex.reference)
}

make.incollection <- function(reference){

 #TAKES REFERNCE RIS OBJECT AND RETURNS BIBTEX TAGS FOR MAIN FIELD ENTRIES

bibtex.fields <- list(
 #AUTHOR
c("author.primary"),
 #EDITOR
c("author.secondary"),
 #TITLE
c("title.primary"),
 #BOOKTITLE
c("title.secondary","journal"),
 #PUBLISHER
c("publisher"),
 #YEAR
c("year"),
 #PAGES
c("pages"),
 #ADDRESS
c("address")
)

bibtex.reference <- mapply(get.bibtexvalue,possible=bibtex.fields,
MoreArgs=list(reference=reference))

bibtex.reference <- as.list(bibtex.reference)

names(bibtex.reference) <- c("author","editor","title","booktitle","publisher","year","pages","address")

bibtex.reference[["pages"]] <- paste(bibtex.reference[["pages"]],collapse="-")

return(bibtex.reference)
}

make.thesis <- function(reference){

bibtex.fields <- list(
 #AUTHOR
c("author.primary","author.secondary"),
 #TITLE
c("title.primary","title.secondary","journal"),
 #PUBLISHER
c("publisher"),
 #YEAR
c("year"),
 #ADDRESS
c("address")
)

bibtex.reference <- mapply(get.bibtexvalue,possible=bibtex.fields,
MoreArgs=list(reference=reference))

bibtex.reference <- as.list(bibtex.reference)

names(bibtex.reference) <- c("author","title","school","year","address")

return(bibtex.reference)
}

make.misc<- function(reference){

bibtex.fields <- list(
 #AUTHOR
c("author.primary","author.secondary"),
 #TITLE
c("title.primary","title.secondary","journal"),
 #PUBLISHER
c("publisher","address","journal"),
 #YEAR
c("year")
)

bibtex.reference <- mapply(get.bibtexvalue,possible=bibtex.fields,
MoreArgs=list(reference=reference))

bibtex.reference <- as.list(bibtex.reference)

names(bibtex.reference) <- c("author","title","howpublished","year")

return(bibtex.reference)
}

######


if(citation$type=="article"){

reference <- make.article(citation)

textVersion <- get.textVersion(reference,c("author","year","title","journal","volume","number","pages"))
}
else if(citation$type=="book"){

reference <- make.book(citation)

textVersion <- get.textVersion(reference,c("author","year","title","publisher","address"))

}
else if(citation$type%in%c("incollection","proceedings","inproceedings")){

reference <- make.incollection(citation)

textVersion <- get.textVersion(reference,c("author","year","title","editor",
"booktitle","publisher","address","pages"))
}
else if(citation$type=="phdthesis"){

reference <- make.thesis(citation)

textVersion <- get.textVersion(reference,c("author","year","title","school","address"))
}
else{

reference <- make.misc(citation)

textVersion <- get.textVersion(reference,c("author","year","title","howpublished"))

}

 #CONVERT AUTHORS TO PERSONLIST
if(any(names(reference)%in%c("author","editor"))){

i <- which(names(reference)%in%c("author","editor"))

for(j in i){
reference[[j]] <- authors2persons(reference[[j]])
  }
 }

citentry.list(entry=citation$type,textVersion=textVersion,ref.list=reference)
}

lapply(citation,ris2citentry.oneris)
}


