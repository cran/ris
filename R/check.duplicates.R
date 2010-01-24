check.duplicates <- function(references){

 #####DEPENDENT FUNCTIONS

partial.title <-  function(x){
string <- tolower(gsub("[[:punct:]]","",x))
 #KEEP ALL WORDS OF 5 or more letters
string <- strsplit(string," ")[[1]]
string <- paste(string[which(nchar(string)>=5)],collapse="")
 #TAKE FIRST 50 CHAR
substr(string,1,50)
}

 #REPLACE TITLES WITH PARTIAL MATCH
for(i in 1:length(references)){
references[[i]]$title.primary <- partial.title(references[[i]]$title.primary)
}

duplicate.index <- function(references,field){
 #VECTOR LENGTH OF NUMBER OF REFERENCE ENTRIES
collapsed.fields <- sapply(references,function(x){paste(unlist(x[field]),collapse="")})

count <- table(collapsed.fields)

 #REMOVE NULL AND ""

count <- count[names(count)!="NULL"&names(count)!=""]

if(any(count>1)){
matches <- lapply(names(count)[count>1],function(name){which(collapsed.fields==name)})
 }
else{
matches <- list(NA)
 }

return(matches)
 }

#######

 #DUPLICATE IDENTIFIERS

id.list <- duplicate.index(references,"id")
isbn.list <- duplicate.index(references,"isbn")
author.title.list <- duplicate.index(references,
		  c("title.primary","author.primary"))

suspects <- c(
unlist(id.list),unlist(isbn.list),unlist(author.title.list)
)

unique(suspects[!is.na(suspects)])
}




