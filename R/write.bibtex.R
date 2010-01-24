write.bibtex <- function(citations,file,append=TRUE){

bibtex.list <- function(citations){
 #TAKES A LIST OF CITATIONS
lapply(citations,toBibtex)
}


bib.list <- bibtex.list(citations)

 #WRITES LIST OF BIBTEX ENTRIES TO FILE

for(cites in bib.list){
 if(!append){
write("",file=file,append=FALSE)
  }
write(cites,file=file,append=TRUE)
write("\n",file=file,append=TRUE)
 }
}
