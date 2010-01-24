ris.citation <- function (package = "base", lib.loc = NULL) 
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") 
        stop(gettextf("package '%s' not found", package), domain = NA)

    meta <- packageDescription(pkg = package, lib.loc = dirname(dir))
    citfile <- file.path(dir, "CITATION")
    
        #CHECK FOR CITATION DATA
    dir.data <- paste(dir,"/data",sep="")
    citfile.data <- file.path(dir.data, "CITATION.RData")

    if (file_test("-f", citfile)) {
        return(readCitationFile(citfile, meta))
     }
    else if (file_test("-f", citfile.data)){
 #IF NO CITATION FILE BUT CITATION.RData ASSUMES read.ris CITATION OBJECT
     data(CITATION)
     CITATIONS <- ris2citentry(CITATION)
     attr(CITATIONS[[1]],"header") <- paste("To cite package", sQuote(package), 
        "in publications use:")
      return(CITATIONS)
    }
    else if (package == "base") {
        stop("broken installation, no CITATION file in the base package.")
    }
    if ((!is.null(meta$Priority)) && (meta$Priority == "base")) {
        cit <- citation("base")
        attr(cit, "header")[1L] <- paste("The '", package, "' package is part of R.  ", 
            attr(cit, "header")[1L], sep = "")
        return(cit)
    }
    z <- list(title = paste(package, ": ", meta$Title, sep = ""), 
        author = as.personList(meta$Author), year = sub(".*((19|20)[[:digit:]]{2}).*", 
            "\\1", meta$Date), note = paste("R package version", 
            meta$Version))
    if (is.null(meta$Date)) {
        warning(gettextf("no date field in DESCRIPTION file of package '%s'", 
            package), domain = NA)
    }
    else if (!length(z$year)) {
        warning(gettextf("could not determine year for '%s' from package DESCRIPTION file", 
            package), domain = NA)
    }
    z$url <- meta$URL
    class(z) <- "citation"
    attr(z, "entry") <- "Manual"
    attr(z, "package") <- package
    attr(z, "header") <- paste("To cite package", sQuote(package), 
        "in publications use:")
    if (!"recommended" %in% meta$Priority) 
        attr(z, "footer") <- paste("ATTENTION: This citation information has been auto-generated", 
            "from the package DESCRIPTION file and may need manual editing,", 
            "see ", sQuote("help(\"citation\")"), ".")
    author <- as.character(z$author)
    if (length(author) > 1L) 
        author <- paste(paste(author[1:(length(author) - 1)], 
            collapse = ", "), author[length(author)], sep = " and ")
    attr(z, "textVersion") <- paste(author, " (", z$year, "). ", 
        z$title, ". ", z$note, ". ", z$url, sep = "")
    z
}
