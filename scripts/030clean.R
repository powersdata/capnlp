#Clean corpus, save to RDS file
Clean_Corpus <- function(sourceletter, samplepercent = "1", ngram){
        
        starttime <- Sys.time()
        testsource <- c("b", "n", "tw")
        if (!sourceletter %in% testsource) {
                sourceletter <- "a"
        }
        
        
        if (!dir.exists(paste0("data/sample/", samplepercent))) {
                dir.create(paste0("data/sample/", samplepercent))
                source("scripts/025sample.R")
                if (sourceletter == "b") {filename <- "en_US.blogs.txt"}
                if (sourceletter == "n") {filename <- "en_US.news.txt"}
                if (sourceletter == "tw") {filename <- "en_US.twitter.txt"}
                if (!sourceletter %in% testsource) {
                        filename <- "a"
                        sourceletter <- "a"
                        for (filelist in c("b", "n", "tw")) {
                        tmp <- write_training_text(filelist, samplepercent, ngram)
                        }
                }
        }
        
        #directory_source <- DirSource(paste0("data/sample/", samplepercent), encoding = "UTF-8")
        #corpus <- Corpus(directory_source,readerControl=list(reader=readPlain))
        
        
        obj <- paste0(sourceletter, samplepercent, ngram)
        if (!dir.exists(paste0("data/sample/", samplepercent, "/cl"))) {
                dir.create(paste0("data/sample/", samplepercent, "/cl"))
        }
        
        samplefile <- paste0("data/sample/", samplepercent, "/cl/", obj, ".rds")
        
        if (!file.exists(samplefile)) { 
                
                if (!exists(obj)) {
                directory_source <- DirSource(paste0("data/sample/", samplepercent), encoding = "UTF-8")
                corpus <- Corpus(directory_source,readerControl=list(reader=readPlain))
        }
        clean <- tm_map(corpus, FUN = tm_reduce, tmFuns = reducer)
        clean <- tm_map(clean, removeNumbers)
        clean <- tm_map(clean, removePunctuation)
        clean <- tm_map(clean, stripWhitespace)
        saveRDS(clean, file = samplefile)
        } else {clean <- readRDS(samplefile)}

        print(paste("TIME for cleanning", samplepercent, "% of lines:", timer(starttime)))
        return(clean)
}

