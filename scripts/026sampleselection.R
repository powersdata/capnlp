#Find sample and save data
Find_Words <- function(word) {
        starttime <- Sys.time()
        word_length <- length(word)
        
        for (i in c("tw", "n", "b")) {
                if (!exists(i)) {
                        which_import <- "fullcorpus"
                        source("scripts/010imports.R")
                }
                idx <- NULL
                if (i == "tw") {
                        orig <- tw
                }
                if (i == "n") {
                        orig <- n
                }
                if (i == "b") {
                        orig <- b
                }
                
                #source("scripts/027sampleindex.R")
                for (j in 1:word_length) {
                        tmp_word <- word[j]
                        idx_tmp <-
                                grep(
                                        tmp_word, orig[[1]]$content, ignore.case = TRUE, perl = TRUE
                                )
                        idx <- sort(unique(as.integer(
                                rbind(idx_tmp, idx)
                        )))
                }
                
                assign(paste0("idx_", i), idx, envir = .GlobalEnv)
                
        }
        print(paste("TIME for findings:", timer(starttime)))
}

Selected_Sample <-
        function(sourceletter, samplepercent = "s", ngram = 4) {
                starttime <- Sys.time()
                set.seed(42)
                if (!dir.exists(paste0("data/sample/", samplepercent))) {
                        dir.create(paste0("data/sample/", samplepercent))
                }
                
                for (i in c("b", "n", "tw")) {
                        samplefile <-
                                paste0("data/sample/", samplepercent, "/", i, ".txt")
                        if (i == "tw") {
                                orig <- tw
                                idx <- idx_tw
                        }
                        if (i == "n") {
                                orig <- n
                                idx <- idx_n
                        }
                        if (i == "b") {
                                orig <- b
                                idx <- idx_b
                        }
                        sampled <- orig[[1]]$content[idx]
                        
                        #################### limit number of idx
                        prct <-
                                ifelse(length(sampled) > 999, 2000 / length(sampled), 1)
                        
                        nidx <- rbinom(length(sampled), 1, prct)
                        sampled <- sampled[nidx == 1]
                        
                        ####################
                        
                        
                        
                        
                        write.csv(
                                sampled, file = samplefile, row.names = FALSE, quote = F, fileEncoding = "UTF-8"
                        )
                }
                print(paste("TIME for Selection:", timer(starttime)))
        }

#Clean Sample
Clean_Sample <- function(sourceletter) {
        starttime <- Sys.time()
        testsource <- c("b", "n", "tw")
        if (!sourceletter %in% testsource) {
                sourceletter <- "a"
        }
        
        if (!dir.exists(paste0("data/sample/s/cl"))) {
                dir.create(paste0("data/sample/s/cl"))
        }
        
        if (sourceletter == "a") {
                directory_source <-
                        DirSource(paste0("data/sample/s"), encoding = "UTF-8")
                corpus <-
                        Corpus(directory_source,readerControl = list(reader = readPlain))
                clean <-
                        tm_map(corpus, FUN = tm_reduce, tmFuns = reducer)
                clean <- tm_map(clean, removeNumbers)
                clean <- tm_map(clean, removePunctuation)
                clean <- tm_map(clean, stripWhitespace)
                saveRDS(
                        clean, file = paste0("data/sample/s/cl/",sourceletter,".rds"),
                        ascii = TRUE, compress = FALSE
                )
        }
        print(paste("TIME for cleaning:", timer(starttime)))
        return(clean)
        
}

#clean saved corpus
Clean_Saved <- function(corpus) {
        clean <- tm_map(corpus, removeNumbers)
        clean <- tm_map(corpus, removePunctuation)
        clean <- tm_map(corpus, stripWhitespace)
        return(clean)
}

#Ngrams from Sample
Sample_Ngrams <- function(sourceletter, ngram) {
        starttime <- Sys.time()
        testsource <- c("b", "n", "tw")
        if (!sourceletter %in% testsource) {
                sourceletter <- "a"
        }
        corpus <-
                readRDS(paste0("data/sample/s/cl/", sourceletter,".rds"))
        corpus <- Clean_Saved(corpus)
        xgram <- NULL
        for (i in 1:ngram) {
                tmp_ngram <-
                        NGramTokenizer(corpus, Weka_control(min = i, max = i))
                xgram_tmp <-
                        data.frame(table(tmp_ngram), stringsAsFactors = FALSE)
                colnames(xgram_tmp) <- c("nGram","Freq")
                xgram_tmp$nWords <-
                        nchar(as.character(xgram_tmp$nGram)) + 1 - nchar(gsub(" ", "", as.character(xgram_tmp$nGram)))
                xgram_tmp <-
                        xgram_tmp[order(xgram_tmp$Freq, decreasing = TRUE),]
                rownames(xgram_tmp) <- NULL
                
                xgram_tmp$nGram <-
                        gsub("[^a-z ]", "", xgram_tmp$nGram)
                xgram_tmp$nGram <-
                        gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", xgram_tmp$nGram, perl = TRUE)
                
                if (i == 1) {
                        xgram_tmp <- xgram_tmp[1:10,]
                }
                
                idx_count <-
                        sapply(strsplit(xgram_tmp$nGram, "\\s+"), length)
                xgram_tmp <- xgram_tmp[idx_count == i,]
                xgram_tmp <- droplevels(xgram_tmp)
                
                xgram <- rbind(xgram_tmp, xgram)
                
                assign(paste0(sourceletter, i), xgram, envir = .GlobalEnv)
                saveRDS(
                        assign(paste0(sourceletter, i), xgram, envir = .GlobalEnv), paste0("data/sample/s/cl/", sourceletter, i, ".rds")
                )
        }
        #combine all ngram files to one, clean punctuation, spaces with stringi, correct counts of words
        # sort by number of words, freq
        rownames(xgram) <- NULL
        xgram <- xgram[order(-xgram$nWords, -xgram$Freq, xgram$nGram),]
        
        assign(paste0(sourceletter, 1, i), xgram, envir = .GlobalEnv)
        saveRDS(xgram, paste0("data/sample/RDS/", sourceletter, 1, ngram, ".rds"))
        print(paste("TIME for", sourceletter, ngram, timer(starttime)))
        return(xgram)
}
