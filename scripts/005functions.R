#main functions
if (which_function == "main") {
        detach_package <- function(pkg, character.only = FALSE)
        {
                if (!character.only)
                {
                        pkg <- deparse(substitute(pkg))
                }
                search_item <- paste("package", pkg, sep = ":")
                while (search_item %in% search())
                {
                        detach(
                                search_item, unload = TRUE, character.only = TRUE
                        )
                }
                print(paste("detached", pkg, "package"))
        }
        #ie: detach_package(qdap)
        
        timer <- function(starttime) {Sys.time() - starttime}
        
        Usable_Text <- function(x) gsub("[^[:graph:]]", " ", x) 
        
        SCNS <- function(sourceletter, samplepercent, ngram) {
                starttime <- Sys.time()
                obj <- paste0(sourceletter, deparse(substitute(samplepercent)), deparse(substitute(ngram)))
                testsource <- c("b", "n", "tw")
                if (sourceletter == "b") {filename <- "en_US.blogs.txt"}
                if (sourceletter == "n") {filename <- "en_US.news.txt"}
                if (sourceletter == "tw") {filename <- "en_US.twitter.txt"}
                if (!sourceletter %in% testsource) {
                        filename <- "a"
                        sourceletter <- "a"
                }
                
                if (!dir.exists(paste0("data/sample/", samplepercent))) {
                        source("scripts/030clean.R")
                        ccorpus <- Clean_Corpus(sourceletter, samplepercent, ngram)
                        
                }
                else {
                        
                        
                        source("scripts/025sample.R")
                        corpus <- Corpus(VectorSource(write_training_text(sourceletter, samplepercent, ngram)))
                        
                        source("scripts/030clean.R")
                        ccorpus <- Clean_Corpus(sourceletter, samplepercent, ngram)
                }
                
                source("scripts/042ngrams.R")
                object.list <- obj
                for (i in 1:ngram) {
                        tmp.list <- list(Corpus_to_ngram(ccorpus, samplepercent, i))
                        #names(b54.rds) <- c("file", "unigram", "bigram", "trigram", "quadgram")
                        object.list <- c(object.list, tmp.list)
                }
                saveRDS(object.list, file = paste0("data/sample/RDS/", sourceletter, samplepercent, ngram, ".rds"))
                print(paste("TIME for scns", samplepercent, "% of lines and", ngram, "ngram:", timer(starttime)))
                return(object.list)
        }
        
        
}

#other functions
if (which_function == "other") {
        save_source_RDS <-
                function(filename, sourceletter = "t", samplepercent = "1", ngram = "0", model = "base") {
                        saveRDS(
                                filename, file = paste0(
                                        directory, "/data/final/rds_files/", sourceletter,".", samplepercent, ".",
                                        ngram, ".", model, ".rds"
                                )
                        )
                }
        
        
        read_source_RDS <-
                function(sourceletter = "t", samplepercent = "1", ngram = "0", model = "base") {
                        readRDS(
                                file = paste0(
                                        directory, "/data/final/rds_files/", sourceletter,".",
                                        samplepercent, ".", ngram, ".", model, ".rds"
                                )
                        )
                }
        
        
        ###functions used to create sample data that would not Knit
        #counts the number of lines in a text file
        no_of_lines <- function(con_text) {
                readsizeof <- 12000
                no_lines <- 0
                con <- file(paste0("data/final/en_US/", con_text), "r")
                (while ((linesread <-
                         length(
                                 readLines(con ,readsizeof)
                         )) > 0)
                        no_lines <- no_lines + linesread)
                closeAllConnections()
                print(paste("Number of lines is", no_lines, "for", con_text))
                no_lines
                
        }
        
        #writes sample of text file lines to data for training
#         write_training_text <- function(writefile) {
#                 starttime <- Sys.time()
#                 
#                 if (writefile == "en_US.twitter.txt") {
#                         samplelines <- 100; sourceletter <- "t"
#                 }
#                 if (writefile == "en_US.news.txt")    {
#                         samplelines <- 100; sourceletter <- "n"
#                 }
#                 if (writefile == "en_US.blogs.txt")   {
#                         samplelines <- 100; sourceletter <- "b"
#                 }
#                 
#                 size <- no_of_lines(writefile)
#                 #samplelines <- ifelse (size < 30000, size * 0.6, 29999)
#                 rlines <- rbinom(samplelines, size, .5)
#                 con <- file(paste0("data/final/en_US/", writefile), "r")
#                 wlines <- readLines(con, encoding = "UTF-8")[rlines]
#                 closeAllConnections()
#                 save_source_RDS(filename = wlines, sourceletter, samplelines)
#                 write.csv(
#                         wlines, file = paste0("data/sample/", basename(writefile)), row.names = FALSE
#                 )
#                 closeAllConnections()
#                 print(paste("TIME for", writefile, "training is", timer(starttime)))
#         }
        getCorpusSize <- function(corpus){
                
                size <- as.data.frame(sapply(corpus, function(x) length(x$content)))
                names(size) <- "length"
                return(size)
        }
        
        qf <- function(model_type = "t", word_seq = "a") {
                word_count <-
                        nchar(as.character(word_seq)) + 1 - nchar(gsub(" ", "", as.character(word_seq)))
                if (model_type == "t")
                        gram <- t
                if (model_type == "n")
                        gram <- n
                if (model_type == "b")
                        gram <- b
                if (model_type == "a")
                        gram <- a
                gram <- gram[gram$nWords > word_count,]
                idx <- grepl(word_seq, gram$nGram, ignore.case = TRUE)
                #idx <- startsWith(gram$nGram, word_seq, ignore.case = TRUE)
                xgram <- gram[idx,]
                xgram <- xgram[order(xgram$Freq, decreasing = TRUE),]
                head(xgram, 10)
        }
        
        f <- function(query) {
                for (j in 1:length(type)) {
                        for (i in 1:nrow(contractions)) {
                                type[[j]]$content <-
                                        gsub(
                                                contractions[i,2], contractions[i,1], type[[j]]$content, perl = TRUE
                                        )
                        }
                }
                
                idx <- grepl(query, quadgram$nGram, ignore.case = TRUE)
                
                #sort quad based on text and frequency
                xgram <- quadgram[idx,]
                xgram <- xgram[order(xgram$Freq, decreasing = TRUE),]
                head(xgram, 10)
        }
        
}

