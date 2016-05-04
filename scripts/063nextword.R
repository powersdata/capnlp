Next_Word <- function(sourceletter, ngram, wordlist) {
        source("scripts/061fornxt.R")
        starttime <- Sys.time()
        directory <- "data/sample/RDS/"
        nextfile <-
                paste0(directory, sourceletter, 1, ngram, ".rds")
        corpus <- VectorSource(wordlist)
        vector_source <- VectorSource(wordlist)
        corpus <-
                Corpus(vector_source,readerControl = list(reader = readPlain))
        clean <- tm_map(corpus, FUN = tm_reduce, tmFuns = reducer)
        clean <- tm_map(clean, removeNumbers)
        clean <- tm_map(clean, removePunctuation)
        clean <- tm_map(clean, stripWhitespace)
        wordlist <- clean[[1]]$content
        word_count <- sapply(gregexpr("\\S+", wordlist), length)
        lastword <- word(wordlist,-1)
        ngram_count <-
                ifelse(word_count < ngram, (word_count + 1), ngram)
        nxtwrd <- readRDS(nextfile)
        nxtwrd_two <- nxtwrd[nxtwrd$nWord %in% 2:ngram_count,]
        nxtwrd_two <-
                nxtwrd_two[word(nxtwrd_two$nGram,-2) == lastword,]
        i <- 2
        nxtwrd_final <- nxtwrd[nxtwrd$nWord == 1,]
        nxtwrd_tmp <- nxtwrd_two[nxtwrd_two$nWords == i,]
        nxtwrd_final <- rbind(nxtwrd_tmp, nxtwrd_final)
        nxtwrd_final <- nxtwrd_final[1:5,]
#         repeat {
#                 i <- i + 1
#                 if (i > ngram_count) {
#                         break
#                 }
#                 nxtwrd_tmp <- nxtwrd_two[nxtwrd_two$nWords == i,]
#                 wordstofind <- word(wordlist,-(i - 1),-1)
#                 wordsinngram <- word(nxtwrd_tmp$nGram,-i, (i - 1))
#                 nxtwrd_tmp <-
#                         nxtwrd_tmp[wordsinngram == wordstofind,]
#                 if (nrow(nxtwrd_tmp) < 1) {
#                         break
#                 }
#                 nxtwrd_final <- rbind(nxtwrd_tmp, nxtwrd_final)
#         }
        while (i > ngram_count & nrow(nxtwrd_tmp) < 1) {
                i <- i + 1
                nxtwrd_tmp <- nxtwrd_two[nxtwrd_two$nWords == i,]
                wordstofind <- word(wordlist,-(i - 1),-1)
                wordsinngram <- word(nxtwrd_tmp$nGram,-i, (i - 1))
                nxtwrd_tmp <- nxtwrd_tmp[wordsinngram == wordstofind,]
                nxtwrd_final <- rbind(nxtwrd_tmp, nxtwrd_final)
        }
        #nxtwrd_final <- unique(rbind(nxtwrd_final, nxtwrd_one))
        #nxtwrd_final <- nxtwrd_final[1:3,]
        nxtwrd_final <- unique(word(nxtwrd_final$nGram,-1))
        timer(starttime)
        #write(timer(starttime), file = "data/timer.txt")
        #print(paste("TIME for next word find:", timer(starttime)))
        return(nxtwrd_final[1:4])
}
