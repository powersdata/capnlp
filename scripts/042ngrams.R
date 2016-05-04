# token_delim <- " \\t\\r\\n.!?,;\"()"
# token_delim <- " "
# 
# onetoken <- NGramTokenizer(sample_df, Weka_control(min=1,max=1))
# bitoken <- NGramTokenizer(sample_df, Weka_control(min=2,max=2, delimiters = token_delim))
# tritoken <- NGramTokenizer(sample_df, Weka_control(min=3,max=3, delimiters = token_delim))
# quatrtoken <- NGramTokenizer(sample_df, Weka_control(min=4,max=4, delimiters = token_delim))

Corpus_to_ngram <- function(sourceletter, samplepercent, ngram) {
        starttime <- Sys.time()
        object.name <- deparse(substitute(sourceletter))
        rows <- ngram * 10000
        tmp_ngram <- NGramTokenizer(corpus, Weka_control(min = ngram, max = ngram))
        corpus_ngram <- data.frame(table(tmp_ngram), stringsAsFactors = FALSE)
        colnames(corpus_ngram) <- c("nGram","Freq")
        corpus_ngram$nWords <-
                nchar(as.character(corpus_ngram$nGram)) + 1 - nchar(gsub(" ", "", as.character(corpus_ngram$nGram)))
        corpus_ngram <- corpus_ngram[order(corpus_ngram$Freq, decreasing = TRUE),]
        rownames(corpus_ngram) <- NULL
        saveRDS(corpus_ngram, paste0("data/sample/",samplepercent, "/cl/",ngram, "ngram.rds"))
        corpus_ngram <- corpus_ngram[1:rows,]
        print(paste("TIME for", object.name, ngram, "is", timer(starttime)))
        return(corpus_ngram)
}
# corpus_to_ngram <- function(corpus) {
#         starttime <- Sys.time()
#         object.name <- deparse(substitute(corpus))
#         
#         corpus_ngram <- NULL
#         for (i in 1:4) {
#                 tmp_ngram <- NGramTokenizer(corpus, Weka_control(min = i, max = i))
#                 xgram <- data.frame(table(tmp_ngram), stringsAsFactors = FALSE)
#                 corpus_ngram <- rbind(corpus_ngram, xgram)
#         }
#         colnames(corpus_ngram) <- c("nGram","Freq")
#         corpus_ngram$nWords <-
#                 nchar(as.character(corpus_ngram$nGram)) + 1 - nchar(gsub(" ", "", as.character(corpus_ngram$nGram)))
#         corpus_ngram <- corpus_ngram[order(corpus_ngram$Freq, decreasing = TRUE),]
#         rownames(corpus_ngram) <- NULL
#         print(paste("TIME for", object.name, "ngram is", timer(starttime)))
#         return(corpus_ngram)
# }
# clean.tdm <- TermDocumentMatrix(cleanedCorpus, 
#                                 control = list(tokenize = function(x) rwekaTokenizer(x,
#                                                                                      ngrams),
#                                                wordLengths = c(1, Inf)))
# 

#testing
# rm(list=ls(), all = TRUE)
# gc(reset=TRUE)
# 
# bitoken <- NGramTokenizer(csa, Weka_control(min=2,max=2, delimiters = token_delim))
# head(bitoken, 25)
# wordtoken <- WordTokenizer(csa,Weka_control(min=2,max=2, delimiters = token_delim))
# head(wordtoken, 25)
# head(csan, 25)
