
#This is the startting script for the project
directory <- getwd()
setwd(directory)
which_library <- "main"
source("scripts/001libraries.R")

which_function <- "main"
source("scripts/005functions.R")

which_import <- "main"
source("scripts/010imports.R")

#Load Full Corpus
which_import <- "fullcorpus"
source("scripts/010imports.R")

#specific sample words of corpus
source("scripts/010imports.R")
source("scripts/026sampleselection.R")
findwordlist <- c("i'd die", "marital", "monkeys", "stress", "jury to settle the matter", "time to take a picture", "pizza", "asleep")
#findwordlist <- "marital"
Find_Words(findwordlist)
Selected_Sample()
csa <- Clean_Sample("a")
csan <- Sample_Ngrams("a", ngram = 4)

#find next word
source("scripts/063nextword.R")
Next_Word("a", ngram = 4, wordlist = "but i'm")
Next_Word("a", 5, wordlist = "arctic monkeys this")
Next_Word("a", ngram = 4, wordlist = "telling me about his")
Next_Word("a", 4, wordlist = "I'd live and I'd")
Next_Word("a", 4, wordlist = "jury to settle the")
Next_Word("a", 5, wordlist = "settle the pizza")


#looking at files
head(a8, 25)
a8$nGram <- gsub("[^a-z ]", "", a8$nGram)
a8$nGram <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", a8$nGram, perl=TRUE)
cc<-csa[[2]]$content
head(cc,15)

findtext <- grep("list author", cc[[1]]$content)
findtext <- grep("happy", cc)
findtext <- grep("list author", cc[[3]]$content)





source("scripts/042ngrams.R")




#data analysis
source("scripts/020data.R")

#sample corpus
source("scripts/025sample.R")
samplepercent <- 0.1
sn <- Corpus(VectorSource(write_training_text(sourceletter, samplepercent, ngram)))

source("scripts/030clean.R")
csn <- Clean_Corpus(sn, samplepercent)

source("scripts/042ngrams.R")
cs <- csn
csn1 <- Corpus_to_ngram(cs, samplepercent, 1)
csn2 <- Corpus_to_ngram(cs, samplepercent, 2)
csn3 <- Corpus_to_ngram(cs, samplepercent, 3)
csn4 <- Corpus_to_ngram(cs, samplepercent, 4)


sn <- Corpus(VectorSource(write_training_text("en_US.news.txt", samplepercent)))
sb <- Corpus(VectorSource(write_training_text("en_US.blogs.txt", samplepercent)))
stw <- Corpus(VectorSource(write_training_text("en_US.twitter.txt", samplepercent)))
#clean corpus

csb <- Clean_Corpus(sb, samplepercent)
cstw <- Clean_Corpus(stw, samplepercent)

csa1 <- Clean_Corpus(a, .1)
#csa10 <- Clean_Corpus(a, samplepercent)
#csa20 <- Clean_Corpus(a, 20)

#sample, clean and create ngrams and save to RDS
sn <- Corpus(VectorSource(write_training_text("en_US.news.txt", samplepercent)))
sb <- Corpus(VectorSource(write_training_text("en_US.blogs.txt", samplepercent)))
stw <- Corpus(VectorSource(write_training_text("en_US.twitter.txt", samplepercent)))


which_function <- "main"
source("scripts/005functions.R")
sn <- SCNS("n",0.1,4)
sb <- SCNS("b",5,4)
stw <- SCNS("tw",7,4)
sa <- SCNS("a", 0.1, 4)

which_import <- "samples"
source("scripts/010imports.R")
names(b54.rds) <- c("file", "unigram", "bigram", "trigram", "quadgram")
names(n704.rds) <-
        c("file", "unigram", "bigram", "trigram", "quadgram")
names(tw74.rds) <-
        c("file", "unigram", "bigram", "trigram", "quadgram")
for (i in 2:5) {
        starttime <- Sys.time()
        x <- b54.rds[[i]]
        y <- n704.rds[[i]]
        z <- tw74.rds[[i]]
        df <-
                merge(
                        x, y, by.x = c("nGram", "nWords"), by.y = c("nGram", "nWords"), all = TRUE
                )
        df[is.na(df)] <- 0
        
        df$Freq <- as.numeric(df$Freq.x) + as.numeric(df$Freq.y)
        df <- df[-c(3, 4)]
        
        x <- df
        y <- x
        
        df <-
                merge(
                        x, y, by.x = c("nGram", "nWords"), by.y = c("nGram", "nWords"), all = TRUE
                )
        df[is.na(df)] <- 0
        
        df$Freq <- as.numeric(df$Freq.x) + as.numeric(df$Freq.y)
        df <- df[-c(3, 4)]
        df <- df[order(df$Freq, decreasing = TRUE),]
        rownames(df) <- NULL
        rows <- 1000 * df$nWords[1]
        df <- df[1:rows,]
        assign (paste0("top", df$nWords[1], "gram"), df)
        print(paste(
                "TIME for", df$nwords[1], "merging:", timer(starttime)
        ))
        rm(starttime, i , rows, filenames, x, y, z, df)
}

head(top1gram)
head(top2gram, 20)
head(top3gram, 30)
head(top4gram, 40)

#query
which_query <- "query"
source("scripts/051query.R")







#selected model to test
csa <- csa1
csa <- cstw
csa <- csn

rm(csn, csb, cstw, csa1, csa10, csa20)

head(csa4, 10)
#Testing
orig <- stw
orig <- sn
orig <- tw
orig <- n
orig <- b
cc <- csa
source("scripts/030clean.R")
csn <- Clean_Corpus(sn, samplepercent)
cc <- csa

orig[[1]]$content[1:4]
cc[[1]]$content[1:4]


findtext <- grep("list author", cc[[3]]$content)

for (i in c("tw", "n", "b")){

origtext <- grep("jury to settle the", orig[[1]]$content, perl = TRUE)
origtext


origtext <- grep("\\b#(\\S*)\\b", orig[[1]]$content)


cc[[1]]$content[22]
orig[[1]]$content[22]

cc[[1]]$content[head(origtext)]
orig[[1]]$content[head(origtext)]
}

# test  for 
foundtext <- gsub(" *\\b(?<![ai])\\p{L}{1,1}(?![ai])\\b *", " ", cc[[1]]$content[findtext], perl = TRUE)
head(foundtext)

clean[[1]]$content[100]
tw[[1]]$content[2720]
n[[1]]$content[22]
n[[1]]$content[2142]
# atext[[1]]$content[366]
# atext[[1]]$content[813]

#Start testing over
# rm(list=ls())
# gc(reset=TRUE)

#find text in samples
findtext <- grep(findwordlist, a4$nGram, perl = TRUE)
a4[head(findtext),]

#find in sample clean corpus
cc<-csa[[1]]$content
cc<-a8$nGram
head(cc,15)

str_replace_all(cc, "[^a-z ]", "")


cc<-unique(as.character(rbind(csa[[1]]$content, csa[[2]]$content)))
cc<-unique(as.character(rbind(cc,csa[[3]]$content)))
rownames(cc) <- NULLstr_replace_all(clean, "[^a-z ]", "")
findtext <- as.integer(grep("=", cc))
cc[findtext,]
fruits <- c("one apple", "two pears", "three bananas")

fruits <- c("one2 apple", "1two pears", "thr3ee bananas")
str_replace_all(fruits, "[0-9]", "")
str_replace_all(fruits, "[^a-z ]", "")
str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")
fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "([aeiou])", "")
str_replace_all(fruits, "([aeiou])", "\\1\\1")
str_replace_all(fruits, "[aeiou]", c("1", "2", "3"))
str_replace_all(fruits, c("a", "e", "i"), "-")

lastgram <- as.data.frame(word(xgram$nGram, -2))

#                   xgram_tmp[order(xgram_tmp$Freq, decreasing = TRUE),]
#                   dd[ order(-dd[,4], dd[,1]), ]
xgram <- a16
rn <- sort.int(sample(1:nrow(a16), 4000, replace=F))
rn <- append(rn, as.integer(nrow(a16)-2))
rn <- append(rn, as.integer(nrow(a16)-1))
rn <- append(rn, as.integer(nrow(a16)))


xgram <- xgram[rn, ]
clean_xgram_sample <- xgram

xgram <- clean_xgram_sample
head(xgram)
tail(xgram)

#testlast <- word(xgram$nGram, -2, -5)

#lastgram <- xgram[order(word(xgram$nGram, -1)),]
#lastgram <- xgram[order(word(xgram$nGram, -2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -3,-2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -4,-2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -5,-2)),]

#lastgram <- lastgram[order(word(lastgram$nGram, -6,-2)),]


lastgram <- xgram[order(-xgram$nWords, -xgram$Freq, xgram$nGram),]

#sorted <- a16[stri_sort(stri_extract_last_words(a16$nGram), opts_collator = list(numeric = TRUE))]
head(lastgram, 2000)
tail(lastgram)


=======
#This is the startting script for the project
directory <- getwd()
setwd(directory)
which_library <- "main"
source("scripts/001libraries.R")

which_function <- "main"
source("scripts/005functions.R")

which_import <- "main"
source("scripts/010imports.R")

#Load Full Corpus
which_import <- "fullcorpus"
source("scripts/010imports.R")

#specific sample words of corpus
source("scripts/010imports.R")
source("scripts/026sampleselection.R")
findwordlist <- c("i'd die", "about his marital", "arctic monkeys this weekend", "helps reduce your stress", "jury to settle the matter", "time to take a picture")
#findwordlist <- "marital"
Find_Words(findwordlist)
Selected_Sample()
csa <- Clean_Sample("a")
csan <- Sample_Ngrams("a", ngram = 6)

#find next word
source("scripts/063nextword.R")
Next_Word("a", ngram = 6, wordlist = "but i'm")
Next_Word("a", 6, wordlist = "arctic monkeys this")
Next_Word("a", ngram = 6, wordlist = "telling me about his")
Next_Word("a", 6, wordlist = "I'd live and I'd")

#looking at files
head(a8, 25)
a8$nGram <- gsub("[^a-z ]", "", a8$nGram)
a8$nGram <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", a8$nGram, perl=TRUE)
cc<-csa[[2]]$content
head(cc,15)

findtext <- grep("list author", cc[[1]]$content)
findtext <- grep("happy", cc)
findtext <- grep("list author", cc[[3]]$content)





source("scripts/042ngrams.R")




#data analysis
source("scripts/020data.R")

#sample corpus
source("scripts/025sample.R")
samplepercent <- 0.1
sn <- Corpus(VectorSource(write_training_text(sourceletter, samplepercent, ngram)))

source("scripts/030clean.R")
csn <- Clean_Corpus(sn, samplepercent)

source("scripts/042ngrams.R")
cs <- csn
csn1 <- Corpus_to_ngram(cs, samplepercent, 1)
csn2 <- Corpus_to_ngram(cs, samplepercent, 2)
csn3 <- Corpus_to_ngram(cs, samplepercent, 3)
csn4 <- Corpus_to_ngram(cs, samplepercent, 4)


sn <- Corpus(VectorSource(write_training_text("en_US.news.txt", samplepercent)))
sb <- Corpus(VectorSource(write_training_text("en_US.blogs.txt", samplepercent)))
stw <- Corpus(VectorSource(write_training_text("en_US.twitter.txt", samplepercent)))
#clean corpus

csb <- Clean_Corpus(sb, samplepercent)
cstw <- Clean_Corpus(stw, samplepercent)

csa1 <- Clean_Corpus(a, .1)
#csa10 <- Clean_Corpus(a, samplepercent)
#csa20 <- Clean_Corpus(a, 20)

#sample, clean and create ngrams and save to RDS
sn <- Corpus(VectorSource(write_training_text("en_US.news.txt", samplepercent)))
sb <- Corpus(VectorSource(write_training_text("en_US.blogs.txt", samplepercent)))
stw <- Corpus(VectorSource(write_training_text("en_US.twitter.txt", samplepercent)))


which_function <- "main"
source("scripts/005functions.R")
sn <- SCNS("n",0.1,4)
sb <- SCNS("b",5,4)
stw <- SCNS("tw",7,4)
sa <- SCNS("a", 0.1, 4)

which_import <- "samples"
source("scripts/010imports.R")
names(b54.rds) <- c("file", "unigram", "bigram", "trigram", "quadgram")
names(n704.rds) <-
        c("file", "unigram", "bigram", "trigram", "quadgram")
names(tw74.rds) <-
        c("file", "unigram", "bigram", "trigram", "quadgram")
for (i in 2:5) {
        starttime <- Sys.time()
        x <- b54.rds[[i]]
        y <- n704.rds[[i]]
        z <- tw74.rds[[i]]
        df <-
                merge(
                        x, y, by.x = c("nGram", "nWords"), by.y = c("nGram", "nWords"), all = TRUE
                )
        df[is.na(df)] <- 0
        
        df$Freq <- as.numeric(df$Freq.x) + as.numeric(df$Freq.y)
        df <- df[-c(3, 4)]
        
        x <- df
        y <- x
        
        df <-
                merge(
                        x, y, by.x = c("nGram", "nWords"), by.y = c("nGram", "nWords"), all = TRUE
                )
        df[is.na(df)] <- 0
        
        df$Freq <- as.numeric(df$Freq.x) + as.numeric(df$Freq.y)
        df <- df[-c(3, 4)]
        df <- df[order(df$Freq, decreasing = TRUE),]
        rownames(df) <- NULL
        rows <- 1000 * df$nWords[1]
        df <- df[1:rows,]
        assign (paste0("top", df$nWords[1], "gram"), df)
        print(paste(
                "TIME for", df$nwords[1], "merging:", timer(starttime)
        ))
        rm(starttime, i , rows, filenames, x, y, z, df)
}

head(top1gram)
head(top2gram, 20)
head(top3gram, 30)
head(top4gram, 40)

#query
which_query <- "query"
source("scripts/051query.R")







#selected model to test
csa <- csa1
csa <- cstw
csa <- csn

rm(csn, csb, cstw, csa1, csa10, csa20)

head(csa4, 10)
#Testing
orig <- stw
orig <- sn
orig <- tw
orig <- n
orig <- b
cc <- csa
source("scripts/030clean.R")
csn <- Clean_Corpus(sn, samplepercent)
cc <- csa

orig[[1]]$content[1:4]
cc[[1]]$content[1:4]


findtext <- grep("list author", cc[[3]]$content)

for (i in c("tw", "n", "b")){

origtext <- grep("jury to settle the", orig[[1]]$content, perl = TRUE)
origtext


origtext <- grep("\\b#(\\S*)\\b", orig[[1]]$content)


cc[[1]]$content[22]
orig[[1]]$content[22]

cc[[1]]$content[head(origtext)]
orig[[1]]$content[head(origtext)]
}

# test  for 
foundtext <- gsub(" *\\b(?<![ai])\\p{L}{1,1}(?![ai])\\b *", " ", cc[[1]]$content[findtext], perl = TRUE)
head(foundtext)

clean[[1]]$content[100]
tw[[1]]$content[2720]
n[[1]]$content[22]
n[[1]]$content[2142]
# atext[[1]]$content[366]
# atext[[1]]$content[813]

#Start testing over
# rm(list=ls())
# gc(reset=TRUE)

#find text in samples
findtext <- grep(findwordlist, a4$nGram, perl = TRUE)
a4[head(findtext),]

#find in sample clean corpus
cc<-csa[[1]]$content
cc<-a8$nGram
head(cc,15)

str_replace_all(cc, "[^a-z ]", "")


cc<-unique(as.character(rbind(csa[[1]]$content, csa[[2]]$content)))
cc<-unique(as.character(rbind(cc,csa[[3]]$content)))
rownames(cc) <- NULLstr_replace_all(clean, "[^a-z ]", "")
findtext <- as.integer(grep("=", cc))
cc[findtext,]
fruits <- c("one apple", "two pears", "three bananas")

fruits <- c("one2 apple", "1two pears", "thr3ee bananas")
str_replace_all(fruits, "[0-9]", "")
str_replace_all(fruits, "[^a-z ]", "")
str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")
fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "([aeiou])", "")
str_replace_all(fruits, "([aeiou])", "\\1\\1")
str_replace_all(fruits, "[aeiou]", c("1", "2", "3"))
str_replace_all(fruits, c("a", "e", "i"), "-")

lastgram <- as.data.frame(word(xgram$nGram, -2))

#                   xgram_tmp[order(xgram_tmp$Freq, decreasing = TRUE),]
#                   dd[ order(-dd[,4], dd[,1]), ]
xgram <- a16
rn <- sort.int(sample(1:nrow(a16), 4000, replace=F))
rn <- append(rn, as.integer(nrow(a16)-2))
rn <- append(rn, as.integer(nrow(a16)-1))
rn <- append(rn, as.integer(nrow(a16)))


xgram <- xgram[rn, ]
clean_xgram_sample <- xgram

xgram <- clean_xgram_sample
head(xgram)
tail(xgram)

#testlast <- word(xgram$nGram, -2, -5)

#lastgram <- xgram[order(word(xgram$nGram, -1)),]
#lastgram <- xgram[order(word(xgram$nGram, -2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -3,-2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -4,-2)),]
#lastgram <- lastgram[order(word(lastgram$nGram, -5,-2)),]

#lastgram <- lastgram[order(word(lastgram$nGram, -6,-2)),]


lastgram <- xgram[order(-xgram$nWords, -xgram$Freq, xgram$nGram),]

#sorted <- a16[stri_sort(stri_extract_last_words(a16$nGram), opts_collator = list(numeric = TRUE))]
head(lastgram, 2000)
tail(lastgram)


>>>>>>> 004f468cdccad6b1cdbb8e587c3047875b7eb5b7
