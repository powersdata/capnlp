if (which_import == "main") {
        if (!dir.exists("data")) {
                dir.create("data")
        }
        if (!dir.exists("data/sample")) {
                dir.create("data/sample")
        }
        if (!dir.exists("data/sample/RDS")) {
                dir.create("data/sample/RDS")
        }
        if (!dir.exists("data/tw")) {
                dir.create("data/tw")
        }
        if (!dir.exists("data/b")) {
                dir.create("data/b")
        }
        if (!dir.exists("data/n")) {
                dir.create("data/n")
        }
        
        filename <- "Coursera-SwiftKey.zip"
        if (!file.exists(paste0("data/", filename))) {
                fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                setInternet2(use = TRUE)
                download.file(fileUrl, destfile = paste0("data/", filename))
                dateDownloaded <- date()
                write(dateDownloaded, file = "data/dateDownloaded.txt")
                rm(fileUrl)
        }
        
        for (filelist in c("en_US.twitter.txt", "en_US.news.txt", "en_US.blogs.txt")) {
                if (filelist == "en_US.twitter.txt") {sourceletter <- "tw"}
                if (filelist == "en_US.news.txt")    {sourceletter <- "n"}
                if (filelist == "en_US.blogs.txt")   {sourceletter <- "b"}
                
                if ((!file.exists(paste0("data/", filelist)))) {
                        zip_ls = unzip(paste0("data/", filename), list = TRUE)
                        unzip (
                                paste0("data/", filename), files = paste0("final/en_US/", filelist), 
                                junkpaths = TRUE, exdir = "./data"
                        )
                        file.copy(from = paste0("data/", filelist), to = paste0("data/", sourceletter), recursive = TRUE)
                        rm(zip_ls)
                }
        }
        rm(filename, filelist)
        mgsub <- function(pattern, replacement, x, ...) {
                if (length(pattern)!=length(replacement)) {
                        stop("pattern and replacement do not have the same length.")
                }
                result <- x
                for (i in 1:length(pattern)) {
                        result <- gsub(pattern[i], replacement[i], result, ...)
                }
                result
        }
        
        #load profanity list
#         if (file.exists("data/profanity.rds") == FALSE) {
#                 library(RCurl)
#                 fileUrl <- "http://www.frontgatemedia.com/new/wp-content/uploads/2014/03/Terms-to-Block.csv"
#                 profanity <- read.delim(fileUrl, skip = 4, header=FALSE, stringsAsFactors=FALSE,
#                                         sep = ',', fill = TRUE)
#                 profanity <- profanity[,2]
#                 profanity <- gsub(",$","",profanity)
#                 profanity <- unlist(strsplit(profanity, "\n")) #list of swear words
#                 saveRDS(profanity, file = "data/profanity.rds")
#                 rm(fileUrl)
#                 
#         } else {
#                 if (!exists("profanity")) {
#                 profanity <- readRDS("data/profanity.rds")
#                 print("...profanity LOADED.")
#                 }}
        
        
        if (file.exists("data/profanity2.rds") == FALSE) {
                library(RCurl)
                fileUrl <-
                        getURL(
                                "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
                        )
                profanity2 <- unlist(strsplit(fileUrl, "\n")) #list of swear words
                saveRDS(profanity2, file = "data/profanity2.rds")
                rm(fileUrl)
                
        } else {if (!exists("profanity2")) {
                profanity2 <- readRDS("data/profanity2.rds")
                print("...profanity2 LOADED.")
        }}
        
        
#         if (file.exists("data/en_wordlist.rds") == FALSE) {
#                 url_emoji <- "http://www-personal.umich.edu/~jlawler/wordlist"
#                 en_wordlist <- readLines(url_emoji)
#                 rm(url_emoji)
#                 saveRDS(en_wordlist, file = "data/en_wordlist.rds")
#         } else {if (!exists("en_wordlist")) {
#                 en_wordlist <- readRDS("data/en_wordlist.rds")
#                 print("en_wordlist LOADED.")
#         }}
#         
#         if (file.exists("data/emoticonlist.rds") == FALSE) {
#                 library(RCurl)
#                 library(httr)
#                 library(XML)
#                 
#                 #Import :emoji: style emoticons from website
#                 url_emoji <- "http://www.emoji-cheat-sheet.com/"
#                 content_emoji <-
#                         htmlTreeParse(url_emoji, useInternalNodes = T)
#                 x <-
#                         xpathSApply(content_emoji, "//span[@class='name']", xmlValue)
#                 emojilist <-
#                         unlist(lapply(x, function(x) {
#                                 paste0(":",x,":")
#                         }))
#                 
#                 
#                 #Import smileys from website
#                 url_smileys1 <-
#                         "http://cool-smileys.com/text-emoticons"
#                 url_smileys2 <-
#                         "http://cool-smileys.com/text-emoticons-part2"
#                 
#                 content_smileys1 <-
#                         htmlTreeParse(url_smileys1, useInternalNodes = T)
#                 content_smileys2 <-
#                         htmlTreeParse(url_smileys2, useInternalNodes = T)
#                 
#                 y <-
#                         xpathSApply(content_smileys1, "//input", xmlAttrs)
#                 z <-
#                         xpathSApply(content_smileys2, "//input", xmlAttrs)
#                 smileylist1 <-
#                         names(table(sapply(y, function(x)
#                                 x["value"])))
#                 smileylist2 <-
#                         names(table(sapply(z, function(x)
#                                 x["value"])))
#                 
#                 #Concatenate all emoticon lists into single list, emoticonlist
#                 emoticonlist <-
#                         c(emojilist, smileylist2, smileylist2)
#                 
#                 #saveRDS
#                 saveRDS(emoticonlist, file = "data/emoticonlist.rds")
#                 
#                 #Remove other items from memory
#                 rm(x)
#                 rm(y)
#                 rm(z)
#                 rm(url_emoji)
#                 rm(emojilist)
#                 rm(content_emoji)
#                 rm(url_smileys1)
#                 rm(url_smileys2)
#                 rm(content_smileys1)
#                 rm(content_smileys2)
#                 rm(smileylist1)
#                 rm(smileylist2)
#         }else {if (!exists("emoticonlist")) {
#                 emoticonlist <- readRDS("data/emoticonlist.rds")
#                 print("emoticonlist LOADED.")
#         }}

        m_pattern <- c("@\\w+ *", '#\\w+ *','[\\]\\[\\(\\)-/+;:#%$^\\*=^~\\{\\}/"<>«»_\\\\“\\”⁰•‘’–]', "[^[:graph:]]",
                       '[\\!\\?…]', "[\\.]{2,}", "&", "' ", "([[:alpha:]])\\1{3,}", "lb", "oz", "rt")
        m_replacement <- c("", "", "", " ", ".", ".", " and ", "", "\\1\\1", "pound", "ounce", "")
        rplmnt = c("s", " is", " am", "an not", " not", " will", " are", " would", " have")
        ptrn = c("s[^a-z0-9\\s]s", "\\b[^a-z0-9\\s]s\\b", "\\b[^a-z0-9\\s]m\\b", "an[^a-z0-9\\s]t", 
                 "n[^a-z0-9\\s]t", "\\b[^a-z0-9\\s]ll\\b", "\\b[^a-z0-9\\s]re\\b", "\\b[^a-z0-9\\s]d\\b",
                 "\\b[^a-z0-9\\s]ve\\b")
        
        nonwords <- c("\\bhttp(\\S*)\\b" ,"\\b(\\S*)?\\.com\\b", "\\b(\\S*)?\\.gov\\b", 
                      "\\b(\\S*)?\\.org\\b", "\\b(\\S*)?\\.net\\b", "\\b(\\S*)?\\.tv\\b")
        
        Encoding_Issues <- content_transformer(function(x, pattern)
                mgsub(pattern = m_pattern, replacement = m_replacement,x, perl = TRUE))
        
        rm_Contractions <- content_transformer(function(x, pattern)
                mgsub(pattern = ptrn, replacement = rplmnt,x, perl = TRUE))
        
        #         Emoticon <- content_transformer(function(x, pattern) 
        #                 gsub(pattern = paste0(emoticonlist, collapse="|"), replacement = "", x))
        #         
        #         Censor <- content_transformer(function(x, pattern) 
        #                 gsub(pattern = paste0(profanity, collapse="|"), replacement = "", x))
        
        Censor2 <- content_transformer(function(x, pattern) 
                gsub(pattern = paste0(profanity2, collapse="|"), replacement = "", x))
        
        Non_Words <- content_transformer(function(x, pattern) 
                gsub(pattern = paste0(nonwords, collapse="|"), replacement = "", x))
        
        #         reducer <- list(Encoding_Issues, content_transformer(tolower), Non_Words, 
        #                         removeNumbers, Censor2, rm_Contractions, stripWhitespace)
        #         reducer <- list(Encoding_Issues, content_transformer(tolower), Non_Words, Censor2,
        #                         removeNumbers, rm_Contractions, removePunctuation, stripWhitespace)
        #reducer <- list(content_transformer(tolower), rm_Contractions, Encoding_Issues,
        #                Censor2, Non_Words, removeNumbers, removePunctuation, stripWhitespace)
        reducer <- list(content_transformer(tolower),  rm_Contractions, Encoding_Issues,
                        Censor2, Non_Words)
}

#full corpus imports
if (which_import == "fullcorpus"){
        starttime <- Sys.time()
        for (sourceletter in c("b","n","tw")){
                if (!exists("b") & sourceletter == "b"){
                        b <- Corpus(DirSource(paste0("data/", sourceletter)), readerControl = list(language="UTF-8"))
                        con <- file("data/en_US.blogs.txt", "r")
                        #bt <- readLines(con = con)
                        #bl <- length(bt)
                }
                if (!exists("n")& sourceletter == "n"){
                        n <- Corpus(DirSource(paste0("data/", sourceletter)), readerControl = list(language="UTF-8"))
                        con <- file("data/en_US.news.txt", "r")
                        #nl <- length(readLines(con = con))
                }
                if (!exists("tw")& sourceletter == "tw"){
                        tw <- Corpus(DirSource(paste0("data/", sourceletter)), readerControl = list(language="UTF-8"))
                        con <- file("data/en_US.twitter.txt", "r")
                        #twl <- length(readLines(con = con))
                }
        }
        closeAllConnections()
        print(paste("TIME for", which_import, "import is", timer(starttime)))
        #rm(con)
}

#sample files for testing
if (which_import == "samples"){
        starttime <- Sys.time()
        filenames <- list.files(path = "data/sample/RDS/", pattern = "*.rds", full.names = TRUE)
        for (i in 1:length(filenames)) {
                assign(basename(filenames[i]), readRDS(filenames[i]))
        }
        
        #lapply(names <- c("file", "unigram", "bigram", "trigram", "quadgram")
        #lapply(assign, "(", basename(filenames[1]),readRDS)
        #res <- lapply(ldf, summary)
        #names(res)  <- substr(filenames, 17, 30)
        print(paste("TIME for", which_import, "import is", timer(starttime)))
}

