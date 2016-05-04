
require(RWeka)
require(tm)
require(stringr)
#require(gdata)


timer <- function(starttime) {
        x <- difftime(Sys.time(), starttime, units = "sec")
        x <- round(x, digits = 1)
        y <- paste("TIME for next word find:",x,"seconds")
        assign("timelength",y,envir = .GlobalEnv)
        }

#Usable_Text <- function(x) gsub("[^[:graph:]]", " ", x) 
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
Encoding_Issues <- content_transformer(function(x, pattern)
        mgsub(pattern = m_pattern, replacement = m_replacement,x, perl = TRUE))

rm_Contractions <- content_transformer(function(x, pattern)
        mgsub(pattern = ptrn, replacement = rplmnt,x, perl = TRUE))

Censor2 <- content_transformer(function(x, pattern) 
        gsub(pattern = paste0(profanity2, collapse="|"), replacement = "", x))

Non_Words <- content_transformer(function(x, pattern) 
        gsub(pattern = paste0(nonwords, collapse="|"), replacement = "", x))

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

m_pattern <- c("@\\w+ *", '#\\w+ *','[\\]\\[\\(\\)-/+;:#%$^\\*=^~\\{\\}/"<>«»_\\\\“\\”⁰•‘’–]', "[^[:graph:]]",
               '[\\!\\?…]', "[\\.]{2,}", "&", "' ", "([[:alpha:]])\\1{3,}", "lb", "oz", "rt")
m_replacement <- c("", "", "", " ", ".", ".", " and ", "", "\\1\\1", "pound", "ounce", "")
rplmnt = c("s", " is", " am", "an not", " not", " will", " are", " would", " have")
ptrn = c("s[^a-z0-9\\s]s", "\\b[^a-z0-9\\s]s\\b", "\\b[^a-z0-9\\s]m\\b", "an[^a-z0-9\\s]t", 
         "n[^a-z0-9\\s]t", "\\b[^a-z0-9\\s]ll\\b", "\\b[^a-z0-9\\s]re\\b", "\\b[^a-z0-9\\s]d\\b",
         "\\b[^a-z0-9\\s]ve\\b")

nonwords <- c("\\bhttp(\\S*)\\b" ,"\\b(\\S*)?\\.com\\b", "\\b(\\S*)?\\.gov\\b", 
              "\\b(\\S*)?\\.org\\b", "\\b(\\S*)?\\.net\\b", "\\b(\\S*)?\\.tv\\b")

reducer <- list(content_transformer(tolower),  rm_Contractions, Encoding_Issues,
                Censor2, Non_Words)