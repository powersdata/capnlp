#sample and save data
write_training_text <- function(sourceletter, samplepercent, ngram){
                starttime <- Sys.time()
                testsource <- c("b", "n", "tw")
                if (!sourceletter %in% testsource) {
                        sourceletter <- "a"
                        for (filelist in c("b", "n", "tw")) {
                                if (filelist == "b") {filename = "en_US.blogs.txt"}
                                if (filelist == "n") {filename = "en_US.news.txt"}
                                if (filelist == "tw") {filename = "en_US.twitter.txt"}
                                
                                if (!dir.exists(paste0("data/sample/", samplepercent))) {
                                        dir.create(paste0("data/sample/", samplepercent))
                                }
                                
                                samplefile <- paste0("data/sample/", samplepercent, "/", sourceletter, ".txt")
                                
                                if (!file.exists(samplefile) == TRUE) {
                                        #rm(con, con_file)
                                        con_file <- file(paste0("data/", filename), "r")
                                        con <- file(paste0("data/", filename), "r")
                                        con_length <- length(readLines(con = con_file, encoding = "UTF-8", warn = FALSE))
                                        sampled <- Usable_Text(readLines(con = con, n = con_length * as.numeric(samplepercent)/100,
                                                                         encoding = "UTF-8", warn = FALSE, skipNul = TRUE))
                                        #sampled <- Usable_Text(sampled)
                                        #sampled[1:10]
                                        closeAllConnections()
                                        write.csv(sampled, file=samplefile, row.names = FALSE, quote=F, fileEncoding = "UTF-8")
                                        sampled <- read.csv(file = samplefile, header = FALSE, quote = "", 
                                                            row.names = NULL, stringsAsFactors = FALSE)
                                }
                                else {sampled <- read.csv(file = samplefile, header = FALSE, quote = "", 
                                                          row.names = NULL, stringsAsFactors = FALSE)}
                                
                        }
                }
                
                if (sourceletter == "b") {filename = "en_US.blogs.txt"}
                if (sourceletter == "n") {filename = "en_US.news.txt"}
                if (sourceletter == "tw") {filename = "en_US.twitter.txt"}
                
                
                if (!dir.exists(paste0("data/sample/", samplepercent))) {
                        dir.create(paste0("data/sample/", samplepercent))
                }
                
                samplefile <- paste0("data/sample/", samplepercent, "/", sourceletter, ".txt")
                
                if (!file.exists(samplefile) == TRUE) {
                #rm(con, con_file)
                con_file <- file(paste0("data/", filename), "r")
                con <- file(paste0("data/", filename), "r")
                con_length <- length(readLines(con = con_file, encoding = "UTF-8", warn = FALSE))
                sampled <- Usable_Text(readLines(con = con, n = con_length * as.numeric(samplepercent)/100,
                                    encoding = "UTF-8", warn = FALSE, skipNul = TRUE))
                #sampled <- Usable_Text(sampled)
                #sampled[1:10]
                closeAllConnections()
                write.csv(sampled, file=samplefile, row.names = FALSE, quote=F, fileEncoding = "UTF-8")
                sampled <- read.csv(file = samplefile, header = FALSE, quote = "", 
                                    row.names = NULL, stringsAsFactors = FALSE)
                }
                else {sampled <- read.csv(file = samplefile, header = FALSE, quote = "", 
                                          row.names = NULL, stringsAsFactors = FALSE)}
                print(paste("TIME for", filename, "sampling", samplepercent, "% of lines:", timer(starttime)))
                return(sampled)
}
