#word_seq <- query
#model_type <- "n"
#compare to quadrigram
qf <- function(model_type = "t", word_seq = "a"){
        word_count <- nchar(as.character(word_seq)) + 1 - nchar(gsub(" ", "", as.character(word_seq)))
        if (model_type == "t") gram <- t       
        if (model_type == "n") gram <- n
        if (model_type == "b") gram <- b
        if (model_type == "a") gram <- a
        gram <- gram[gram$nWords > word_count,]
        idx <- grepl(word_seq, gram$nGram, ignore.case = TRUE)
        #idx <- startsWith(gram$nGram, word_seq, ignore.case = TRUE)
        xgram <- gram[idx,]
        xgram <- xgram[order(xgram$Freq, decreasing = TRUE),]
        head(xgram, 10)
}
query <- "to the"
qf("a",query)
qf("b",query)
qf("n",query)
qf("t",query)


#find text
#query <- "of beer"
#compare to quadrigram
f<- function(query){
        
        for (j in 1:length(type)) {
                for (i in 1:nrow(contractions)) {
                        type[[j]]$content <-
                                gsub(contractions[i,2], contractions[i,1], type[[j]]$content, perl = TRUE)
                }
        }
        
        idx <- grepl(query, quadgram$nGram, ignore.case = TRUE)
        
        #sort quad based on text and frequency
        xgram <- quadgram[idx,]
        xgram <- xgram[order(xgram$Freq, decreasing = TRUE),]
        head(xgram, 10)
}


be

insane

insensitive

asleep

callous

f(" insane")
f(" insensitive")
f(" asleep")
f(" callous")

```
For each of the sentence fragments below use your natural language processing algorithm to predict the next word in the sentence.

When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd

give

#die

eat

sleep
1
point
2. 
Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his

financial

spiritual

horticultural

#marital
1
point
3. 
I'd give anything to see arctic monkeys this

month

morning

#weekend

decade
1
point
4. 
Talking to your mom has the same effect as a hug and helps reduce your

hunger

sleepiness

happiness

#stress
1
point
5. 
When you were in Holland you were like 1 inch away from me but you hadn't time to take a

look

#picture

walk

minute
1
point
6. 
I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the

#matter

account

incident

case
1
point
7. 
I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each

finger

#hand

toe

arm
1
point
8. 
Every inch of you is perfect from the bottom to the

#top

middle

side

center
1
point
9. 
I’m thankful my childhood was filled with imagination and bruises from playing

daily

weekly

inside

#outside
1
point
10. 
I like how the same people are in almost all of Adam Sandler's

stories

#movies

pictures

novels
# find in corpus
query <- c("live and I'd", "he started telling me about his", "to see arctic monkeys this", 
           "same effect as a hug and helps reduce your", "but you hadn't time to take a",
           "a presentation of evidence, and a jury to settle the",
           "even hold an uneven number of bags of groceries in each", 
           "is perfect from the bottom to the", "filled with imagination and bruises from playing",
           "same people are in almost all of Adam Sandler's")
fq <- function(query){
        for (i in c("tw", "n", "b")){
                if (i == "tw") {orig <- tw}
                if (i == "n") {orig <- n}
                if (i == "b") {orig <- b}
                
                origtext <- grep(query, orig[[1]]$content, ignore.case = TRUE, perl = TRUE)
                print(paste("From", i, query, "TEXT:", orig[[1]]$content[head(origtext)]))
        }
        
}
fq("from playing outside")

topq <- function(query) {
        for (i in 1:4) {
                if (i == "1") {
                        orig <- a1
                }
                if (i == "2") {
                        orig <- a2
                }
                if (i == "3") {
                        orig <- a3
                }
                if (i == "4") {
                        orig <- a4
                }
                
                origtext <- grep(query, orig$nGram, ignore.case = TRUE, perl = TRUE)
                print(paste(query, "From", i, origtext[1] , "TEXT", orig[head(origtext, 3),1]))
        }
}
topq("stress")



origtext <- grep(query, top3gram$nGram, ignore.case = TRUE, perl = TRUE)
