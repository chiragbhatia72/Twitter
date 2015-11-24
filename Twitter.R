save.image("twitter.Rdata")
load("~/twitter.Rdata")
library("twitteR")

#Extracting tweets using api
consumerKey <-"ubAQyKmntRo1IbS31g8IlKYmK"
consumerSecret <- "X8wSL3mkQe5imJNon2rUrCDocBp9pIsBqFSchLqsxupeDPHEv4"
accessToken <- "4069043834-caTSphFnrlm70KD2FrqWFMew4HQYYJfxbscuy63"
accessSecret <- "9Uof4I3DkQCxVIy46RjKcST0H8XNlSUg21FGvJwuQ35WM"
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessSecret)

#Saving tweets
Nitish.list <- searchTwitter("#Nitish",n=1000)
Nitish.df = twListToDF(Nitish.list)
write.csv(Nitish.df,file="~/Desktop/nitish.csv",row.names = F)

#Load sentiment word lists
pos.words <- scan("/home/chiggy/twitter/positive-words.txt",what="character")
neg.words <- scan("/home/chiggy/twitter/negative-words.txt",what="character")

#Sentiment Function
library(stringr)
library(plyr)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
	require(plyr)
	require(stringr)
	
	#For each element of a list or vector, apply function and store result into an array
	scores = laply(sentences, function(sentence, pos.words, neg.words) {
		
		# clean up sentences with R's regex-driven global substitute, gsub():
		sentence = gsub('[[:punct:]]', '', sentence)                 # remove punctuation
		sentence = gsub('[[:cntrl:]]', '', sentence)                 # remove control characters
		sentance = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentance) # remove retweet entities
		sentance = gsub("@\\w+", "", sentance)                       # remove at people
		sentance = gsub("http\\w+", "", sentance)                    # remove html links
		sentance = gsub("[ \t]{2,}", "", sentance)                   # remove unnecessary spaces
		sentance = gsub("^\\s+|\\s+$", "", sentance)
		sentance = gsub("[[:digit:]]", "", sentance)                 # remove numbers
		sentence = gsub('\\d+', '', sentence)                        
		# and convert to lower case:
		sentence = tolower(sentence)

		# split into words. str_split is in the stringr package
		word.list = str_split(sentence, '\\s+')
		
		# it simplifies list to produce a vector
		words = unlist(word.list)

		# compare our words to the dictionaries of positive & negative terms
		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)
	
		# match() returns the position of the matched term or NA
		# we just want a TRUE/FALSE:
		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)

		# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		score = sum(pos.matches) - sum(neg.matches)

		return(score)
	}, pos.words, neg.words, .progress=.progress )

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}

# Score all tweets
Nitish.score <- score.sentiment(Nitish.df$text,pos.words,neg.words,.progress = 'text')
write.csv(Nitish.score."/home/chiggy/twitter/NitishScore.csv")

#Plot histogram
library(RColorBrewer)
hist(Nitish.score$score,xlab="Tweet's Scores",main="Nitish's Tweet Score",col=brewer.pal(9,"Set2"))

