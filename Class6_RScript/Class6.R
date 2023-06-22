###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 06: Importing and Tokenising Text
# --------------------------------------

# Here are the same setup commands we used last week. Remember to change the
# folder location to wherever you downloaded today's files.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")

getwd()
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week06")


# There are a couple of new libraries we'll be using today - install them
# with these commands.

install.packages('corrplot')
install.packages('RColorBrewer')


# Now let's load our libraries.

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(corrplot)
library(ggrepel)



#-----------------------------------------------------------------------------#
# 0.1   Built-in Corpus Data

# One useful thing about Quanteda is that it has a lot of text data built
# into the package, so you can try out various methods without having to load
# your own data. This will save us some time since we don't have to worry about
# importing data files into R. Of course, for your own projects you'll still
# need to use your own data!

# Here's Quanteda's built-in corpus of inaugural speeches by US presidents:

summary(data_corpus_inaugural)

# You can see that we have all 59 speeches, from Washington to Biden.

# Let's just use the more recent speeches for our analysis. There's a good 
# reason for this - the English language has changed a lot since the 1700s,
# which means analysing differences might give us results that reflect 
# linguistic drift over time, rather than actual political differences.

inaugural_speeches <- corpus_subset(data_corpus_inaugural, Year > 1980)

summary(inaugural_speeches)

# Now we are focusing on 11 speeches, spanning the last 40 years.

# One quick thing to do - can you see a problem?

docvars(inaugural_speeches)

# In this time, we had two presidents called "Bush" - George Bush, and 
# his son, George W. Bush. In the document variables, that means we have
# three entries for "Bush" in the President variable, but one of those
# is a different person!

# Let's construct a "FullName" document variable which will identify
# presidents with the same surname correctly.

inaugural_speeches$FullName <- paste(inaugural_speeches$FirstName,
                                     inaugural_speeches$President)

docvars(inaugural_speeches)


# CONSTRUCTING THE DFM

# Just like last week, we now prepare our data for analysis.
# First we tokenise it; then remove stopwords; then stem (lemmatize) the words.

inaug_tokens <- inaugural_speeches %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en", source = "marimo")) %>%
  tokens_wordstem(language = "en")

head(inaug_tokens)

# Note that this time we're using the tidyverse pipe function (%>%) to
# pass the result of each step into the next step. This allows us to do
# the whole thing in one command, creating a data pipeline between the
# different tasks (tokenising, removing stopwords, stemming).

# Finally we construct a DFM from this data.

inaug_dfm <- dfm(inaug_tokens)

inaug_dfm

# We have 2,365 vocabulary words (features) here - we could trim this
# further using the dfm_trim() command we saw last week, but let's leave
# it alone for now.


#-----------------------------------------------------------------------------#
# 1.0   Word Clouds

# The first thing we want to try today is visualising the contents of our
# corpus using a Word Cloud. These can be a helpful way to show you what kind
# of information is in the corpus and what the differences between documents
# are. 

# Note that the set.seed() command is just ensuring that we get the same
# layout every time. You can put any number you like in here - one approach
# to getting a nice layout is to try lots of random numbers until you find
# a layout you like. You can recreate that layout any time by using the same
# number again.

set.seed(128)
textplot_wordcloud(inaug_dfm, min_count = 20, random_order = FALSE, rotation = 0.1)

# This would be a bit easier to see if we added some colours.

set.seed(128)
textplot_wordcloud(inaug_dfm, min_count = 20, random_order = FALSE, rotation = 0.1,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# Here we see the same word cloud, but with colours assigned - we picked the Dark2
# palette from RColorBrewer (google for "RColorBrewer palettes" to see a full list
# of the ones you could have chosen) and just put colours onto the words depending
# on their prominence. 



# We can also plot Word Clouds that compare some documents. In those cases, the 
# colours will indicate which document (or group) the words are associated with.
# For example, let's make a subset of the DFM grouped by party.

set.seed(128)
inaug_dfm %>%
  dfm_group(groups = Party) %>%
  textplot_wordcloud(min_count = 20, comparison = TRUE,
                     color = RColorBrewer::brewer.pal(3, "Dark2"))


# We could also group by President. Remember to use the FullName variable we
# created earlier, to avoid mixing up the Bushes!

set.seed(128)
inaug_dfm %>%
  dfm_group(groups = FullName) %>%
  textplot_wordcloud(min_count = 10, comparison = TRUE,
                     color = RColorBrewer::brewer.pal(8, "Dark2"))

# That's... not very readable. This function technically works for up to
# eight documents / groups, but realistically it's going to be hard to read
# the cloud it creates for anything more than four documents.

# Let's just look at the post-2000 presidents for example.

set.seed(150)
inaug_dfm %>%
  dfm_subset(Year > 2000) %>%
  dfm_group(groups = FullName) %>%
  textplot_wordcloud(min_count = 20, max_size = 4, comparison = TRUE,
                     color = RColorBrewer::brewer.pal(8, "Dark2"))


#-----------------------------------------------------------------------------#
# 2.0   Cosine Similarity

# The function to calculate the cosine similarity between documents is
# textstat_simil() - let's have a look at how it works.

inaug_simil <- textstat_simil(inaug_dfm, margin = "documents", method = "cosine")

inaug_simil

# As you can see, it calculates the similarity on a pairwise basis, and
# returns a matrix with all of the comparisons between documents.

# We can use this to generate something more readable - a correspondence plot.

corrplot(as.matrix(inaug_simil), method = "number", type = "lower")

# The shades of blue here are quite subtle - let's set the limits of the 
# colour legend to match the actual values we have (which run from about
# 0.4 to 0.7).

corrplot(as.matrix(inaug_simil), method = "number", type = "lower",
         is.corr = FALSE, col.lim = c(0.4, 1.0), col = COL1("Blues", 12))


# We can also create a dotchart which shows us the distance - according to
# cosine similarity - between any given speech and all the other speeches.

# For example, here's the similarity between Trump's inauguration speech
# and all the others.

dotchart(as.list(inaug_simil)$"2017-Trump", xlab = "Cosine similarity (Trump 2017)", pch = 19)

# And here's Biden...

dotchart(as.list(inaug_simil)$"2021-Biden", xlab = "Cosine similarity (Biden 2021)", pch = 19)


#-----------------------------------------------------------------------------#
# 2.1   Similarity & Distance

# Another way to compare documents that's very similar to cosine similarity is
# Euclidean Distance - which also treats each document vector as a point in
# high dimensional space, and then calculates the geometric distance between
# points.

# We can calculate that with the textstat_dist() function, which works very
# similarly to the textstat_simil() function we used for cosine similarity.

inaug_dist <- textstat_dist(inaug_dfm, margin = "documents", method = "euclidean")

inaug_dist

# We could do similar things with this data - dotcharts and so on - but one 
# especially useful thing to do with distance matrices is clustering.

# By clustering documents, we can see groups of similar documents, and get an
# idea about the hierarchy of the documents we're working with.

inaug_cluster <- hclust(as.dist(inaug_dist))
inaug_cluster$labels <- docnames(inaug_dfm)
plot(inaug_cluster, xlab = "", sub = "", main = "Clustered by Euclidean Distance")


#-----------------------------------------------------------------------------#
# 3.0   Document Scaling

# The model for Document Scaling we're going to look at today is Wordfish,
# which is implemented with the textmodel_wordfish() command.

# The dir parameter here gives the index for a pair of documents in the DFM,
# and it doesn't change the results of the analysis - it just tells the
# command that the first document (7) should appear to the left of the second
# document (8) in any analysis. You could pick any two documents. This is 
# useful if you've got some well-known left- and right-wing parties in your
# data set and want them to appear on the left and right respectively.

inaug_wf <- textmodel_wordfish(inaug_dfm, dir = c(7, 8))

# Let's look at our results.
textplot_scale1d(inaug_wf)

# There's arguably a left-right axis here, but George W. Bush appears on the left,
# which is unexpected. This might indicate that he used language in his 
# inaugural speeches that was unusually similar to more left-wing presidents.

textplot_scale1d(inaug_wf, groups = docvars(inaug_dfm, "Party"))

# If we divide up by party, we can really see what an outlier Bush is within the
# Republican presidents according to this analysis.

# Let's try to understand what's happening here by checking to see which words
# Wordfish has chosen to focus on.

wf_features <- data.frame(inaug_wf$features, inaug_wf$beta, inaug_wf$psi)
str(wf_features)

# We've just created a dataframe that contains all the words in the corpus,
# and the scores Wordfish has assigned. 

# Psi is a measurement of the probability that a word appears in a given
# document, so very common words have higher Psi.
# Beta is a measurement of the weighting of each word - i.e., if we see this
# word in a document, what position is that document probably in? Words with
# very high or very low beta are distinctly associated with the ends of the
# Wordfish scale.

textplot_scale1d(inaug_wf, margin = "features")


# We could also check the top 30 features by their Beta score, both negative
# and positive.
head(wf_features[order(wf_features$inaug_wf.beta),], 30)
tail(wf_features[order(wf_features$inaug_wf.beta),], 30) 

# Interpretation here remains tricky. We can see the stems of words like
# "religion", "ownership", "darkest", "tyranny", etc. on the right of the
# graph, but the left of the graph is more confused - some words are clearly
# related to wartime. Some may be related to social welfare though - "pay" and
# "food" appear, for example.

# This might be easier to see in a plot:

ggplot(wf_features, aes(inaug_wf.beta, inaug_wf.psi)) +
  geom_point() +
  theme_light() +
  labs(title  = "Key Features in Inaugural Speeches",
       x = "Estimated Beta",
       y = "Estimated Psi") +
  geom_text_repel(data = filter(wf_features, abs(inaug_wf.beta)>3), 
                  aes(label = inaug_wf.features),
                  max.overlaps = 100)


#-----------------------------------------------------------------------------#
# 3.1   Iteration

# When you run into difficulty with document scaling (or any text analysis 
# method), it's often worth going back to the start and thinking about whether
# you made good decisions regarding the early steps - tokenisation, stemming,
# and so on.

# In this case you could make an argument that perhaps the stemming function
# was quite aggressive, and retry the analysis without stemming the words. 
# (It doesn't actually make much difference, but it's still worth trying!)

# You might also think about whether to trim your DFM in certain ways - for
# example, you might remove uncommon words if you're worried that they're 
# biasing the results more than they should.

# It's always a good idea to write your code in distinct, clear blocks, 
# so you can try one approach, then copy and paste the block, change some
# variables and functions, and try a new approach quickly.




