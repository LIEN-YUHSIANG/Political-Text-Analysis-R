###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 09: Unsupervised Classification
# ------------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week09")


# Now let's load our libraries.

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textmodels)

# We also have a new library to use today for topic modelling.
# These commands will check if it's installed already, and install it for
# you if not:

if(!require("topicmodels")) {install.packages("topicmodels"); library(topicmodels)}
if(!require("wordcloud")) {install.packages("wordcloud"); library(wordcloud)}

#-----------------------------------------------------------------------------#
# 1.0   Dictionary Analysis


# Let's start by loading the corpus of State of the Union speeches by US 
# Presidents which we were using a few weeks ago.

sotu <- readtext("./SOTU/*.txt",
                 docvarsfrom = "filenames",
                 dvsep = "_",
                 docvarnames = c("Year", "President", "Party"))
sotu

# We want to tokenise these texts, as usual, and convert to a DFM.
# This week we're going to do one extra step first, and remove all the
# apostrophe characters from the text. Quanteda, for some reason, has no idea
# how to deal with apostrophes, which hasn't caused a problem for us so far,
# but will start to be an issue as we use more complex analysis.

sotu$text <- gsub("'", " ", sotu$text)
sotu$text <- gsub("’", " ", sotu$text)
# (Why are we doing it twice? Because there are two subtly different symbols
#  for apostrophe, and we can't be sure which one is used in the text...)


sotu_dfm <- sotu %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_wordstem(language = "en") %>%
  dfm() %>%
  dfm_remove(min_nchar=2)


# 1.1   Creating a Dictionary
#----------------------------

# To perform a dictionary analysis in Quanteda, we first need to have a
# `dictionary` object - a special type of R object which is designed for storing
# words in a set of categories. This kind of dictionary is often described as
# a lexicon.

# We can create our own dictionary objects quite easily.

# Here, I'm going to create a dictionary of Populist and Liberal terms, based
# on the definitions used by Rooduijn & Pauwels (2011) and updated by Puschmann
# & Haim (2019).

populism_dict <- dictionary(list(populism = c("elit*", "consensus*", "undemocratic*", 
                                              "referend*", "corrupt*", "propagand", 
                                              "politici*", "*deceit*", "*deceiv*", 
                                              "*betray*", "shame*", "scandal*", 
                                              "truth*", "dishonest*", "establishm*", 
                                              "ruling*", "state", "fake"), 
                                 liberalism = c("liber*", "free*", "indiv*", 
                                                "open*", "law*", "rules", 
                                                "order", "rights", "trade", 
                                                "global", "inter*", "trans*", 
                                                "minori*", "exchange", "market*")))


populism_dict

# We can now apply this dictionary to our data. We do this using the `dfm_lookup()`
# command, which will apply our dictionary to the text and count the number of 
# instances of words in each category which appear.

sotu_populism <- dfm_lookup(sotu_dfm, populism_dict)
sotu_populism

# You can see that the resulting table just features raw counts - the actual
# number of instances of words in each category. We would probably prefer to see
# this as a proportion, since some speeches may be longer than others, so it's
# hard to compare the basic counts. You can convert any DFM to a proportional
# version (where the numbers for each document will add up to 1.0) using the
# `dfm_weight()` command.

sotu_populism_prop <- dfm_weight(sotu_populism, scheme = "prop")
sotu_populism_prop

# To visualise this data, we can convert the DFM into a data frame and use 
# normal ggplot2 commands.

sotu_pop_df <- convert(sotu_populism_prop, "data.frame")

ggplot(data = sotu_pop_df, aes(x = doc_id, y = populism)) +
  geom_col() +
  ylim(0.0, 1.0) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + 
  ylab("Share of populism [%]")

# We can see here that - at least according to this very simple dictionary! -
# there is no major change in the level of populist vocabulary used by presidents
# over the past ten years, with the exception of Biden's 2023 speech being low.
# Trump's 2019 speech is the highest percentage of populist vocabulary, but
# it's not that unusual compared to the other speeches in the corpus.

# We could also analyse the presidents individually, by grouping their
# speeches together.

sotu_byPres_df <- sotu_dfm %>%
  dfm_group(groups = President) %>%
  dfm_lookup(populism_dict) %>%
  dfm_weight(scheme = "prop") %>%
  convert("data.frame")

ggplot(data = sotu_byPres_df, aes(x = doc_id, y = populism)) +
  geom_col() +
  ylim(0.0, 1.0) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + 
  ylab("Share of populism [%]")

# (Don't read too much into this! The dictionary we created was really very
#  simple, and not really a good way to measure a concept as complex as populism.)


# 1.2   Using a Pre-Made Dictionary
#----------------------------------

# There are several pre-made dictionaries available online, which can be used
# to divide up texts into a variety of categories.

# For our first example, we'll use the Lexicoder Policy Agendas dictionary,
# which contains keywords related to 28 different policy areas.

lexiTopic <- dictionary(file = "./dictionaries/policy_agendas_english.lcd") 
lexiTopic

# You may be able to spot a small problem with this dictionary - whereas we have
# split our texts into individual words, some of the dictionary entries are
# multi-word phrases. 

# The easiest solution to this is to apply the dictionary to a Tokens object,
# rather than a DFM - because the Tokens object preserves the order of the
# words, it can look for these multi-word phrases.

sotu_tokens <- sotu %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_wordstem(language = "en")


sotu_lexiTopic <- sotu_tokens %>%
  tokens_lookup(lexiTopic)

sotu_lexiTopic

# Now we have to reconstruct the DFM from these lists of topic words:

sotu_lexiTopic <- sotu_lexiTopic %>%
  dfm() %>%
  dfm_weight(scheme = "prop") %>%
  convert("data.frame")

sotu_lexiTopic  

# We can see that there are a lot of topics here which have very low numbers
# across the board. Let's remove any which are consistently low, so the data
# is easier to understand.

sotu_lexiTopic <- sotu_lexiTopic %>%
  select(where(~ max(.x) > 0.12))

sotu_lexiTopic

# Now we see just six topics which meet the criteria. Let's graph them.

sotu_lexiTopic %>%
  pivot_longer(macroeconomics:defence, names_to = "Topic", values_to = "Proportion") %>%
  ggplot(aes(doc_id, Proportion, group = Topic, fill = Topic)) +
  geom_bar(stat = 'identity') +
  scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + 
  ylab("Topic Proportion [%]")

# This does give us some ideas about different priorities among the three 
# presidents - note for example that Biden talks a huge amount about healthcare,
# or that Trump was very focused on crime-related issues. We also see here that
# Trump 2019 seems to be an outlier - focused heavily on defence and crime,
# but spending only a tiny amount of time on education or labour issues.


# 1.3   Sentiment Analysis
#-------------------------

# By far the most common use for dictionary approaches is actually not to do 
# with finding topics, but with calculating the tone or sentiment of a text.

# Quanteda has a built-in sentiment dictionary, the Lexicoder Sentiment
# Dictionary, which contains thousands of English words organised by the
# sentiment they express.

head(data_dictionary_LSD2015)

# Let's examine the sentiment of the speeches we've been considering. We'll 
# just use the first two sentiments - positive and negative - and ignore the
# more complex additional ones.

sotu_sentiment <- sotu_dfm %>%
  dfm_lookup(dictionary = data_dictionary_LSD2015[1:2]) %>%
  dfm_weight(scheme = "prop") %>%
  convert("data.frame")

sotu_sentiment

# Once again, let's try graphing this...

sotu_sentiment %>%
  pivot_longer(negative:positive, names_to = "Sentiment", values_to = "Proportion") %>%
  ggplot(aes(doc_id, Proportion, group = Sentiment, fill = Sentiment)) +
  geom_bar(stat = 'identity') +
  scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + 
  ylab("Sentiment Proportion [%]")

# Perhaps unsurprisingly, State of the Union speeches tend to be very positive,
# since the President is talking about the accomplishments of his own 
# administration! Is there however a small downward trend over time...?



#-----------------------------------------------------------------------------#
# 2.0   Topic Models

# Let's continue working with the same DFM, but trimming it slightly to 
# remove some of the words that are too rare or too common to be useful:

sotu_dfm_trim <- dfm_trim(sotu_dfm, 
                          min_termfreq = 5, min_docfreq = 3,
                          max_docfreq = 9)

sotu_dfm_trim

# By constraining the DFM to only use words that appear at least five times
# over at least three of the speeches, we end up with an almost 75% reduction
# in the vocabulary we're analysing. For just ten documents, this doesn't make
# much difference, but if you were studying hundreds or thousands of documents,
# this could make a huge difference to how long it takes for the analysis to
# run on your computer.

# Let's run an experimental topic model on the corpus. We don't actually know
# how many topics should exist in this corpus, but we can make a start by looking
# at ten topics and seeing how they turn out.

# We need to convert our DFM into a format that the topicmodels package can
# process - luckily quanteda has a function for this.

sotu_tm <- convert(sotu_dfm_trim, "topicmodels")

# Now let's run the topic model with ten topics.

set.seed(100)
sotu_lda.10 <- LDA(sotu_tm, method = "Gibbs", k = 10)

sotu_lda.10

# Well, it's finished running, but that's not a terribly informative output.
# There are two pieces of data we want to see: which topics have been created,
# and which documents are associated with them.

# For the former, we use the terms() command to see the top vocabulary words
# that are strongly associated with each topic.

as.data.frame(terms(sotu_lda.10, 10))

# Some of these topics possibly make sense - Topic 2 is to do with healthcare,
# Topic 3 is probably to do with border security, Topic 9 is maybe to do with
# higher education? 

# These topics are quite mixed up though - 10 is probably the wrong number of
# topics for this content!


# 2.1   Tuning Topic Models
#--------------------------

# There are statistical ways to "tune" topic models such as to find the 
# most appropriate number of topics, but none of them are very effective
# (and unfortunately, the `ldatuning` R package which was the most popular
#  way to do this now seems to have some major bugs and can crash your
#  computer entirely - I don't recommend using it).

# Your best bet is probably to try various different numbers for n.topics, 
# and see which ones make the most sense to you given your knowledge of the
# subject. This is normal for an unsupervised method: you need to spend a lot
# of effort interpreting the results of the analysis!

set.seed(100)
sotu_lda.8 <- LDA(sotu_tm, method = "Gibbs", k = 8)

set.seed(100)
sotu_lda.12 <- LDA(sotu_tm, method = "Gibbs", k = 12)

as.data.frame(terms(sotu_lda.8, 10))
as.data.frame(terms(sotu_lda.12, 10))

# Do these topics make more sense to you? It depends on your ability to interpret
# them, of course.

# One problem with this data set is probably that it doesn't really have enough
# documents for the topic model to be effectively trained. Larger data sets
# tend to give better results with topic models simply because there is more data
# for the model to learn from. 

# In fact, a major problem here is that we've only got 10 documents, but we're 
# training around that many topics - as a general rule you should have far more 
# documents than the number of topics you want to train!


# 2.3   Topic Distributions
#--------------------------

# To see how the topics we've found are distributed within the documents, we
# first output the posterior distribution of the topic model - i.e., the 
# model's prediction for the topics that make up each document.

tm_result <- posterior(sotu_lda.8)

tm_result$topics

# These numbers are proportional - so for example, the model believes that 
# 0.036, or 3.6%, of Barack Obama's 2014 speech was from Topic 1.

# Generally, we would use a cut-off to remove low-scoring topics from each
# document - we're only really interested in seeing the topics that make up a
# large chunk of the document, over 5% or even 10%.

# These topic names aren't very descriptive - we can make some rough ones
# automatically by sticking together the top vocabulary terms from each topic.

top5terms <- terms(sotu_lda.8, 5)
topicNames <- apply(top5terms, 2, paste, collapse=" ")

topicNames

# We can visualise the topic distributions with a stacked bar chart:

theta <- data.frame(tm_result$topics)
colnames(theta) <- topicNames

theta %>%
  mutate(Doc_ID = row.names(.)) %>%
  pivot_longer(topicNames[1]:topicNames[8], names_to = "Topic", values_to = "Proportion") %>%
  ggplot(aes(Doc_ID, Proportion, group = Topic, fill = Topic)) +
  geom_bar(stat = 'identity') +
  scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + 
  ylab("Topic Proportion [%]")


# This visualisation starts to give us some idea of which politicians and which
# speeches specific topics are related to - again, this could be very useful
# for interpreting the results we're seeing.


# 2.3   Visualising Topics
#-------------------------

# Finally for today, let's look at a way to visualise the data we get
# from topic models by making a word cloud from the keywords for each topic. 
# This can help to understand what the topic is actually about - many people 
# find this easier to interpret than a big list of words.

target_topic <- 8   # Let's look at topic number eight.

topterms <- sort(tm_result$terms[target_topic,], decreasing=TRUE)[1:20]
words <- names(topterms)
wordcloud(words, topterms, random.order = FALSE, color = brewer.pal(8, "Dark2"))

# You can change the target_topic variable to any one you like and re-run the
# above code to see the words that define that topic.
