###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 11: Keyword-Assisted Topic Models
# --------------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week11")


# Now let's load our libraries.

library(tidyverse)
library(readtext)
library(quanteda)


if(!require("keyATM")) {install.packages("keyATM"); library(keyATM)}


#-----------------------------------------------------------------------------#
# 0.0   Re-loading the Guardian data


# Let's work with the same data we used last week - the articles from The Guardian
# in 2021.

news <- readtext("guardian_21.csv", text_field = "Text")

news

# We're going to preprocess this much as we did last week:

news$Month <- month(news$Date)   # Make a docvar for Month
news$MonthText <- month(news$Date, label = TRUE, abbr = TRUE)
news$text <- gsub("’", "'", news$text)      # Remove non-standard quote marks

# Tokenise, remove stopwords, lemmatise, and finally trim the resulting DFM
news_dfm <- news %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         split_hyphens = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_wordstem(language = "en") %>%
  dfm() %>%
  dfm_remove(min_nchar=2) %>%
  dfm_trim(min_docfreq = 20)

news_dfm

# Remember that we also need to remove any documents which end up with no tokens
# in them (i.e., all their included vocabulary has been removed, so their vector
# representation is just a long string of zeros).

news_dfm <- news_dfm[ ntoken(news_dfm) > 0, ]

news_dfm


#-----------------------------------------------------------------------------#
# 1.0   Keyword-Assisted Topic Models (KeyATM)


# Once again, we need to convert our Quanteda DFM into something that can be read
# by the analysis package, in this case KeyATM. In previous examples, we used the
# built-in convert() function of Quanteda, but KeyATM helpfully provides its own
# function to read and convert a Quanteda DFM.

news_docs <- keyATM_read(texts = news_dfm)
summary(news_docs)


# 1.1   Preparing Keywords
#-------------------------

# The major difference between KeyATM and STM is that we can provide KeyATM with
# some "seed words" that it will use to build up its topics. These should be 
# words that are strongly and exclusively associated with a specific topic.

# KeyATM doesn't guarantee that these words will actually appear in our final
# topics - it will use them as a starting point, but it's possible that the
# algorithm won't be able to find a pattern of words around this keyword that
# makes up a coherent topic.

# Let's try creating a list of keywords for some of the topics we expect to 
# find in the Guardian dataset.

keywords <- list(
  COVID_Vaccine  = c("vaccin", "pfizer", "moderna", "dose", "jab"),
  COVID_Variants = c("variant", "omicron"),
  Afghanistan    = c("afghanistan", "taliban", "kabul"),
  Ukraine        = c("ukraine", "russia", "kyiv", "crimea"),
  Healthcare     = c("patient", "nhs", "doctor", "hospit"),
  Brexit_EU      = c("eu", "brexit", "barnier", "frost", "protocol"),
  UK_Government  = c("johnson", "raab", "patel", "sunak"),
  UK_Opposition  = c("starmer", "labour", "union"),
  Energy         = c("gas", "coal", "oil", "tax", "energi"),
  ExtremeWeather = c("flood", "storm", "temperatur"),
  ClimateChange  = c("emiss", "climat", "ipcc"),
  SocialMedia    = c("facebook", "twitter", "googl", "platform", "content"),
  US_Politics    = c("trump", "biden", "senat", "republican", "democrat"),
  Culture        = c("film", "music", "art", "artist")
)

# Note that a lot of these are similar to the ones we found with STM - it's
# common to do an STM or LDA analysis first, and use the keywords it finds 
# as a basis for creating KeyATM keyword groups. 

# In some cases, though, we have split up topics that STM combined. Note that
# we have separate topics for Afghanistan and Ukraine (which were one topic in
# our prior analysis), as well as for Brexit and UK Government. Let's see if
# doing this allows us to generate coherent topics for those issues.

# Before running the model, we should check to see if the keywords we have
# specified are actually present in the documents!

visualize_keywords(docs = news_docs, keywords = keywords)

# A couple of things are worth looking at here. Firstly, we have a warning
# because some keywords don't appear at all - Ukraine, its capital Kyiv, and
# the EU's lead Brexit negotiator, Michel Barnier. It seems unlikely that
# these words wouldn't be in the data somewhere, so why aren't they being
# found?

# We can explore the vocabulary in our DFM to find the answers, by using
# the grep() command to search for substrings in the words. Let's check
# for Ukraine:

featnames(news_dfm) %>%
  grep("^ukr", ., value = TRUE) 

# The ^ symbol indicates the start of a word, meaning we only want to see
# instances where "ukr" are the first three letters - excluding cases 
# where those letters appear inside a word instead.

# This is obvious enough - the lemmatiser has removed the "e" from
# Ukraine. How about Kyiv?

featnames(news_dfm) %>%
  grep("^ky", ., value = TRUE) 

# Nope.. Perhaps the Russian spelling, Kiev, appears?

featnames(news_dfm) %>%
  grep("^ki", ., value = TRUE) 

# Apparently not. This is unusual, but probably means that Kyiv/Kiev were
# not mentioned often enough in 2021 to get into the vocabulary. Bear in
# mind that the Russian invasion didn't start until early 2022.

# How about Michel Barnier?

featnames(news_dfm) %>%
  grep("^barn", ., value = TRUE) 

# There's a good chance his name has actually been shortened to "barn" by
# the lemmatiser. We could use this, but it's probably a bad idea, since 
# "barn" could also refer to things that have nothing to do with EU 
# negotiations. 

# In this case, we'll drop Barnier from our list. If this was a really
# important keyword, however, we could go back to the start and add a _
# to the end of the word Barnier before we did the stemming process:
#     tokens_replace("barnier", "barnier_", case_insensitive = TRUE)
# This would mean the stemmer would skip this word and not cut it down.

# Okay, so we're going to drop Barnier and Kyiv, and change Ukraine to
# "ukrain". What else do we notice?

# "ipcc", "crimea" and "moderna" do appear but they're very rare and may 
# not be good keywords. "IPCC" and "Crimea" might just not be very common 
# and perhaps should be dropped, but they're relevant to these topics so
# let's keep them for now. 

# "Moderna", however, was the wrong keyword to use! The UK didn't use many 
# Moderna shots, preferring their home-developed vaccine, AstraZeneca.

featnames(news_dfm) %>%
  grep("^astra", ., value = TRUE) 

# Sure enough, there it is - let's swap that in for Moderna.

keywords <- list(
  COVID_Vaccine  = c("vaccin", "pfizer", "astrazeneca", "dose", "jab"),
  COVID_Variants = c("variant", "omicron"),
  Afghanistan    = c("afghanistan", "taliban", "kabul"),
  Ukraine        = c("ukrain", "russia", "crimea"),
  Healthcare     = c("patient", "nhs", "doctor", "hospit"),
  Brexit_EU      = c("eu", "brexit", "frost", "protocol"),
  UK_Government  = c("johnson", "raab", "patel", "sunak"),
  UK_Opposition  = c("starmer", "labour", "union"),
  Energy         = c("gas", "coal", "oil", "tax", "energi"),
  ExtremeWeather = c("flood", "storm", "temperatur"),
  ClimateChange  = c("emiss", "climat", "ipcc"),
  SocialMedia    = c("facebook", "twitter", "googl", "platform", "content"),
  US_Politics    = c("trump", "biden", "senat", "republican", "democrat"),
  Culture        = c("film", "music", "art", "artist")
)
visualize_keywords(docs = news_docs, keywords = keywords)


# None of these keywords seem too bad now, although "vaccine" might arguably
# be a bit too common - we'll see how it turns out. 


# 1.2   Training the Model
#-------------------------

# Now that we have our keywords, we can start training the model. We have one
# more thing to choose before we start, though - we need to decide how many
# other topics (apart from the ones we have defined with keywords) we think
# exist in the model.

# We defined 14 topics in our keyword list. Let's assume there are six more
# we haven't defined yet, for a round 20 topics in the dataset. 

news_key <- keyATM(
  docs              = news_docs,    
  no_keyword_topics = 6,              # number of topics without keywords
  keywords          = keywords,
  model             = "base",
  options           = list(seed = 250,
                           iterations = 250)
)

# Note that we're setting "seed" (the random seed - keeping this the same
# ensures that the model will give the same output every time), and also 
# specifying a number of iterations, just like we did for STM. The default
# here is 1500, which could take a very long time to run - but would likely
# create a better model, so you should leave it at that when doing this
# kind of analysis for real!


# Let's examine the words in our resulting categories.

top_words(news_key, measure = "probability")

# Viewing the words by the probability measure, we can see that a lot of words
# are repeated - for example, "said" appears in lots of topics, because lots
# of different news articles will quote spokespeople using this word.

# Some of these topics seem to have worked well and are very coherent, but others
# don't really encapsulate what we were looking for - the Ukraine topic seems to
# mostly actually be about China. Perhaps the build-up to the war in Ukraine just
# isn't as present in this data as we might expect?

top_words(news_key, measure = "lift")

# We can also view top words by Lift, which divides their frequency in each topic 
# by their frequency in other topics, so we get to see words that are very unique
# to each topic. This is a good metric to check to confirm that the topics are
# really talking about what we expected.

# We can also, of course, make a graph of topic probabilities:

plot_topicprop(news_key)

# At this point, you would want to spend some time investigating these topics in
# depth. Do they make sense? Are there keywords in there which suggest that some
# topics have been mixed together incorrectly? Pay particular attention to the 
# "Other" topics - are there topics in there which you can give a label to, or
# which might indicate important topics you have overlooked?

# As with any other topic modelling approach, looking carefully at the results, 
# interpreting them based on your knowledge of the area, and then re-running
# the model with new parameters or keywords is an important process.


# 1.2   Dynamic KeyATM
#---------------------

# In the last example, we trained KeyATM using the "base" model. There are two
# others - "covariate" and "dynamic". "Covariate" behaves much like STM and lets
# you add extra variables to the model to assist with establishing the topic
# distribution across documents. "Dynamic" is interesting and worth looking at
# in more depth - it is a version of KeyATM specifically tuned for looking at
# time series data, so we can see how topic distributions rise and fall over
# time.

# For this, we're going to use the Month variable we stored earlier.

docvars(news_dfm)$Month

# When running the model, we set the model type to "dynamic", and tell it
# where to find the time sequence in the model_settings parameters. Note
# that you can't just pass dates; the time index must be a set of integers
# starting with 1 and counting upwards. In our case, since we're looking at
# a single year, we can just pass the number of the month - but if, for 
# example, you were looking at a two year period, the second January would
# become "13" and the second December would be "24". You are not restricted
# to months; you can use any basis for your time index that you like.

news_dynamic <- keyATM(
  docs              = news_docs,    
  no_keyword_topics = 6,              # number of topics without keywords
  keywords          = keywords,
  model             = "dynamic",
  model_settings    = list(time_index = docvars(news_dfm)$Month,
                           num_states = 12),
  options           = list(seed = 250,
                           iterations = 250)
)

# We can examine the Dynamic model in the same ways as before - for example,
# plotting the topic distributions:

plot_topicprop(news_dynamic)

# Note that this has changed quite a bit! The model having the new information
# about the time series has changed its assumptions about the data, and the
# results have changed accordingly.

# The key new thing we can do with this Dynamic model is to view how the
# distribution of topics changed over time.

plot_timetrend(news_dynamic, time_index_label = docvars(news_dfm)$MonthText)

# These topics are very imperfect and would take some work to be ready for
# use in actual research, but some of these graphs already make sense.

# Perhaps the most notable is the huge spike in stories about Afghanistan 
# in August - this was the timing of the US withdrawal from Kabul and the
# Taliban offensive which retook the capital.

# We can also see stories about COVID variants rising in early summer as
# the Delta variant became prevalent, and then rising sharply again in
# October as Omicron became a problem.
