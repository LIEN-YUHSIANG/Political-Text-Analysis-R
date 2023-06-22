###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 10: Structural Topic Models
# --------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week10")
getwd()

# As ever, we'll use Tidyverse, readtext, and Quanteda:
library(tidyverse)
library(readtext)
library(quanteda)

# We're also using a new library this week, `stm`
if(!require("stm")) {install.packages("stm"); library(stm)}

#-----------------------------------------------------------------------------#
# 1.0    Loading the Corpus: Guardian News Articles

# For today's exercise, we're going to look at a sample of articles from The
# Guardian, a major British newspaper, taken from across the newspaper's 
# various sections in 2021.

news <- readtext("guardian_21.csv", text_field = "Text")

news

# As you can see, along with the text of each article, we also have the section
# it was published in, the headline of the article, and the date of publication.

# Let's break the dates up a bit - it would be useful to have Month and Day 
# fields, not just single Date fields. We'll see why later.

news$Month <- month(news$Date)
news$Day <- day(news$Date)
  
  

# As with all our previous analysis work, we first change any 'smart' apostrophes
# (which Quanteda doesn't know how to handle) into simple apostrophes, then 
# convert the data into a corpus, tokenise it, and build a document-feature matrix. 

news$text <- gsub("’", "'", news$text)

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
  dfm_remove(min_nchar=2)

news_dfm

# This DFM has more than 60,000 features (i.e., unique words) which is definitely
# far too many - processing this would be very challenging. Let's trim it down
# to a more reasonable number. We can probably assume that any major news topic
# will have appeared in at least 20 articles, so we remove any term that doesn't
# appear across 20 documents.

news_dfm_trim <- dfm_trim(news_dfm, min_docfreq = 20)

news_dfm_trim

# This reduces our feature space to 8,537 - a lot more reasonable. We could
# probably reduce it even further if we wanted to, but let's stick with this 
# for now.

# One final thing. Having removed so much vocabulary, it's possible that there
# are now some articles in the corpus (especially short articles) that don't
# have any vocabulary left in the DFM at all - in other words, their vector
# representation is entirely full of zeros. We can't create a topic model for
# those documents (they're literally blank as far as the algorithm knows) so
# we remove them from the DFM.

# Let's check for that:
news_dfm_trim[ ntoken(news_dfm_trim) == 0, ]

# Here we see that 33 documents are now entirely blank. We have to remove those.

news_dfm_trim <- news_dfm_trim[ ntoken(news_dfm_trim) > 0, ]

news_dfm_trim


#-----------------------------------------------------------------------------#
# 2.0    Training a Structural Topic Model

# Let's start by just training an STM without telling it anything extra about
# the data. Remember that STM will try to find correlated topics by default, so
# even though we're not giving it any more information here, it's still
# going to perform differently to standard LDA.

# As we did with LDA, we first have to move our DFM into a format which the stm
# package can read - it can't work directly with a Quanteda DFM. Unlike the 
# command we used for LDA, however, we're also going to need to move our 
# document variables (docvars) over to the new format, so those will also need
# to be specified in the command.

head(docvars(news_dfm_trim))

# Let's keep the Section and Month variables, as well as the Headline.
news_stm_dfm <- convert(news_dfm_trim, 
                        to = "stm", 
                        docvars( news_dfm_trim, c("Section", "Month", "Headline") )
                        )

# Now let's train a "naïve" STM, which doesn't know anything about those
# docvars (referred to here as "meta variables", since they describe the
# documents but are not part of the documents themselves.)

set.seed(123)
news_stm_basic <- stm(news_stm_dfm$documents, 
                      news_stm_dfm$vocab, 
                      K = 15, 
                      max.em.its = 100)

# While this runs (it will take quite a while), let's talk about the various
# parameters we used here. First we passed the $documents and $vocab parts of
# the STM object - this will always be the same.

# As with LDA, we need to provide a number of topics to search for. Here we 
# picked K = 15, which is pretty much chosen at random. You would generally 
# expect to have to run the model quite a few times to find the "correct" value
# for K.

# Finally, I'm setting a parameter called max.em.its here - this is the number
# of iterations (i.e., attempts to train and refine the model) that the STM
# algorithm will perform before stopping and giving you its best result. The
# lower the value, the less time the algorithm will take to run. However, this
# can have the effect of interrupting the process early, and give lower quality
# results. I'm setting it to a low value here so that the process doesn't take
# too long in class - the default is 500, and you should leave it at that
# when training a model for actual research work.


# 2.1   Examining the Topics
#---------------------------

# The model has now finished training, and if we're lucky, it should have 
# finished before reaching its maximum number of iterations, in which case we'll
# get the message "Model Converged" - this simply means that the algorithm
# could not find any mathematical way to improve the model by doing further
# iterations. It does not mean the model is "correct" - any number of problems
# could still exist, starting with the wrong number of topics being chosen in 
# the first place!

# Let's have a look at the topics it generates.

labelTopics( news_stm_basic, n = 8)


# For every topic, this gives us four different lists of words that can help
# us to understand what the topic contains. Arguably the most useful of these 
# is the FREX (FRequency and EXclusivity) list, which shows words that are both
# high-frequency within a topic, and quite exclusive to that topic (i.e., they
# don't appear much in other topics).


# We can also quickly view what the distribution of these topics in the corpus
# is like - from the most common to the least.
plot.STM(news_stm_basic, type = "summary", labeltype = c("frex"), n=5)

# Another plot command will let us see the distribution of these topics across
# the documents. This isn't a very informative graph for this specific set of
# data, but it can hint at which topics (if any) tend to appear across a large
# number of documents, mixed in with other topics, rather than being uniquely
# associated with a smaller number of documents.
plot.STM(news_stm_basic, type = "hist", labeltype = c("frex"))


# 2.2   Naming Topics
#--------------------

# In the prior plots, we allowed the STM package to name each topic according
# to the top FREX keywords. However, we can also assign more descriptive names
# to the topics. The most flexible way to do this is to create a list of
# topic names which we'll use any time we're plotting a graph.

newsBasic_names <- c("Heealthcare",
                     "Gender",
                     "Calture",
                     "Afganistan",
                     "Natrue Disaster",
                     "UK Opposition",
                     "Covid-19",
                     "SNS",
                     "Nature",
                     "Crime",
                     "Us Politics",
                     "Energy",
                     "UK Gov",
                     "Cost of living",
                     "Supply Chains")


# We can recreate any of the plots we've been creating by setting this new list
# as the topic.names parameter. We also need to set custom.labels to a blank
# string, otherwise it will also print some keywords for each label.

plot.STM(news_stm_basic, type = "summary", 
         topic.names = newsBasic_names, custom.labels = "")


# 2.3   Topic Correlations
#-------------------------

# One of the major differences between STM and standard topic models is that
# STM understands that topics may be correlated with each other - i.e., the 
# presence of one topic may make it more (or less) likely that another topic
# will appear in the same document.

# We can view its estimate of which topics are related to each other on a 
# network graph:

news_stm_basic_corr <- topicCorr(news_stm_basic)
plot.topicCorr(news_stm_basic_corr, vlabels = newsBasic_names)


#-----------------------------------------------------------------------------#
# 3.0    Adding Structure to the Model

# Up until now, we've been working with a naive model - i.e., one that is not
# aware of any meta data related to the documents. However, we have two variables
# related to these documents, Section and Month, which should reasonably give us
# some more information about the distribution of the topics.

# For example, we can reasonably expect that topics should be concentrated within
# the same section of the newspaper; and we can also expect that the month has
# some effect, since the focus of news reporting changes over time.

# We tell STM about these by defining a model for topic prevalence - in other
# words, by telling it to use Section and Month to help to predict the distribution
# of topics over the documents.

set.seed(456)
news_stm <- stm(news_stm_dfm$documents, 
                news_stm_dfm$vocab, 
                K = 15, 
                prevalence =~ Section + s(Month),
                data = news_stm_dfm$meta,
                max.em.its = 100)

# This is the same command as before, with two key additions. Firstly, we're 
# telling it to look for the meta data in the $meta section of the data object.

# Secondly, we're telling it to use this model definition to help predict the
# distribution of topics:

#           prevalence =~ Section + s(Month)

# `Section` is self-explanatory, we would expect the section category to be
# relevant to the topics we find. For `Month`, we are enclosing it in an s()
# function, meaning `spline()`, because Month is a numeric value (from 1 to 12)
# but we don't want it to be treated as a quantitative variable! The s() operator
# tells STM that the influence of Month on topic distribution can move around
# from month to month - it won't linearly fall or rise as the number increases.


labelTopics( news_stm, n = 6)
plot.STM(news_stm_basic, type = "summary", labeltype = c("frex"), n=5)

# This isn't terribly different from our original model. Let's go through and
# label them again, to make plotting a bit easier.

newsStruc_names <- c("Health Care",
                     "Medical Research",
                     "Calture",
                     "Afganistan",
                     "Natural Disaster",
                     "UK Opposition",
                     "Covid-19",
                     "SNS",
                     "Nature",
                     "Policing",
                     "US Politics",
                     "Energy",
                     "UK Gov",
                     "Cost of living",
                     "Supply Chain")


# How does the correlation map look now?
news_stm_corr <- topicCorr(news_stm)
plot.topicCorr(news_stm_corr, vlabels = newsStruc_names)

# Again, roughly the same. It's not clear yet whether adding this new metadata
# to the model has improved it much, but at the very least it hasn't broken it!


# 3.1   Observing Documents
#--------------------------

# So far, we've been looking at the topics and trying to interpret them based
# on the keywords they include. However, we can also take a look at the actual
# documents that contain that topic.

# Earlier when we were constructing the data object for STM to work on, we saved
# the headlines for each article as a variable. We can now search for the top
# headlines for each topic:

findThoughts(news_stm, texts = news_stm_dfm$meta$Headline, n = 3, topics = 1:15)


# Does examining the headlines clarify any of the topics we were looking at?
# Often looking at the texts reveals things you couldn't interpret correctly
# just from the keywords.


# (If you were looking at another form of short text, like tweets or individual
#  paragraphs of a longer document, you could save the original text into the
#  STM object in the same way and use the findThoughts() command to view them.)


# 3.2   Estimating Effects
#-------------------------

# Once you have fitted a model like this, the next step in your analysis is to 
# see what the effects of various different variables are. The STM package itself
# provides some quite powerful ways to do this (although for more advanced 
# analysis, you may wish to export the per-document topic proportions and write
# your own statistical functions to study them).

# The core function here is estimateEffect(), which essentially runs a kind of
# regression model with topic proportions as the outcome variable. You pass it
# a model description telling it which variables you want it to consider, and you
# can then plot the effects on a graph.

# For example: let's see what the effects of the Month variable were.

monthEffects <- estimateEffect( 1:15 ~ s(Month), 
                                news_stm,
                                meta = news_stm_dfm$meta,
                                uncertainty = "Global" )

# For example, here's Topic 4
plot(monthEffects, "Month", 
     method = "continuous", topics = 4, 
     model = news_stm,
     printlegend = FALSE)


# And here's Topic 7
plot(monthEffects, "Month", 
     method = "continuous", topics = 7, 
     model = news_stm,
     printlegend = FALSE)


# We could also estimate the effects of the Section category. This is not a very
# useful thing to do for one newspaper, but you can imagine having data from a
# lot of newspapers, and estimating the effect of the Newspaper (not the section)
# on topic distribution - which would tell you something about the newspapers'
# news reporting priorities.

sectionEffects <- estimateEffect( 1:15 ~ Section, 
                                  news_stm,
                                  meta = news_stm_dfm$meta,
                                  uncertainty = "Global" )

# By the way, these are the sections that were included in the data:

unique(news_stm_dfm$meta$Section)


# Let's see, for example, which topics were more likely to appear in News vs.
# those appearing in Business.

plot(sectionEffects, covariate = "Section", topics = 1:15,
     model = news_stm, method = "difference",
     cov.value1 = "News", cov.value2 = "Business",
     xlab = "More Business ... More News",
     main = "Effect of News vs. Business sections",
     xlim = c(-.2, .2), labeltype = "custom",
     custom.labels = newsStruc_names)


#-----------------------------------------------------------------------------#
#  SUMMARY

# We've now seen how to fit and explore a Structural Topic Model with meta
# variables that help to describe the topic distribution across documents.

# There are many, many ways to use this kind of model - the variables you can 
# use are limited only by the available data and your imagination.

# The key commands we saw for exploring the model were:

#      labelTopics()     - explores the words which make up a topic
#      findThoughts()    - explores the documents which contain a topic
#      estimateEffect()  - explores the topic distribution with meta variables

# Note that to use STM in a real project, you would not just randomly pick a 
# number of topics (15 in this case) out of thin air and hope for the best!
# To work with this data in real life, you'd expect to run several versions of
# the model to find the one that best fits your data. We got quite a good result
# with 15 topics here, but better fitting models are undoubtedly possible!





