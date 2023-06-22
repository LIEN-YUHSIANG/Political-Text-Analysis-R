###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 05: Importing and Tokenising Text
# --------------------------------------

# First of all, here are some commands we'll use when starting to work in R
# every week. These first two commands just set the language (locale) that
# we're using - I'm just doing this to ensure that error messages and warnings
# are all displayed in English, but you can leave this in your native language
# if you prefer.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")

# Next, we check the Working Directory ("wd") which is the folder that R will
# read and write any data from. You'll need to set this to whichever folder
# you downloaded the week's class files to - note that if you're using Windows,
# this will start with something like "C:\" instead of "~". 

getwd()
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week05")


# If you didn't install the various packages we're using in this class already,
# you can remove the comment marks (#) from the lines below and run them to get
# everything installed. Be warned that this might take a while!

# install.packages('tidyverse')
# install.packages('quanteda')
# install.packages('quanteda.textplots')
# install.packages('quanteda.textstats')
# install.packages('quanteda.textmodels')
# install.packages('readtext')

library(tidyverse)
library(readtext)
library(quanteda)


#-----------------------------------------------------------------------------#
# 1.1   Reading text from a CSV file

# There are two likely scenarios you'll encounter when trying to read text into
# R, the first of which is having a CSV file (or perhaps an Excel file) that
# contains your text, with one "document" (unit of text) on each row of the file.

# This is very common when looking at short texts like social media posts, so
# let's start by taking a look at a file containing some tweets.

tweets <- read_csv('vaccine_tweets.csv')
glimpse(tweets)

# Okay, we can see that we've got four columns (variables), and 4791 tweets.
# Note that the actual text is stored in a column called `full_text`.

# Right now, this data is stored in a data frame, which is R's standard
# object for storing tabular data. Data frames are great for numeric data
# such as statistics, but we're interested in the text data, and for that,
# the quanteda package provides a much more flexible and useful type of
# object - the readtext object.

# To load our text instead into a readtext, we use a different command.
# Instead of `read_csv`, let's use `readtext`. Note that when you're
# loading in something like a CSV file, you need to tell readtext which
# column it should look for the actual text data in.

tweets <- readtext('vaccine_tweets.csv', text_field = 'full_text')
tweets

# This is exactly the same data, but it's presented in a somewhat different
# way. Every tweet (i.e., every row of the CSV) is now considered a 'document',
# with its own document ID and text field. The other columns from the CSV file
# are being referred to as "docvars" - document variables, i.e., additional
# variables that are related to each document, separately from the text of the
# document.


#-----------------------------------------------------------------------------#
# 1.2   Reading text from a folder

# The other situation you might commonly encounter is having a folder full of
# text files that you want to read into R and treat as a corpus for analysis.
# This is common when you're dealing with longer texts - speeches, manifestos,
# newspaper articles, and so on. Each document is stored in its own text file,
# and the filenames contain some information about their contents.

# In the files for today's class, you've got a folder containing the full
# text of the last ten State of the Union addresses by US presidents.

list.files(path = "./SOTU")

# (Note that the . in a folder path is shorthand for "the current folder" - i.e.,
#  the folder you defined in the setwd() command at the start of this file.)

# We can read all of those files into a readtext object using the `readtext` 
# command. In the following command, * is what is called a 'wildcard' - it means, 
# match any file in the SOTU/ folder which ends in .txt, no matter what the first
# part of the filename is.

sotu <- readtext("./SOTU/*.txt")
sotu

# We now have all ten speeches in a corpus. However, we can improve upon this.
# Note that the filenames contain some useful information - the year of the
# speech, the name of the president making the speech, and the party that
# president belongs to (R or D). It would be helpful to store those things as
# document variables, so we can use them in our analysis later!

# There's a little more work we have to do to accomplish this than with a
# CSV file, because we have to explain to readtext how to extract the
# variables from the filenames - telling it what variables exist and how
# to separate them from one another.

sotu <- readtext("./SOTU/*.txt",
                 docvarsfrom = "filename",
                 dvsep = "_",
                 docvarnames = c("year", "president", "party"))
sotu

# Here we're telling readtext to get its docvars from the filenames, to
# extract them by treating _ as the separator between values, and that the
# variables in those filenames are the year, the president, and the party.

# As you can see, that gives us a nice neat dataset with three docvars which
# we can use for analysis later.


#-----------------------------------------------------------------------------#
# 2.1   Working with Corpora: Selecting Documents

# Quanteda works internally with a representation of the text data called a
# corpus. You can construct a corpus from a readtext object easily:

sotu_corpus <- corpus(sotu)
summary(sotu_corpus)

# You can see that the corpus object retains all the document variables we
# had previously, but also now includes new information - the number of 
# sentences, tokens, and types (i.e., unique tokens) in the document.

# This means that constructing a corpus has carried out a kind of simple
# tokenisation process in the background - actually, it's just divided up the
# text according to the spaces between words, which isn't ideal for doing
# analysis work, but does give you a good sense of how large each document
# is and how varied the vocabulary it contains is.


# We can use the docvars to select and extract subsets of the documents.

docvars(sotu_corpus)

# (Note: if you get an error here, it's probably because you loaded the
#  readtext package after loading quanteda. This is a major failing of the R
#  language, but the order in which you load packages / libraries is really
#  important - both readtext and quanteda have a command called `docvars`,
#  and they do almost the same thing but are incompatible with each other.
#  To ensure you don't get an error, you should load readtext first, and
#  then load quanteda afterwards, so quanteda's version of the `docvars`
#  command takes precedence.)

# Using the `party` variables, we can create a subset of the corpus with
# only the speeches by Democrat presidents...

corpus_subset(sotu_corpus, party == "D") %>%
  summary()


# ... Or only speeches prior to 2018...

corpus_subset(sotu_corpus, year < 2018) %>%
  summary()

# ... Or combine rules, to see only speeches by Republicans since 2018:

corpus_subset(sotu_corpus, year > 2018 & party == "R") %>%
  summary()


#-----------------------------------------------------------------------------#
# 2.2   Working with Corpora: Unit of Observation

# You will recall that it's very important to get the unit of observation right
# when doing text analysis - deciding whether to classify text at the level of
# an entire speech, a paragraph, or a sentence, can change the results of your
# analysis significantly.

# Quanteda's corpora provide some basic tools for changing the unit of observation,
# most notably the ability to reshape a corpus so that long documents are divided
# up into individual sentences or paragraphs.

# Sentences:
sotu_sentences <- corpus_reshape(sotu_corpus, to = "sentences")
ndoc(sotu_sentences)
head(summary(sotu_sentences))

# Paragraphs:
sotu_paragraphs <- corpus_reshape(sotu_corpus, to = "paragraphs")
ndoc(sotu_paragraphs)
head(summary(sotu_paragraphs))

# Note that Quanteda isn't really doing anything clever when it divides up the
# documents in this way - it's just using punctuation marks to divide into
# sentences, and paragraph breaks to divide into paragraphs.

# This means that if your text document is formatted oddly - for example, if 
# every sentence is on a line on its own, instead of being arranged into
# paragraphs - these functions may perform in ways you don't expect.

# Sometimes, there's no avoiding the boring work of having to go through all
# your text files and arrange the texts into properly formatted paragraphs...

# What is useful, though, is that when you use these commands, the document
# variables are preserved - so even after dividing the corpus up into thousands
# of sentences, we can still see which sentences were spoken by which president,
# in which year, etc.


#-----------------------------------------------------------------------------#
# 3.1   Tokenisation and the Document-Feature Matrix

# Quanteda has some vague idea of how many tokens are in each document of the
# corpus: you can see that in the summary:

summary(sotu_corpus)

# However, before we do any analysis we should properly tokenise this data.
# This process includes removing things like punctuation marks, converting
# everything to lowercase (so "Country" and "country" will be treated as the
# same word), and so on.

# We do this with quanteda's `tokens` function, which has a lot of options for
# how the text should be treated.

sotu_tokens <- tokens(sotu_corpus,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_numbers = TRUE,
                      remove_separators = TRUE)
head(sotu_tokens)

# You can see how the speeches have been divided up into individual words, and
# things like punctuation have been removed.

# (remove_symbols gets rid of more complex symbols, including emoji, while
#  remove_numbers does exactly what it sounds like. remove_separators will get
#  rid of the hidden symbols in text that create line endings and paragraph
#  separators - we usually don't want those to stay in the text.)


# The next step is to turn this big list of tokens into a document-feature
# matrix (DFM).

sotu_dfm <- dfm(sotu_tokens)
sotu_dfm

# Okay, here we see each document represented as a list of word frequencies.
# You can see how many times each word was used in each speech.

# Note that by default, the dfm() command has switched everything to lower
# case - that's generally how we want text to be handled.

# We can see what the most common features (words) were:

topfeatures(sotu_dfm, n = 20)


#-----------------------------------------------------------------------------#
# 3.2   Cleaning up the Tokens

# Currently, this DFM has 6,343 features - i.e., there were 6,343 unique words
# used during the ten speeches we're examining.

# That's a lot of words, and many of them probably aren't very meaningful in 
# terms of analysis. You can already see that from the topfeatures() command
# we just used - most of the top 20 words are just ordinary parts of speech
# that don't tell us anything about the speaker's political positions, tone,
# attitudes, etc. Thus, we can simplify this dataset quite a lot without losing
# any important information.

# First, let's remove the stopwords - terms that are used really commonly in 
# all sentences, and aren't useful for extracting meaning or statistical
# differences between texts.

# Quanteda has a basic built-in list:

stopwords(language = "en")

# There's also a more comprehensive one from a stopwords list called Marimo,
# which also has lists for Japanese and other languages. I'd generally
# suggest using this one.

stopwords(language = "en", source = "marimo")

# (Here's the Japanese list if you're interested.)
stopwords(language = "ja", source = "marimo")


# To remove these stopwords, we use the `tokens_remove` command. Of course,
# you could also pass your own custom list of words to tokens_remove, if there
# are other specific terms you'd like to exclude.

sotu_clean <- tokens_remove(sotu_tokens, 
                            stopwords(language = "en", source = "marimo"))
head(sotu_clean)

# Note that we're doing this to the list of tokens, not to the DFM! We'll 
# reconstruct the DFM once we've finished tidying up our token lists.


# We can also stem the words - removing grammatical endings from verbs, 
# and so on. This is the simplest form of lemmatization. More complex
# versions of lemmatization are also possible, but when working with
# English text, Quanteda's built-in stemming function is usually fine.

sotu_clean <- tokens_wordstem(sotu_clean, language = "en")
head(sotu_clean)


# Note that some significant changes have been made by the stemmer - for
# example, "President" has become "Presid", and "Lady" has become "Ladi"
# (which would also be the shortened version of "Ladies").

# Let's reconstruct a DFM using this newly tidied data set.

sotu_clean_dfm <- dfm(sotu_clean)
sotu_clean_dfm

# This now has 4,314 features - we've removed more than 2000 words from
# the vocabulary.


topfeatures(sotu_clean_dfm, 20)

# Now the most common features are more meaningful, so our cleaning work
# seems to have done a good job.


#-----------------------------------------------------------------------------#
# 3.3   Trimming the DFM

# Some of those words may only have occurred a handful of times, which makes
# them largely irrelevant to a statistical comparison of the documents.

# Other words may appear very often in every document, which makes them equally
# irrelevant - they're basically just background noise if they're used just as
# often in every speech.

# `dfm_trim` lets us remove features based on the minimum and maximum frequency
# with which they appear in documents.

# There are two concepts here:
#    termfreq: How often does a feature appear in the corpus overall?
#     docfreq: How many of the corpus' documents does the feature appear in?

# Let's trim our DFM so that we only include words that appear at least ten
# times across all of the speeches, and only words that appear in more than one
# speech.

sotu_trim_dfm <- dfm_trim(sotu_clean_dfm,
                          min_termfreq = 10,
                          min_docfreq = 2)
sotu_trim_dfm

# This has a dramatic effect! We're down to 778 unique vocabulary words. Those
# are arguably the words that were actually important and relevant in these 
# speeches, since they determine the actual themes and topics of the speech,
# and the differences between each president's content and speaking style.



#-----------------------------------------------------------------------------#
# 4.1   Some basic statistics


# Now that we've got a DFM for those ten speeches, let's look at some really
# basic statistics about them. We'll come back and do some much more complex
# work on them next week!


# Firstly, note that we can use the docvars to view the top features according
# to each group - which shows us some differences between presidents, years,
# and parties.

topfeatures(sotu_trim_dfm, groups = president)

# It's not super insightful yet, but you can see a couple of interesting things
# here - "great" appears in Donald Trump's list, likely because of his campaign
# slogan "Make America Great Again", while "world" is in Biden's list, which
# may reflect a higher focus on foreign policy.


# There are some other interesting tools in the quanteda.textstats package.
# We visualise them with tools from the quanteda.textplots package.

library(quanteda.textstats)
library(quanteda.textplots)

# LEXICAL DIVERSITY

# Lexical Diversity is a measurement of how broad the vocabulary used within a
# document is. It's commonly measured using Type-Token Ratio (TTR), which is
# the ratio of unique words to the number of actual words - so basically, it's
# a measurement of word repetition.

# It's argued that more populist leaders tend to have low lexical diversity, 
# choosing to use more simple language to have broad appeal with voters, while
# potentially sacrificing precision and nuance.

textstat_lexdiv(sotu_trim_dfm)

# We can make a quick plot of that statistic:

ggplot(data = textstat_lexdiv(sotu_trim_dfm), 
       aes(x = document, y = TTR, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks=c("2014_BarackObama_D.txt","2017_DonaldTrump_R.txt","2021_JosephBiden_D.txt")) +
  theme_linedraw()

# This may not be what you expected - the speeches with the highest lexical
# diversity are Donald Trump's. Lexical Diversity here is being defined as
# the TTR, which means "Number of Unique Tokens" divided by "Total Number of
# Tokens" - can you think of any potential flaws with this measurement?


# KEYNESS

# Keyness is a measurement of how strongly associated a certain term is with
# a specific document (or group of documents) as compared to all the other
# documents in the corpus.

# In other words, it tests how uniquely associated the term is with that 
# document - does that speaker or author use this term significantly more
# than other speakers or authors?

# In this way, we could check to see if each of the presidents in our
# data set have some unique words associated with them.

# First, we use `dfm_group` to make a new DFM that groups together
# the speeches by each president.

sotu_presid <- dfm_group(sotu_trim_dfm, groups = president)
sotu_presid


# Now, let's look at the keywords associated wth Barack Obama.

obama_keyness <- textstat_keyness(sotu_presid, target = "BarackObama")
head(obama_keyness)

# Note that this is using a Chi-2 test, which you may be familiar with
# from other statistics or quantitative analysis classes. Basically, it's
# checking to see if a certain term appears more frequently than we
# would expect it to appear, given a normal distribution in the data.

# Because of this, each term also has a p-value - which means some of
# them are not statistically significant and can be removed.

obama_keyness <- obama_keyness[ which(obama_keyness$p<=0.05), ]

# We can visualise this with a graph:
textplot_keyness(obama_keyness)

# How about Donald Trump's unique or defining words?
trump_keyness <- textstat_keyness(sotu_presid, target = "DonaldTrump")
trump_keyness <- trump_keyness[ which(trump_keyness$p<=0.05), ]
textplot_keyness(trump_keyness)


# Can you generate the same graph for Joe Biden's speeches?
biden_keyness <- textstat_keyness(sotu_presid, target = "JosephBiden")
biden_keyness <- biden_keyness[ which(biden_keyness$p<=0.05), ]
textplot_keyness(biden_keyness)


