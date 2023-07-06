###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 12: Supervised Classification
# ----------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week12")

library(tidyverse)
library(readtext)
library(quanteda)

# We have quite a lot of new software to install this week...

if(!require("caret")) {install.packages("caret"); library(caret)}
if(!require("naivebayes")) {install.packages("naivebayes"); library(naivebayes)}
if(!require("e1071")) {install.packages("e1071"); library(e1071)}
if(!require("randomForest")) {install.packages("randomForest"); library(randomForest)}


#-----------------------------------------------------------------------------#
# 0.0   Our Data for This Week: Tweets on Trump and Biden

# In two separate CSV files, we have a selection of tweets sent about Biden and
# Trump ahead of the 2020 election. These tweets have all been given a category
# label - FAVOR, AGAINST, or NONE - indicating whether they are positive or
# negative about the candidate they are referring to.

biden_tweets <- readtext("biden_tweets.csv", text_field = "text")
trump_tweets <- readtext("trump_tweets.csv", text_field = "text")

# Let's focus on Biden for now. 
head(biden_tweets)

# We can check to see what the proportion of different labels in the corpus is.
table(biden_tweets$label)

# These labels are fairly evenly distributed. That's good - if one label 
# totally dominates the corpus, it can be very hard to train good classifiers,
# because they are biased towards predicting the dominant label very often,
# resulting in lots of false positives for that label (and thus false negatives
# for all the others). This is an especially big problem if the dominant label
# is "Other" or "None", because it means you'd be consistently missing labels
# for your actual categories of interest.

# Let's do our standard pre-processing steps:

biden_dfm <- biden_tweets %>%
  mutate(text = gsub("’", "'", .$text)) %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         split_hyphens = TRUE,
         remove_separators = TRUE,
         remove_url = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_remove(c("0*", "@*")) %>%
  tokens_wordstem(language = "en") %>%
  dfm() %>%
  dfm_remove(min_nchar=2) %>%
  dfm_trim(min_docfreq = 5)

# We're doing two things differently from before here. Take note of these if
# you are ever doing text analysis on social media data!

# Firstly, we're removing all some extra types of tokens, to handle the fact
# that this is data from Twitter.

# Tokens that look like "0*" (i.e., anything starting with a 0) are non-standard 
# Unicode symbols (remember our discussion about Unicode, the world's standard 
# for how symbols in every language can be represented on computers?). This will
# include many symbols Quanteda / R don't know how to handle, so it's best to
# get rid of them. (In the past this included Emoji, but Quanteda now knows how
# to deal with those - which is good, as emoji can contain a lot of context
# information about a tweet!)

# We're also removing all usernames by getting rid of any tokens that look like
# "@*", and in the tokens() command, we've added `remove_url = TRUE` to get rid
# of any links that people have put in their tweets.

# Secondly, we're trimming to a `min_docfreq` of 5, when we used 20 last week.
# Why is this? It's because tweets are very short texts - so they include very
# few words. You can safely assume that almost every word about a topic will
# appear in a speech or newspaper article about that topic (so the document
# frequency should be high), but a tweet will only contain a small subset of
# those words, so the document frequency may be low, even for a very relevant
# word.


# Remember that we need to check for any documents which end up with no features
# (words) at all in the dataset:
biden_dfm[ ntoken(biden_dfm) == 0, ]

# In this case there were none - all tweets have at least one feature.


# Finally, we'll turn our Dependent Variable - the label for each category -
# into a factor, so R understands that it's a category rather than just a string
# of text.

biden_dfm$label <- as.factor(biden_dfm@docvars$label)


#-----------------------------------------------------------------------------#
# 1.1   Preparing for Model Training: Train-Test Splitting


# To train and test a model, we need to divide our data into Training and
# Test sets. It makes sense for ~20% of the data to be "held back" from the 
# training process, so we can test how well the model performs using this
# unseen data.

# To make this easier, let's give every document in the corpus an index number.

biden_dfm$numeric_id <- 1:nrow(biden_dfm)

# We'll use the `createDataPartition` command from the Caret package to divide 
# our data set semi-randomly.

# Why "semi-randomly"? Because although we want to select randomly from the
# data, we also want to preserve the proportion of each label - so if 33% of the
# overall data has the label "FAVOR", 33% of the randomly selected data for
# training should also have that label. This is the advantage of using the 
# Caret package - it's a smarter way to divide the data than pure random 
# selection would be.
set.seed(1234)
trainIndex <- createDataPartition(biden_dfm$label, 
                                  p = .8, 
                                  list = TRUE, 
                                  times = 1)
head(trainIndex)

# We end up with a list of all the ID numbers that should be included in our
# training set (so all the numbers not on this list should be the test set).
# The `p` value of .8 we passed to the command tells it to put 80% of the data
# into the training set.

# We can now create two data sets for training and testing, using the list of
# index numbers we got above. Note that the ! in the second of these commands
# is a logical NOT - i.e., it's selecting documents where numeric_id is NOT in
# trainIndex$Resample1.

biden_dfm.train <- dfm_subset(biden_dfm, biden_dfm$numeric_id %in% trainIndex$Resample1 )
biden_dfm.test <- dfm_subset(biden_dfm, !biden_dfm$numeric_id %in% trainIndex$Resample1 )

# Here we go - two DFMs, randomly divided up into an 80:20 ratio, but preserving
# the basic ratio of the category labels within each sample.

biden_dfm.train
biden_dfm.test


#-----------------------------------------------------------------------------#
# 1.0   Preparing for Model Training: Matrix Conversion

# Although Quanteda can do some simple classification by itself, to use more
# powerful methods we'll need to use other packages - and these generally prefer
# R's standard Matrix objects, not Quanteda's DFMs.

biden.train <- as.matrix(biden_dfm.train)
biden.test <- as.matrix(biden_dfm.test)

# Note that these are extremely large objects - because they have to store 
# thousands of zeroes for every tweet, one for every word that is not in the 
# tweet, as well as the values for the words that actually are in the tweet.

# There is no consistent work-around for this problem; if you're dealing with
# a very large data set, it can cause problems if your computer's memory is
# not sufficiently large. It should be fine for most smaller data sets though.


#-----------------------------------------------------------------------------#
# 2.0   Our First Classifier Model: Naive Bayes

# Every classifier works in essentially the same way - predicting the dependent
# variable Y based on the contents of a vector of features X.

# In our case, the dependent variable is the label:
str(biden_dfm.train$label)

# This is a factor, which is as it should be. If this is "chr", a character
# vector, it will cause problems with model training.

# Okay! Let's train a model using our training data and labels.

set.seed(1234)
biden.nb <- multinomial_naive_bayes(x = biden.train,
                                    y = biden_dfm.train$label)


# That probably took just a moment... let's see how the results look.
summary(biden.nb)

# We can now use the predict() function on that model to see what labels it
# would give to some data...

biden_nb.predict <- predict(biden.nb, newdata = biden.test)

# ... Then we compare them to the actual values by creating what is called a 
# Confusion Matrix.

confusionMatrix(biden_nb.predict,
                biden_dfm.test$label)

# This is giving us a lot of data. Let's look at it closely.
# First we get the actual Confusion Matrix. Here we see the predicted values
# in the rows: it's telling us that the model predicted AGAINST 58 times when
# the value really was AGAINST, 5 times when the value was FAVOR, and 19 times
# when the value was NONE.

# Below that we get some statistics. Accuracy is the percentage of times the 
# model got things right - 65.06% of the time.
# The No Information Rate is how well you'd expect a model guessing totally
# randomly to get things right, and the P-Value being very low here tells us
# that our model is statistically doing a better job than random guessing.

# Kappa is Cohen's Kappa, which we discussed a few months ago when we were 
# talking about inter-coder reliability (ICR) testing. It's useful when you
# have unbalanced classes - i.e., if you have some categories with only a small
# number of cases - because it will fall sharply if the small categories are
# being mislabelled a lot, whereas the overall accuracy score might stay high.
# 0.47 is an okay-ish score here, but Kappa is less about finding a "good" score
# than keeping an eye on it when comparing models - if a new model you try out 
# looks good but its Kappa score is sharply lower, you may have a problem.

# McNemar's Test P-Value is a test to see if our model is generating false
# positives and false negatives pretty much evenly - i.e., whether the model's
# errors are random, or if they're biased in some way. The P-Value here is 
# far over 0.05, so our model is biased in its errors - it's likely to have
# a systematic bias for labelling in a certain direction.

# Finally, we see Statistics by Class. This can help us to figure out how
# the model is biased.

# For each model, we see Sensitivity - this is the proportion of actual cases
# of that class that were labelled correctly, i.e., out of all the Tweets
# labelled AGAINST, how many were correctly labelled AGAINST by the model?
# This is the same as Recall, which we discussed last week.

# We also see Specificity - out of all the tweets that were NOT labelled as
# AGAINST, how many were labelled as something other than AGAINST? 

# Positive Predictive Value is the same as Precision, which we discussed last
# week; out of all the tweets the algorithm labelled as AGAINST, how many of
# them were actually AGAINST?

# You don't need to go through all these stats carefully - but keep an eye on
# the main statistics, and the Balanced Accuracy at the bottom. If one category
# has an especially low Balanced Accuracy, it means your model is doing a really
# bad job of identifying that category.


#-----------------------------------------------------------------------------#
# 2.1   Another Model: Support Vector Machine

# Let's try training on another model to see if we can improve on the Naive
# Bayes performance.

# The `e1071` package contains functions that let us train a type of classifier
# called a Support Vector Machine, or SVM. This is an algorithm that works by
# manipulating the number of dimensions in the data in order to find the best
# possible 'hyperplane' to separate different classes of data.

# Internally, it works very differently to Naive Bayes - but the actual command
# we use to train the model, and the system for evaluating its performance,
# remains the same as before.

set.seed(1234)
biden.svm <- svm(x = biden.train,
                 y = biden_dfm.train$label)

# We can check its performance on the test set as seen below...

biden_svm.predict <- predict(biden.svm, biden.test)

confusionMatrix(biden_svm.predict,
                biden_dfm.test$label)

# Is this an improvement over our Naive Bayes model?


#-----------------------------------------------------------------------------#
# 2.2   And Another: Random Forest

# Finally, let's look at a more complex model - this one will take a while to 
# train. Random Forest builds a large number of smaller predictive models, 
# called Decision Trees, and those models "vote" on the correct category for
# each document. 

# Again, despite being a mathematically very different model, the code to 
# train / fit, predict, and test the results of this model is almost identical
# to the models above.

set.seed(1234)
biden.rf <- randomForest(x = biden.train,
                         y = biden_dfm.train$label)

biden_rf.predict <- predict(biden.rf, biden.test)

confusionMatrix(biden_rf.predict,
                biden_dfm.test$label)

# This model seems to be a significant improvement!



#-----------------------------------------------------------------------------#
# NEXT WEEK: Hyper-Parameters and Cross-Validation

# For each of the models we just trained, for the sake of simplicity we allowed
# the model to run with its default settings. However, every model has its own
# settings, which are called HYPER-PARAMETERS.

# (The data you use to train the model is full of "parameters" - the values for 
#  all the various features. "Hyper-parameters" are called that because they
#  are higher-level parameters which apply to the entire model and change how it
#  works, as distinct from ordinary parameters which are just used to train the
#  model and set its weights etc.)

# Every model's hyper-parameters are unique (although some of them overlap), so
# how can we know what the best values to use are for these settings? We can't,
# which is why systems exist to find the best values through trial and error - 
# the so-called "hyper-parameter search" or "grid search" approach, which trains
# many models and finds the most effective parameters. We'll see how to do this
# next week.


#-----------------------------------------------------------------------------#
# IN-CLASS / AT-HOME EXERCISE

# Can you train a model that performs similarly well for the Trump-related
# tweets? 

# These are stored in the `trump_tweets` object - we loaded them at the start.

# Repeat the relevant steps from the above code, and see what the best-performing
# model for Trump's tweets is. Write your code below. If you can't finish this
# during the class, please do so at home - there will be an assignment on 
# Moodle where you can submit this file, regardless of whether you finished it
# in class or at home.

# Prepare Trump dfm
trump_dfm <- trump_tweets %>%
  mutate(text = gsub("’", "'", .$text)) %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         split_hyphens = TRUE,
         remove_separators = TRUE,
         remove_url = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_remove(c("0*", "@*")) %>%
  tokens_wordstem(language = "en") %>%
  dfm() %>%
  dfm_remove(min_nchar=2) %>%
  dfm_trim(min_docfreq = 5)

trump_dfm[ ntoken(trump_dfm) == 0, ]
trump_dfm <- trump_dfm[ ntoken(trump_dfm) > 0, ]
trump_dfm[ ntoken(trump_dfm) == 0, ]

trump_dfm$label <- as.factor(trump_dfm@docvars$label)

# Train-Test Splitting
trump_dfm$numeric_id <- 1:nrow(trump_dfm)

set.seed(1234)
trainIndex <- createDataPartition(trump_dfm$label, 
                                  p = .8, 
                                  list = TRUE, 
                                  times = 1)
head(trainIndex)

trump_dfm.train <- dfm_subset(trump_dfm, trump_dfm$numeric_id %in% trainIndex$Resample1 )
trump_dfm.test <- dfm_subset(trump_dfm, !trump_dfm$numeric_id %in% trainIndex$Resample1 )

trump_dfm.train
trump_dfm.test

# Matrix Conversion
trump.train <- as.matrix(trump_dfm.train)
trump.test <- as.matrix(trump_dfm.test)

# Naive Bayes
str(biden_dfm.train$label)

set.seed(1234)
trump.nb <- multinomial_naive_bayes(x = trump.train,
                                    y = trump_dfm.train$label)

summary(trump.nb)

trump_nb.predict <- predict(trump.nb, newdata = trump.test)

confusionMatrix(trump_nb.predict,
                trump_dfm.test$label)

# Support Vector Machine
set.seed(1234)
trump.svm <- svm(x = trump.train,
                 y = trump_dfm.train$label)

trump_svm.predict <- predict(trump.svm, trump.test)

confusionMatrix(trump_svm.predict,
                trump_dfm.test$label)

# Random Forest
set.seed(1234)
trump.rf <- randomForest(x = trump.train,
                         y = trump_dfm.train$label)

trump_rf.predict <- predict(trump.rf, trump.test)

confusionMatrix(trump_rf.predict,
                trump_dfm.test$label)

# The best performing model among the three we use in this file is the 
# "Random Forest" model, the balance accuracy of the random forest model 
# outperform the other two model by providing us a balance accuracy on 
# "AGAINST", "FOVOR", and "NONE" class 0.81, 0.87, and 0.75 respectively.

