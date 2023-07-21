###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 13: GridSearch and Cross-Validation
# ----------------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week13")

library(tidyverse)
library(readtext)
library(quanteda)

library(caret)
library(naivebayes)
library(e1071)
if(!require("kernlab")) {install.packages("kernlab"); library(kernlab)}

#-----------------------------------------------------------------------------#
# 0.0   Load Some Data: Trump Tweets

# Last week we looked at Biden-related tweets, and then you trained a model
# to classify Trump-related tweets in class. Let's work on the Trump tweets
# to begin with this week.

trump_tweets <- readtext("trump_tweets.csv", text_field = "text")

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

# Remember that after constructing the DFM, we have to check to see if any
# documents are now empty (i.e., all the words have been trimmed out in one
# step or another).

trump_dfm[ ntoken(trump_dfm) == 0, ]

# Two documents have zero tokens, so we have to drop them:

trump_dfm <- trump_dfm[ !ntoken(trump_dfm) == 0, ]

# Also remember that the dependent variable - the category we are trying to
# classify - should be explicitly set as a Factor variable, so that R knows it's
# a category label with a set of possible values (and not a String variable 
# which could contain any word at all).

trump_dfm$label <- as.factor(trump_dfm@docvars$label)



#-----------------------------------------------------------------------------#
# 1.0   Training and Test Sets

# Although we are going to do cross-validation (i.e., repeatedly dividing the
# data into training and test sets), we still need to hold out some Test data
# so that we can check the final model on data it has never seen before.

# To do this, give a unique numeric ID to each row...
trump_dfm$numeric_id <- 1:nrow(trump_dfm)

# Then partition the data randomly, preserving the label proportions...
set.seed(1234)
trainIndex <- createDataPartition(trump_dfm$label, 
                                  p = .8, 
                                  list = TRUE, 
                                  times = 1)

# And divide the data into two sets based on that index.
trump_dfm.train <- dfm_subset(trump_dfm, trump_dfm$numeric_id %in% trainIndex$Resample1 )
trump_dfm.test <- dfm_subset(trump_dfm, !trump_dfm$numeric_id %in% trainIndex$Resample1 )

# Finally, convert them into the matrix formats we use for training classifiers.
trump.train <- as.matrix(trump_dfm.train)
trump.test <- as.matrix(trump_dfm.test)

# From now on we're going to focus on using the training data only. We'll come
# back and check performance on the test data later.

#-----------------------------------------------------------------------------#
# 2.0   Setting up the Grid Search

# Let's start by looking at one model we ran last week - Support Vector
# Machine.
# For comparison, we can train a standard version and see how it performs.

system.time({
  set.seed(1234)
  trump.svm <- svm(x = trump.train,
                   y = trump_dfm.train$label)
  })

# You may notice that we wrapped the training function in the system.time({})
# enclosure here. This is just a timer function that will tell us how many
# seconds have elapsed - i.e., how long this model takes to train.

# We check performance by predicting our test set and then running the
# confusionMatrix() command, like last week.

trump_svm.predict <- predict(trump.svm, 
                            newdata = trump.test)

confusionMatrix(trump_svm.predict,
                trump_dfm.test$label)

# Accuracy is 0.63, Kappa is 0.42. Let's call that our baseline.


# Tuning SVM
# ----------

# The e1071 package we're using has built-in commands to find the best 
# hyper-parameters for SVM. Later on we'll see a more generic approach which
# can work for many types of model, but we'll focus on SVM for now.

# This takes two "tuning" parameters - `gamma` and `cost`. We set these
# to a selection of values to see what works.

system.time({
  svm.tune <- tune.svm(x = trump.train,
                       y = trump_dfm.train$label,
                       gamma = c(0.05, 0.1, 0.5, 1, 2),
                       cost = 10^(0:3))
  })

# We can finally ask what the most effective parameters it found were...
svm.tune$best.parameters

# If you want to see how all the different models performed, the data is here:
svm.tune$performances


# Now that we know our best parameters, we train a final model using them.
trump.svm.tuned <- svm(x = trump.train,
                       y = trump_dfm.train$label,
                       gamma = 1,
                       cost = 1000)

# Let's check the performance.
trump.svm.tuned.predict <- predict(trump.svm.tuned, 
                                   newdata = trump.test)

confusionMatrix(trump.svm.tuned.predict,
                trump_dfm.test$label)



#-----------------------------------------------------------------------------#
# APPENDIX I:   A Generalised Approach with Caret

# Specifying the Model
# --------------------

# To set up a more complex training process, we need to know a few things.
# Firstly, we need to know the exact name of the model we're going to train,
# and then we need to know what settings it takes.

# We can look up available models here: 
# https://topepo.github.io/caret/available-models.html

# If you search for "SVM" on that site, you find that there are actually many
# different kinds of SVM model. Notably, there are three models from `kernlab`
# which have different types of "kernel" (i.e., the approach to adding new
# dimensions to the data to assist in classifying non-linear data):

#   svmLinear
#   svmRadial
#   svmPoly


# We can also find out more about each model using the getModelInfo() command.

getModelInfo("svmRadial")[[1]]$parameters
getModelInfo("svmRadial")[[1]]$grid

# The $grid is the function that the train() command would use to try to find
# the best hyper-parameters if you didn't define anything. It often has very
# sensible basic values - and here it has a smart function that will assess
# the data and decide what to do.

# However, if you look up svmRadial on the website, you see that it lists a 
# different set of values - `sigma` and `C`. The website is correct in this
# case (I tested with `sigma` and `tau` and it creates an error), so let's
# set some values for those manually.

svm_grid <- expand.grid(sigma = c(1, 10, 100, 1000),
                       C = c(0.1, 1, 10)
                       )
svm_grid

# You can see what's happened here - the expand_grid() command has created a 
# table with every possible combination of these values. There are 12 rows in
# the table, so using this grid will train 12 models and test them against each
# other automatically.


#-----------------------------------------------------------------------------#
# 3.0   Setting up Cross-Validation

# We have our grid stored in nb_grid. Let's now set up cross-validation.

# We do this by creating a trainControl() object, which will specify how the
# training process will run.

svm_trCtrl <- trainControl(method = "cv",
                           number = 5)

# This object will train the model using cross-validation ("cv"), with five
# "folds" of the data - i.e., it'll cut the data into five parts, train on
# four of them, test on the fifth one, and then repeat that process five times
# so that every part of the data has been used for both training and testing.


#-----------------------------------------------------------------------------#
# 4.0   Training the Model


# We now train our model using the train() command - instead of directly calling
# the model function itself.

system.time({
  set.seed(1234)
  trump_svm_c1 <- train(x = trump.train,
                        y = trump_dfm.train$label,
                        method = "svmRadial",
                        tuneGrid = svm_grid,
                        trControl = svm_trCtrl
                        )
  })

# Obviously this took MUCH longer to run than the previous command - because it
# is both creating and testing 12 different models.

# Let's check the results:

trump_svm_c1

# So the best results here come from `sigma` being 1 and C being 10.


# The best model selected by this function will be saved in the $finalModel
# sub-object - so we can use that to Predict values just like before.

trump_svm_c1.predict <- predict(trump_svm_c1$finalModel, 
                                newdata = trump.test)

confusionMatrix(trump_svm_c1.predict,
                trump_dfm.test$label)



#-----------------------------------------------------------------------------#
# APPENDIX II:   Set up Parallel Processing

# Modern computers generally have more than one "core" - the unit that does the
# processing. This allows them to do more than one thing at a time - one core
# could be decoding a video while another core handles an application you're 
# running, for example.

# When we're doing an intensive operation like training a classifier, it can
# often be speeded up by allowing it to run on multiple cores at the same time.
# This is especially true when we're training a LOT of classifiers so that we
# can compare their results - each core in your computer can be training a
# different classifier, so they're being trained at the same time, rather than
# one after the other. This is called Parallel Processing.

# To set this up, you need to know how many cores your computer actually has:

parallel::detectCores()

# This is the maximum number you can tell R to use. However, you probably 
# shouldn't use the total number! It's better to leave a couple of cores free
# so that you can do other stuff on your computer while the models are being
# trained, especially if that process is likely to take a long time.

# My laptop has 12 cores, so I'm going to use 10 of them for training models.
# Set this to an appropriate level for your own computer.
numCores = 10

# Now you can set up your system for parallel processing with this command:

doParallel::registerDoParallel(cores = numCores)

# Note that while this will ONLY accelerate functions which actually know how 
# to use parallel processing! Many R functions won't use this ability.


