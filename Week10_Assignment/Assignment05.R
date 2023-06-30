###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Assignment 05:  Topic Models
# ----------------------------


# The file guardian_20.csv contains a very similar dataset to the one we used
# in class - a selection of articles from The Guardian, but this time from 2020.

# Your task is import this data, pre-process it as required, and then run a
# Structural Topic Model analysis to find the best model to describe the data.

# Most of the code required to do this will be exactly the same as the in-class
# example. The real assignment here is INTERPRETATION - you need to figure out
# what each topic means (by looking at keywords and headlines), and assign it
# a descriptive name. If there are topics you can't interpret, or if you think
# some topics are mixing up categories that should be separated, you should try
# re-running STM with a different number of topics until you get a list you are
# happy with.

# Note: Remember that we set the maximum iterations to a low level so that the
#       would finish running quickly in class. You should remove this parameter
#       when running these models at home.


# TASKS:
library(tidyverse)
library(readtext)
library(quanteda)
library(stm)

# 1) Import the data, fit a Structural Topic Model.
news <- readtext("guardian_20.csv", text_field = "Text")
news
news$Month <- month(news$Date)
news$Day <- day(news$Date)
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

news_dfm_trim <- dfm_trim(news_dfm, min_docfreq = 20)
news_dfm_trim[ ntoken(news_dfm_trim) == 0, ]
news_dfm_trim <- news_dfm_trim[ ntoken(news_dfm_trim) > 0, ]
news_dfm_trim

head(docvars(news_dfm_trim))

news_stm_dfm <- convert(news_dfm_trim, 
                        to = "stm", 
                        docvars( news_dfm_trim, c("Section", "Month", "Headline") )
)

set.seed(123)
news_stm_basic <- stm(news_stm_dfm$documents, 
                      news_stm_dfm$vocab, 
                      K = 13)

# 2) Interpret the categories, giving each one a descriptive name. You may well
#    need to re-run the STM process with a different category number.
labelTopics( news_stm_basic, n = 8)

plot.STM(news_stm_basic, type = "summary", labeltype = c("frex"), n=5)
plot.STM(news_stm_basic, type = "hist", labeltype = c("frex"))

# Over here I find out that there are multiple topics that all related to covid,
# so I try to lower the number of the topics to try unify them.
# Yet after limit to topics to 10, find out that some of the important topics 
# have been washout. So increase the number of topics to 13 again.

newsBasic_names <- c("Economics",
                     "UK exist",
                     "Health care",
                     "Animal",
                     "Environment protection",
                     "Culture",
                     "Global news",
                     "UK politics",
                     "Covid policy",
                     "Astralia vs big tech",
                     "BLM event",
                     "US politics",
                     "Covid")

plot.STM(news_stm_basic, type = "summary", 
         topic.names = newsBasic_names, custom.labels = "")

news_stm_basic_corr <- topicCorr(news_stm_basic)
plot.topicCorr(news_stm_basic_corr, vlabels = newsBasic_names)


# 3) Identify any categories in your data which deal with the COVID-19 pandemic.
#    Estimate the effect of the Month variable on these categories, and plot
#    a chart showing the topic proportion for the COVID categories each month.

# Add structure
set.seed(456)
news_stm <- stm(news_stm_dfm$documents, 
                news_stm_dfm$vocab, 
                K = 13, 
                prevalence =~ Section + s(Month),
                data = news_stm_dfm$meta,
                max.em.its = 100)
labelTopics( news_stm, n = 6)

newsStruc_names <- c("Economics",
                     "UK exist",
                     "Health care",
                     "Animal",
                     "Environment protection",
                     "Culture",
                     "Global news",
                     "UK politics",
                     "Covid policy",
                     "Astralia vs big tech",
                     "BLM event",
                     "US politics",
                     "Covid")
# There are two topics related to covid-19, which I name Covid and Covid policy.
plot.STM(news_stm, type = "summary", 
         topic.names = newsStruc_names, custom.labels = "")

news_stm_corr <- topicCorr(news_stm)
plot.topicCorr(news_stm_corr, vlabels = newsStruc_names)

findThoughts(news_stm, texts = news_stm_dfm$meta$Headline, n = 3, topics = 1:13)

monthEffects <- estimateEffect( 1:13 ~ s(Month), 
                                news_stm,
                                meta = news_stm_dfm$meta,
                                uncertainty = "Global" )
# plot the month effect of the covid related topics
plot(monthEffects, "Month", 
     method = "continuous", topics = 13, 
     model = news_stm,
     printlegend = FALSE)
plot(monthEffects, "Month", 
     method = "continuous", topics = 9, 
     model = news_stm,
     printlegend = FALSE)
# We can see a dump in around Jenurary2020, it might due to its the time when UK
# officially exist EU. So news change focus slightly to UK exist topic.

# Your submission should consist of an R file (include detailed comments, especially
# if something isn't working!), and a PDF or PNG file with the chart for Task 
# 3. You can export charts from RStudio with the "Export" button above the 
# chart display.

# Remember, even if you can't complete some part of the assignment, you can get
# a decent grade by attempting it and showing how you tried to solve any problem
# you encountered. Using Google, Stack Overflow, and ChatGPT is encouraged, but
# be sure to document how you used them - show what questions you asked, and how
# you understood the response.



