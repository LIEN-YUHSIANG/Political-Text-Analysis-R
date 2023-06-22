###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################


# Week 08: More Document Scaling
# --------------------------------------

# The usual setup commands. Remember to change the folder location.

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
setwd("~/Dropbox/Waseda/Political Text Analysis 2023/Week08")


# Now let's load our libraries.

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)


#-----------------------------------------------------------------------------#
# 1.0   Wordscores


# Wordscores is another algorithm for scaling documents - i.e., finding their
# locations in a latent dimension that tells us something about the position
# of the document or its author on a certain issue.

# Unlike Wordfish, Wordscores is a supervised method. This means that we 
# have to provide it with some data up front. Specifically, we need to
# figure out the location (on a single-dimensional scale) of a few documents,
# which Wordscores will use as reference texts. It will then decide where
# the other documents should be located relative to the ones with known scores.


# First, let's load in some data. Today we're going to look at manifestos
# written by UK political parties in the 1990s. This is the data used in the 
# paper by Benoit, Laver, & Garry, the inventors of Wordscores.

# In the files you downloaded, there is a directory of text files called 
# `uk_manifestos`, in which each file has the Year and the name of the Party
# that wrote the manifesto. Just like in Week 5, we're going to pull that
# data out of the filenames using the readtext() command.

ukman <- readtext("./uk_manifestos/*.txt",
                  docvarsfrom = "filenames",
                  dvsep = "_",
                  docvarnames = c("Year", "Party"))
print(ukman)

#-----------------------------------------------------------------------------#
# 1.1   Preparing the Corpus

# As before, we want to convert this object into a Corpus:

ukman_corp <- corpus(ukman)
summary(ukman_corp)

# Let's tidy up the naming a bit. By default, Quanteda just uses the filename
# as the name for each row, but we can construct a cleaner name by combining
# the Year and Party docvars. (The paste() command sticks together sets of
# variables into text strings with custom separators between the values.)

quanteda::docnames(ukman_corp) <- paste(quanteda::docvars(ukman_corp)$Party, 
                                        quanteda::docvars(ukman_corp)$Year, 
                                        sep = " ")

summary(ukman_corp)


# Now we want to convert our corpus into tokens. Remember that there are
# several steps here: we're converting into tokens, removing stopwords,
# and then stemming the words to remove grammatical parts and just 
# preserve the core meaning.

ukman_tokens <- ukman_corp %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_wordstem(language = "en")

head(ukman_tokens)
  
# Note that some of these documents are all in capital letters while some are
# in sentence case. No idea why: this is just how the documents available
# online were structured. However, it doesn't matter, since in the next 
# step everything will be automatically converted to lower-case anyway.

# Convert the token list into a Document-Feature Matrix:

ukman_dfm <- dfm(ukman_tokens)

topfeatures(ukman_dfm, 20)


#-----------------------------------------------------------------------------#
# 1.2   Running Wordscores

# Okay, now we need to insert the values for the documents whose
# positions we know. In this case, we have values for the 1992 
# manifestos, taken from an expert survey conducted at that election.
# Specifically, on economic policy, the experts scored the Labour
# Party as 5.35, the Liberal Democrats as 8.21, and the Conservatives
# as 17.21 (higher scores mean more fiscally right-wing policies).

quanteda::docnames(ukman_dfm)

# We need to assign those scores to a new vector in exactly the 
# same order as the documents in the DFM, so refer to the docnames()
# list when constructing this new object!

# Note that documents with no score are included as NA entries.
# These are the documents Wordscores will estimate a score for.

refscores <- c(17.21, 5.35, 8.21, NA, NA, NA)


# Now we can actually run Wordscores and check our results.

ukman_ws <- textmodel_wordscores(ukman_dfm, refscores)

summary(ukman_ws) 

# Note that the output is literally showing us the "word scores", 
# not the document scores. This is useful as a sanity check at this
# point - some of the scores assigned to these words should make
# sense to you. For example, here we see that "conserv" has a 
# score of 17.21, which makes sense - this word is by far most likely
# to appear in the Conservative manifesto!

# We can visualise the word locations using a graph. It's also 
# possible to pick out specific words and highlight their locations.

textplot_scale1d(ukman_ws, 
                 highlighted = c( "budget", "green", "co-operation", "pension", "pollution", "futur"), 
                 highlighted_color = "red")

# Note that the Word Score axis runs from 5.35 to 17.21. This model
# cannot estimate any positions that are higher than the highest
# scoring reference document, or lower than the lowest scoring one!

# Finally, we use these word scores to predict / estimate the 
# positions of the unscored texts.

ws_result <- predict(ukman_ws,
                     interval = "confidence", 
                     newdata = ukman_dfm)

# We get a warning message because there are words used in the 1997 
# manifesto texts that did not appear in the 1992 texts. We don't
# have scores for those words, and have to ignore them in the model.

ws_result

textplot_scale1d(ws_result)

# Note that the scores for the 1992 texts have changed - 
# they are no longer the scores we gave to the algorithm! That's
# because it has treated those texts as new, and estimated their
# positions again. This is not a problem - this is a latent
# dimension after all, so the scores are less important than the
# relative positions of the documents.

# We can also divide this plot up by party:

textplot_scale1d(ws_result, 
                 margin = "documents",  
                 groups = quanteda::docvars(ukman_corp, "Party"),
                 sort = FALSE)

# (If you don't set `sort` to FALSE, it will plot each party's
#  values starting with the furthest left on the scale - which
#  can be confusing if it mixes up the order of 1997 and 1992.)


# This is actually a very good representation of our understanding
# of what happened in the UK general election of 1997. The Labour
# party had a young new leader, Tony Blair, who pushed the party
# towards the economic centre ground, abandoning many long-held
# left wing positions along the way. The Conservatives tried
# to respond to this threat by also moving towards the centre,
# but were ultimately defeated in a landslide.


#-----------------------------------------------------------------------------#
# 1.1   Changing the Topic

# A very powerful thing about Wordscores is that by changing the scores
# for the reference texts, we can actually change the results to reflect
# a different latent dimension.

# In the previous example, we were looking at the scores for Economic policy.
# The same expert survey also rated the 1992 manifestoes according to their
# positions on decentralisation (i.e., the establishment of regional / local
# assemblies, and moving powers away from the central government). The
# ratings were 5.26 for the Liberal Democrats, 10.21 for Labour, and 15.61
# for the Conservatives (higher scores mean less support for decentralising).

# Let's re-run Wordscores with these new data.


quanteda::docnames(ukman_dfm)

refscores_decen <- c(15.61, 10.21, 5.26, NA, NA, NA)

ukman_ws_decen <- textmodel_wordscores(ukman_dfm, refscores_decen)

summary(ukman_ws_decen) 

# If you compare these scores to the previous example, you would see that the
# words now have different scoring attached to them. Wordscores is now 
# focusing on the words that are best at distinguishing the new latent
# dimension we have given it to work with - the decentralisation dimension.

# One way to see this clearly is to recreate the plot we made for the
# Economic dimension, highlighting the same words - remember how these
# words were quite clearly associated with the Left, Centre, and Right
# positions in the previous analysis?
textplot_scale1d(ukman_ws_decen, 
                 highlighted = c( "budget", "green", "co-operation", "pension", "pollution", "futur"), 
                 highlighted_color = "red")

# Now they're all over the place (except co-operation, which is still 
# totally associated with the Conservative position). The words shifting
# position mean that the documents will also be scored differently.

ws_result_decen <- predict(ukman_ws_decen,
                           interval = "confidence", 
                           newdata = ukman_dfm)

ws_result_decen

textplot_scale1d(ws_result_decen, 
                 margin = "documents",  
                 groups = quanteda::docvars(ukman_corp, "Party"),
                 sort = FALSE)

# We can see here that Labour's position on Decentralisation doesn't change
# very much, but the Liberal Democrats and Conservatives both move towards
# a more centrist position on the issue. This makes sense given the 
# situation in UK politics at the time - 1997 is remembered as being a
# landslide victory for Labour that forced the Conservatives out of power
# for the first time in almost 20 years, but in the background it was also
# a hugely important election for devolution (i.e., the establishment of
# regional governments for Scotland, Wales, and Northern Ireland). Both
# the Scottish National Party and Plaid Cymru had strong election results,
# so it makes sense that the major Westminster parties would have taken
# centrist stances to avoid alienating voters who supported more devolution.

# (We could argue that when you see all the major parties moving towards the
#  same position on a certain issue - in this case, the centre - it means 
#  that issue has become a valence issue, i.e., a political issue where all
#  the parties agree on what should be done, but disagree on which of them
#　is the most competent to achieve it.)



