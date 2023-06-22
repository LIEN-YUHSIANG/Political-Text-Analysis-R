###############################################################################
#        Waseda University :: School of Political Science & Economics         #
#            - Political Text Analysis [2023 Spring Semester] -               #
#             Instructor: Rob Fahey <robfahey@aoni.waseda.jp>                 #
###############################################################################
#             Assignment 03: Document Similarity and Scaling                  #
###############################################################################


# In Quanteda, there is a corpus called `data_corpus_irishbudget2010`

# This corpus contains speeches by Irish politicians in a debate about the
# national budget in 2010.

# The speeches have a number of document variables, including the name of
# the politician and the party they belong to.


# Your tasks are:

# 1) Prepare this corpus for analysis:
#        - Tokenise the corpus, remove stopwords, and stem the tokens.
#        - Construct a document-feature matrix.


# 2) Compare the speeches using Cosine Similarity, and create a graph
#    illustrating this similarity.


# 3) Using the Wordfish algorithm, scale these documents and place them
#    on a graph relative to one another.
#    This graph should group the speakers by Party.


# 4) Finally: the two biggest parties in Ireland at this time were Fianna
#    Fail (FF) and Fine Gael (FG). Create a Word Cloud which compares the
#    terminology used in the speeches of these two parties.


# Write the code for each one of these tasks in the file below, and submit
# this file on Moodle as your assignment. Do not submit any other files.
# You don't need to submit the figures, graphs etc., just the R code to
# create them. 

# If you get stuck and can't complete a task, that's fine - show me what you
# tried to do, and write some comments explaining your understanding of the
# problem and what you tried to do to fix it. You can get a good grade even
# if you don't complete a task, as long as you show a solid effort being
# made to find a solution to the problem you encountered. Use Google, Stack
# Overflow, etc. - you may also use ChatGPT to debug your code if you're 
# stuck, in which case you should include your chat log with ChatGPT in your 
# submission file.


# I'll get you started...

Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(corrplot)


budget_speeches <- data_corpus_irishbudget2010
summary(budget_speeches)

# Now write the code to do tasks 1-4 above using the budget_speeches object.

################################################################################

# I have to apologize for turning in the assignment late due to the mismanagement 
# of the deadline. 
# Best regard

# Task 1
docvars(budget_speeches)

budget_tokens <- budget_speeches %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en", source = "marimo")) %>%
  tokens_wordstem(language = "en")

head(budget_tokens)

budget_dfm <- dfm(budget_tokens)
budget_dfm

# Task 2
budget_simil <- textstat_simil(budget_dfm, margin = "documents", method = "cosine")
budget_simil

corrplot(as.matrix(budget_simil), method = "number", type = "full")

# Since the plot is too crowded, create the chat for each speech separately
dotchart(as.list(budget_simil)$"Lenihan, Brian (FF)", xlab = "Cosine similarity (Lenihan, Brian (FF))", pch = 19)
dotchart(as.list(budget_simil)$"Bruton, Richard (FG)", xlab = "Cosine similarity (Bruton, Richard (FG))", pch = 19)
dotchart(as.list(budget_simil)$"Burton, Joan (LAB)", xlab = "Cosine similarity (Burton, Joan (LAB))", pch = 19)
dotchart(as.list(budget_simil)$"Morgan, Arthur (SF)", xlab = "Cosine similarity (Morgan, Arthur (SF))", pch = 19)
dotchart(as.list(budget_simil)$"Cowen, Brian (FF)", xlab = "Cosine similarity (Cowen, Brian (FF))", pch = 19)
dotchart(as.list(budget_simil)$"Kenny, Enda (FG)", xlab = "Cosine similarity (Kenny, Enda (FG))", pch = 19)
dotchart(as.list(budget_simil)$"ODonnell, Kieran (FG)", xlab = "Cosine similarity (ODonnell, Kieran (FG))", pch = 19)
dotchart(as.list(budget_simil)$"Gilmore, Eamon (LAB)", xlab = "Cosine similarity (Gilmore, Eamon (LAB))", pch = 19)
dotchart(as.list(budget_simil)$"Higgins, Michael (LAB)", xlab = "Cosine similarity (Higgins, Michael (LAB))", pch = 19)
dotchart(as.list(budget_simil)$"Quinn, Ruairi (LAB)", xlab = "Cosine similarity (Quinn, Ruairi (LAB) )", pch = 19)
dotchart(as.list(budget_simil)$"Gormley, John (Green)", xlab = "Cosine similarity (Gormley, John (Green))", pch = 19)
dotchart(as.list(budget_simil)$"Ryan, Eamon (Green)", xlab = "Cosine similarity (Ryan, Eamon (Green))", pch = 19)
dotchart(as.list(budget_simil)$"Cuffe, Ciaran (Green)", xlab = "Cosine similarity (Cuffe, Ciaran (Green))", pch = 19)
dotchart(as.list(budget_simil)$"OCaolain, Caoimhghin (SF)", xlab = "Cosine similarity (OCaolain, Caoimhghin (SF))", pch = 19)

# Task 3
# The FF party is right wing 
budget_wf <- textmodel_wordfish(budget_dfm, dir = c(2, 1))

textplot_scale1d(budget_wf)
textplot_scale1d(budget_wf, groups = docvars(budget_dfm, "party"))

# Task 4

# Construct dfn with only the two largest party
budget_tokensb2 <- budget_speeches %>%
  corpus_subset(party %in% c("FF", "FG")) %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en", source = "marimo")) %>%
  tokens_wordstem(language = "en")

head(budget_tokensb2)

budget_dfmb2 <- dfm(budget_tokensb2)
budget_dfmb2

set.seed(150)
budget_dfmb2 %>%
  dfm_group(groups = party) %>%
  textplot_wordcloud(min_count = 20, max_size = 4, comparison = TRUE,
                     color = RColorBrewer::brewer.pal(8, "Dark2"))