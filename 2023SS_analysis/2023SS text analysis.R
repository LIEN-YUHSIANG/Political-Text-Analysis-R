library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textmodels)
library(stm)
library(topicmodels)
library(wordcloud)

# Load data in
speech <- readtext("./SF/*.txt",
                 docvarsfrom = "filenames",
                 dvsep = "_",
                 docvarnames = c("Date", "President", "Party", "Num"))

speech$Month <- month(speech$Date)
speech$Day <- day(speech$Date)

speech$text <- gsub("’", "'", speech$text)

# Create dfm
speech_dfm <- speech %>%
  corpus() %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE, 
         remove_separators = TRUE) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_wordstem(language = "en") %>%
  dfm() %>%
  dfm_remove(min_nchar=2)

# Trim DFM
speech_dfm_trim <- dfm_trim(speech_dfm, min_docfreq = 20)
speech_dfm_trim[ ntoken(speech_dfm_trim) == 0, ]
# All document are not empty
speech_dfm_trim

# Train STM
head(docvars(speech_dfm_trim))
speech_stm_dfm <- convert(speech_dfm_trim, 
                        to = "stm", 
                        docvars( speech_dfm_trim, c("Num", "Month", "Day") )
)

set.seed(123)
speech_stm_basic <- stm(speech_stm_dfm$documents, 
                      speech_stm_dfm$vocab, 
                      K = 10)

labelTopics( speech_stm_basic, n = 10)
plot.STM(speech_stm_basic, type = "summary", labeltype = c("frex"), n=5)
plot.STM(speech_stm_basic, type = "hist", labeltype = c("frex"))

# Naming
newsBasic_names <- c("1",
                     "2",
                     "3",
                     "4",
                     "5",
                     "6",
                     "7",
                     "8",
                     "9",
                     "10",
                     "11",
                     "12",
                     "13",
                     "14",
                     "15")

plot.STM(speech_stm_basic, type = "summary", 
         topic.names = newsBasic_names, custom.labels = "")

# Topic correlation
speech_stm_basic_corr <- topicCorr(speech_stm_basic)
plot.topicCorr(speech_stm_basic_corr, vlabels = newsBasic_names)

# Add structure to model
set.seed(456)
speech_stm <- stm(speech_stm_dfm$documents, 
                speech_stm_dfm$vocab, 
                K = 15, 
                prevalence =~ s(Month),
                data = speech_stm_dfm$meta,
                max.em.its = 100)

monthEffects <- estimateEffect( 1:15 ~ s(Month), 
                                speech_stm,
                                meta = speech_stm_dfm$meta,
                                uncertainty = "Global" )

plot(monthEffects, "Month", 
     method = "continuous", topics = 4, 
     model = speech_stm,
     printlegend = FALSE)

plot(monthEffects, "Month", 
     method = "continuous", topics = 7, 
     model = speech_stm,
     printlegend = FALSE)