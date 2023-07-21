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
                 docvarnames = c("Date", "Value", "Type", "Num"))

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
                        docvars( speech_dfm_trim, c("Type", "Month", "Day") )
)

# 10 topics
set.seed(123)
speech_stm_basic <- stm(speech_stm_dfm$documents, 
                      speech_stm_dfm$vocab, 
                      K = 10)

labelTopics( speech_stm_basic, n = 10)
plot.STM(speech_stm_basic, type = "summary", labeltype = c("frex"), n=5)
plot.STM(speech_stm_basic, type = "hist", labeltype = c("frex"))

# 6 topics
set.seed(123)
speech_stm_basic <- stm(speech_stm_dfm$documents, 
                        speech_stm_dfm$vocab, 
                        K = 6)

labelTopics( speech_stm_basic, n = 6)
plot.STM(speech_stm_basic, type = "summary", labeltype = c("frex"), n=5)
plot.STM(speech_stm_basic, type = "hist", labeltype = c("frex"))

# 8 topics
set.seed(123)
speech_stm_basic <- stm(speech_stm_dfm$documents, 
                        speech_stm_dfm$vocab, 
                        K = 8)

labelTopics( speech_stm_basic, n = 8)
plot.STM(speech_stm_basic, type = "summary", labeltype = c("frex"), n=5)
plot.STM(speech_stm_basic, type = "hist", labeltype = c("frex"))

# Naming 8 topics
speechBasic_names <- c("War Casualties",
                     "Effect of War on Energy Food",
                     "Motivate Ukraine People",
                     "International Assist",
                     "Russian Attack",
                     "Russia War Crime",
                     "Ukraine Military Operation",
                     "Siutation of War"
                     )

plot.STM(speech_stm_basic, type = "summary", 
         topic.names = speechBasic_names, custom.labels = "")

# Topic correlation
speech_stm_basic_corr <- topicCorr(speech_stm_basic)
plot.topicCorr(speech_stm_basic_corr, vlabels = speechBasic_names)

# Add structure to model
set.seed(456)
speech_stm_struc <- stm(speech_stm_dfm$documents, 
                speech_stm_dfm$vocab, 
                K = 8, 
                prevalence =~ Type + s(Month),
                data = speech_stm_dfm$meta,
                max.em.its = 100)

labelTopics( speech_stm_struc, n = 20)

speechStruc_names <- c("War Casualties",
                       "Effect of War on Energy Food",
                       "Motivation for Ukraine People",
                       "International Assist",
                       "Russian Attack",
                       "Russian War Crime",
                       "Ukraine Military Operation",
                       "Siutation of War"
                       )

plot.STM(speech_stm_struc, type = "summary", 
         topic.names = speechStruc_names, custom.labels = "")

# Topic correlation
speech_stm_corr <- topicCorr(speech_stm_struc)
plot.topicCorr(speech_stm_corr, vlabels = speechStruc_names)

# Estimating effect
monthEffects <- estimateEffect( 1:8 ~ s(Month), 
                                speech_stm_struc,
                                meta = speech_stm_dfm$meta,
                                uncertainty = "Global" )

# Month Effects
plot(monthEffects, "Month", 
     method = "continuous", topics = 1, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "1 Month Effect of War Casualities")

plot(monthEffects, "Month", 
     method = "continuous", topics = 2, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "2 Month Effect of Effect of War on Energy Food") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 3, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "3 Month Effect of Motivation for Ukraine People") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 4, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "4 Month Effect of International Assist") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 5, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "5 Month Effect of Russian Attack") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 6, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "6 Month Effect of Russian War Crime") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 7, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "7 Month Effect of Ukraine Military Operation") 

plot(monthEffects, "Month", 
     method = "continuous", topics = 8, 
     model = speech_stm_struc,
     printlegend = FALSE,
     main = "8 Month Effect of Siutation of War") 

# Type effect on Domestic and international
typeEffects <- estimateEffect( 1:8 ~ Type, 
                                  speech_stm_struc,
                                  meta = speech_stm_dfm$meta,
                                  uncertainty = "Global" )

unique(speech_stm_dfm$meta$Type)

plot(typeEffects, covariate = "Type", topics = 1:8,
     model = speech_stm_struc, method = "difference",
     cov.value1 = "D", cov.value2 = "I",
     xlab = "More D ... More I",
     main = "Effect of Domestic vs. International Types",
     xlim = c(-.3, .3), labeltype = "custom",
     custom.labels = speechStruc_names)
