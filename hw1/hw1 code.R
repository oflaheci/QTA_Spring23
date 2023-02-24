library(dplyr)
library(textstem)
library(lubridate)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(stringr)

setwd(getwd())

data <- read.csv("./data/us_tweets.csv", 
                 stringsAsFactors=FALSE,
                 encoding = "utf-8")
saveRDS(data, "data/dftweets")

summary(data)
head(data)
colnames(data)

## 1.0 Print the number of tweets that are in this dataset.
tweet_number <- nrow(data)
tweet_number

## 1.1 Create a new dataframe that only includes original tweets (remove retweets) and print the number of rows.
typeof(data) #list 
df <- as_tibble(data)
head(df)
colnames(df)

df <- df %>%
  filter(is_retweet == "FALSE")
nrow(df)

#### 1.2 

## df for dt tweets
df_dt <- df %>%
  filter(screen_name == "realDonaldTrump")
nrow(df_dt)

## dt tweets with exclamation points
df_dt_exc <- df_dt %>%
  filter(grepl("!", text, ignore.case = TRUE))
print(list(nrow(df_dt_exc), df_dt_exc$text[246]))

## winning
# tidying tibble
tidydt <- df_dt %>%
  select(X,
         text,
         created_at
  )
head(tidydt)
  
tidydt_win <- tidydt %>%
  filter(
    grepl(pattern = '\\bwon\\b|\\bwin\\b|\\bwins\\b|winn', text, ignore.case = TRUE)
    )
print(list(nrow(tidydt_win), tidydt_win$text[5]))
tidydt_win$text 

## employment
tidydt_emp <- tidydt %>%
  filter(grepl('\\bjobs\\b|employ', text, ignore.case = TRUE)) # 'job' alone personal
print(list(nrow(tidydt_emp), tidydt_emp$text[1]))
tidydt_emp$text 

## immigration
# explore words
#dream <- 
  tidydt %>%
  filter(grepl('dream', tidydt$text, ignore.case = T))
#dream$text

tidydt_imm <- tidydt %>%
  filter(grepl('immigr', text, ignore.case = TRUE))
print(list(nrow(tidydt_imm), tidydt_imm$text[3]))
tidydt_imm$text 

## hoax 
tidydt_hoax <- tidydt %>%
  filter(grepl(pattern = 'hoax', text, ignore.case = TRUE))
print(list(nrow(tidydt_hoax), tidydt_hoax$text[2]))
tidydt_hoax$text 

###################
# Question 2
###################

## Make a corpus object 
tidy_df <- df %>%
  select(X,
         screen_name,
         text,
         datetime = created_at
  ) %>%
  mutate(datetime = as_datetime(datetime))

summary(tidy_df)
typeof(tidy_df$datetime)
unique(tidy_df$screen_name) # 4 # "RudyGiuliani"    "SpeakerPelosi"   "RepAdamSchiff"   "realDonaldTrump"

corpus_df <- corpus(tidy_df, 
                     docid_field = "X", 
                     text_field = "text")
#checking
corp_sum <- summary(corpus_df, 
        n = nrow(docvars(corpus_df)) #note: the default is n=100
) 
corp_sum

summary(corpus_df, 6)
as.character(corpus_df)[4]

### creating dfm 

## Make tokens 
tokns <- quanteda::tokens(corpus_df, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)
tokns <- tokens_tolower(tokns)
stop_list <- stopwords("english")
tokns <- tokens_remove(tokns, c(stop_list, "amp"))

## stemming
stem_tokns <- tokens_wordstem(tokns)

## Lemmatization
# tokns_list <- as.list(tokns) 
# lemma_tokns_list <- lapply(tokns_list, lemmatize_words) 
# lemma_tokns <- as.tokens(lemma_tokns_list) 

#checking 
# lemma_tokns[5]
# tokns[5]

# identify collocations
collocations <- textstat_collocations(stem_tokns, size = 2)
collocations # interesting to #30
coll_list <- collocations$collocation[1:30]
coll_list

# combine with tokens list
complete_tokns <- tokens_compound(stem_tokns, coll_list)

# convert to a dfm
dfm_twt <- dfm(complete_tokns)
saveRDS(dfm_twt, "data/dfm_twt")

# checking
topfeatures(dfm_twt)
# remove 'amp' from &amp loading error above 

# wordcloud visualisation
dfm_twt %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 0.5, max_size = 5, max_words = 100)

## top 30 terms by politician 
#dfm_polit <- dfm_group(dfm_twt, groups = screen_name)
#summary(dfm_polit)

#dfm_twt %>%
  #filter(screen_name == "realDonaldTrump") 

unique(tidy_df$screen_name) # 4 # "RudyGiuliani"    "SpeakerPelosi"   "RepAdamSchiff"   "realDonaldTrump"

freq_feat_dt <- dfm_twt %>%
  dfm_subset(screen_name == "realDonaldTrump") %>%
  textstat_frequency(., n = 30) %>%
  ggplot(aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Feature")+
  ylab("Frequency")+
  ggtitle("Donald Trump Most Frequent Terms")
freq_feat_dt

freq_feat_rg <- dfm_twt %>%
  dfm_subset(screen_name == "RudyGiuliani") %>%
  textstat_frequency(., n = 30) %>%
  ggplot(aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Feature")+
  ylab("Frequency")+
  ggtitle("Rudy Giuliani Most Frequent Terms")
freq_feat_rg

freq_feat_np <- dfm_twt %>%
  dfm_subset(screen_name == "SpeakerPelosi") %>%
  textstat_frequency(., n = 30) %>%
  ggplot(aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Feature")+
  ylab("Frequency")+
  ggtitle("Nancy Pelosi Most Frequent Terms")
freq_feat_np

freq_feat_as <- dfm_twt %>%
  dfm_subset(screen_name == "RepAdamSchiff") %>%
  textstat_frequency(., n = 30) %>%
  ggplot(aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Feature")+
  ylab("Frequency")+
  ggtitle("Adam Schiff Most Frequent Terms")
freq_feat_as

# defo a way to have all in same plot

## key terms Pelosi and Trump

#together
dfm_np_dt <- dfm_twt %>%
  dfm_subset(screen_name == "realDonaldTrump" | screen_name =="SpeakerPelosi") # %>%

result_keyness <- textstat_keyness(dfm_np_dt)
textplot_keyness(result_keyness, show_reference = FALSE)

# dt 
dfm_key_dt <- dfm_twt %>%
  dfm_subset(screen_name == "realDonaldTrump")

result_keyness_dt <- textstat_keyness(dfm_key_dt)
textplot_keyness(result_keyness_dt, show_reference = FALSE)

# np 
dfm_key_np <- dfm_twt %>%
  dfm_subset(screen_name == "SpeakerPelosi")

result_keyness_np <- textstat_keyness(dfm_key_np)
textplot_keyness(result_keyness_np, show_reference = FALSE)

## keyword in context

df_np_dt_kwic <- kwic(corpus_df,
                 pattern = phrase(patt <- c("founde*","statement", "claim", "presid*", "hole")),
                 window = 5,
                 case_insensitive = TRUE,
                 valuetype = "glob")
head(df_np_dt_kwic)

## dt sentiment analysis
dfm_sentiment <- dfm_lookup(dfm_key_dt, data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups = datetime)
dfm_sentiment

docvars(dfm_sentiment, "prop_negative") <- as.numeric(dfm_sentiment[,1] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "prop_positive") <- as.numeric(dfm_sentiment[,2] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "net_sentiment") <- docvars(dfm_sentiment, "prop_positive") - docvars(dfm_sentiment,"prop_negative")

docvars(dfm_sentiment) %>%
  ggplot(aes(x = yday(datetime), y = net_sentiment, group = week(datetime))) + #yday is suitable
  geom_smooth(aes(colour = as.character(week(datetime)))) +
  labs(title = "Trump's Tweets' Sentiment over Time", 
       x = "Day of 2019", y = "Net Sentiment", 
       colour = "week")
# aka trump became a grump 





