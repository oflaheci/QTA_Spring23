install.packages("quanteda")
install.packages("dplyr")
install.packages("lubridate")
install.packages("readtext")

library(quanteda)
## Package version: 3.2.0
## Unicode version: 13.0
## ICU version: 66.1
## Parallel computing: 12 of 12 threads used.
## See https://quanteda.io for tutorials and examples.
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(lubridate)
## 
## Attaching package: 'lubridate'
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
library(readtext) # for importing external corpus data

setwd("./QTA_Spring23")

# Read in csv file with readtext. Note this address will be different for your system.
kcna_csv <- readtext("./kcna_example2015.csv", 
                     encoding = "utf-8",
                     text_field = "newsText") # the column with the header "newsText" in the csv contains the article text.

# Generate datetimes
kcna_csv$date <- dmy(kcna_csv$newsDate)
kcna_csv$week <- week(kcna_csv$date)

# Create a quanteda corpus using the imported data
corpus_kcna <- corpus(kcna_csv)

# Create a dfm object with processed text
kcna_dfm <- corpus_kcna %>%
  quanteda::tokens(remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_hyphens = TRUE,
                   remove_separators = TRUE,
                   remove_url = TRUE,
                   include_docvars = TRUE) %>%
  dfm()


sample(data_dictionary_LSD2015$negative, 10) # sample of 10 negative words
##  [1] "execrat*"  "suffocat*" "crying"    "crude*"    "bury*"     "dolour*"  
##  [7] "demolish*" "scuffl*"   "traps"     "worse*"
sample(data_dictionary_LSD2015$positive, 10) # sample of 10 positive words
##  [1] "dexter*"       "one of a kind" "prize*"        "tolera*"      
##  [5] "powerful*"     "comforts*"     "entranced"     "impeccable*"  
##  [9] "enchant*"      "cooperat*"
# The LSD also has “neg_positive” and “neg_negative” keys that include negations. For now, we will ignore these keys and just focus on negative and positive. We can create a dfm object of dictionary words that match our corpus.

kcna_sent_dfm <- dfm_lookup(kcna_dfm, 
                            dictionary = data_dictionary_LSD2015[1:2])

head(kcna_sent_dfm, 5)
## Document-feature matrix of: 5 documents, 2 features (10.00% sparse) and 7 docvars.
##                         features
## docs                     negative positive
##   kcna_example2015.csv.1        2        1
##   kcna_example2015.csv.2      155      447
##   kcna_example2015.csv.3        0        9
##   kcna_example2015.csv.4       10       56
##   kcna_example2015.csv.5        1        5
# As we can see, there are differences within documents in terms of negative and positive sentiment matches. Importantly, it seems that document length also varies significantly. Let’s normalize the sentiment scores by total words and then calculate the difference between positive and negative proportion of words.

docvars(kcna_dfm, "prop_negative") <- as.numeric(kcna_sent_dfm[,1] / ntoken(kcna_dfm)) # add proportion of negative words to original dfm
docvars(kcna_dfm, "prop_positive") <- as.numeric(kcna_sent_dfm[,2] / ntoken(kcna_dfm))
docvars(kcna_dfm, "net_sentiment") <- docvars(kcna_dfm, "prop_positive") - docvars(kcna_dfm, "prop_negative") # calculate net sentiment
# How did net sentiment vary over time for all articles?
  
  library(ggplot2)
kcna_sent_plot <- ggplot(docvars(kcna_dfm),
                         aes(x = date,
                             y = net_sentiment)) +
  geom_smooth() + 
  theme_minimal()
kcna_sent_plot
  theme_minimal()