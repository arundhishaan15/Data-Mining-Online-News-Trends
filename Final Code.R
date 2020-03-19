# Calling libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(pdftools) 
library(wordcloud)
library(reshape2)

#######################################################################################################
#### Sentiment Analysis on Trending 20 Wall Street Journal Articles about India National News ######### 
#######################################################################################################

# Importing all PDF files from the same folder
setwd("/Users/pdf_wsj")
nm <- list.files(path="/Users/pdf_wsj")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
my_pdf_text <- as.data.frame(my_pdf_text)

# Merge all columns
my_df <- unite(my_pdf_text, col = wsj, sep = " ")

# Custom stopwords
custom <- data_frame(word = c("http","rt","https","t.io","india","india's","articles", "copy","copies", "personal","visit","ready",
                              "commercial","www.djreprints.com", "www.wsj.com", "vibhuti","agarwal","krishna","pokharel","2020","copyright",
                              "dow","jones","colleagues","clients"),lexicon=rep("custom", each=25))
new_stopwords <- bind_rows(custom, stop_words)

# Tokenization
wsj_tokenized <- my_df %>%
  unnest_tokens(word, wsj) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(wsj_tokenized) # This is Tidy Format

# Bi-grams
my_bigrams <- my_df %>%
  unnest_tokens(bigram, wsj, token = "ngrams", n=2)

bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% new_stopwords$word) %>%
  filter(!word2 %in% new_stopwords$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

print(bigram_counts)

# Get Sentiments

# Using bing dictionary
wsj_sentiments <- my_df %>%
  unnest_tokens(word, wsj) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(wsj_sentiments)

# Using afinn dictionary
wsj_sentiments <- my_df %>%
  unnest_tokens(word, wsj) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn")) %>%
  count(word, sort=T) %>%
  ungroup()

print(wsj_sentiments)

# Using nrc dictionary
wsj_sentiments <- my_df %>%
  unnest_tokens(word, wsj) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(wsj_sentiments)

# Sentiment Analysis using the nrc dictionary:

wsj_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Wordcloud based on nrc dictionary on Wall Street Journal Articles

wsj_sentiments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=150, scale = c(0.9,0.9), 
                   fixed.asp = TRUE,
                   title.size = 1)

#######################################################################################################
#### Sentiment Analysis on Trending 20 Times of India Articles about India National News ############## 
#######################################################################################################

# Importing all PDF files from the same folder
setwd("/Users/pdf_toi")
nm <- list.files(path="/Users/pdf_toi")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
my_pdf_text <- as.data.frame(my_pdf_text)
my_pdf_text <- my_pdf_text[,c(2:6)]

# Merge all columns
my_df <- unite(my_pdf_text, col = toi, sep = " ")

# Custom stopwords
custom <- data_frame(word = c("http","rt","https","t.io","india","india's","articles", "copy","copies", "personal","visit","ready",
                              "commercial","timeso???ndia.indiatimes.com","timesofindia.indiatimes.com", "cms","ad","articleshow",
                              "news","times","live","results","elections","videos","diva","videoshow","updates","miss", "shefali","sood",
                              "daughter's","cancer","9","lakh","rs","treatment","simple","trick","disappointing","74072501","can;t","afford"),lexicon=rep("custom", each=42))
new_stopwords <- bind_rows(custom, stop_words)

# Tokenization
toi_tokenized <- my_df %>%
  unnest_tokens(word, toi) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(toi_tokenized) # This is Tidy Format

# Bi-grams
my_bigrams <- my_df %>%
  unnest_tokens(bigram, toi, token = "ngrams", n=2)

bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% new_stopwords$word) %>%
  filter(!word2 %in% new_stopwords$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

print(bigram_counts)

# Get Sentiments

# Using bing dictionary
toi_sentiments <- my_df %>%
  unnest_tokens(word, toi) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(toi_sentiments)

# Using afinn dictionary
toi_sentiments <- my_df %>%
  unnest_tokens(word, toi) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn")) %>%
  count(word, sort=T) %>%
  ungroup()

print(toi_sentiments)

# Using nrc dictionary
toi_sentiments <- my_df %>%
  unnest_tokens(word, toi) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(toi_sentiments)

# Sentiment Analysis using the nrc dictionary:

toi_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Wordcloud based on nrc dictionary on Wall Street Journal Articles

toi_sentiments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.9,0.9), 
                   fixed.asp = TRUE,
                   title.size = 1)

#######################################################################################################
#### Sentiment Analysis on Trending 20 Hindustan Times Articles about India National News ######### 
#######################################################################################################

# Importing all PDF files from the same folder
setwd("/Users/pdf_ht")
nm <- list.files(path="/Users/pdf_ht")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
my_pdf_text <- as.data.frame(my_pdf_text)
my_pdf_text <- my_pdf_text[,c(2,3)]

# Merge all columns
my_df <- unite(my_pdf_text, col = ht, sep = " ")

# Custom stopwords
custom <- data_frame(word = c("http","rt","https","t.io","india","india's","articles", "copy","copies", "personal","visit","ready",
                              "commercial","bjp","aap","ht","top","news","delhi","2020","opinion","inte","city","world","don't","bra",
                              "live","trends"),lexicon=rep("custom", each=28))
new_stopwords <- bind_rows(custom, stop_words)

# Tokenization
ht_tokenized <- my_df %>%
  unnest_tokens(word, ht) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(ht_tokenized) # This is Tidy Format

# Bi-grams
my_bigrams <- my_df %>%
  unnest_tokens(bigram, ht, token = "ngrams", n=2)

bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% new_stopwords$word) %>%
  filter(!word2 %in% new_stopwords$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

print(bigram_counts)

# Get Sentiments

# Using bing dictionary
ht_sentiments <- my_df %>%
  unnest_tokens(word, ht) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(ht_sentiments)

# Using afinn dictionary
ht_sentiments <- my_df %>%
  unnest_tokens(word, ht) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn")) %>%
  count(word, sort=T) %>%
  ungroup()

print(ht_sentiments)

# Using nrc dictionary
ht_sentiments <- my_df %>%
  unnest_tokens(word, ht) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(ht_sentiments)

# Sentiment Analysis using the nrc dictionary:

ht_sentiments %>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Wordcloud based on nrc dictionary on Wall Street Journal Articles

ht_sentiments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=150, scale = c(0.9,0.9), 
                   fixed.asp = TRUE,
                   title.size = 1)

#######################################################################################################
#### Sentiment Analysis on Trending 20 The Indian Express Articles about India National News ############## 
#######################################################################################################

# Importing all PDF files from the same folder
setwd("/Users/pdf_iex")
nm <- list.files(path="Users/pdf_iex")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
my_pdf_text <- as.data.frame(my_pdf_text)
my_pdf_text <- my_pdf_text[,c(2,3)]

# Merge all columns
my_df <- unite(my_pdf_text, col = iex, sep = " ")

# Custom stopwords
custom <- data_frame(word = c("http","rt","https","t.io","india","india's","articles", "copy","copies", "personal","visit","ready",
                              "commercial","indian","express","news","click","channel","indianexpress","stay","updated","indianexpress.com",
                              "post","comment","live","blog","cleared","manually"),lexicon=rep("custom", each=28))
new_stopwords <- bind_rows(custom, stop_words)

# Tokenization
iex_tokenized <- my_df %>%
  unnest_tokens(word, iex) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(iex_tokenized) # This is Tidy Format

# Bi-grams
my_bigrams <- my_df %>%
  unnest_tokens(bigram, iex, token = "ngrams", n=2)

bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% new_stopwords$word) %>%
  filter(!word2 %in% new_stopwords$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

print(bigram_counts)

# Get Sentiments

# Using bing dictionary
iex_sentiments <- my_df %>%
  unnest_tokens(word, iex) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(iex_sentiments)

# Using afinn dictionary
iex_sentiments <- my_df %>%
  unnest_tokens(word, iex) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn")) %>%
  count(word, sort=T) %>%
  ungroup()

print(iex_sentiments)

# Using nrc dictionary
iex_sentiments <- my_df %>%
  unnest_tokens(word, iex) %>%
  anti_join(new_stopwords) %>% #here's where we remove tokens
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

print(iex_sentiments)

# Sentiment Analysis using the nrc dictionary:

iex_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Wordcloud based on nrc dictionary on Wall Street Journal Articles

iex_sentiments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=80, scale = c(0.9,0.9), 
                   fixed.asp = TRUE,
                   title.size = 1)



###########################################################################
# What type of articles does Wall Street Journal write about India?
###########################################################################


my_df <- bind_rows(
  mutate(wsj_sentiments, newspaper = "Wall Street Journal"),
  mutate(toi_sentiments, newspaper = "Times of India"),
  mutate(ht_sentiments, newspaper = "Hindustan Times"),
  mutate(iex_sentiments, newspaper = "The Indian Express")
)

my_df <- my_df %>%
  bind_tf_idf(word, newspaper, n)

my_df %>%
  arrange(desc(tf_idf))

my_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(newspaper) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=newspaper))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~newspaper, ncol=2, scales="free")+
  coord_flip()





