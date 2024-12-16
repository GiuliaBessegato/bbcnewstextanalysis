install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("tm")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("pals")
install.packages("lda")
install.packages("ldatuning")
install.packages("kableExtra")
install.packages("DT")
install.packages("flextable")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidyr")
library(wordcloud)
library(tm)
library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(lda)
library(ldatuning)
library(flextable)
library(tidytext) 
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
library(tidyr)

#Read the excel file
bbc_news <- read.csv("data\\bbc_news.csv")

#delete missing data, 10 missing
bbc_news %>%
  select(tag, raw_text) %>%
  na.omit

#Convert text to lowercase
bbc_news <- bbc_news %>% 
  mutate(raw_text = tolower(raw_text))

#tokenize the text
bbc_news_word <- bbc_news %>%
  mutate(raw_text = str_replace_all(raw_text, "[[:punct:]]", "")) %>% #remove punctation
  unnest_tokens(word, raw_text) %>%   #Splits text into words
  anti_join(stop_words) %>%  #Remove stop words
  mutate(word = str_replace_all(word, "[0-9]+", "")) %>% #Removes numbers
  mutate(word = str_squish(word)) #Remove white spaces

# TF(text frequency)-IDF
#count the words
bbc_news_word_count <- bbc_news_word %>%
 count(tag, word, sort = TRUE)

#calculate tf-idf scores
bbc_tf_idf <- bbc_news_word_count %>%
  bind_tf_idf(word, tag, n)

#delete n column and order in descending the scores 
bbc_tf_idf %>%
  select(-n) %>%
  arrange(desc(tf_idf))

# Prepare the data for plotting
bbc_tf_idf_plot <- bbc_tf_idf %>%
  group_by(tag) %>%
  top_n(10, tf_idf) %>% #consider the top 10 scores
  mutate(word = reorder(word, tf_idf))

# Plot
ggplot(bbc_tf_idf_plot, aes(x = tf_idf, y = word, fill = tag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tag, ncol = 2, scales = "free") +
  scale_fill_manual(values = c("#0A2463","#DD6E42","#4EA5D9","#8367C7","#B2C9AB")) +
  theme_bw()

#SENTIMENT ANALYSIS

#LEXICON AFINN
afinn <- get_sentiments(lexicon = "afinn") 
#create dataframe with all words in AFINN lexicon
summary(afinn)

#creates new data-frame 
bbc_afinn <- bbc_news_word %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")%>% #joins words in AFINN lexicon
  rename(sentiment = value) #rename the column value in sentiment

#summarize sentiment scores for each category
bbc_afinn_tag <- bbc_afinn %>%
  group_by(tag) %>%
  summarise(afinn = sum(sentiment)) %>%
  ungroup()

#barplot the AFINN scores for each category
g_afinn <- ggplot(bbc_afinn_tag, aes(tag, afinn, fill = afinn>0,)) + #fill = makes values below zero in red
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("lightcoral","lightsteelblue3"))+
  labs(x= "Category",y="Sentiment Score",
       title=NULL)+
  theme_classic() +
  scale_y_continuous(breaks = seq(-4000, 4000, by = 1000)) +
  geom_hline(yintercept = seq(-4000, 4000, by = 1000), color = "grey80", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) 
g_afinn

#NRC SENTIMENT EMOTION LEXICON

#At first, without considering the categories

# Calculate the scores for each sentiment
sentiment_df_all <- bbc_news_word %>%
  inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%  # Acknowledge many-to-many relationship
  filter(!(sentiment %in% c("positive", "negative"))) %>%  # Filter out "positive" and "negative" sentiments
  count(word, sentiment) %>%  # Create a count of words by sentiment
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)  #Spread sentiment into columns, fill missing values with 0

#Count the total scores of sentiment for each word
sentiment_long_all <- sentiment_df_all %>%
  pivot_longer(cols = trust:anticipation, names_to = "emotion", values_to = "count")

# Calculate the total scores of emotions and the normalized_count
sentiment_long_all <- sentiment_long_all %>%
  mutate(total_emotions = sum(count)) %>%
  ungroup() %>%
  mutate(normalized_count = count / total_emotions)

#NRC barplot for all the articles 
ggplot(sentiment_long_all, aes(x = emotion, y = normalized_count, fill = emotion)) +
  geom_col(show.legend = TRUE) +
  labs(x = "Emotions",
       y = "NRC scores") +  
  theme(
    axis.text.x = element_blank()  # Remove labels for x axis
  ) +
  geom_hline(yintercept = seq(0.00, 0.25, by = 0.05), color = "#95918e", linetype = "dashed") +
  scale_fill_manual(values = c("#D7C0D0", "#BA2D0B", "#a6d854", "#247BA0", 
                               "#77BA99", "#8da0cb", "#0A2463", "#EE964B"))


#Now the NRC analysis considering the division in categories

#Calculate the scores for each emotion in each category
sentiment_df <- bbc_news_word %>%
  inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%  
  filter(!(sentiment %in% c("positive", "negative"))) %>%  #delete positive and negative from the analysis
  mutate(tag = as.factor(tag)) %>% #consider the tag columns as a factor
  count(tag, sentiment) %>%  
  spread(sentiment, n, fill = 0) #transform data from a long format back to a wide format 

#count the total sum of emotion scores
sentiment_long <- sentiment_df %>%
  pivot_longer(cols = anger:trust, names_to = "emotion", values_to = "count")

# Calculate total and normalized scores
sentiment_long <- sentiment_long %>%
  group_by(tag) %>%
  mutate(total_emotions = sum(count)) %>%
  ungroup() %>%
  mutate(normalized_count = count / total_emotions)

#plot the total scores for each category
ggplot(sentiment_long, aes(x = emotion, y = normalized_count, fill = emotion)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ tag, scales = "fixed") +
  labs(x = "Emotions",
       y = "NRC scores") +  
  theme(
    axis.text.x = element_blank() 
  ) +
  scale_fill_manual(values = c("#D7C0D0", "#BA2D0B", "#a6d854", "#247BA0", 
                               "#77BA99", "#8da0cb", "#0A2463", "#EE964B"))

#NRC analysis considering positive and negative sentiments

#count the sentimental scores
sentiment_df_pn <- bbc_news_word %>%
  inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>% 
  mutate(tag = as.factor(tag)) %>% 
  count(tag, sentiment) %>% 
  spread(sentiment, n, fill = 0) 

#count the total sum of sentimental scores
sentiment_long_pn <- sentiment_df_pn %>%
  pivot_longer(cols = anger:trust, names_to = "emotion", values_to = "count")

#count normalized and total sentimental scores
sentiment_long_pn <- sentiment_long_pn %>%
  group_by(tag) %>%
  mutate(total_emotions = sum(count)) %>%
  ungroup() %>%
  mutate(normalized_count = count / total_emotions)

#plot the results
ggplot(sentiment_long_pn, aes(x = emotion, y = normalized_count, fill = emotion)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ tag, scales = "fixed") +
  labs(x = "Emotions",
       y = "NRC scores") +  
  theme(
    axis.text.x = element_blank()  # Rimuove le etichette dell'asse x
  ) +
  scale_fill_manual(values = c("#D7C0D0", "#BA2D0B", "#a6d854", "#247BA0", 
                               "#77BA99", "#8da0cb", "#0A2463", "#EE964B", "#56351E","#440381" ))

#BING lexicon divided per categories
sentiment_bing <- bbc_news_word %>%
  left_join(get_sentiments(lexicon = "bing"), by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  group_by(tag) %>%
  ungroup()
sentiment_bing %>%print(n = 5)

#indicates for each news how many positive, negative and neutral words
sentiment_bing_news <- sentiment_bing %>%  
  count(tag, sentiment) 
sentiment_bing_news

#makes a table in which per category indicates the number of positive, negative and neutral words
sentiment_bing_news <- sentiment_bing %>%  
  count(tag, sentiment) %>%
  tidyr::spread(key = sentiment, value = n) %>%
  select("tag", "positive", "negative", "neutral")  # reorder columns
sentiment_bing_news %>%print(n = 5)

#count the bing score for each category
sentiment_bing_news <- sentiment_bing %>% 
  count(tag, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  mutate(total = (positive + negative + neutral)) %>%   # create "total" column 
  mutate(bing = ((positive - negative) / total)) %>% # create column with calculated score
  select("tag", "positive", "negative", "neutral", "total", "bing")  # reorder columns
sentiment_bing_news %>%print(n = 5)

#Consider only datasets and bing scores
sentiment_bing_news %>%
  select("tag", "bing") %>%
  print(n=5)

#bar - graph 
g1 <- ggplot(sentiment_bing_news, aes(tag, bing, fill = bing>0,)) + #fill = makes values below zero in red
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("lightcoral","lightsteelblue3"))+
  labs(x= NULL,y=NULL,
       title="Bing Sentiment Scores for BBC news macro topic")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
g1

#Most used negative words
sentiment_bing %>%
  filter(sentiment == "negative") %>%
  count(word, sentiment, sort = TRUE)
sentiment_bing <- sentiment_bing %>%
  count(sentiment)


#TOPIC MODELING
words_to_remove <- c("said", "will", "also", "can", "last", "one", "two", "first", "told", "year")

# Rename columns to match the required format for DataframeSource
bbc_news_dataframe <- data.frame(doc_id = seq.int(nrow(bbc_news)), text = bbc_news$raw_text)

# Create the corpus using the DataframeSource
document <- Corpus(DataframeSource(bbc_news_dataframe))
document<- tm_map (document, content_transformer (tolower))#convert all text to lower case 
document<- tm_map (document, removeNumbers) #Remove numbers from document 
document<- tm_map (document, removeWords,stopwords ("english"))#remove stopwords in English
document<- tm_map (document, removeWords, words_to_remove)
document<- tm_map (document, removePunctuation, preserve_intra_word_dashes = TRUE) 
document<- tm_map (document, stripWhitespace)#remove white spacies

DTM <- DocumentTermMatrix(document)

#Create model with 5 topics
Model_LDA <- LDA(DTM,k = 5, control = list(seed = 1234))
Model_LDA

#Shows the probability of a word being associated to a topic
beta_topics <- tidy(Model_LDA, matrix = "beta") #creates the beta model
beta_topics #shows all the info in beta_topics

# Get the top terms per topic
top_terms <- beta_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

#Grouping terms by topic
beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create a named vector for the topic labels
topic_labels <- c("1" = "Entertainment", 
                  "2" = "Business", 
                  "3" = "Sport", 
                  "4" = "Tech", 
                  "5" = "Politics")

# Modify the plot to use these labels in the facet titles
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free", labeller =   as_labeller(topic_labels)) +
  scale_fill_manual(values = c("#0A2463","#DD6E42","#4EA5D9","#8367C7","#B2C9AB")) +
  scale_y_reordered()


#ANALYSIS HE AND SHE PRONOUNS

# Dissect the texts into bigrams: 2-word combinations
bbc_bigrams <- bbc_news %>%
  unnest_tokens(bigram, raw_text, token = "ngrams", n = 2)

#separate all the words in 2 different variables so it's easier to work with them
he_she_bigrams <- bbc_bigrams %>%
  separate(bigram, into = c("word1", "word"), sep = " ") %>%
  filter(word1 %in% c("he", "she")) %>%
  anti_join(stop_words, by = "word")  # Removes stopwords from the "word2" columns

#rename word column in word2
he_she_bigrams <- he_she_bigrams %>%
  rename(word2 = word)

#count the bigrams in the articles
he_she_counts <- he_she_bigrams %>%
  count(word1, word2, sort = TRUE)

# Find the ratio of words paired with 'he' or 'she'
he_she_words = he_she_counts %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = he + she) %>%
  mutate(he = (he + 1) / sum(he + 1), she = (she + 1) / sum(she + 1)) %>% # adding 1 is called "smoothing," which helps avoid division by zero.
  mutate(log_ratio = log2(she / he)) %>%
  arrange(desc(log_ratio)) 

# Prepare the data for plotting
he_she_counts_plot = he_she_words %>%
  filter(total>= 3) %>% # only words that are used enough
  mutate(abslogratio = abs(log_ratio)) %>% # choosing the number of words to display based on highest abs(log_ratio)
  group_by(log_ratio < 0) %>% 
  top_n(5, abslogratio) %>%
  mutate(word = reorder(word2, log_ratio)) # reorder a factor according to another variable so they are in order in the plot

# Plot
ggplot(he_she_counts_plot, aes(word, log_ratio, color = log_ratio < 0)) +
  geom_segment(aes(x = word2, xend = word, y = 0, yend = log_ratio)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(x = "Words", 
       y = "Relative appearance") +
  scale_color_discrete(name = "", labels = c("More 'she'", "More 'he'")) +
  scale_y_continuous(breaks = seq(-2, 2),
                     labels = c("4x", "2x", 
                                "Same", "2x", "4x")) +
  scale_fill_manual(values = c("#247BA0","#BA2D0B")) +
  theme_bw()





