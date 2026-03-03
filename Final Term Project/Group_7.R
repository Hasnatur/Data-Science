install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")
install.packages("qdapRegex")
install.packages("hunspell")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("textclean")
install.packages("quanteda")

library(rvest)
library(dplyr)
library(tidytext)
library(stringr)
library(tm)
library(SnowballC) 
library(hunspell)
library(textclean)
library(topicmodels)
library(quanteda)


scrape_article <- function(url) {
  webpage <- read_html(url)
  
  article_text <- webpage %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ")
  
  return(article_text)
}

url <- "https://education.nationalgeographic.org/resource/air-pollution/"
raw_text <- scrape_article(url)


clean_text <- function(text) {
  text <- tolower(text)  
  text <- str_replace_all(text, "[^[:alnum:]\\s]", " ")  
  text <- str_replace_all(text, "\\d+", " ")  
  text <- str_replace_all(text, "\\s+", " ")  
  text <- str_trim(text)  
  return(text)
}


tokenize_text <- function(text) {
  tokens <- unnest_tokens(data.frame(text = text), word, text)
  return(tokens)
}


remove_stopwords <- function(tokens) {
  tokens %>%
    anti_join(stop_words, by = "word")
}


stem_words <- function(words) {
  wordStem(words, language = "english")
}


handle_contractions <- function(text) {
  replace_contraction(text)
}


remove_emojis <- function(text) {
  str_replace_all(text, "[^[:ascii:]]", "")
}


spell_check <- function(words) {
  corrected_words <- sapply(words, function(word) {
    suggestions <- hunspell_suggest(word)
    if(length(suggestions) > 0 && !hunspell_check(word)) {
      return(suggestions[[1]][1])
    }
    return(word)
  })
  return(corrected_words)
}


processed_text <- raw_text %>%
  handle_contractions() %>%
  remove_emojis() %>%
  clean_text() %>%
  tokenize_text() %>%
  remove_stopwords()


processed_text$stemmed_word <- stem_words(processed_text$word)


processed_text$corrected_word <- spell_check(processed_text$word)


write.csv(processed_text, "processed_text_data.csv", row.names = FALSE)


head(processed_text)


processed_data <- read.csv("D:/AIUB/SEMESTER 11/INTRODUCTION TO DATA SCIENCE/Last/processed_text_data.csv")


processed_data$doc_id <- 1  


dtm <- processed_data %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)


tfidf <- weightTfIdf(dtm)
print(tfidf)


set.seed(123)

k <- 5
lda_model <- LDA(dtm, k = k, method = "Gibbs", 
                 control = list(iter = 2000, verbose = 25))


top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)


print("Most probable words for each topic:")
print(top_terms)


doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  arrange(document, -gamma)


print("Topic proportions for the document:")
print(doc_topics)


library(ggplot2)

top_terms %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms per Topic",
       x = "Term",
       y = "Beta")


ggsave("topic_model_visualization.png")


top_terms <- top_terms %>%
  rename(term_beta = beta)

doc_topics <- doc_topics %>%
  rename(doc_gamma = gamma)


combined_results <- top_terms %>%
  left_join(doc_topics, by = "topic") %>%
  select(
    topic,             
    term,              
    term_beta,          
    document,           
    doc_gamma           
  )


write.csv(combined_results, "combined_topic_analysis.csv", row.names = FALSE)

head(combined_results)


comprehensive_results <- processed_data %>%

  left_join(
    tidy(lda_model, matrix = "beta") %>%
      group_by(term) %>%
      mutate(dominant_topic = topic[which.max(beta)],
             topic_probability = max(beta)) %>%
      select(term, dominant_topic, topic_probability) %>%
      distinct(),
    by = c("word" = "term")
  )


topic_labels <- paste("Topic", 1:k)
comprehensive_results$topic_label <- topic_labels[comprehensive_results$dominant_topic]


final_dataset <- comprehensive_results %>%
  select(
    word,                  
    stemmed_word,         
    corrected_word,       
    dominant_topic,       
    topic_label,          
    topic_probability     
  ) %>%
  
  arrange(topic_label, desc(topic_probability)) %>%
  
  group_by(topic_label) %>%
  mutate(rank_in_topic = row_number()) %>%
  ungroup()


final_dataset_with_spaces <- final_dataset %>%
  group_by(topic_label) %>%
  do(add_row(., 
             word = "",
             stemmed_word = "",
             corrected_word = "",
             dominant_topic = NA,
             topic_label = "",
             topic_probability = NA,
             rank_in_topic = NA,
             .after = nrow(.))) %>%
  ungroup()

write.csv(final_dataset_with_spaces, "comprehensive_text_analysis.csv", row.names = FALSE)

head(final_dataset)
