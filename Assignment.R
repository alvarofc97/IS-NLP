library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)


##Some adjustions for showing the visuals

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

##Loading the data: important to change the path where the dataset is stored

tweets <- read.csv("D:/Master/Frist Semester/Intelligent Systems/NLP/Entrega/tweets.csv") %>% 
  tbl_df()

#Filtering the data from because there is too many data

tweets <-
  tweets %>%
  separate(date_time, into = c("Date", "Hour"), sep = " ") %>%
  separate(Date, into = c("Day", "Month", "Period"), sep = "/",
           remove = FALSE) %>%
  mutate(Date = dmy(Date),
         Week = week(Date) %>% as.factor()) %>%
  filter(Period == 2016 )

##Some data exploration

body <- Corpus(VectorSource(tweets$content))

# convert to lower case
body <- tm_map(body, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
body <- tm_map(body, content_transformer(removeURL))

# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
body <- tm_map(body, content_transformer(removeNumPunct))

# remove extra whitespace
body <- tm_map(body, stripWhitespace)

# keep a copy for stem completion later
bodyCopy <- body

#Creation of a term document matrix
tdm <- TermDocumentMatrix(body, control = list(wordLengths = c(1, Inf)))

#Showing the most frequetn terms

freq.terms <- findFreqTerms(tdm, lowfreq = 20)

freq.terms[1:50]

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 1000)
df <- data.frame(term = names(term.freq), freq = term.freq)


library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 300,
          random.order = F, colors = pal)

## Loading the lexicon

afinn <- read.csv("D:/Master/Frist Semester/Intelligent Systems/NLP/Entrega/lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

tweets_afinn <- 
  tweets %>%
  unnest_tokens(input = "content", output = "Word") %>%
  inner_join(afinn, ., by = "Word") %>%
  mutate(Type = ifelse(Puntuacion > 0, "Positive", "Negative")) 



tweets <-
  tweets_afinn %>%
  group_by(id) %>%
  summarise(Punctuation_tweet = mean(Puntuacion)) %>%
  left_join(tweets, ., by = "id") %>% 
  mutate(Punctuation_tweet = ifelse(is.na(Punctuation_tweet), 0, Punctuation_tweet))



# Total number of Words by author
tweets_afinn %>%
  count(author)

# Unique words used by author
tweets_afinn %>% 
  group_by(author) %>% 
  distinct(Word) %>% 
  count()

#Removing words no and yes
tweets_afinn <-
  tweets_afinn %>%
  filter(Word != "no" & Word != "yes") 


map(c("Positive", "Negative"), function(feeling) {
  tweets_afinn %>%
    filter(Type ==  feeling) %>%
    group_by(author) %>%
    count(Word, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Word, n, fill = author) +
    geom_col() +
    facet_wrap("author", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = feeling) +
    tema_graf
})



## Date modeling


tweets_afinn_date <-
  tweets_afinn %>%
  group_by(id) %>%
  mutate(Sum = mean(Puntuacion)) %>%
  group_by(author, Date) %>%
  summarise(Mean = mean(Puntuacion))


tweets_afinn_date %>%
  ggplot() +
  aes(Date, Mean, color = author) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")


tweets_afinn %>%
  count(author, Type) %>%
  group_by(author) %>%
  mutate(Proportion = n / sum(n)) %>%
  ggplot() +
  aes(author, Proportion, fill = Type) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")



## Extracting the most radical authors in order to make conclusions
  
tweets_afinn_date <- 
  tweets_afinn_date %>%
  filter(author == "britneyspears" | author == "cnnbrk" | author == "rihanna" | author == "justinbieber")

tweets_afinn_date %>%
  ggplot() +
  aes(Date, Mean, color = author) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")


tweets_afinn_date %>%
  ggplot() +
  aes(Date, Mean, color = author) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(author~.) +
  tema_graf +
  theme(legend.position = "none")

tweets_afinn_date %>%
  ggplot() +
  aes(Date, Mean, color = author) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf +
  theme(legend.position = "top")


tweets_afinn_date %>%
  ggplot() +
  aes(Date, Mean, color = author) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~author) +
  tema_graf
