set.seed(1234)

library(tidyverse)
library(vosonSML) 
library(dplyr) 
library(textclean) 
library(stringi)   
library(tidytext)  
library(tm)       
library(oii)       
library(lubridate)
library(igraph)    
library(ggpubr)    
library(ggraph)   
library(ggplot2)    
library(wordcloud2)
library(psych) 
library(textdata)  
library(tidyr)     
library(vader)     
library(topicmodels)
library(gmodels)
library(oii)


videos <- c("https://www.youtube.com/watch?v=ndl1W4ltcmg",
            "https://www.youtube.com/watch?v=2aMVzFlApa0",
            "https://www.youtube.com/watch?v=SzS8Ao0H6Co",
            "https://www.youtube.com/watch?v=HK9dU6JIroU")

youtubeAuth <- Authenticate("youtube", apiKey = "AIzaSyBa_MzxyNVzCGBVT6vgqNkdVD-MNfoYTHA")

df <- youtubeAuth %>%
  Collect(videos, verbose = T, writeToFile = F)

write.csv(df, "df_witcher_new.csv")

View(df)
str(df)

df <- df %>%
  mutate(LikeCount = as.numeric(LikeCount),
         is_viral = ifelse(LikeCount > 100, TRUE, FALSE),
         ReplyCount = as.numeric(ReplyCount),
         date_formatted = ymd_hms(PublishedAt),
         filmID = recode(VideoID,
                         "ndl1W4ltcmg" = "The Witcher - S1",
                         "2aMVzFlApa0" = "The Witcher - S2",
                         "SzS8Ao0H6Co" = "The Witcher - S3",
                         "HK9dU6JIroU" = "The Witcher - S4"),
         row_num = row_number())

write.csv(df, "df_witcher_new_raw.csv")

########################
######## EXAM ##########
########################


set.seed(1234)

library(tidyverse)
library(vosonSML) 
library(dplyr) 
library(textclean) 
library(stringi)   
library(tidytext)  
library(tm)       
library(oii)       
library(lubridate)
library(igraph)    
library(ggpubr)    
library(ggraph)   
library(ggplot2)    
library(wordcloud2)
library(psych) 
library(textdata)  
library(tidyr)     
library(vader)     
library(topicmodels)
library(gmodels)
library(oii)

videos <- c("https://www.youtube.com/watch?v=ndl1W4ltcmg",
            "https://www.youtube.com/watch?v=2aMVzFlApa0",
            "https://www.youtube.com/watch?v=SzS8Ao0H6Co",
            "https://www.youtube.com/watch?v=HK9dU6JIroU")

youtubeAuth <- Authenticate("youtube", apiKey = "AIzaSyC3GZ0wcvhxUD_aNOzqgpjAFX7VDewUa28")

df <- youtubeAuth %>%
  Collect(videos, verbose = T, writeToFile = F)


write.csv(df, "df_witcher.csv")
df <- read.csv("df_witcher_new.csv")


df <- df %>%
  mutate(LikeCount = as.numeric(LikeCount),
         is_viral = ifelse(LikeCount > 100, TRUE, FALSE),
         ReplyCount = as.numeric(ReplyCount),
         date_formatted = ymd_hms(PublishedAt),
         filmID = recode(VideoID,
                         "ndl1W4ltcmg" = "The Witcher - S1",
                         "2aMVzFlApa0" = "The Witcher - S2",
                         "SzS8Ao0H6Co" = "The Witcher - S3",
                         "HK9dU6JIroU" = "The Witcher - S4"),
         row_num = row_number())

#####Velocity ####

df %>%
  count(date_formatted, sort = T) %>% #counts how many reviews were posted on each date 
  ggplot(aes(x = date_formatted, y = n)) +
  scale_y_log10() +  # ask if we keep log or not / compare Feia 
  geom_smooth(fill = "pink")

describe(df$Comment)

## VARIETY = distribution of comment length###

# comments have on average 19.12 words, 
# but there is a lot of variance with a maximum of 765 words
# here is a skewness of 4.77 which is extremely high, so a few very long comments impact the distribution with a long right tail 

describe(batman$Nwords_clean)


cleaning <- df %>%
  mutate(clean_text = Comment, # create clean_text variable
         clean_text = tolower(clean_text)) %>%
  mutate(clean_text = gsub("[\u2018\u2019]", "'", clean_text)) %>% # replace curly quotation with straight quotation 
  mutate(clean_text = gsub("[\u201C\u201D]", "\"", clean_text)) %>% # replace curly quotation with straight quotation 
  mutate(clean_text = replace_contraction(clean_text),  
         clean_text = replace_word_elongation(clean_text),
         clean_text = replace_url(clean_text, replacement =  "URL"),
         clean_text = replace_email(clean_text, replacement = "EMAIL"),
         clean_text = replace_hash(clean_text, replacement = "HASHTAG"),
         clean_text = replace_tag(clean_text, replacement = "USER")) %>%
  mutate(clean_text = gsub("\\b[A-Za-z]\\b", "", clean_text)) %>% # removes standalone letters with regex 
  mutate(clean_text = gsub("[[:punct:]]", "", clean_text)) %>% # removes punctation with regex 
  mutate(clean_text = replace_non_ascii(clean_text)) %>% # removes emojis entirely instead of keeping the text version of them 
  mutate(clean_text = replace_white(clean_text)) %>%
  mutate(clean_text = removeNumbers(clean_text))


df <- cleaning %>%
  left_join(cleaning)



df_with_vader <- vader_df(df$Comment)

#add row_num() identifier
df_with_vader <- df_with_vader %>%
  mutate(row_num = row_number())


#store the data
write.csv(df_with_vader, "df_review_with_vader.csv")

df_ <- read.csv("vader.csv")

df_ %>%
  group_by(filmID) %>%
  summarize(mean_pos = mean(pos.y, na.rm = T),
            mean_neg = mean(neg.y, na.rm = T),
            mean_ang = mean(neu.y, na.rm = T),
            mean_comp = mean(compound.y, na.rm = T))


df$filmID <- as.factor(df$filmID)
describeBy(df_$compound.y, group = df_$filmID, na.rm = TRUE)

describeBy(df_$compound.y, group = df$filmID, na.rm = TRUE)



filter(has_Cavill == T)


df_2 <- read.csv("df_cleaner.csv")

df_2 <- df_2 %>%
  mutate(authors = str_detect(Comment, "henrycavill"))

cortest()

oii.freq(df_2$authors)

describeBy(df_2$authors, group = df_2$filmID)
t.test(df_2$authors~df_2$filmID)

comments <- df_2 %>%
  count(authors, sort = TRUE)


df_2 %>%
  group_by(filmID) %>%
  count(authors, sort = TRUE)

describeBy(df_2$authors, group = df_2$filmID) # you look at the mean score in order to fill in the table 
fitauthors <- aov(authors~filmID, data = df_2)
summary(fitauthors) # F-statistic and p-value for test statistic 
TukeyHSD(fitauthors) # shows which movie(s) differ significantly


prop.table()


df_2 %>%
  mutate(filmID = as.numeric(filmID))
         
as.numeric()
cor.test(df_2$filmID, df_2$authors)

36878 - 32358 
10630 / 12691 - 10630
9622 / 10491 - 9622
15426 / 17068 - 15426
#####Velocity ####

df %>%
  count(date_formatted, sort = T) %>% #counts how many reviews were posted on each date 
  ggplot(aes(x = date_formatted, y = n)) +
  scale_y_log10() +  # ask if we keep log or not / compare Feia 
  geom_smooth(fill = "pink")

## VARIETY = distribution of comment length###

# comments have on average 19.12 words, 
# but there is a lot of variance with a maximum of 765 words
# here is a skewness of 4.77 which is extremely high, so a few very long comments impact the distribution with a long right tail 

df <- read.csv("df_cleaner.csv")
View(df)
#df %>%
  ggplot(aes(x = Comment, fill = Comment)) +
  geom_histogram() +
  facet_wrap(~VideoID) +
  theme_minimal()

g_uni <- df %>%
  unnest_tokens(input = cleaner_text, output = cleaner)

g_uni %>% count(cleaner, sort=TRUE) %>% top_n(10)

gram_uni <- g_uni %>% count(cleaner, sort=TRUE) %>% top_n(10)%>%
  ggplot(aes(x= reorder(cleaner, n), y= n)) +
  geom_bar(stat ="identity", fill= "darkgreen")+
  coord_flip()+
  theme_classic()+
  labs(title="Cleaned words; no stopwords",
       x = "Words",
       y = "Frequency")
plot(gram_uni)

gram_bi <- df %>%
  dplyr::select(cleaner_text) %>%
  unnest_tokens(paired_words, cleaner_text, token = "ngrams", n = 2)

gram_bi %>%
  filter(!is.na(paired_words)) %>%
  count(paired_words, sort = T)

gram_bi %>%
  count(paired_words, sort = T)  %>% 
  filter(!is.na(paired_words)) %>%
  top_n(10)%>%
  ggplot(aes(x= reorder(paired_words, n), y= n)) +
  geom_bar(stat ="identity", fill= "lightblue")+
  coord_flip()+
  theme_classic()+
  labs(title="Most frequent bigrams",
       x = "Bigram",
       y = "Frequency")

gram_net1 <- gram_bi %>%
  tidyr::separate(paired_words, c("word1", "word2"), sep = " ")

gram_net2 <- gram_net1 %>%
  count(word1, word2, sort = T, na.rm = T) %>%
  filter(!row_number() %in% 1)


library(igraph)

gram_net2 %>%
  filter(n >= 150) %>%
  filter(!is.na(word1))%>%

  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "blue", size = 3, alpha = 0.5) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 4) + 
  geom_edge_link0(alpha = 0.6) +
  labs(title = "Bigram network, 100+ appearances") + 
  theme(panel.background = element_rect("white"))

##gemni
gram_net_count <- gram_bi %>%
  # 过滤 NA 值：在分割前过滤，防止 NA 破坏 tidyr::separate
  filter(!is.na(paired_words)) %>%
  # 将双词组分割成两个单独的词：word1 和 word2
  # ngrams 的分隔符通常是单个空格 " "
  tidyr::separate(paired_words, c("word1", "word2"), sep = " ", remove = FALSE) %>% 
  # 再次检查，确保 word1 或 word2 不是 NA
  filter(!is.na(word1), !is.na(word2)) %>%
  # 统计每对词 (word1, word2) 出现的频率 (n)
  count(word1, word2, sort = TRUE) 

# --- 4. 绘制 Bigram 共现网络图 (修复筛选条件) ---

# 使用 igraph 和 ggraph 绘制网络图
gram_net_count %>%
  # 筛选频率 n >= 150 的边，您可以根据数据调整这个阈值
  filter(n >= 150) %>%
  
  # 使用 graph_from_data_frame() 从数据框创建图对象
  # word1 对应 "from" (来源节点), word2 对应 "to" (目标节点)
  graph_from_data_frame() %>% 
  
  # 使用 ggraph 绘制图表
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = n), show.legend = FALSE, width = 0.5) + # 根据 n 值调整边的透明度/粗细
  geom_node_point(color = "blue", size = 3) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 4) + # 使用 repel 防止标签重叠
  
  labs(title = "Bigram Network (Frequency >= 150)") +
  theme_void() # 使用 theme_void 清理背景，更适合网络图
##gemni

#Topic modeling requires a Document-Term Matrix / Document-Feature Matrix


tokenized_df <- df %>%
  select(row_num, clean_text) %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(stop_words, by = "word")

dtm <- tokenized_df %>%
  count(row_num, word) %>%
  cast_dtm(row_num, word, n) 

k <- 15

#Run the model; build-up the function, step by step
lda_model <- LDA(dtm, #document-term matrix
                 k = k, #number of expected topics == subjective, based on trial and error!
                 control = list(seed = 1234),  
                 method = "Gibbs") #we use Gibbs sampling in the LDA model


#show the top 10 terms
terms(lda_model, 10)

#create dataframe with all words and their beta scores
topics <- tidy(lda_model, matrix = "beta")

#get all words, per topic and their associated beta scores
top_words_per_topic <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot that
top_words_per_topic %>%
  ggplot(
    aes(x=reorder(term, beta), 
        y=beta, 
        fill = factor(topic))
  )+
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

####建模第二轮：

k <- 10

#Run the model; build-up the function, step by step
lda_model <- LDA(dtm, #document-term matrix
                 k = k, #number of expected topics == subjective, based on trial and error!
                 control = list(seed = 1234),  
                 method = "Gibbs") #we use Gibbs sampling in the LDA model


#show the top 10 terms
terms(lda_model, 10)

#create dataframe with all words and their beta scores
topics <- tidy(lda_model, matrix = "beta")

#get all words, per topic and their associated beta scores
top_words_per_topic <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot that
top_words_per_topic %>%
  ggplot(
    aes(x=reorder(term, beta), 
        y=beta, 
        fill = factor(topic))
  )+
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

####建模第三轮：

k <- 8

lda_model <- LDA(dtm, 
                 k = k, 
                 control = list(seed = 1234),  
                 method = "Gibbs") 


topics <- tidy(lda_model, matrix = "beta")


top_words_per_topic <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_words_per_topic %>%
  ggplot(
    aes(x=reorder(term, beta), 
        y=beta, 
        fill = factor(topic))
  )+
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#### Let's get the topics into our base dataframe
lda.results <- posterior(lda_model)
beta <- lda.results$terms
theta <- lda.results$topics #look at film 2, for example. Topic appears nicely
View(theta)

#so now we want to add all theta scores to dataframe

#add topic
colnames(theta) <- paste0("topic_", colnames(theta))

theta <- theta %>%
  as.data.frame() %>%
  rename(
    T_Character_Casting_Controversy = topic_1,
    T_Season_Hype_Viewing_Expectation = topic_2,
    T_Trailer_Specific_Geralt_Elements = topic_3,
    T_NetflixEmotional_Feedback = topic_4,
    T_General_Online_Chatter = topic_5,
    T_Source_Material_Woke_Debate = topic_6,
    T_Book_Game_Comparison = topic_7,
    T_Lead_Actor_Change_Replacement = topic_8)%>%
  mutate(row_num = row_number())



#join them together
df <- df %>%
  left_join(theta)

View((df))

write.csv(df, "df_topic.csv")
topic_cols <- df %>% select(starts_with("T_")) 

df <- df %>% 
  mutate(
    dominant_topic = colnames(topic_cols)[
      max.col(as.matrix(topic_cols))
      ]
    )


df %>% 
  count(dominant_topic, sort=T)%>% 
  filter(!is.na(dominant_topic))%>%
  ggplot(aes(x=reorder(dominant_topic, n), y = n)) + 
  geom_col(fill = "lavender") + 
  coord_flip()+
  ylab("frequency") + 
  xlab("topics")+
  theme_minimal()

CrossTable(df$filmID, df$dominant_topic, chisq = TRUE)

df %>%
  group_by(filmID) %>%
  count(dominant_topic, sort = T) %>%
  filter(!is.na(dominant_topic)) %>%
  mutate(dominant_topic = reorder_within(dominant_topic, n, filmID)) %>%
  ggplot(aes(x=dominant_topic, y=n, fill=filmID)) +
  geom_col() +
  coord_flip() +
  ylab("frequency") +
  xlab("topics")+
  theme_minimal() +
  facet_wrap(~filmID, scale = "free") +
  scale_x_reordered()


df<- df %>%
  mutate(authors = str_detect(Comment, "henry|cavill"))
describeBy(df$authors,group = df$filmID)
t.test(df_2$filmID~df_2$authors)

32358 / 36878
10630 / 12691
9622 / 10491
15426 / 17068