library(rvest)

url = "http://www.imdb.com/name/nm0000233/?ref_=nv_sr_1"
title_links <- read_html(url) %>% html_nodes('b a') %>% html_attr("href") 
title_text <- read_html(url) %>% html_nodes('b a') %>% html_text 


dim(title_links)
dim(title_text)
title = title_text[-1,]
df = cbind(title_text, title_links)

df


Filmography = df[1:28,]

film = as.data.frame(Filmography)
film$title = paste0("http://www.imdb.com/title",substr(film$title_links, 7, 17), "keywords?ref_=tt_stry_kw")
film

m = "http://www.imdb.com/title/tt7713358/keywords?ref_=tt_stry_kw"
get_keywords = function(m) {
  p = read_html(m) %>% html_nodes(xpath='//*[@id="keywords_content"]/table')
  p = p %>% rvest::html_table() %>%
    purrr::map_df(~ janitor::clean_names(.))
  # x1 = data_frame(word = p$x1)
  # x2 = data_frame(word = p$x2)
  # p = bind_rows(x1, x2)
 return(p)
}

  
film$words = mapply(get_keywords, film$title)

mydf = film %>% 
  mutate(keywords = strsplit(as.character(w1), ",")) %>% 
  unnest(keywords)

mydf$keywords = sub("\\\\.*", "", mydf$keywords)
mydf$keywords = gsub("^c\\(|\\)$", "", mydf$keywords)
mydf$keywords = gsub("\"", "",mydf$keywords)
mydf$keywords = gsub("Is this relevant? Relevant? Yes No","",mydf$keywords)
View(mydf)

mydf$w1 = NULL

mydf %>% group_by(keywords) %>% anti_join(nrc, by ="keywords")


library(purrr)
library(janitor)

html_nodes(p, 'tbody') %>% head
html_nodes(p, 'table')
str(html_attr(p)[1:3])

page = read_html(url)


movie.nodes <- html_nodes(page,'b')

movie.link = sapply(html_attrs(movie.nodes, 'href'))
movie.link = paste0("http://www.imdb.com",movie.link)
movie.cast = sapply(html_attrs(movie.nodes),`[[`,'title')
movie.name = html_text(movie.nodes)

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

[1] "Sing"          "Moana"         "Moonlight"     "Hacksaw Ridge"

[5] "Passengers"    "Trolls"


url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

library(cleanNLP)

text = c("In Nazi-occupied France during World War II, a plan to assassinate Nazi leaders by a group of Jewish U.S. soldiers coincides with a theatre owner's vengeful plans for the same.")

text <- paste(text, collapse = " ")

library(cleanNLP)
init_spaCy()
obj <- run_annotators(text, as_strings = TRUE)
get_token(obj)


get_token(obj) %>%
  filter(upos == "NOUN") %>%
  group_by(lemma) %>%
  summarize(count = n()) %>%
  top_n(n = 42, count) %>%
  arrange(desc(count))



get_dependency(obj, get_token = TRUE) %>%
  left_join(get_document(obj)) %>%
  filter(year == 2001) %>%
  filter(relation == "dobj") %>%
  select(id = id, start = word, word = lemma_target) %>%
  left_join(word_frequency) %>%
  filter(frequency < 0.0005) %>%
  select(id, start, word) %>%
  sprintf("%s => %s", start, word)
