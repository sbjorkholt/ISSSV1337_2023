## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE,
                      comment = "##",
                      tidy = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("quanteda")

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("quanteda/quanteda.corpora")

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("kbenoit/quanteda.dictionaries")

## ---- message = FALSE---------------------------------------------------------
library("quanteda")

## -----------------------------------------------------------------------------
corp_uk <- corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(corp_uk)

## -----------------------------------------------------------------------------
docvars(corp_uk, "Party") <- names(data_char_ukimmig2010)
docvars(corp_uk, "Year") <- 2010
summary(corp_uk)

## ---- eval=FALSE--------------------------------------------------------------
#  require(readtext)
#  
#  # Twitter json
#  dat_json <- readtext("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
#  corp_twitter <- corpus(dat_json)
#  summary(corp_twitter, 5)
#  # generic json - needs a textfield specifier
#  dat_sotu <- readtext("~/Dropbox/QUANTESS/Manuscripts/collocations/Corpora/sotu/sotu.json",
#                    textfield = "text")
#  summary(corpus(dat_sotu), 5)
#  # text file
#  dat_txtone <- readtext("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt", cache = FALSE)
#  summary(corpus(dat_txtone), 5)
#  # multiple text files
#  dat_txtmultiple1 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", cache = FALSE)
#  summary(corpus(dat_txtmultiple1), 5)
#  # multiple text files with docvars from filenames
#  dat_txtmultiple2 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt",
#                               docvarsfrom = "filenames", sep = "-",
#                               docvarnames = c("Year", "President"))
#  summary(corpus(dat_txtmultiple2), 5)
#  # XML data
#  dat_xml <- readtext("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml",
#                    textfield = "COMMON")
#  summary(corpus(dat_xml), 5)
#  # csv file
#  write.csv(data.frame(inaug_speech = as.character(data_corpus_inaugural),
#                       docvars(data_corpus_inaugural)),
#            file = "/tmp/inaug_texts.csv", row.names = FALSE)
#  dat_csv <- readtext("/tmp/inaug_texts.csv", textfield = "inaug_speech")
#  summary(corpus(dat_csv), 5)

## -----------------------------------------------------------------------------
as.character(data_corpus_inaugural)[2]

## -----------------------------------------------------------------------------
summary(data_corpus_inaugural, n = 5)

## ---- fig.width = 8-----------------------------------------------------------
tokeninfo <- summary(data_corpus_inaugural)
tokeninfo$Year <- docvars(data_corpus_inaugural, "Year")
if (require(ggplot2))
    ggplot(data = tokeninfo, aes(x = Year, y = Tokens, group = 1)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(labels = c(seq(1789, 2017, 12)), breaks = seq(1789, 2017, 12)) +
    theme_bw()

# Longest inaugural address: William Henry Harrison
tokeninfo[which.max(tokeninfo$Tokens), ]

## -----------------------------------------------------------------------------
corp1 <- corpus(data_corpus_inaugural[1:5])
corp2 <- corpus(data_corpus_inaugural[53:58])
corp3 <- corp1 + corp2
summary(corp3)

## -----------------------------------------------------------------------------
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))

## -----------------------------------------------------------------------------
data_tokens_inaugural <- tokens(data_corpus_inaugural)
kwic(data_tokens_inaugural, pattern = "terror")

## -----------------------------------------------------------------------------
kwic(data_tokens_inaugural, pattern = "terror", valuetype = "regex")

## -----------------------------------------------------------------------------
kwic(data_tokens_inaugural, pattern = "communist*")

## -----------------------------------------------------------------------------
kwic(data_tokens_inaugural, pattern = phrase("United States")) %>%
    head() # show context of the first six occurrences of "United States"

## -----------------------------------------------------------------------------
# inspect the document-level variables
head(docvars(data_corpus_inaugural))

## -----------------------------------------------------------------------------
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!",
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)
tokens(txt, remove_numbers = TRUE,  remove_punct = TRUE)
tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
tokens(txt, remove_numbers = TRUE,  remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)

## -----------------------------------------------------------------------------
tokens("Great website: http://textasdata.com?page=123.", what = "character")
tokens("Great website: http://textasdata.com?page=123.", what = "character",
         remove_separators = FALSE)

## -----------------------------------------------------------------------------
# sentence level       
tokens(c("Kurt Vongeut said; only assholes use semi-colons.",
         "Today is Thursday in Canberra:  It is yesterday in London.",
         "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"),
          what = "sentence")

## -----------------------------------------------------------------------------
tokens("New York City is located in the United States.") %>%
    tokens_compound(pattern = phrase(c("New York City", "United States")))

## -----------------------------------------------------------------------------
corp_inaug_post1990 <- corpus_subset(data_corpus_inaugural, Year > 1990)

# make a dfm
dfmat_inaug_post1990 <- tokens(corp_inaug_post1990) %>%
  dfm()
dfmat_inaug_post1990[, 1:5]

## -----------------------------------------------------------------------------
# make a dfm, removing stopwords and applying stemming
dfmat_inaug_post1990 <- dfm(dfmat_inaug_post1990,
                            remove = stopwords("english"),
                            stem = TRUE, remove_punct = TRUE)
dfmat_inaug_post1990[, 1:5]

## -----------------------------------------------------------------------------
head(stopwords("en"), 20)
head(stopwords("ru"), 10)
head(stopwords("ar", source = "misc"), 10)

## ----warning=FALSE, fig.width = 8, fig.height = 8-----------------------------
dfmat_uk <- tokens(data_char_ukimmig2010, remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  dfm()
dfmat_uk

## -----------------------------------------------------------------------------
topfeatures(dfmat_uk, 20) # 20 most frequent words

## ----warning=FALSE, fig.width = 7, fig.height = 7-----------------------------
set.seed(100)
library("quanteda.textplots")
textplot_wordcloud(dfmat_uk, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

## -----------------------------------------------------------------------------
dfmat_pres <- tail(data_corpus_inaugural, 20) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  dfm() %>%
  dfm_group(groups = Party)

## -----------------------------------------------------------------------------
dfm_sort(dfmat_pres)

## -----------------------------------------------------------------------------
corp_inaug_post1991 <- corpus_subset(data_corpus_inaugural, Year > 1991)

## -----------------------------------------------------------------------------
dict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                        economy = c("jobs", "business", "grow", "work")))

## -----------------------------------------------------------------------------
dfmat_inaug_post1991_dict <- tokens(corp_inaug_post1991) %>%
  tokens_lookup(dictionary = dict) %>%
  dfm()
dfmat_inaug_post1991_dict

## ---- eval = FALSE------------------------------------------------------------
#  dictliwc <- dictionary(file = "LIWC2001_English.dic", format = "LIWC")
#  dfmat_inaug_subset <- dfm(tokens(data_corpus_inaugural[52:58]), dictionary = dictliwc)
#  dfmat_inaug_subset[, 1:10]

## ----fig.width = 6, fig.height = 3, eval = FALSE------------------------------
#  library("quanteda.textstats")
#  dfmat_inaug_post1980 <- corpus_subset(data_corpus_inaugural, Year > 1980) %>%
#    tokens(remove_punct = TRUE) %>%
#    tokens_wordstem(language = "en") %>%
#    tokens_remove(stopwords("en")) %>%
#    dfm()
#  tstat_obama <- textstat_simil(dfmat_inaug_post1980,
#                                dfmat_inaug_post1980[c("2009-Obama", "2013-Obama"), ],
#                                margin = "documents", method = "cosine")
#  as.list(tstat_obama)
#  dotchart(as.list(tstat_obama)$"2013-Obama", xlab = "Cosine similarity", pch = 19)

## ---- eval = FALSE------------------------------------------------------------
#  data_corpus_sotu <- readRDS(url("https://quanteda.org/data/data_corpus_sotu.rds"))
#  dfmat_sotu <- corpus_subset(data_corpus_sotu, Date > as.Date("1980-01-01")) %>%
#    tokens(remove_punct = TRUE) %>%
#    tokens_wordstem(language = "en") %>%
#    tokens_remove(stopwords("en")) %>%
#    dfm()
#  dfmat_sotu <- dfm_trim(dfmat_sotu, min_termfreq = 5, min_docfreq = 3)

## ----echo = FALSE-------------------------------------------------------------
load("../tests/data/dfmat_sotu.rda")

## ---- fig.width = 8, fig.height = 5, eval = FALSE-----------------------------
#  # hierarchical clustering - get distances on normalized dfm
#  tstat_dist <- textstat_dist(dfm_weight(dfmat_sotu, scheme = "prop"))
#  # hiarchical clustering the distance object
#  pres_cluster <- hclust(as.dist(tstat_dist))
#  # label with document names
#  pres_cluster$labels <- docnames(dfmat_sotu)
#  # plot as a dendrogram
#  plot(pres_cluster, xlab = "", sub = "",
#       main = "Euclidean Distance on Normalized Token Frequency")

## ---- eval = FALSE------------------------------------------------------------
#  tstat_sim <- textstat_simil(dfmat_sotu, dfmat_sotu[, c("fair", "health", "terror")],
#                            method = "cosine", margin = "features")
#  lapply(as.list(tstat_sim), head, 10)

## ----fig.width = 7, fig.height = 5--------------------------------------------
if (require("quanteda.textmodels") && require("quanteda.textplots")) {
  dfmat_ire <- dfm(tokens(data_corpus_irishbudget2010))
  tmod_wf <- textmodel_wordfish(dfmat_ire, dir = c(2, 1))
  
  # plot the Wordfish estimates by party
  textplot_scale1d(tmod_wf, groups = docvars(dfmat_ire, "party"))
}

## -----------------------------------------------------------------------------
if (require("quanteda.textmodels")) {
    quant_dfm <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE, remove_numbers = TRUE) %>%
        tokens_remove(stopwords("en")) %>%
        dfm()
    quant_dfm <- dfm_trim(quant_dfm, min_termfreq = 4, max_docfreq = 10)
    quant_dfm 
}

## ----fig.width = 7, fig.height = 5--------------------------------------------
if (require("stm") && require("quanteda.textmodels")) {
    set.seed(100)
    my_lda_fit20 <- stm(quant_dfm, K = 20, verbose = FALSE)
    plot(my_lda_fit20)    
}

