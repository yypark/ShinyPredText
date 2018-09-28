# global.R: defines objects available to both ui.R and server.R

library(shiny)
library(quanteda, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
library(stringi)
library(data.table)
library(wordcloud)

#########################################################
# functions to use:
#########################################################

preproc_token_DT <- function(corpus_or_chvec, set_Ngram=1) { # corpus, or char vector
    x <- iconv(corpus_or_chvec, "utf-8", "ascii", sub="") # special char ==> ascii or ""
    x <- stri_trans_tolower(x) 
    x <- tokens(x, remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_separators = TRUE,
                remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
    x <- tokens_remove(x, stopwords("english"))     # removing stopwords
    if (set_Ngram > 1) {
        x <- tokens(x, ngrams = set_Ngram)
    }
    x <- dfm(x) 
    x <- textstat_frequency(x)
    x <- data.table(x[,c("feature","frequency")])
}

preproc_inputText <- function(inputText, Ngram="trigram") {
    token_tmp <- iconv(inputText, "utf-8", "ascii", sub="")  %>% # character vector
        stri_trans_tolower() %>%
        tokens() %>%
        tokens(remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE) %>%
        tokens_remove(stopwords("english"))# removing stopwords
    if (Ngram == "trigram") {
        givenText <- stri_paste(tail(as.character(token_tmp), 2), sep = "_", collapse = "_") # for trigram search
    } else if (Ngram == "bigram") {
        givenText <- str_test_1w <- tail(as.character(token_tmp), 1) # for bigram search
    } else {
        givenText <- NULL
        stop("only trigrams and bigrams are supported")
    }
    print(givenText)
    givenText
}


#########################################################
# data loading
#########################################################

if (sum(file.exists("DT_unigram.rds", "DT_bigram_key.rds", "DT_trigram_key.rds")) == 3 ) { # when all three Ngram files exist
    DT_unigram <- readRDS("DT_unigram.rds") 
    DT_bigram <- readRDS("DT_bigram_key.rds")
    DT_trigram <- readRDS("DT_trigram_key.rds")
} else { # if any of Ngram files is missing, download and process 'Coursera-SwiftKey.zip' data
    #
    temp1 <- tempfile()
    temp2 <- tempfile()
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url=url, destfile = temp1)
    
    unzip(zipfile = temp1, exdir = temp2)
    en_us_news <- readLines(file.path(temp2, "final", "en_US", "en_US.news.txt"), encoding='UTF-8')
    en_us_blogs <- readLines(file.path(temp2, "final", "en_US", "en_US.blogs.txt"), encoding='UTF-8')
    en_us_twitter <- readLines(file.path(temp2, "final", "en_US", "en_US.twitter.txt"), encoding='UTF-8')
    #
    unlink(c(temp1, temp2))
    
    # split data into 10 equal batches
    allTexts <- c(en_us_news, en_us_blogs, en_us_twitter)
    glist <- split(allTexts, sample(1:10, length(allTexts), replace=T)) # list of 10 groups
    rm(en_us_news, en_us_blogs, en_us_twitter); gc()
    
    ##########
    # 1-,2-,and 3-grams for train data
    #
    # 1-gram:
    list_DT_unigram <- lapply(glist, function(x) { # This solves memory issue!!!!
        preproc_token_DT(x, 1)
    })
    DT_unigram_rbind <- do.call("rbind", list_DT_unigram)
    DT_unigram <- DT_unigram_rbind[,.(frequency=sum(frequency)), by=feature][order(-frequency, feature)]
    rm(list_DT_unigram, DT_unigram_rbind); gc()
    
    DT_unigram <- DT_unigram[frequency>1, ]# more than one appearance
    saveRDS(DT_unigram, file = "DT_unigram.rds")
    
    # 2-gram:
    #
    list_DT_bigram <- lapply(glist, function(x) {
        preproc_token_DT(x, 2)
    })
    DT_bigram_rbind <- do.call("rbind", list_DT_bigram)
    DT_bigram <- DT_bigram_rbind[,.(frequency=sum(frequency)), by=feature][order(-frequency, feature)]
    rm(list_DT_bigram, DT_bigram_rbind); gc()
    DT_bigram <- DT_bigram[frequency>1, ][order(-frequency, feature)]  # more than one appearance
    ### generation of 'queryKey' and 'predText' column for 'DT_bigram'
    DT_bigram[, queryKey := sub('(.*)_.*$', '\\1', feature)][]
    DT_bigram[, predText := sub('.*_(.*)$', '\\1', feature)][]
    DT_bigram[, feature := NULL][] # clean-up
    saveRDS(DT_bigram, file = "DT_bigram_key.rds")  # 
    
    # 3-gram:#
    list_DT_trigram <- lapply(glist, function(x) {
        preproc_token_DT(x, 3)
    })
    DT_trigram_rbind <- do.call("rbind", list_DT_trigram)
    DT_trigram <- DT_trigram_rbind[,.(frequency=sum(frequency)), by=feature][order(-frequency, feature)]
    rm(list_DT_trigram, DT_trigram_rbind); gc()
    DT_trigram <- DT_trigram[frequency>1, ][order(-frequency, feature)]# more than one appearance
    ### generation of 'queryKey' and 'predText' column for 'DT_trigram'
    DT_trigram[, queryKey := sub('(.*)_.*$', '\\1', feature)][]
    DT_trigram[, predText := sub('.*_(.*)$', '\\1', feature)][]
    DT_trigram[, feature := NULL][] # clean-up
    saveRDS(DT_trigram, file = "DT_trigram_key.rds")#
    
    rm(glist, allTexts); gc()
}

#####
setkey(DT_bigram, queryKey)
setkey(DT_trigram, queryKey)


#########################################################
# predictive text model: 3-gram and backoff model
#########################################################

predNextWords_DT <- function(inputText) { # return all the predicted words found as DT
    # get predicted words using backoff model
    # 1) 3-gram first
    query3gram <- preproc_inputText(inputText, "trigram")
    predWords <- DT_trigram[query3gram, on="queryKey" ][, .SD, .SDcols= "frequency", "predText"]  # all found words
    if (sum(is.na(predWords)) > 1) { # then, move to 2-gram
        query2gram <- preproc_inputText(inputText, "bigram")
        predWords <- DT_bigram[query2gram, on="queryKey" ][, .SD, .SDcols= "frequency", "predText"]  # all found words
        if (sum(is.na(predWords)) > 1) { # then, move to 1-gram
            predWords <- DT_unigram[, .SD[1:200]][sample(.N, 99)][order(-frequency)] # random selection 100 words from top 110 words
        }
    } 
    predWords
}