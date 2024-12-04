#!/usr/bin/env Rscript
#
# TRAIN data 
#
################################################################################

library( 'quanteda' )
library( 'caret' )


# read train data
df_train <- data.frame( readr::read_tsv( 'out.00.analyse.1A//df_train.tsv', show_col_types = FALSE ) )

# import stop words
stopwords <- read.csv( 'data/stopwords.csv', row.names = 1 )$stopwords


# make corpus
corpus_train <- corpus( df_train$fsc_letter_without_considerations_andEEG )
docvars( corpus_train, "group" ) <- df_train$fu_diagnosis_after_FU # was df_train$group (!)


# pre-process
toks_train <- tokens( corpus_train, remove_punct = TRUE, remove_symbols = TRUE, 
                     remove_url = TRUE, remove_separators = TRUE ) %>% tokens_tolower()

# n-gram
toks_ngram_train <- tokens_ngrams( toks_train, n = 1:2 )

# clean
toks_clean_train <- tokens_remove( toks_ngram_train, stopwords )

# make dfm
dfm_train <- dfm( toks_clean_train )

# make tf-idf
dfm_train_tfidf <- dfm_tfidf( dfm_train )


################################################################################
### RFE ###
################################################################################
set.seed( 567 )


# Selecteer de top 8000 meest frequente features
top_features <- topfeatures( dfm_train_tfidf, n = 8000 )
dfm_train_tfidf_top8000 <- dfm_select( dfm_train_tfidf, pattern = names( top_features ) )

# Converteer naar data frame
dfm_train_tfidf_top8000_df <- convert( dfm_train_tfidf_top8000, to = "data.frame" )
  
# Verwijder niet-numerieke kolommen als die er zijn
dfm_train_tfidf_top8000_df_clean <- dfm_train_tfidf_top8000_df[ , sapply(dfm_train_tfidf_top8000_df, is.numeric ) ]
  
# Zorg ervoor dat de doelvariabele een factor is
df_train$fu_diagnosis_after_FU <- as.factor( df_train$fu_diagnosis_after_FU ) # ! was: as.factor( df_train$group )
  
# Stel de rfeControl in
ctrl <- rfeControl( functions = rfFuncs, method = "cv", number = 2, verbose = TRUE )
  
# Voer de RFE uit en selecteer 300 features
rfe_results <- rfe( dfm_train_tfidf_top8000_df_clean, df_train$fu_diagnosis_after_FU, sizes = 300, rfeControl = ctrl )

# get predictors
preds <- predictors( rfe_results )

# save all objects to disk (will be loaded in next script)
save( list = ls() , file = "environment.Rdata" )
