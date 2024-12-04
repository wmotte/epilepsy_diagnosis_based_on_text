#!/usr/bin/env Rscript
#
# TRAIN data, and TEST data with two co-variates added to the naive base model
#
################################################################################


################################################################################
# BEGIN CUSTOM FUNCTIONS
################################################################################

###
# Function to visualize top tf-idf
##
visualize_top_tfidf_with_plot <- function( top_tfidf_words_train_final, outdir )
{
    # Visualiseer de top 30 TF-IDF woorden
    p <- ggplot( top_tfidf_words_train_final, aes(x = reorder(feature, frequency), y = frequency)) +
        geom_bar(stat = "identity", fill = "gray80", color = "gray30") +
        coord_flip() +
        labs( x = "Features (top 30)",
              y = "TF-IDF Frequency") +
        theme_minimal() +
        scale_fill_manual(values = rep("darkslategray3", 30))
    
    # write image to disk
    ggsave( plot = p, file = paste0( outdir, '/top_30_features.png' ),  dpi = 600, width = 4, height = 6, bg = 'white' )
}


###
# Function for visualization
##
visualize_top_features <- function( nb_model, outdir )
{
    ##visualiseren features##
    # Visualiseer de top 30 positieve features
    top_features <- sort( nb_model$param["epilepsy", ] / colSums( nb_model$param ), decreasing = TRUE)[1:30]
    
    # Zet dit om in een data.frame
    top_features_df <- data.frame( Feature = names(top_features), Importance = top_features )
    
    # Bekijk de data
    print( head( top_features_df) )
    
    # Maak de barplot
    p2 <- ggplot(top_features_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
        geom_bar(stat = "identity", fill = "gray80", color = "gray30") +
        coord_flip() +
        labs( x = "Features (top 30) - epilepsy group",
              y = "TF-IDF Frequency") +
        theme_minimal() +
        scale_fill_manual(values = rep("darkslategray3", 30))
    
    # Visualiseer de top 30 negatieve features
    top_negative_features <- sort(nb_model$param["no epilepsy", ] / colSums(nb_model$param), decreasing = TRUE)[1:30]
    
    # Zet dit om in een data.frame
    top_negative_features_df <- data.frame( Feature = names(top_negative_features), Importance = top_negative_features )
    
    # Maak de barplot
    p3 <- ggplot(top_negative_features_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
        geom_bar( stat = "identity", fill = "gray80", color = "gray30" ) +
        coord_flip() +
        labs( x = "Features (top 30) - no epilepsy group",
              y = "TF-IDF Frequency") +
        theme_minimal() +
        scale_fill_manual(values = rep("darkslategray3", 30))
    
    # write image to disk
    ggsave( plot = p2, file = paste0( outdir, '/top_30_features__epilepsy.png' ),  dpi = 600, width = 4, height = 6, bg = 'white' )
    # write image to disk
    ggsave( plot = p3, file = paste0( outdir, '/top_30_features__no_epilepsy.png' ),  dpi = 600, width = 4, height = 6, bg = 'white' )
}

###
# Get performance based on 2x2 table.
##
get_performance_mtx <- function( mtx )
{
    # get cells
    TP <- mtx[ 'epilepsy', 'epilepsy' ]
    TN <- mtx[ 'no epilepsy', 'no epilepsy' ]
    
    FP <- mtx[ 'epilepsy', 'no epilepsy' ]
    FN <- mtx[ 'no epilepsy', 'epilepsy' ]
    
    # Total
    N <- TP + TN + FP + FN
    
    ## Sensitivity with 95% confidence interval
    sensMean <- caret::sensitivity( mtx )
    sens_errors <- sqrt( caret::sensitivity( mtx ) * ( 1 - caret::sensitivity( mtx ) ) / sum( mtx[ , 1 ] ) )
    sensLower <- caret::sensitivity( mtx ) - 1.96 * sens_errors
    sensUpper <- caret::sensitivity( mtx ) + 1.96 * sens_errors
    
    ## Specificity with 95% confidence interval
    specMean <- caret::specificity( mtx )
    spec_errors <- sqrt( caret::specificity( mtx ) * ( 1 - caret::specificity( mtx ) ) / sum( mtx[ , 2 ] ) )
    specLower <- caret::specificity( mtx ) - 1.96 * spec_errors
    specUpper <- caret::specificity( mtx ) + 1.96 * spec_errors
    
    ## Positive Predictive Value with 95% confidence interval
    ppvMean <- caret::posPredValue( mtx )
    ppv_errors <- sqrt( caret::posPredValue( mtx ) * ( 1 - caret::posPredValue( mtx ) ) / sum( mtx[ 1 , ] ) )
    ppvLower <- caret::posPredValue( mtx ) - 1.96 * ppv_errors
    ppvUpper <- caret::posPredValue( mtx ) + 1.96 * ppv_errors
    
    ## Negative Predictive Value with 95% confidence interval
    npvMean <- caret::negPredValue( mtx )
    npv_errors <- sqrt( caret::negPredValue( mtx ) * ( 1 - caret::negPredValue( mtx ) ) / sum( mtx[ 2 , ] ) )
    npvLower <- caret::negPredValue( mtx ) - 1.96 * npv_errors
    npvUpper <- caret::negPredValue( mtx ) + 1.96 * npv_errors
    
    ## Accuracy with 95% confidence interval
    additional <- caret::confusionMatrix( mtx )
    acc <- additional$overall[ c( 'Accuracy', 'AccuracyLower', 'AccuracyUpper' ) ]
    accMean <- acc[ 1 ]
    accLower <- acc[ 2 ]
    accUpper <- acc[ 3 ]
    
    ## Balanced accuracy (average of sens/spec)
    baccMean <- ( sensMean + specMean ) / 2
    baccLower <- ( sensLower + specLower ) / 2
    baccUpper <- ( sensUpper + specUpper ) / 2
    
    if( sensLower < 0 )
        sensLower <- 0
    
    if( specLower < 0 )
        specLower <- 0
    
    if( ppvLower < 0 )
        ppvLower <- 0
    
    if( npvLower < 0 )
        npvLower <- 0
    
    if( specUpper > 1 )
        specUpper <- 1
    
    if( sensUpper > 1 )
        sensUpper <- 1
    
    if( ppvUpper > 1 )
        ppvUpper <- 1
    
    # collect output
    out <- cbind( N, TP, TN, FP, FN,
                  baccMean = round( baccMean, 2 ), baccLower = round( baccLower, 2 ), baccUpper = round( baccUpper, 2 ),
                  accMean = round( accMean, 2 ), accLower = round( accLower, 2 ), accUpper = round( accUpper, 2 ),
                  sensMean = round( sensMean, 2 ), sensLower = round( sensLower, 2 ), sensUpper = round( sensUpper, 2 ),
                  specMean = round( specMean, 2 ), specLower = round( specLower, 2 ), specUpper = round( specUpper, 2 ),
                  ppvMean = round( ppvMean, 2 ), ppvLower = round( ppvLower, 2 ), ppvUpper = round( ppvUpper, 2 ),
                  npvMean = round( npvMean, 2 ), npvLower = round( npvLower, 2 ), npvUpper = round( npvUpper, 2 ) )
    
    rownames( out ) <- NULL
    out <- data.frame( out )
    
    # collect relevant    
    p1 <- paste0( "N: ", out$N )
    p2 <- paste0( "TP: ", out$TP )
    p3 <- paste0( "TN: ", out$TN )
    p4 <- paste0( "FP: ", out$FP )
    p5 <- paste0( "FN: ", out$FN )
    
    p6 <- paste0( "Accuracy: ", out$accMean, " (95%CI: ", out$accLower, '–', out$accUpper, ")" )
    p7 <- paste0( "Sensitivity: ", out$sensMean, " (95%CI: ", out$sensLower, '–', out$sensUpper, ")" )
    p8 <- paste0( "Specificity: ", out$specMean, " (95%CI: ", out$specLower, '–', out$specUpper, ")" )
    p9 <- paste0( "PPV: ", out$ppvMean, " (95%CI: ", out$ppvLower, '–', out$ppvUpper, ")" )
    p10 <- paste0( "NPV: ", out$npvMean, " (95%CI: ", out$npvLower, '–', out$npvUpper, ")" )
    
    out <- data.frame( stats = c( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 ) )
    
    return( out )
}




################################################################################
# END CUSTOM FUNCTIONS
################################################################################

library( 'quanteda' )
library( 'quanteda.textmodels' )
library( 'quanteda.textstats' )
library( 'caret' )
library( 'ggplot2' )
library( 'Matrix' )

# load all saved R-ojbects from disk into memory
load( "environment.Rdata" )

# outdir 
outdir <- 'out.02.analyse.1Ac'
dir.create( outdir, showWarnings = FALSE )

# load test data
df_test <- data.frame( readr::read_tsv( 'out.00.analyse.1A/df_test.tsv', show_col_types = FALSE ) )

###corpus aanmaken voor de test set ###
corpus_test <- corpus( df_test$fsc_letter_without_considerations_andEEG ) 
docvars( corpus_test, "fu_diagnosis_after_FU" ) <- df_test$fu_diagnosis_after_FU
summary( corpus_test )


###preprocessing op het corpus van de test set###
toks_test <- tokens( corpus_test, remove_punct = TRUE, remove_symbols = TRUE, 
                     remove_url = TRUE, remove_separators = TRUE ) %>% tokens_tolower()
toks_ngram_test <- tokens_ngrams( toks_test, n = 1:2 )
toks_clean_test <- tokens_remove( toks_ngram_test, stopwords )

###DFM aanmaken voor de test set###
dfm_test <- dfm( toks_clean_test )

###TF-IDF berekenen voor de test set###
dfm_test_tfidf <- dfm_tfidf( dfm_test )

### verder werken met de top 300 van de top 8000 features ###
dfm_train_tfidf_final <- dfm_select( dfm_train_tfidf, pattern = preds )
dfm_test_tfidf_final <- dfm_select( dfm_test_tfidf, pattern = preds )
dfm_test_tfidf_final <- dfm_match( dfm_test_tfidf_final, features = featnames( dfm_train_tfidf_final ) )


### Frequentie-analyse op de TF-IDF DFM van de trainingset ###
tstat_freq_train_tfidf_final <- textstat_frequency( dfm_train_tfidf_final, force = TRUE )
head( tstat_freq_train_tfidf_final, 30 )

# Selecteer de top 30 woorden
top_tfidf_words_train_final <- head( tstat_freq_train_tfidf_final[ 
                                order(-tstat_freq_train_tfidf_final$frequency ), ], 30 )
  

# visualized with ggplot
visualize_top_tfidf_with_plot( top_tfidf_words_train_final, outdir )


#######################################################    
########### add co-variates to the model (age, gender )
#######################################################

covariate1 <- df_train$p_sex
covariate2 <- df_train$p_age_fsle

covariate1[ covariate1 %in% 'female' ] <- 0
covariate1[ covariate1 %in% 'male' ] <- 1

# select covariates
additional_covariates <- data.frame( covariate1 = as.numeric( covariate1 ), 
                                    covariate2 = as.numeric( covariate2 ) )


# Convert covariates to a matrix
covariates_matrix <- as.matrix( additional_covariates )

# Convert to sparse matrix format
covariates_sparse <- Matrix( covariates_matrix, sparse = TRUE )

# Create a DFM-like object from the sparse matrix
dfm_covariates <- as.dfm( covariates_sparse )

# Make sure the number of rows matches your DFM
nrow( additional_covariates ) == nrow( dfm_train_tfidf_final )

# Combine DFM with covariates
dfm_combined <- cbind( dfm_train_tfidf_final, dfm_covariates )


#################################    
# Train het Naive Bayes model ###
#################################
    
# naive-base classifier, with covariates added
nb_model <- textmodel_nb( dfm_combined, docvars( dfm_combined, "group" ), distribution = 'Bernoulli' )
    
#check werkelijke calssificatie
actual_class_train <- docvars( dfm_combined, "group" )
    
#predict class
predicted_class_train <- predict( nb_model, newdata = dfm_combined )
    
#2x2 tabel
tab_class_train <- table( actual_class_train, predicted_class_train )
print( tab_class_train )
    
#confusion matrix
confusion_mat_train <- caret::confusionMatrix( tab_class_train, mode = "everything", positive = "epilepsy" )
print( confusion_mat_train )

# visualize in plots
visualize_top_features( nb_model, outdir )


#################################################
######################## test ###################
################################################# 
    
### Test het Naive Bayes model ###

# add covariates
covariate1 <- df_test$p_sex
covariate2 <- df_test$p_age_fsle

covariate1[ covariate1 %in% 'female' ] <- 0
covariate1[ covariate1 %in% 'male' ] <- 1

# select covariates
additional_covariates <- data.frame( covariate1 = as.numeric( covariate1 ), 
                                     covariate2 = as.numeric( covariate2 ) )


# Convert covariates to a matrix
covariates_matrix <- as.matrix( additional_covariates )

# Convert to sparse matrix format
covariates_sparse <- Matrix( covariates_matrix, sparse = TRUE )

# Create a DFM-like object from the sparse matrix
dfm_covariates <- as.dfm( covariates_sparse )

# Make sure the number of rows matches your DFM
nrow( additional_covariates ) == nrow( dfm_test_tfidf_final )

# Combine DFM with covariates
dfm_test_combined <- cbind( dfm_test_tfidf_final, dfm_covariates )



#check werkelijke calssificatie
actual_class_test <- docvars( dfm_test_combined, "fu_diagnosis_after_FU" )

#predict class
predicted_class_test <- predict( nb_model, newdata = dfm_test_combined )

#2x2 tabel
tab_class_test <- table( actual_class_test, predicted_class_test )
print( tab_class_test )

# confusion matrix
confusion_mat_test <- caret::confusionMatrix( tab_class_test, mode = "everything", positive = "epilepsy" )
print( confusion_mat_test )

# get performance in standardized format
perf_train <- get_performance_mtx( confusion_mat_train$table )
perf_test <- get_performance_mtx( confusion_mat_test$table )

# write to disk
readr::write_tsv( perf_train, file = paste0( outdir, '/performance_nb_bayes__df_train.tsv' ), quote = 'all' )
readr::write_tsv( perf_test, file = paste0( outdir, '/performance_nb_bayes__df_test.tsv' ), quote = 'all' )




