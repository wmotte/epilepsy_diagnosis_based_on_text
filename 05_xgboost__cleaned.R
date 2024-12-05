#!/usr/bin/env Rscript
#
# XG Boost on diagnostic epilepsy text embeddings.
# check: 
# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
#
################################################################################
library( "xgboost" )

################################################################################
# BEGIN CUSTOM FUNCTIONS
################################################################################

###
# Get embedding
##
get_embeddings_mat <- function( embeddings )
{
    # Convert 'embeddings' column to a numeric matrix
    embeddings_matrix <- do.call(rbind, 
        lapply( embeddings, function(x) as.numeric(unlist(strsplit(x, ",")))) )
    
    # Return the structure of the resulting matrix
    return( embeddings_matrix )
}

###
# Get performance based on 2x2 table.
##
get_performance_mtx <- function( mtx )
{
    # get cells
    TP <- mtx[ 'Epilepsy', 'Epilepsy' ]
    TN <- mtx[ 'Control', 'Control' ]
    
    FP <- mtx[ 'Epilepsy', 'Control' ]
    FN <- mtx[ 'Control', 'Epilepsy' ]
    
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

set.seed( 123 )

# outdir
outdir <- 'out.05.xgboost.cleaned'
dir.create( outdir, showWarnings = FALSE )

# read input data
df <- read.csv( 'colab_embedding/embedded_df_cleaned.csv', row.names = 1 )

# get training data
df_train <- df[ df$set == 'train', ]

# get matrix from training data
data <- get_embeddings_mat( df_train$embeddings )

# label (numeric, binary)
label <- df_train$y
label[ label == 'Control' ] <- 0
label[ label == 'Epilepsy' ] <- 1
label <- as.numeric( label )

# train Area under the curve: 0.7621 [5, 0.1, 2]
model <- xgboost( data = data, label = label, max.depth = 5, eta = 0.10, nrounds = 2, 
                  nthread = 2, 
                  objective = "binary:logistic", verbose = 3, booster = "gblinear" )



########################
######## TEST ##########
########################

# get test data
df_test <- df[ df$set == 'test', ]

# get matrix from training data
data_test <- get_embeddings_mat( df_test$embeddings )

# label (numeric, binary)
label_test <- df_test$y
label_test[ label_test == 'Control' ] <- 0
label_test[ label_test == 'Epilepsy' ] <- 1
label_test <- as.numeric( label_test )

# predict
pred_test <- predict( model, data_test )

# Create the ROC curve
roc_obj <- pROC::roc( label_test, pred_test )
plot( roc_obj )

# Calculate the AUC
auc_value <- auc( roc_obj ) 


# convert to binary
pred_test_binary <- as.numeric( pred_test > 0.5 )

tab_class_test <- table( label_test, pred_test_binary )
colnames( tab_class_test ) <- c( 'Control', 'Epilepsy' )
rownames( tab_class_test ) <- c( 'Control', 'Epilepsy' )
# confusion matrix
confusion_mat_test <- caret::confusionMatrix( tab_class_test, mode = "everything", positive = "Epilepsy" )
print( confusion_mat_test )

# get performance in standardized format
perf_test <- get_performance_mtx( confusion_mat_test$table )
#perf_test
auc_value



#########################
######## TRAIN ##########
#########################

# get train data
df_train <- df[ df$set == 'train', ]

# get matrix from training data
data_train <- get_embeddings_mat( df_train$embeddings )

# label (numeric, binary)
label_train <- df_train$y
label_train[ label_train == 'Control' ] <- 0
label_train[ label_train == 'Epilepsy' ] <- 1
label_train <- as.numeric( label_train )

# predict
pred_train <- predict( model, data_train )

# Create the ROC curve
#roc_obj <- pROC::roc( label_train, pred_train )
#plot( roc_obj )

# Calculate the AUC
#auc_value <- auc( roc_obj )
#auc_value

# convert to binary
pred_train_binary <- as.numeric( pred_train > 0.5 )

tab_class_train <- table( label_train, pred_train_binary )
colnames( tab_class_train ) <- c( 'Control', 'Epilepsy' )
rownames( tab_class_train ) <- c( 'Control', 'Epilepsy' )
# confusion matrix
confusion_mat_train <- caret::confusionMatrix( tab_class_train, mode = "everything", positive = "Epilepsy" )
print( confusion_mat_train )

# get performance in standardized format
perf_train <- get_performance_mtx( confusion_mat_train$table )
perf_train




###########################
######## UNCLEAR ##########
###########################

# get unclear data
df_unclear <- df[ df$unclear == 'yes', ]

# get matrix from unclear data
data_unclear <- get_embeddings_mat( df_unclear$embeddings )

# label (numeric, binary)
label_unclear <- df_unclear$y
label_unclear[ label_unclear == 'Control' ] <- 0
label_unclear[ label_unclear == 'Epilepsy' ] <- 1
label_unclear <- as.numeric( label_unclear )

# predict
pred_unclear <- predict( model, data_unclear )

# Create the ROC curve
#roc_obj <- pROC::roc( label_unclear, pred_unclear )
#plot( roc_obj )

# Calculate the AUC
#auc_value <- auc( roc_obj ) 
#auc_value

# convert to binary
pred_unclear_binary <- as.numeric( pred_unclear > 0.5 )

tab_class_unclear <- table( label_unclear, pred_unclear_binary )
colnames( tab_class_unclear ) <- c( 'Control', 'Epilepsy' )
rownames( tab_class_unclear ) <- c( 'Control', 'Epilepsy' )
# confusion matrix
confusion_mat_unclear <- caret::confusionMatrix( tab_class_unclear, mode = "everything", positive = "Epilepsy" )
print( confusion_mat_unclear )

# get performance in standardized format
perf_unclear <- get_performance_mtx( confusion_mat_unclear$table )


# write to disk
readr::write_tsv( perf_train, file = paste0( outdir, '/performance_xgboost__df_train.tsv' ), quote = 'all' )
readr::write_tsv( perf_test, file = paste0( outdir, '/performance_xgboost__df_test.tsv' ), quote = 'all' )
readr::write_tsv( perf_unclear, file = paste0( outdir, '/performance_xgboost__df_unclear.tsv' ), quote = 'all' )

