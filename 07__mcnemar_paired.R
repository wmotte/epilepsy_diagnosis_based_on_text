#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Paired NB vs BERT (Sentence-Embedding) comparison
# - McNemar from paired predictions
#
# https://en.wikipedia.org/wiki/McNemar%27s_test
#
################################################################################
library( "dplyr" )
library( "readr" )


###
# Paired McNemar based on goldstandard and model A + model B predictions
##
mcnemar <- function( gold, pred_A, pred_B )
{
    # convert to correctness
    correct_A <- pred_A == gold
    correct_B <- pred_B == gold
    
    # Build the paired 2×2 table
    tab <- table( correct_A, correct_B )
    
    # The McNemar test statistic is calculated using only the discordant pairs (tab[1,2] vs tab[2,1]).
    tt <- mcnemar.test( tab )
    
    return( data.frame( chi_squared = round( tt$statistic, 2 ), df = tt$parameter, pvalue = round( tt$p.value, 3 ) ) )
    
}

################################################################################
# END FUNCTIONS
################################################################################

# load data
df_bert_test <- readr::read_tsv( 'out.05.xgboost.cleaned/predicted_xgboost__test.tsv', show_col_types = FALSE )         # 293
df_bert_unclear <- readr::read_tsv( 'out.05.xgboost.cleaned/predicted_xgboost__unclear.tsv', show_col_types = FALSE )   # 316

df_nb_test <- readr::read_tsv( 'out.02.analyse.1Ac/predicted_nb_bayes__df_test.tsv', show_col_types = FALSE )               # 293
df_nb_unclear <- readr::read_tsv( 'out.02.analyse.NB.unclears/predicted_nb_bayes__df_unclear.tsv', show_col_types = FALSE ) # 316

df_goldstandard_test <- readr::read_tsv( 'out.02.analyse.1Ac/gold_standard__test.tsv', show_col_types = FALSE )                 # 293
df_goldstandard_unclear <- readr::read_tsv( 'out.02.analyse.NB.unclears//gold_standard__unclear.tsv', show_col_types = FALSE )  # 316

# paired mc-nemar tests
mcnemar( df_goldstandard_test$true_class, df_nb_test$predicted_class, df_bert_test$predicted_class )            # p = 0.804
mcnemar( df_goldstandard_unclear$true_class, df_nb_unclear$predicted_class, df_bert_unclear$predicted_class )   # p = 0.461


#                       chi_squared df pvalue
# McNemar's chi-squared        0.06  1  0.804

#                      chi_squared df pvalue
#McNemar's chi-squared        0.54  1  0.461
