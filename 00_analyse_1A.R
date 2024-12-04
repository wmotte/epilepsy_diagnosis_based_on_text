#!/usr/bin/env Rscript
#
# Preprocess data 
#
################################################################################

# outdir 
outdir <- 'out.00.analyse.1A'
dir.create( outdir, showWarnings = FALSE )

# import data
raw <- readxl::read_excel( "data/dataset.xlsx" )


# get problematic sets [n = 6]
idx_problematic <- stringr::str_count( raw$fsc_letter_without_considerations_andEEG, ' ' ) < 10

# TODO: check "QD01294", "QD00246", "QD00290", "QD01599"
tmp <- raw[ idx_problematic, ]
tmp$Participant.Id

# keep others
df <- raw[ !idx_problematic, ]




# add epilepsy diagnosis group
df$group <- NA
df$group <- ifelse( df$fu_diagnosis_after_FU == 'epilepsy', 'yes', 
            ifelse( df$fu_diagnosis_after_FU == 'no epilepsy', 'no', 'unclear' ) )

# remove 'unclear' in final diagnosis [514 'epilepsy' and 958 'no epilepsy']
df <- df[ df$fu_diagnosis_after_FU %in% c( 'epilepsy', 'no epilepsy' ), ]

# change labels
df[ df$fu_diagnosis_after_FU == 'epilepsy', 'fu_diagnosis_after_FU' ] <- 'Epilepsy'
df[ df$fu_diagnosis_after_FU == 'no epilepsy', 'fu_diagnosis_after_FU' ] <- 'Control'




# split dataset based on the FU outcome (!) => 80% train, 20% test
set.seed( 123 )
index <- caret::createDataPartition( df$fu_diagnosis_after_FU, p = 0.8, list = FALSE )
df_train <- df[ index, ]
df_test <- df[ -index, ]

# write train/test to disk (also for LLM processing)
readr::write_tsv( df_train, file = paste0( outdir, '/df_train.tsv' ), quote = 'all' )
readr::write_tsv( df_test, file = paste0( outdir, '/df_test.tsv' ), quote = 'all' )

# select 'unclears' at day 1
df_unclear <- df[ df$fsc_diagnosis == 'unclear', ]

# Control: 219, Epilepsy: 97, 
summary( as.factor( df_unclear$fu_diagnosis_after_FU ) )

# write unclear to disk (also for LLM processing)
readr::write_tsv( df_unclear, file = paste0( outdir, '/df_unclear.tsv' ), quote = 'all' )


# save also as csv, statement, status
df_train_csv <- data.frame( statement = df_train$fsc_letter_without_considerations_andEEG, status = df_train$fu_diagnosis_after_FU )
df_test_csv <- data.frame( statement = df_test$fsc_letter_without_considerations_andEEG, status = df_test$fu_diagnosis_after_FU )
df_unclear_csv <- data.frame( statement = df_unclear$fsc_letter_without_considerations_andEEG, status = df_unclear$fu_diagnosis_after_FU )
rownames( df_train_csv ) <- NULL
rownames( df_test_csv ) <- NULL
rownames( df_unclear_csv ) <- NULL

# write to disk
write.csv( df_train_csv, file = paste0( outdir, '/df_train.csv' ), quote = TRUE )
write.csv( df_test_csv, file = paste0( outdir, '/df_test.csv' ), quote = TRUE )
write.csv( df_unclear_csv, file = paste0( outdir, '/df_unclear.csv' ), quote = TRUE )

