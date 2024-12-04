#!/usr/bin/env Rscript
#
# Preprocess data 
#
################################################################################

# outdir 
outdir <- 'out.00.analyse.1A'
dir.create( outdir, showWarnings = FALSE )

# import data
df <- readxl::read_excel( "data/dataset.xlsx" )

# add epilepsy diagnosis group
df$group <- NA
df$group <- ifelse( df$fu_diagnosis_after_FU == 'epilepsy', 'yes', 
            ifelse( df$fu_diagnosis_after_FU == 'no epilepsy', 'no', 'unclear' ) )

# remove 'unclear' in final diagnosis [514 'epilepsy' and 958 'no epilepsy']
df <- df[ df$fu_diagnosis_after_FU %in% c( 'epilepsy', 'no epilepsy' ), ]

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

# epilepsy: 97, no_epilepsy: 219
summary( as.factor( df_unclear$fu_diagnosis_after_FU ) )

# write unclear to disk (also for LLM processing)
readr::write_tsv( df_unclear, file = paste0( outdir, '/df_unclear.tsv' ), quote = 'all' )
