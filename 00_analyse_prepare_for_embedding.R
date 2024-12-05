#!/usr/bin/env Rscript
#
# Preprocess data 
#
################################################################################
library( "fedmatch" )


# outdir 
outdir <- 'out.00.analyse.prepare.for.embedding'
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
index <- as.vector( caret::createDataPartition( df$fu_diagnosis_after_FU, p = 0.8, list = FALSE ) )

df$set <- 'test'
df[ index, 'set' ] <- 'train'

# add unclear category
df$unclear <- 'no'
df[ df$fsc_diagnosis == 'unclear', 'unclear' ] <- 'yes'

# test: 293, train: 1173
summary( as.factor( df$set ) )

# no: 1150, yes: 316
summary( as.factor( df$unclear ) )

# write train/test to disk (also for LLM processing)
readr::write_tsv( df, file = paste0( outdir, '/df.tsv' ), quote = 'all' )

# save also as csv, statement, status
df_csv <- data.frame( set = df$set, 
                      unclear = df$unclear,  
                      y = df$fu_diagnosis_after_FU,
                      text = df$fsc_letter_without_considerations_andEEG )

# clean rownames
rownames( df_csv ) <- NULL

# write to disk
write.csv( df_csv, file = paste0( outdir, '/df.csv' ), quote = TRUE )

 

# make small set n = 50
set.seed( 7 )
idx <- sample( 1:nrow( df_csv ) )[ 1:50 ]
df_small_csv <- df_csv[ idx, ]

# C: 30, E: 20
summary( as.factor( df_small_csv$y ) )

# save to disk
write.csv( df_small_csv, file = paste0( outdir, '/df_small.csv' ), quote = TRUE )



######### CLEAN ##############
df_csv_cleaned <- df_csv
df_small_csv_cleaned <- df_small_csv

# clean text
df_csv_cleaned$text <- fedmatch::clean_strings( df_csv_cleaned$text )
df_small_csv_cleaned$text <- fedmatch::clean_strings( df_small_csv_cleaned$text )

# save to disk
write.csv( df_csv_cleaned, file = paste0( outdir, '/df_cleaned.csv' ), quote = TRUE )
write.csv( df_small_csv_cleaned, file = paste0( outdir, '/df_small_cleaned.csv' ), quote = TRUE )

