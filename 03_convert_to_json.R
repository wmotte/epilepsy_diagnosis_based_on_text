#!/usr/env/bin Rscript
#
# Convert epilepsy diagnosis database to json 'alpaca' format for LLM modeling.
# 
# Wim Otte (w.m.otte@umcutrecht.nl)
################################################################################
library( 'jsonlite' )

###
#
##
get_selected_data <- function( raw )
{
    # get center
    df <- raw
    center <- df$Site.Abbreviation
      
    # patient-id
    patient_id <- df$Participant.Id
    
    # letter with considerations removed
    input <- df$fsc_letter_without_considerations_andEEG
    
    # final diagnosis (after follow-up)
    output <- as.factor( df$fu_diagnosis_after_FU )
    levels( output ) <- c( 'Epilepsy', 'Control' )

    # construct df
    export_data <- data.frame( id = 1:length( patient_id ),
                               patient_id = patient_id,
                               center = center,
                               instruction = "Predict epilepsy diagnosis",
                               input = input,
                               output = output )
    
    return( export_data )
}

################################################################################
################################################################################
################################################################################

# output dir
outdir <- 'out.03.convert.to.json'
dir.create( outdir, showWarnings = FALSE )


# get train/test data similar to Naive Bayes model [80%/20%]
raw_train <- as.data.frame( readr::read_tsv( 'out.00.analyse.1A/df_train.tsv', show_col_types = FALSE ) ) # [1173]
raw_eval <- as.data.frame( readr::read_tsv( 'out.00.analyse.1A/df_test.tsv', show_col_types = FALSE ) ) # [293]

# unclears
raw_unclear <- as.data.frame( readr::read_tsv( 'out.00.analyse.1A/df_unclear.tsv', show_col_types = FALSE ) ) # [316]

# select variables for LLM
df_train <- get_selected_data( raw_train )
df_eval <- get_selected_data( raw_eval )
df_unclear <- get_selected_data( raw_unclear )

# write to file
json_train <- toJSON( x = df_train, dataframe = 'rows', pretty = TRUE )
json_eval <- toJSON( x = df_eval, dataframe = 'rows', pretty = TRUE )
json_unclear <- toJSON( x = df_unclear, dataframe = 'rows', pretty = TRUE )

# Write JSON to disk
write( json_train, paste0( outdir, '/epilepsy_diagnosis__train_random80perc.json' ) )
write( json_eval, paste0( outdir, '/epilepsy_diagnosis__eval_random20perc.json' ) )
write( json_eval, paste0( outdir, '/epilepsy_diagnosis__unclears.json' ) )
