rm(list = ls())

run_replication <- function(clean = FALSE, # clean.R
                            #get.distances = FALSE, # calculate distances.R
                            #merge = FALSE, # merge_precinct_coordinates.R
                            analyze = TRUE # analysis.R
) {
  
  # Create directories for output of files
  if(!file.exists("./figs")) dir.create("./figs")
  if(!file.exists("./tables")) dir.create("./tables")
  if(!file.exists("./data_clean")) dir.create("./data_clean")
  
  if(clean) {
    print("Running clean.R")
    source("./scripts/clean.R", echo = T) # Can change echo = TRUE/FALSE if needed
  }
#  if(get.distances) {
#    print("Running calculate_distances.R")
#    source("./scripts/calculate_distances.R", echo = T)
#  }
#  if(merge) {
#    print("Running merge_prcinct_coordinates.R")
#    source("./scripts/merge_precinct_coordinates.R", echo = T)
#  }
  if(analyze) {
    print("Running analysis.R")
    source("./scripts/analysis.R", echo = T) 
  }
}

run_replication(clean = T, 
                #get.distances = T, merge = T, 
                analyze = T)
