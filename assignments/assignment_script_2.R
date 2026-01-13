# Copy of the "read_bseervations" function


read_observations = function(scientificname = "Placopecten magellanicus",
                             minimum_year = 1970, 
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  # eventDate must exist
  x = x |>
    dplyr::filter(!is.na(eventDate))
  
  # individualCount must exist
  x = x |>
    dplyr::filter(!is.na(individualCount))
  
  db = brickman_database() |>
    dplyr::filter(scenario == "STATIC", var == "mask")
  
  mask = read_brickman(db)
  
  hitOrMiss = extract_brickman(mask, x)
  
  x = x |>
    dplyr::filter(!is.na(hitOrMiss$value))
  
  return(x)
}

source("setup.R")
obs = read_observations()
dim(obs)
