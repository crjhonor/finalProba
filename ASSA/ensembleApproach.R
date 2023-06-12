library(tidyverse)
ensemble_error <- function(n_classifier, error){
  k_state = ceiling(n_classifier/2.)
  ensemble_error <- 0
  for(k in k_state:n_classifier){
    ensemble_error <- ensemble_error + length(combn(n_classifier, k, simplify = FALSE)) * error ** k * (1-error) ** (12 - k)
  }
  return(ensemble_error)
}

ens_error <- ensemble_error(n_classifier=12, error=0.4149)