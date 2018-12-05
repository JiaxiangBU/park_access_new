
#' Calculate destination choice logsums from a distance matrix
#' 
#' @param distances An $n\times p$ matrix with the distance from all tracts to
#'   all parks in miles.
#' @param sizes A p-length vector of park sizes
#' @param tweets A p-length vector of tweets at parks
#' @param betas A vector containing the size, distance, and tweet coefficients. 
#'   If we submit two variables the tweets are ignored.
#'   
#' @return An n-length vector containing the weighted log-sum based
#'   accessibility between a tract and all parks.
#' @details If we have n tracts and p parks, distances needs to be a 
#' 
calculate_park_logsums <- function(distances, sizes, tweets = NULL,
                                   betas = c(.00001, -15, .001)){
  
  # A is n x p
  a <- betas[2] * distances 
  
  # B is p x 1
  b <- betas[1] * sizes
  
  if(!is.null(tweets)) 
    b <- b + betas[3] * tweets
  
  # calculate observed utility by adding the weighted park-level attributes 
  # to the columns of the matrix
  # V is n x p, with b added by-column to each element in a
  V <- sweep(a, MARGIN = 2, b, `+`)
  
  # log-sum of exponentiated utility, Output is n-length vector
  log(rowSums(exp(V)))
  
}