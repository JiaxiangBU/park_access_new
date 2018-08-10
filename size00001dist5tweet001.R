#' Calculate destination choice accessibility logsum
#' 
#' @param size A vector of park sizes in acres
#' @param distance A vector of distances to the parks in `size` in miles
#' @param tweets A vector of the number of tweets recorded within each park in September 2014
#' @param betas A vector containing the size and distance coefficients
#'
#' @return The accessibility logsum
#' 
park_logsum <- function(size, distance, betas = c(.00001, -5, 1)){
  
  # calculate exponentiated utility of all parks
  U <- betas[1] * size + betas[2] * distance + betas[3] * park_tweets
  eU <- exp(U)
  
  # log-sum
  log(sum(eU))
}


#' Calculate destination choice logsums from a distance matrix
#' 
#' @param tracts A spatial points dataframe containing the 
#'   tract geographic information and their attributes.
#' @param parks A spatial points dataframe containing the 
#'   park geographic information and their attributes.
#' @param betas A vector containing the size and distance coefficients
#'   
#' @return An accessibility logsum calculation for each tract to every park.
#' 
calculate_park_logsums <- function(tracts, parks, betas = c(.00001, -5, .001)){
  
  # calculate centroids of tracts and parks
  tract_centroids <- gCentroid(tracts, byid = TRUE) 
  park_centroids <- gCentroid(parks, byid = TRUE)
  
  # distances from all tracts to all zones
  distances <- gDistance(park_centroids, tract_centroids, byid = TRUE) / 5280
  
  # vector containing park sizes 
  park_sizes <- parks$Park_Acres
  
  # vector containing tweet counts
  park_tweets <- parks$TWEET_COUNT
  
  # calculate observed utility 
  #   beta_size * size + beta_distance * distance + beta_tweet * tweets
  V <- sweep(betas[2] * distances, MARGIN = 2, 
             betas[1] * park_sizes + betas[3] * log(park_tweets + 0.01), `+`)
  
  # log-sum of exponentiated utility
  log(rowSums(exp(V)))
  
  
}