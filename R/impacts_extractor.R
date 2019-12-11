impacts_extractor <- function(sdm) {
  impacts_summary <-
    summary(impacts(sdm, tr = trMC, R = 1000), zstats = TRUE)
  
  coef <- list(
    direct = impacts_summary$direct_sum,
    indirect = impacts_summary$indirect_sum,
    total = impacts_summary$total_sum
  ) %>%
    lapply(function(s) {
      data_frame(var = names(s$statistics[, 1]),
                 impact = s$statistics[, 1])
    }) %>%
    bind_rows(.id = "effect")
  
  pval <- as_data_frame(impacts_summary$pzmat) %>%
    mutate(var = rownames(impacts_summary$pzmat)) %>%
    gather("effect", "p-val",-var) %>%
    mutate(effect = tolower(effect))
  
  
  left_join(coef, pval, by = c("effect", "var"))
}