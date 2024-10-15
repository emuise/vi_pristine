split_pqs <- list.files(split_loc, full.names = T)

str_vars <- c("elev_p95", "total_biomass", "percentage_first_returns_above_2m", "elev_cv")
dhi_vars <- c("CumDHI", "VarDHI", "MinDHI")
all_vars <- c(str_vars, dhi_vars)
all_vars_names <- c("height", "biomass", "cover", "complexity", "cum", "var", "min")

split_pqs %>%
  head() %>%
  map(\(x) {
    # since every file has a matched treatment, just change out the folder
    # to the treatment folder
    
    # read both
    base <- read_parquet(x)
    y <- here::here(treat_loc, basename(x))
    
    treat <- read_parquet(str_replace(x, "split_strata", "treatment_strata"))
    cutoff <- 0.9
    
    savename <- here::here(mahal_loc, basename(x))
    
    # function that takes a treatment file, a list of variables, and a cutoff value
    # which variables to operate on, and generates the mahalanobis distance, and names it something relevant
    # the format should be "mahal_height-90_cover-90_str-prod.parquet"
    # so mahalanobis distance, top 10% height, top 10% cover
    # distanec for both structure and productivity
    
    filter_vars <- c("elev_p95")
    distance_vars <- str_vars
    
    calc_mahal <- function(filter_vars, distance_vars, cutoff) {
      filt_treat <- treat %>%
        # the !! makes it so that the string evaluates the variable,
        # allowing us to do this filter all at once if there are multiple
        filter(if_all(!!filter_vars, .fns = \(x) {
          x >= quantile(x, 0.9)
        })) %>%
        select(all_of(distance_vars))
      
      filt_means <- filt_treat %>%
        summarize(across(everything(), mean)) %>%
        pivot_longer(cols = everything())
      
      filt_cov <- filt_treat %>%
        as.matrix() %>%
        cov()
      
      base_mat <- base %>%
        select(all_of(distance_vars)) %>%
        as.matrix()
      
      mahal_dist <- mahalanobis(
        base_mat,
        center = filt_means %>% pull(value),
        cov = filt_cov,
        tol = 1e-50
      )
      
      mahal_pval <- pchisq(mahal_dist, 
                           df = length(distance_vars) - 1, 
                           lower.tail = F) %>%
        round(4)
      
      
      clean_filter_names <- all_vars_names[all_vars %in% filter_vars]
      
      var_cuts <- glue("{clean_filter_names}-{cutoff}") %>%
        glue_collapse(sep = "_")
      
      if (length(distance_vars) == 7) {
        dist_name <- "all"
      } else if (length(distance_vars) == 4) {
        dist_name <- "str"
      } else {
        dist_name <- "dhi"
      }
      colname <- glue("{var_cuts}_{dist_name}")
      
      out <- tibble("mahal_{colname}" := mahal_dist, "p_{colname}" :=mahal_pval)
      
      return(out)
    }
    
    filt_list <- list(c("elev_p95"),
                      c("elev_p95", "percentage_first_returns_above_2m"),
                      c("percentage_first_returns_above_2m"),
                      c("CumDHI"),
                      c("VarDHI"))
    
    dist_list <- list(all_vars,
                      str_vars,
                      dhi_vars)
    
    crossed <- crossing(filt_list, dist_list)
    
    egg <- map2(.x = crossed$filt_list, .y = crossed$dist_list, .f = calc_mahal, cutoff = 0.9)
    
    mahal_tosave <- base %>%
      select(x, y) %>%
      bind_cols(egg)
    
    write_parquet(mahal_tosave, savename)
  })