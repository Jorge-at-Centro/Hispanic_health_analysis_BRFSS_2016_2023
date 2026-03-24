Data Analysis BRFSS Data
================
Jorge R. Soldevila Irizarry
2026-03-24

# Library Setup

``` r
# Install pacman if not already installed on the system. This will allow us to install desired libraries. 
if (!require("pacman")) install.packages("pacman")

# Load libraries using p_load(), which installs missing packages and loads
#libraries.
pacman::p_load(tidyverse,     # For data manipulation
               janitor,       # For cleaning column names
               survey,        # For analyzing complex survey designs
               broom,         # For tidying statistical model outputs into 
                              # data frames
               haven,         # For reading SAS Transport files, like BRFSS data
               cdlTools,      # For converting FIPS codes to state abbreviations
               expss,         # For adding variable and value labels to data frames
               openxlsx,      # For exporting data frames to Excel files
               glue,          # For easy string interpolation
               here           # For dynamic file path
               )
```

# Load processed data

## Path configuration

``` r
# Use here() to point to the data and output folders relative to the project root
data_path <- here("data")
output_path <- here("outputs")

# Ensure the outputs directory exists before running to prevent export errors.
if (!dir.exists(output_path)) dir.create(output_path)
```

## Load data

We will load the cleaned data set we saved as .RDS in
`01_data_wrangling.Rmd`.

``` r
#Load Processed Data from RDS.
df_combined <- readRDS(file.path(data_path, "df_combined_cleaned.rds"))
```

# Helper Functions

We create helper functions that will be used to generate total
population counts by race and ethnicity, total population counts and
weighted means by race and ethnicity and health condition status, and
perform statistical analysis.

``` r
#' Generate Population Totals by Race and Ethnicity
#' 
#' Iterates over a list of variables to calculate estimated population totals 
#' across race/ethnicity categories using the complex survey design.
#'
#' @param design A survey design object created via svydesign().
#' @param vars A character vector of target health variables.
#' @return A named list of `svyby` objects containing the calculated totals.

get_totals <- function(design, vars) {
  results <- lapply(vars, function(v) {
    form <- as.formula(paste0("~", v))
    svyby(form, ~racexhisp, design, svytotal, na.rm = TRUE)
  })
  names(results) <- vars
  return(results)
}

#' Calculate Proportions and Confidence Intervals
#' 
#' Generates weighted proportions of the population that responded to each 
#' health condition, broken down by race and ethnicity, along with 
#' their corresponding Standard Errors and Confidence Intervals.
#'
#' @param design A survey design object created via svydesign().
#' @param vars A character vector of target health variables.
#' @return A named list of tidy data frames containing means, SEs, and CIs.

get_means_and_ci <- function(design, vars) {
  results <- lapply(vars, function(v) {
    form <- as.formula(paste0("~", v))
    mean_res <- svyby(form, ~racexhisp, design, svymean, na.rm = TRUE)
    
    estimates <- unlist(coef(mean_res))          
    std_errors <- unlist(SE(mean_res))           
    ci_res <- confint(mean_res)          
    
    tidy_df <- data.frame(
      group_and_level = names(estimates),
      mean_proportion = as.numeric(estimates),
      standard_error = as.numeric(std_errors),
      lower_ci = as.numeric(ci_res[, 1]),
      upper_ci = as.numeric(ci_res[, 2]),
      stringsAsFactors = FALSE
    )
    
    tidy_df <- tidy_df %>%
      separate(group_and_level, into = c("race_ethnicity", "response_level"), 
               sep = ":", extra = "merge") %>%
      mutate(response_level = str_remove(response_level, paste0("^", v)))
    
    return(tidy_df)
  })
  names(results) <- vars
  return(results)
}

#' Run T-Tests Across Survey Years
#' 
#' Performs t-tests to determine if differences in health condition responses 
#' between the two survey years are statistically significant within each 
#' race/ethnicity group.
#'
#' @param var_name The specific health variable to test (character string).
#' @param design A survey design object encompassing both years.
#' @param race_groups A character vector of the race/ethnicity levels.
#' @return A single bound data frame containing t-statistics, p-values, and CIs.

run_ttests <- function(var_name, design, race_groups) {
  var_levels <- levels(design$variables[[var_name]])
  results_list <- list()
  
  for (race in race_groups) {
    sub_desgn <- subset(design, racexhisp == race)
    if(nrow(sub_desgn$variables) == 0) next
    
    for (lvl in var_levels) {
      formula_obj <- as.formula(paste0("I(", var_name, " == '", lvl, "') ~ year"))
      
      # Use tryCatch to gracefully handle cases where a sub-group lacks enough 
      # variance or data to compute a t-test
      test_result <- tryCatch({
        svyttest(formula_obj, sub_desgn)
      }, error = function(e) NULL)
      
      if (!is.null(test_result)) {
        results_list[[paste(race, lvl)]] <- data.frame(
          variable = var_name,
          race = race,
          health_level = lvl,
          t_stat = test_result$statistic,
          p = test_result$p.value,
          lower_ci = test_result$conf.int[1],
          upper_ci = test_result$conf.int[2],
          row.names = NULL
        )
      }
    }
  }
  return(bind_rows(results_list))
}
```

# State Analysis

``` r
# Adjust variance estimation globally for strata containing only a single 
# primary sampling unit (PSU) to prevent standard error calculation failures.
options(survey.lonely.psu="adjust")

# Identify the states available in the filtered dataset
available_states <- unique(df_combined$state)

for (current_state in available_states) {
  
  cat("Processing state:", current_state, "\n")
  
  # Filter data for current state
  df_state <- df_combined %>% filter(state == current_state)
  race_levels <- levels(df_state$racexhisp)
  
  # Define the complex survey design for the current state.
  # We interact the stratum variable (ststr) with year to ensure strata 
  # are properly nested and distinct across the two different survey years.
  state_dsgn <- svydesign(id = ~psu,
                          strata = ~interaction(ststr, year),
                          weights = ~llcpwt,
                          data = df_state,
                          nest = TRUE)
  
  dsgn_2016 <- subset(state_dsgn, year == 2016)
  dsgn_2023 <- subset(state_dsgn, year == 2023)
  
  # --- 1. Export Population Distributions by Race ---
  totals_2016 <- svytable(~racexhisp, dsgn_2016, na.rm=TRUE)
  write.xlsx(totals_2016, file.path(output_path,glue("totals_2016_race_{current_state}.xlsx"))) 
  
  totals_2023 <- svytable(~racexhisp, dsgn_2023, na.rm=TRUE)
  write.xlsx(totals_2023, file.path(output_path,glue("totals_2023_race_{current_state}.xlsx")))
  
  # --- 2. Export Cross-tabulated Totals by Race and Health Category ---
  total_2016_list <- get_totals(dsgn_2016, health_vars)
  write.xlsx(total_2016_list, file.path(output_path,glue("totals_2016_hlth_list_{current_state}.xlsx")))
  
  total_2023_list <- get_totals(dsgn_2023, health_vars)
  write.xlsx(total_2023_list, file.path(output_path,glue("totals_2023_hlth_list_{current_state}.xlsx")))
  
  # --- 3. Export Weighted Means and CIs by Race and Health Category ---
  mean_ci_2016_list <- get_means_and_ci(dsgn_2016, health_vars)
  write.xlsx(mean_ci_2016_list, file.path(output_path,glue("mean_and_ci_2016_{current_state}.xlsx")))
  
  mean_ci_2023_list <- get_means_and_ci(dsgn_2023, health_vars)
  write.xlsx(mean_ci_2023_list, file.path(output_path,glue("mean_and_ci_2023_{current_state}.xlsx")))
  
  # --- 4. Export T-Test Results: 2016 vs 2023 Significance Testing ---
  all_ttest_results <- lapply(health_vars, function(var) {
    run_ttests(var, state_dsgn, race_levels)
  })
  names(all_ttest_results) <- health_vars
  write.xlsx(all_ttest_results, file.path(output_path,glue("ttest_list_{current_state}.xlsx")))
  
  cat("Completed exports for:", current_state, "\n\n")
}
```

    ## Processing state: FL 
    ## Completed exports for: FL 
    ## 
    ## Processing state: NY 
    ## Completed exports for: NY 
    ## 
    ## Processing state: PR

    ## Completed exports for: PR
