Health_Analysis_FL
================
Jorge R. Soldevila Irizarry
2026-03-10

# Library Setup

``` r
# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load libraries
pacman::p_load(tidyverse,
               janitor,
               survey,
               totalcensus,
               broom,
               haven,
               cdlTools,
               expss,
               openxlsx)
```

# Data Loading and Wrangling

``` r
# Read and subset 2016
df_2016 <- read_xpt("./LLCP2016.XPT") %>% 
  select(`_STATE`,`_PSU`,`_STSTR`,`_LLCPWT`,`_HISPANC`,`_MRACE1`,GENHLTH,`_PHYS14D`,
         ADDEPEV2,`_LTASTH1`,DIABETE3,`_MICHD`,CHCSCNCR,CHCOCNCR,HLTHPLN1) %>% 
  filter(`_STATE` == 12) %>%
  mutate(year = factor(2016)) %>%
  clean_names()

# Read, subset, and standardize 2023 column names to match 2016
df_2023 <- read_xpt("./LLCP2023.XPT") %>% 
  select(`_STATE`,`_PSU`,`_STSTR`,`_LLCPWT`,`_HISPANC`,`_MRACE1`,GENHLTH,`_PHYS14D`,
         ADDEPEV3,`_LTASTH1`,DIABETE4,`_MICHD`,CHCSCNC1,CHCOCNC1, `_HLTHPL1`) %>% 
  filter(`_STATE` == 72) %>%
  rename(ADDEPEV2 = ADDEPEV3, 
         DIABETE3 = DIABETE4,
         CHCSCNCR = CHCSCNC1,
         CHCOCNCR = CHCOCNC1,
         HLTHPLN1 = `_HLTHPL1`) %>%
  mutate(year = factor(2023)) %>%
  clean_names()

# Combine datasets
df_combined <- bind_rows(df_2016, df_2023)

# Convert state FIPS to Abbreviation
df_combined$state <- fips(df_combined$state, to = 'Abbreviation')
```

# Create new variables

``` r
df_combined <- df_combined %>%
  mutate(
    # Group General Health and Diabetes responses
    genhlth2 = case_when(genhlth %in% 1:3 ~ 1,
                         genhlth %in% 4:5 ~ 2,
                         TRUE ~ genhlth),
    diabetes = case_when(diabete3 %in% 1:2 ~ 1,
                         diabete3 == 3 ~ 2,
                         TRUE ~ diabete3),
    
    # Create the Hispanic x Race variable
    racexhisp = case_when(
      hispanc == 1 & mrace1 %in% 1:7 ~ mrace1,
      hispanc == 1 & mrace1 == 77 ~ 8,
      hispanc == 1 & mrace1 == 99 ~ 9,
      hispanc == 2 & mrace1 %in% 1:7 ~ mrace1 + 9,
      hispanc == 2 & mrace1 == 77 ~ 17,
      hispanc == 2 & mrace1 == 99 ~ 18,
      TRUE ~ NA_real_
    )
  )
```

# Adding labels

``` r
df_combined <- apply_labels(df_combined,
  racexhisp = "Hispanic by Race",
  racexhisp = c("Hispanic White"=1, 
                "Hispanic Black"=2, 
                "Hispanic AIAN"=3, 
                "Hispanic Asian"=4, 
                "Hispanic NHPI"=5, 
                "Hispanic Other"=6, 
                "Hispanic Multirracial"=7, 
                "Hispanic Not Sure"=8, 
                "Hispanic Refused"=9, 
                "Non Hispanic White"=10, 
                "Non Hispanic Black"=11,
                "Non Hispanic AIAN"=12, 
                "Non Hispanic Asian"=13, 
                "Non Hispanic NHPI"=14, 
                "Non Hispanic Other"=15, 
                "Non Hispanic Multirracial"=16, 
                "Non Hispanic Not Sure"=17, 
                "Non Hispanic Refused"=18),
  genhlth = c("Excellent"=1, 
              "Very Good"=2, 
              "Good"=3, 
              "Fair"=4, 
              "Poor"=5, 
              "Dont know"=7, 
              "Refused"=9),
  genhlth2 = c("Good"=1, 
               "Fair/Poor"=2, 
               "Dont know"=7, 
               "Refused"=9),
  phys14d = c("Zero Days"=1, 
              "1 to 13 Days"=2, 
              "14+ Days"=3, 
              "Dont know"=9),
  addepev2 = c("Yes"=1, 
               "No"=2, 
               "Dont know"=7, 
               "Refused"=9),
  ltasth1 = c("No"=1, 
              "Yes"=2, 
              "Dont know"=7),
  diabete3 = c("Yes"=1, 
               "Yes,during pregnancy"=2, 
               "No"=3, "Pre-diabetes"=4, 
               "Dont know"=7, 
               "Refused"=9),
  diabetes = c("Yes"=1, 
               "No"=2, 
               "Dont know"=7, 
               "Refused"=9),
  michd = c("Yes"=1, 
            "No"=2),
  chcscncr = c("Yes"=1, 
               "No"=2, 
               "Dont know"=7, 
               "Refused"=9),
  chcocncr = c("Yes"=1, 
               "No"=2, 
               "Dont know"=7, 
               "Refused"=9),
  hlthpln1 = c("Yes"=1, 
               "No"=2, 
               "Dont know"=7, 
               "Refused"=9)
)

# Convert targeted variables to factors
factor_vars <- c("racexhisp", "genhlth", "genhlth2", "phys14d", 
                 "addepev2", "ltasth1", "diabete3", "diabetes", 
                 "michd", "chcscncr", "chcocncr", "hlthpln1")

df_combined[factor_vars] <- lapply(df_combined[factor_vars], as.factor)
```

# Survey Design Setup

``` r
# Set up a combined survey design
options(survey.lonely.psu="adjust")

df_comb_dsgn <- svydesign(id = ~psu,
                          strata = ~interaction(ststr, year),
                          weights = ~llcpwt,
                          data = df_combined,
                          nest = TRUE)

# Subset designs by year if needed for independent means
dsgn_2016 <- subset(df_comb_dsgn, year == 2016)
dsgn_2023 <- subset(df_comb_dsgn, year == 2023)
```

# Calculate Totals by Race

``` r
totals_2016 <- svytable(~racexhisp, dsgn_2016, na.rm=TRUE)
write.xlsx(totals_2016, "totals_2016_race_FL.xlsx")

totals_2023 <- svytable(~racexhisp, dsgn_2023, na.rm=TRUE)
write.xlsx(totals_2023, "totals_2023_race_FL.xlsx")
```

# Calculate Totals by Race and Health Category

``` r
health_vars <- c("genhlth", "genhlth2", "addepev2", "ltasth1", "diabete3", 
                 "diabetes", "michd", "chcscncr", "chcocncr", "hlthpln1")

# Helper function to get totals
get_totals <- function(design, vars) {
  results <- lapply(vars, function(v) {
    form <- as.formula(paste0("~", v))
    svyby(form, ~racexhisp, design, svytotal, na.rm = TRUE)
  })
  names(results) <- vars
  return(results)
}

total_2016_list <- get_totals(dsgn_2016, health_vars)
write.xlsx(total_2016_list, "totals_2016_hlth_list_FL.xlsx")

total_2023_list <- get_totals(dsgn_2023, health_vars)
write.xlsx(total_2023_list, "total_2023_hlth_list_FL.xlsx")
```

# Calculate Means by Race and Health Category

``` r
# Helper function to get tidy means and CIs
get_means_and_ci <- function(design, vars) {
  results <- lapply(vars, function(v) {
    form <- as.formula(paste0("~", v))
    
    # 1. Calculate means (proportions for factor variables) and standard errors
    mean_res <- svyby(form, ~racexhisp, design, svymean, na.rm = TRUE)
    
    # 2. Extract components
    estimates <- unlist(coef(mean_res))          # The actual mean/proportion
    std_errors <- unlist(SE(mean_res))           # Standard error
    ci_res <- confint(mean_res)          # 95% Confidence Intervals
    
    # 3. Bind them into a single dataframe
    tidy_df <- data.frame(
      group_and_level = names(estimates),
      mean_proportion = as.numeric(estimates),
      standard_error = as.numeric(std_errors),
      lower_ci = as.numeric(ci_res[, 1]),
      upper_ci = as.numeric(ci_res[, 2]),
      stringsAsFactors = FALSE
    )
    
    # 4. Clean up the names (svyby combines the subgroup and response level 
    #with a colon)
    # e.g., "Hispanic White:genhlthExcellent" -> splits into two columns
    tidy_df <- tidy_df %>%
      separate(group_and_level, into = c("race_ethnicity", "response_level"), 
               sep = ":", extra = "merge") %>%
      # Remove the variable prefix from the response column for cleaner reading
      mutate(response_level = str_remove(response_level, paste0("^", v)))
    
    return(tidy_df)
  })
  
  # Name the sheets based on the variables
  names(results) <- vars
  return(results)
}

# Run the function for 2016 and 2023
mean_ci_2016_list <- get_means_and_ci(dsgn_2016, health_vars)
mean_ci_2023_list <- get_means_and_ci(dsgn_2023, health_vars)

# Export to Excel - Each health variable gets its own sheet with CIs included!
write.xlsx(mean_ci_2016_list, "mean_and_ci_2016_FL.xlsx")
write.xlsx(mean_ci_2023_list, "mean_and_ci_2023_FL.xlsx")
```

# Testing for Significance Differences

``` r
# Extract race levels from the data
race_levels <- levels(df_combined$racexhisp)

# Function to run T-tests across all levels of a specific health variable
run_ttests <- function(var_name, design, race_groups) {
  var_levels <- levels(design$variables[[var_name]])
  results_list <- list()
  
  for (race in race_groups) {
    # Subset design object for specific race
    sub_desgn <- subset(design, racexhisp == race)
    
    # Skip if subset is empty to avoid errors
    if(nrow(sub_desgn$variables) == 0) next
    
    for (lvl in var_levels) {
      formula_obj <- as.formula(paste0("I(", var_name, " == '", lvl, "') ~ year"))
      
      # Use tryCatch to gracefully handle cases where a subgroup doesn't 
      #have variation
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

# Run the function over all health variables
all_ttest_results <- lapply(health_vars, function(var) {
  run_ttests(var, df_comb_dsgn, race_levels)
})

# Name the list elements and export to Excel
names(all_ttest_results) <- health_vars
write.xlsx(all_ttest_results, "ttest_list_FL.xlsx")
```
