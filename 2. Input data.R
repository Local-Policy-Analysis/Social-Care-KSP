###############################################################################
##                              *INPUT DATA*                                 ##
###############################################################################
# Author:     Becky Foster & Pierre Lack
# Script Description: Input data set up for the KSP project
# -----------------------------------------------------------------------------


## FUNCTIONS ----
remotes::install_local('Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/fosteR_0.1.0.tar.gz', dependencies = TRUE)
library(fosteR)
    ## GDP deflator function ----
fosteR::get_latest_gdp_deflator(c("2023/24", "2015/16"), covid = TRUE)
    ## Set of imputation functions ----

## DATASETS ----
    ## Imputed revenue database ----
load("Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Revenue/Revenue_R/Final Output/revenue_data_imputed_full.RData")

    ## Population data ----
    population_path <- "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Inputs/Population/myebtablesenglandwales20112023.xlsx"
      population_estimates <- read_excel(population_path, sheet="MYEB1", skip=1)
      
    # Desired outputs: for each year, child pop, 18+ population (all adult), 65+ population (older adults), and 18-64 (working age adults)
    
    # First, reshape the data to long format and rename columns for ease of RAP. Also get rid of Welsh authorities
    population_data <- population_estimates %>% 
      pivot_longer(cols = starts_with("population"), names_to = "year", values_to= "population") %>%
      mutate(year= as.numeric(sub("population_", "", year))) %>%
      rename_with(~ gsub("^ladcode.*", "ecode", .x)) %>%
      rename_with(~ gsub("^laname.*", "authority", .x)) %>%
      filter(country == "E") %>%
      dplyr:: select(-country)
    
    population_data <- population_data %>%
      mutate(year = as.numeric(year),
             year = paste0(year, "/", substr(year + 1, 3, 4)))
    
    # We are not interested in the sex split so get rid 
    population_data <- population_data %>%
      group_by(ecode, authority, age, year) %>%
      summarise(population = sum(population), .groups = 'drop')
    
    # We create categories for all adult, working age adults and older adults, and then get the counts for each category in each authority-year
    population_data <- population_data %>%
      mutate(
        age_group = case_when(
          age >= 85 ~ "85+",
          age >= 65 ~ "older age adults",
          age >= 18 & age < 65 ~ "working age adults",
          age >= 18 ~ "all adult",
          TRUE ~ "under 18"
        )
      )  %>%
      group_by(ecode, authority, year, age_group) %>%
      summarise(population = sum(population, na.rm = TRUE)) %>%
      pivot_wider(names_from = age_group, values_from = population, values_fill = list(population = 0)) %>%
      mutate(`all adult` = `working age adults` + `older age adults`)
    
    rm(population_estimates)

    ## CSP ----
    remove_all_but_final_underscore <- function(col_name) {
      underscores <- gregexpr("_", col_name)[[1]]
      if (length(underscores) > 1) {
        # Extract everything before the last underscore
        last_underscore_pos <- underscores[length(underscores)]
        prefix <- sub("_[^_]*$", "", col_name)  # Remove the last segment
        suffix <- substr(col_name, last_underscore_pos, nchar(col_name))
        paste0(gsub("_", "", prefix), suffix)
      } else {
        col_name
      }
    }
    
    FYstart <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
    CSPyears <- c("csp_2015", "csp_2016", "csp_2017", "csp_2018", "csp_2019", "csp_2020", "csp_2021", "csp_2022", "csp_2023", "csp_2024")
    csp_data <- read_excel("Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Inputs/CSP/CSP_information_table_2024-25_Final_Settlement.xlsx", sheet="input_data")
    csp <- csp_data %>%
      dplyr::select(contains(c('ons', 'authority', 'ecode')), all_of(CSPyears)) %>% rename_with(~ sapply(.x, remove_all_but_final_underscore)) %>%
      mutate_at(-c(1:3), as.numeric) %>% 
      pivot_longer(cols=ends_with(c(FYstart)), names_to= c(".value", "year"), names_sep="_") %>%
      filter(ecode!="TE")
    
    ## Changing reporting level
    # Given that CSP is in millions, but we want 1000's for consistency with other datasets (at least initially)
    
    csp <- csp %>%
      mutate(across(
        .cols = where(is.numeric) & !all_of(c("ons_code", "authority", "ecode", "year")),
        .fns = ~ .x * 1000
      ))
    
    rm(csp_data)
    
    csp <- csp %>%
      mutate(ecode = str_remove(ecode, "O$"),  # Remove 'O' only if it is the last character
             ons_code = str_remove(ons_code, "O$"))
    
    # we have some duplicate year/ ecode/ ons_code combos, so removing
    csp <- csp %>%
      group_by(ecode, year) %>%
      slice_max(csp, n = 1) %>%  # Keep the row with the largest 'csp'
      ungroup()
    
    csp <- csp %>%
      mutate(year = as.numeric(year),
             year = paste0(year, "/", substr(year + 1, 3, 4)))
    
    ## S251 ----
S251 <- read_excel("Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Inputs/S251/S251.xlsx")
    #S251 is in £'s, but we need in £1000's for consistency
    S251 <- S251 %>%
      mutate(net_current_expenditure = net_current_expenditure/1000)
    
    S251 <- S251 %>%
      mutate(year = as.numeric(time_period),
             year = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, 6)))
    
    #Pierre's manual fix for incorrect ecodes
    S251 <- S251 %>%
      mutate(new_la_code = ifelse(new_la_code == "E06000060" & year %in% c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20"), 
                                  "E10000002", new_la_code))
    population_data <- population_data %>%
      mutate(ecode = case_when(
        ecode == "E06000060" & year %in% c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20") ~ "E10000002",
        ecode == "E06000058" & year %in% c("2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19") ~ "E06000028",
        TRUE ~ ecode
      ))
    
    ## ASC-FR and NHS ----
ASCFR <- read_excel("Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Inputs/ASCFR/ASCFR (NCE) Data Tables 2023-24.xlsx", 
                    sheet = "T4", 
                    skip = 6)
ASCFR <- head(ASCFR, -4)

ASCFR <- ASCFR %>% 
  mutate(year = gsub("-", "/", Year)) %>% 
  rename(ASC_ASCFR = 'Cash Terms')




