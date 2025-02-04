## KSP data processing (RAP version)

principal_authorities <- c("SC", "UA", "MD", "LB", "SD")
outturn_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
all_outturn_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

## DATA PROCESSING ----
# Convert calendar years to financial years ----
csp <- csp %>%
  mutate(year = as.numeric(year),
         year = paste0(year, "/", substr(year + 1, 3, 4)))

population_data <- population_data %>%
  mutate(year = as.numeric(year),
         year = paste0(year, "/", substr(year + 1, 3, 4)))

S251 <- S251 %>%
  mutate(year = as.numeric(time_period),
         year = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, 6)))

ASCFR <- ASCFR %>% 
  mutate(year = gsub("-", "/", Year)) %>% 
  rename(ASC_ASCFR = 'Cash Terms')


# Join the dataframes ----
data_nominal <- revenue_data_imputed %>%
  full_join(csp, by = c("ons_code", "year")) %>%  
  full_join(S251 %>% 
              rename(ons_code = new_la_code,  
                     S251_CSC = net_current_expenditure), 
            by = c("ons_code" = "ons_code", "year")) %>%  
  full_join(population_data %>% 
              rename(ons_code = ecode), 
            by = c("ons_code" = "ons_code", "year"))
# Calculate GNSS and TSE ----
data_nominal <- data_nominal %>%
  mutate(GNSS = rowSums(select(., RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_Highwaysandtransportse, 
                               RS_NCE_HousingservicesGFRAon, RS_NCE_Culturalandrelatedserv, RS_NCE_Environmentalandregulat,
                               RS_NCE_Planninganddevelopment, RS_NCE_Fireandrescueservices, RS_NCE_Eduservices_adj2nosc, 
                               RS_NCE_Centralservices, RS_NCE_Otherservices, RS_Other_IntegratedTransportAu, 
                               RS_Other_WasteDisposalAuthorit, RS_NCE_PublicHealth), na.rm = TRUE)) %>%  
  mutate(TSE = rowSums(select(., RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_Highwaysandtransportse, 
                              RS_NCE_HousingservicesGFRAon, RS_NCE_Culturalandrelatedserv, RS_NCE_Environmentalandregulat,
                              RS_NCE_Planninganddevelopment, RS_NCE_Fireandrescueservices, RS_NCE_Educationservices_adj2, 
                              RS_NCE_Centralservices, RS_NCE_Otherservices, RS_Other_IntegratedTransportAu, 
                              RS_NCE_Policeservices, RS_NCE_PublicHealth), na.rm = TRUE)) %>%
  mutate(authority = coalesce(authority.x, authority.y, authority)) %>%
  select(authority, 
         year, 
         ons_code,  
         class,  
         region,  
         missing_outturn,  
         `all adult`,  
         `under 18`,  
         csp,  
         S251_CSC,  
         RS_NCE_AdultSocialCare,  
         RS_NCE_ChildrenSocialCare_adj2,  
         GNSS, 
         TSE,  
         RS_NCE_TOTALSERVICEEXPENDITURE,  
         RS_NCE_Highwaysandtransportse,  
         RS_NCE_HousingservicesGFRAon,  
         RS_NCE_Culturalandrelatedserv,  
         RS_NCE_Environmentalandregulat,  
         RS_NCE_Planninganddevelopment,  
         RS_NCE_Fireandrescueservices,  
         RS_NCE_Eduservices_adj2nosc,  
         RS_NCE_Centralservices,  
         RS_NCE_Otherservices,  
         RS_Other_IntegratedTransportAu,  
         RS_Other_WasteDisposalAuthorit,  
         RS_NCE_PublicHealth) %>%
  filter(year %in% all_outturn_years) %>%
  arrange(authority, year) %>%
  group_by(authority) %>%
  fill(class, .direction = "downup") %>%
  ungroup()



# Create a total england and total principal row ----

identifying_columns <- c("authority", "year", "ons_code", "class", "region", "missing_outturn", "all adult", "under 18")
finance_columns <- setdiff(names(data_nominal), identifying_columns)

data_nominal <- data_nominal %>%
  bind_rows(
    data_nominal %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                `all adult` = sum(`all adult`, na.rm=TRUE),
                `under 18` = sum(`under 18`, na.rm=TRUE),
.groups = "drop") %>%
      mutate(class = "ENGLAND", authority = "ENGLAND"),
    
    data_nominal %>%
      filter(class %in% principal_authorities) %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
      mutate(class = "PRINCIPAL", authority = "PRINCIPAL")
  )

# add in per capita info ----
data_nominal <- data_nominal %>% 
  mutate(RS_NCE_AdultSocialCare_pc = RS_NCE_AdultSocialCare/`all adult`,
         RS_NCE_ChildrenSocialCare_adj2_pc = RS_NCE_ChildrenSocialCare_adj2/`under 18`,
           S251_CSC_pc = S251_CSC/`under 18`)


# real terms (most recent year) ----
data_real_2023 <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2023_24)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x*deflator_ratio_2023_24))

# real terms (2015 as base) ----
data_real_2015 <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2015_16)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x*deflator_ratio_2015_16))




## ----

##



England_summary <- data_nominal2 %>%
  group_by(year) %>%
  summarise(across(all_of(finance_columns), \(x) sum(x, na.rm=TRUE)))

#### ----

## one table in nominal terms
## one table in real terms (most recent year)
## one table in real terms (2015/16=100)
