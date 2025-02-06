## KSP data processing (RAP version)

principal_authorities <- c("SC", "UA", "MD", "LB", "SD")
outturn_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
all_outturn_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

## DATA PROCESSING ----
# Join the dataframes ----
data_nominal <- revenue_data_imputed %>%
  left_join(csp, by = c("ons_code", "year")) %>%
  mutate(authority = coalesce(authority.x, authority.y)) %>%
  mutate(ecode = coalesce(ecode.x, ecode.y)) %>%
  dplyr:: select(year, authority, ecode, class, region, ons_code, missing_outturn, csp,
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
                 RS_NCE_PublicHealth,
                 RS_NCE_AdultSocialCare,  
                 RS_NCE_ChildrenSocialCare_adj2,
                 RS_NCE_Educationservices_adj2,
                 RS_NCE_Policeservices)
  
 data_nominal <- data_nominal %>%
  full_join(S251 %>% 
              rename(ons_code = new_la_code,  
                     S251_CSC = net_current_expenditure) %>%
              dplyr:: select(ons_code, year, S251_CSC), 
            by = c("ons_code" = "ons_code", "year")) 
 
data_nominal <- data_nominal %>% 
  full_join(population_data %>% 
              rename(ons_code = ecode), 
            by = c("ons_code", "year")) %>%
  mutate(authority = coalesce(authority.x, authority.y)) %>%
  dplyr:: select(-authority.x, -authority.y) %>%
  dplyr:: select(authority, year, ecode, ons_code, everything())

data_nominal <- data_nominal %>%
  arrange(authority, year) %>%
  group_by(authority) %>%
  fill(class, .direction = "downup") %>%
  ungroup() %>%
  filter(year %in% all_outturn_years)

# Calculate GNSS and TSE and TSC ----
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
  mutate(TSC = rowSums(select(., RS_NCE_ChildrenSocialCare_adj2, RS_NCE_AdultSocialCare), na.rm=TRUE))
       
       

# Create a total england and total principal row ----

identifying_columns <- c("authority", "year", "ons_code", "class", "region", "missing_outturn", "all adult", "under 18", "ecode", "85+", "older age adults", "working age adults")
finance_columns <- setdiff(names(data_nominal), identifying_columns)

data_nominal <- data_nominal %>%
  bind_rows(
    data_nominal %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                `all adult` = sum(`all adult`, na.rm=TRUE),
                `under 18` = sum(`under 18`, na.rm=TRUE),
.groups = "drop") %>%
      mutate(class = "ENGLAND", authority = "ENGLAND", ecode = "ENGLAND"),
    
    data_nominal %>%
      filter(class %in% principal_authorities) %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                `all adult` = sum(`all adult`, na.rm=TRUE),
                `under 18` = sum(`under 18`, na.rm=TRUE),
                .groups = "drop") %>%
      mutate(class = "PRINCIPAL", authority = "PRINCIPAL", ecode = "PRINCIPAL")
  )


# add in ASCFR values to ENGLAND and PRINCIPAL row ---- 
data_nominal <- data_nominal %>%
  left_join((ASCFR %>% dplyr:: select(year, ASC_ASCFR)), by = "year") %>%  
  mutate(ASC_ASCFR = if_else(authority %in% c("ENGLAND", "PRINCIPAL"), ASC_ASCFR, NA_real_))

# add in per capita info ----
data_nominal <- data_nominal %>% 
  mutate(RS_NCE_AdultSocialCare_pc = RS_NCE_AdultSocialCare/`all adult`,
         RS_NCE_ChildrenSocialCare_adj2_pc = RS_NCE_ChildrenSocialCare_adj2/`under 18`,
         S251_CSC_pc = S251_CSC/`under 18`,
         ASC_ASCFR_pc = ASC_ASCFR/`all adult`)


# real terms (most recent year) ----
finance_columns <- setdiff(names(data_nominal), identifying_columns)
data_real_2023 <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2023_24)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x/deflator_ratio_2023_24))

# real terms (2015 as base) ----
data_real_2015 <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2015_16)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x/deflator_ratio_2015_16))




# indexed (with 2015/16) = 100
#first we need one row per authority-year combo

aggregated_df <- data_real_2023 %>%
  group_by(ecode, year) %>%
  summarise(across(all_of(finance_columns), 
                   ~ if (all(is.na(.))) NA_real_ else max(., na.rm = TRUE)),  # Handle all NA cases
            .groups = "drop")

reference_values <- aggregated_df %>%
  filter(year == "2015/16") %>%
  select(ecode, all_of(finance_columns)) %>%
  rename_with(~ paste0(.x, "_ref"), all_of(finance_columns))  # Rename reference columns

indexed_df <- aggregated_df %>%
  left_join(reference_values, by = "ecode") %>%  # Join reference values
  mutate(across(all_of(finance_columns), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         . / get(paste0(cur_column(), "_ref")) * 100, 
                         NA_real_))) %>%  # Perform indexing
  select(-ends_with("_ref"))  # Remove reference columns

indexed_df %>% filter(ecode=="PRINCIPAL") %>% dplyr::select(year, RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2)

data_real_2023 %>% filter(ecode=="PRINCIPAL") %>% dplyr::select(year, RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2)


## ----

#### TABLES ----
wb <- createWorkbook()

## 1)	ASC NCE (RO): nominal, real and annual real % change ----

table_1 <- bind_rows(
  data_nominal %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Nominal"), 
  data_real_2023 %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Real")
) %>%
  select(year, source, RS_NCE_AdultSocialCare) %>%
  arrange(year, source) %>%
  pivot_wider(names_from = source, values_from= RS_NCE_AdultSocialCare) %>%
  mutate(Nominal = Nominal/1e6,
         Real = Real/ 1e6,
        `Real YoY% change` = (Real/ lag(Real)-1)*100) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from= year, values_from = Value) %>%
  mutate(
    `2022/23 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2022/23`) / `2022/23` * 100, 1),
      NA_real_
    ),
    `2015/16 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2015/16`) / `2015/16` * 100, 1),
      NA_real_
    ),
    `2010/11 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2010/11`) / `2010/11` * 100, 1),
      NA_real_
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) 

addWorksheet(wb, "Table 1")
writeData(wb, "Table 1", table_1)

## 2)	ASC NCE (ASC-FR): nominal, real and annual real % change ----
table_2 <- bind_rows(
  data_nominal %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Nominal"), 
  data_real_2023 %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Real")
) %>%
  select(year, source, ASC_ASCFR) %>%
  arrange(year, source) %>%
  pivot_wider(names_from = source, values_from= ASC_ASCFR) %>%
  mutate(Nominal = Nominal/1e6,
         Real = Real/ 1e6,
         `Real YoY% change` = (Real/ lag(Real)-1)*100) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from= year, values_from = Value) %>%
  mutate(
    `2022/23 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2022/23`) / `2022/23` * 100, 1),
      NA_real_
    ),
    `2015/16 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2015/16`) / `2015/16` * 100, 1),
      NA_real_
    ),
    `2010/11 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2010/11`) / `2010/11` * 100, 1),
      NA_real_
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) 

addWorksheet(wb, "Table 2")
writeData(wb, "Table 2", table_2)

## 3)	ASC NCE including NHS funding and BCF: nominal, real and annual real % change ----
## 4)	CSC NCE (RO): nominal, real and annual real % change ----
table_4 <- bind_rows(
  data_nominal %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Nominal"), 
  data_real_2023 %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Real")
) %>%
  select(year, source, RS_NCE_ChildrenSocialCare_adj2) %>%
  arrange(year, source) %>%
  pivot_wider(names_from = source, values_from= RS_NCE_ChildrenSocialCare_adj2) %>%
  mutate(Nominal = Nominal/1e6,
         Real = Real/ 1e6,
         `Real YoY% change` = (Real/ lag(Real)-1)*100) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from= year, values_from = Value) %>%
  mutate(
    `2022/23 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2022/23`) / `2022/23` * 100, 1),
      NA_real_
    ),
    `2015/16 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2015/16`) / `2015/16` * 100, 1),
      NA_real_
    ),
    `2010/11 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2010/11`) / `2010/11` * 100, 1),
      NA_real_
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) 

addWorksheet(wb, "Table 4")
writeData(wb, "Table 4", table_4)

## 5)	CSC S251: nominal, real and annual real % change ----
table_5 <- bind_rows(
  data_nominal %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Nominal"), 
  data_real_2023 %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Real")
) %>%
  select(year, source, S251_CSC) %>%
  arrange(year, source) %>%
  pivot_wider(names_from = source, values_from= S251_CSC) %>%
  mutate(Nominal = Nominal/1e6,
         Real = Real/ 1e6,
         `Real YoY% change` = (Real/ lag(Real)-1)*100) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from= year, values_from = Value) %>%
  mutate(
    `2022/23 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2022/23`) / `2022/23` * 100, 1),
      NA_real_
    ),
    `2015/16 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2015/16`) / `2015/16` * 100, 1),
      NA_real_
    ),
    `2010/11 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2010/11`) / `2010/11` * 100, 1),
      NA_real_
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) 

addWorksheet(wb, "Table 5")
writeData(wb, "Table 5", table_5)

## 6)	Total Social Care NCE (RO): nominal, real and annual real % change ----
table_6 <- bind_rows(
  data_nominal %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Nominal"), 
  data_real_2023 %>% 
    filter(authority == "ENGLAND") %>%
    mutate(source="Real")
) %>%
  select(year, source, TSC) %>%
  arrange(year, source) %>%
  pivot_wider(names_from = source, values_from= TSC) %>%
  mutate(Nominal = Nominal/1e6,
         Real = Real/ 1e6,
         `Real YoY% change` = (Real/ lag(Real)-1)*100) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from= year, values_from = Value) %>%
  mutate(
    `2022/23 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2022/23`) / `2022/23` * 100, 1),
      NA_real_
    ),
    `2015/16 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2015/16`) / `2015/16` * 100, 1),
      NA_real_
    ),
    `2010/11 to 2023/24 % Change` = ifelse(
      Metric %in% c("Nominal", "Real"),
      round((`2023/24` - `2010/11`) / `2010/11` * 100, 1),
      NA_real_
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) 

addWorksheet(wb, "Table 6")
writeData(wb, "Table 6", table_6)


saveWorkbook(wb, "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Tables/tables_2023.xlsx", overwrite = TRUE)
## ----
