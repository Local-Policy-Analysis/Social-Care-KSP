
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
combined_data <- revenue_data_imputed %>%
    full_join(csp, by = c("ons_code", "year")) %>%  
    full_join(S251 %>% 
                  rename(ons_code = new_la_code,  
                         S251_CSC = net_current_expenditure), 
              by = c("ons_code" = "ons_code", "year")) %>%  
    full_join(population_data %>% 
                  rename(ons_code = ecode), 
              by = c("ons_code" = "ons_code", "year"))
# Calculate GNSS and TSE ----
combined_data <- combined_data %>%
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
    arrange(authority, year) %>%
    group_by(authority) %>%
    fill(class, .direction = "downup") %>%
    ungroup()


# England level ----
Eng_data <- combined_data %>%
    filter(class %in% c("UA", "SC", "SD", "MD", "LB")) %>%
    group_by(year) %>%
    summarise(
        adult_pop = sum(`all adult`, na.rm = TRUE),
        child_pop = sum(`under 18`, na.rm = TRUE),
        CSP = sum(csp, na.rm = TRUE),  
        CSC_S251 = sum(S251_CSC, na.rm = TRUE),  
        ASC_RO = sum(RS_NCE_AdultSocialCare, na.rm = TRUE),
        CSC_RO = sum(RS_NCE_ChildrenSocialCare_adj2, na.rm = TRUE),
        GNSS = sum(GNSS, na.rm = TRUE),
        TSE = sum(TSE, na.rm = TRUE),  
    ) %>%
    select(year, 
           adult_pop, 
           child_pop, 
           CSP, 
           CSC_S251, 
           ASC_RO, 
           CSC_RO, 
           GNSS, 
           TSE) %>%
    left_join(gdp_deflator_data %>% select(year, deflator_ratio_2023_24), by = "year") %>% 
    left_join(ASCFR %>% select(year, ASC_ASCFR), by = "year")


Eng_data_LGF <- combined_data %>%
    filter(class %in% c("UA", "SC", "SD", "MD", "LB")) %>%
    group_by(year) %>%
    summarise(
        ASC_RO = sum(RS_NCE_AdultSocialCare, na.rm = TRUE),
        CSC_RO = sum(RS_NCE_ChildrenSocialCare_adj2, na.rm = TRUE),
        GNSS = sum(GNSS, na.rm = TRUE),  
        Trans_RO = sum(RS_NCE_Highwaysandtransportse, na.rm = TRUE) + sum(RS_Other_IntegratedTransportAu, na.rm = TRUE),  
        Hous_RO = sum(RS_NCE_HousingservicesGFRAon, na.rm = TRUE),  
        Cult_RO = sum(RS_NCE_Culturalandrelatedserv, na.rm = TRUE),  
        Env_RO = sum(RS_NCE_Environmentalandregulat, na.rm = TRUE) + sum(RS_Other_WasteDisposalAuthorit, na.rm = TRUE),  
        Plan_RO = sum(RS_NCE_Planninganddevelopment, na.rm = TRUE),  
        Fire_RO = sum(RS_NCE_Fireandrescueservices, na.rm = TRUE),  
        Edu_RO = sum(RS_NCE_Eduservices_adj2nosc, na.rm = TRUE),  
        Other_RO = sum(RS_NCE_Centralservices, na.rm = TRUE) + sum(RS_NCE_Otherservices, na.rm = TRUE),  
        PH_RO = sum(RS_NCE_PublicHealth, na.rm = TRUE)
    ) %>%
    select(year,  
           ASC_RO, 
           CSC_RO, 
           GNSS, 
           Trans_RO,  
           Hous_RO,  
           Cult_RO,  
           Env_RO,  
           Plan_RO,  
           Fire_RO,  
           Edu_RO,  
           Other_RO,  
           PH_RO) %>%
    left_join(gdp_deflator_data %>% select(year, deflator_ratio_2023_24), by = "year")

