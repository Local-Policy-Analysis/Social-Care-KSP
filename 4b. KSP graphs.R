
# Create per capita versions of SC variables ----
Eng_data$ASC_ASCFR_pc <- Eng_data$ASC_ASCFR / Eng_data$adult_pop
Eng_data$CSC_S251_pc <- Eng_data$CSC_S251 / Eng_data$child_pop
Eng_data$ASC_ASC_RO_pc <- Eng_data$ASC_RO / Eng_data$adult_pop

# Define colours for variables ----
variable_colours <- c(
    "ASC_RO" = chart_palette[1],
    "ASC_ASCFR" = chart_palette[1],
    "ASC_ASCFR_pc" = chart_palette[2],
    "CSC_RO" = chart_palette[2],
    "CSC_S251" = chart_palette[2],
    "CSC_S251_pc" = chart_palette[1],
    "SC_RO" = chart_palette[15],
    "TSE" = chart_palette[9],
    "GNSS" = chart_palette[3],
    "CSP" = chart_palette[13],
    "ASC_perc_GNSS" = chart_palette[1],
    "CSC_perc_GNSS" = chart_palette[2],
    "ASC_perc_TSE" = chart_palette[1],
    "CSC_perc_TSE" = chart_palette[2],
    "ASC_perc_CSP" = chart_palette[1],
    "CSC_perc_CSP" = chart_palette[2]
)

variable_names <- c(
    "ASC_RO" = "ASC (RO)",
    "ASC_ASCFR" = "ASC (ASC-FR)",
    "ASC_ASCFR_pc" = "ASC (ASC-FR) per capita",
    "CSC_RO" = "CSC (RO)",
    "CSC_S251" = "CSC (S251)",
    "CSC_S251_pc" = "CSC (S251) per capita",
    "SC_RO" = "Total Social Care (RO)",
    "TSE" = "TSE (RO)",
    "GNSS" = "GNSS (RO)",
    "CSP" = "CSP",
    "ASC_perc_TSE" = "ASC as a % of TSE",
    "CSC_perc_TSE" = "CSC as a % of TSE", 
    "SC_perc_TSE" = "SC as a % of TSE",
    "ASC_perc_GNSS" = "ASC as a % of GNSS",
    "CSC_perc_GNSS" = "CSC as a % of GNSS",
    "SC_perc_GNSS" = "SC as a % of GNSS",
    "ASC_perc_CSP" = "ASC as a % of CSP",
    "CSC_perc_CSP" = "CSC as a % of CSP",  
    "SC_perc_CSP" = "SC as a % of CSP"
)

variable_names_LGF <- c(
  "ASC_RO" = "Adult Social Care",
  "CSC_RO" = "Children's Social Care",
  "GNSS" = "General Net Service Spend",  
  "Trans_RO" = "Highways and Transport",  
  "Hous_RO" = "Housing (excl. HRA)", 
  "Cult_RO" = "Cultural Services", 
  "Env_RO" = "Environmental and Regulatory",
  "Plan_RO" = "Planning and Development",  
  "Fire_RO" = "Fire and Rescue",  
  "Edu_RO" = "Education (excl. schools grant)",  
  "Other_RO" = "Other Services",  
  "PH_RO" = "Public Health"
)

variable_colours_LGF <- c(
  "ASC_RO" = chart_palette[1],
  "CSC_RO" = chart_palette[2],
  "GNSS" = chart_palette[3], 
  "Trans_RO" = chart_palette[4], 
  "Hous_RO" = chart_palette[5],
  "Cult_RO" = chart_palette[6], 
  "Env_RO" = chart_palette[7],
  "Plan_RO" = chart_palette[8], 
  "Fire_RO" = chart_palette[9],
  "Edu_RO" = chart_palette[10],
  "Other_RO" = chart_palette[11],
  "PH_RO" = chart_palette[12]
)


#### ASC graphs ####
  ## ASC bar (figure 1) ----
  ASC_bar <- ggplot(Eng_data %>% 
             filter(year >= '2015/16' & year <= '2023/24') %>%
             mutate(
                 ASC_RO = (ASC_RO / deflator_ratio_2023_24 * 1000) / 1000000000,
                 ASC_ASCFR = (ASC_ASCFR / deflator_ratio_2023_24 * 1000) / 1000000000,
             ) %>%
             pivot_longer(cols = c(ASC_RO, ASC_ASCFR),
                          names_to = "variable", values_to = "value") %>%
             mutate(variable = factor(variable, 
                                      levels = c("ASC_RO", 
                                                 "ASC_ASCFR"))),
         aes(x = year, y = value, fill = variable, pattern = variable)) +
      geom_bar_pattern(stat = "identity", position = "dodge", 
                       pattern_fill = "white", pattern_density = 0.3, 
                       pattern_spacing = 0.02, pattern_angle = 45, 
                       colour = "black") +  
      scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +  
      scale_pattern_manual(values = c("ASC_RO" = "none", 
                                      "ASC_ASCFR" = "stripe"),  
                           labels = variable_names) +
      labs(title = "Real terms ASC Net Current Expenditure (NCE): RO & ASC-FR, 
  All principal LAs, 2015-16 to 2023-24, £bns (2023-24 prices)",
           x = NULL,
           y = "£bns (2023-24 prices)",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  ## ASC line (figure 2) ----
  ASC_line <- ggplot(Eng_data %>% 
                         filter(year >= '2015/16' & year <= '2023/24') %>%
                         mutate(
                             ASC_RO = ASC_RO / deflator_ratio_2023_24,
                             ASC_ASCFR = ASC_ASCFR / deflator_ratio_2023_24,
                             ASC_ASCFR_pc = ASC_ASCFR_pc / deflator_ratio_2023_24
                         ) %>%
                         pivot_longer(cols = c(ASC_RO, ASC_ASCFR, ASC_ASCFR_pc),
                                      names_to = "variable", values_to = "value") %>%
                         group_by(variable) %>%
                         mutate(indexed_value = value / value[year == '2015/16'] * 100,  
                                percentage_increase = (indexed_value[year == '2023/24'] - 100)),
                     aes(x = year, 
                         y = indexed_value, 
                         color = variable, 
                         linetype = variable,  
                         group = variable)) +
      geom_line(linewidth = 1) +  
      geom_text(data = . %>% filter(year == '2023/24'),
                aes(label = ifelse(percentage_increase > 0, 
                                   paste0("+", round(percentage_increase, 1), "%"), 
                                   paste0(round(percentage_increase, 1), "%"))),
                hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
      scale_y_continuous(limits = c(100, 130), breaks = seq(100, 130, by = 10)) +
      scale_color_manual(values = c("ASC_RO" = "#00625E", 
                                    "ASC_ASCFR" = "#00625E", 
                                    "ASC_ASCFR_pc" = "#932A72"),  
                         labels = variable_names) +
      scale_linetype_manual(values = c("ASC_RO" = "solid", 
                                       "ASC_ASCFR" = "dotdash", 
                                       "ASC_ASCFR_pc" = "dotdash"),  
                            labels = variable_names) +
      labs(title = "Indexed real terms ASC Net Current Expenditure (NCE): RO & ASC-FR, 
  All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
           x = NULL,
           y = "2015/16 = 100",
           color = NULL,
           linetype = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  
  

#### CSC graphs ####
  ## CSC bar (figure 3) ----
  CSC_bar <- ggplot(Eng_data %>% 
                            filter(year >= '2015/16' & year <= '2023/24') %>%
                            mutate(
                                CSC_RO = (CSC_RO / deflator_ratio_2023_24 * 1000) / 1000000000,
                                CSC_S251 = (CSC_S251 / deflator_ratio_2023_24) / 1000000000,
                            ) %>%
                            pivot_longer(cols = c(CSC_RO, CSC_S251),
                                         names_to = "variable", values_to = "value") %>%
                            mutate(variable = factor(variable, 
                                                     levels = c("CSC_RO", 
                                                                "CSC_S251"))),
                        aes(x = year, y = value, fill = variable, pattern = variable)) +
      geom_bar_pattern(stat = "identity", position = "dodge", 
                       pattern_fill = "white", pattern_density = 0.3, 
                       pattern_spacing = 0.02, pattern_angle = 45, 
                       colour = "black") +  
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +  
      scale_pattern_manual(values = c("CSC_RO" = "none", 
                                      "CSC_S251" = "stripe"),  
                           labels = variable_names) +
      labs(title = "Real terms CSC Net Current Expenditure (NCE): RO & S251, 
  All principal LAs, 2015-16 to 2023-24, £bns (2023-24 prices)",
           x = NULL,
           y = "£bns (2023-24 prices)",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  ## CSC line (figure 4) ----
  CSC_line <- ggplot(Eng_data %>% 
                            filter(year >= '2015/16' & year <= '2023/24') %>%
                            mutate(
                                CSC_RO = CSC_RO / deflator_ratio_2023_24,
                                CSC_S251 = CSC_S251 / deflator_ratio_2023_24,
                                CSC_S251_pc = CSC_S251_pc / deflator_ratio_2023_24
                            ) %>%
                            pivot_longer(cols = c(CSC_RO, CSC_S251, CSC_S251_pc),
                                         names_to = "variable", values_to = "value") %>%
                            group_by(variable) %>%
                            mutate(indexed_value = value / value[year == '2015/16'] * 100,  
                                   percentage_increase = (indexed_value[year == '2023/24'] - 100)),
                        aes(x = year, 
                            y = indexed_value, 
                            color = variable, 
                            linetype = variable,  
                            group = variable)) +
      geom_line(linewidth = 1) +  
      geom_text(data = . %>% filter(year == '2023/24'),
                aes(label = ifelse(percentage_increase > 0, 
                                   paste0("+", round(percentage_increase, 1), "%"), 
                                   paste0(round(percentage_increase, 1), "%"))),
                hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
    scale_y_continuous(limits = c(95, 140), breaks = seq(100, 140, by = 10)) +
      scale_color_manual(values = variable_colours,  
                         labels = variable_names) +
      scale_linetype_manual(values = c("CSC_RO" = "solid", 
                                       "CSC_S251" = "dotdash", 
                                       "CSC_S251_pc" = "dotdash"),  
                            labels = variable_names) +
      labs(title = "Indexed real terms CSC Net Current Expenditure (NCE): RO & S251, 
  All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
           x = NULL,
           y = "2015/16 = 100",
           color = NULL,
           linetype = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  

#### SC charts ####
  ## SC bar (figure 5) ----
  SC_bar <- ggplot(Eng_data %>% 
                       filter(year >= '2015/16' & year <= '2023/24') %>%
                       mutate(
                           ASC_RO = (ASC_RO / deflator_ratio_2023_24 * 1000) / 1000000000,
                           CSC_RO = (CSC_RO / deflator_ratio_2023_24 * 1000) / 1000000000,
                           Total = ASC_RO + CSC_RO,
                       ) %>%
                       pivot_longer(cols = c(ASC_RO, CSC_RO),
                                    names_to = "variable", values_to = "value") %>%
                       mutate(variable = factor(variable, 
                                                levels = c("ASC_RO", 
                                                           "CSC_RO"))),  
                   aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10)) +
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +
      labs(title = "Real terms Total Social Care (TSC) Net Current Expenditure (NCE): RO, 
  All principal LAs, 2015-16 to 2023-24, £bns (2023-24 prices)",
           x = NULL,
           y = "£bns (2023-24 prices)",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank()) +
      geom_text(aes(label = round(value, 1)), 
                position = position_stack(vjust = 0.5), 
                color = "white", 
                fontface = "bold") +
      geom_text(aes(x = year, y = Total, label = round(Total, 1)),
                vjust = -0.5,
                fontface = "bold")
  
  ## SC line (figure 6) ----
  SC_line <- ggplot(Eng_data %>% 
                         filter(year >= '2015/16' & year <= '2023/24') %>%
                         mutate(
                             SC_RO = (CSC_RO + ASC_RO) / deflator_ratio_2023_24,
                         ) %>%
                         pivot_longer(cols = c(SC_RO),
                                      names_to = "variable", values_to = "value") %>%
                         group_by(variable) %>%
                         mutate(indexed_value = value / value[year == '2015/16'] * 100,  
                                percentage_increase = (indexed_value[year == '2023/24'] - 100)),
                     aes(x = year, 
                         y = indexed_value, 
                         color = variable, 
                         linetype = variable,  
                         group = variable)) +
      geom_line(linewidth = 1) +  
      geom_text(data = . %>% filter(year == '2023/24'),
                aes(label = ifelse(percentage_increase > 0, 
                                   paste0("+", round(percentage_increase, 1), "%"), 
                                   paste0(round(percentage_increase, 1), "%"))),
                hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
      scale_color_manual(values = variable_colours,  
                         labels = variable_names) +
      scale_linetype_manual(values = c("SC_RO" = "solid"), 
                            labels = variable_names) +
      labs(title = "Indexed real terms Total Social Care (TSC) Net Current Expenditure (NCE): RO, 
  All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
           x = NULL,
           y = "2015/16 = 100",
           color = NULL,
           linetype = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  
  
#### LG spend graphs ####
  ## LG spend line (figure 7) ----
  LG_spend_line <- ggplot(Eng_data %>% 
                              filter(year >= '2015/16' & year <= '2023/24') %>%
                              mutate(
                                  ASC_RO = ASC_RO / deflator_ratio_2023_24,
                                  CSC_RO = CSC_RO / deflator_ratio_2023_24,
                                  SC_RO = ASC_RO + CSC_RO,
                                  TSE = TSE / deflator_ratio_2023_24,
                                  GNSS = GNSS / deflator_ratio_2023_24,  
                                  CSP = (CSP * 1000) / deflator_ratio_2023_24
                              ) %>%
                              pivot_longer(cols = c(ASC_RO, CSC_RO, SC_RO, TSE, GNSS, CSP),
                                           names_to = "variable", values_to = "value") %>%
                              group_by(variable) %>%
                              mutate(indexed_value = value / value[year == '2015/16'] * 100,  
                                     percentage_increase = (indexed_value[year == '2023/24'] - 100)),
                          aes(x = year, 
                              y = indexed_value, 
                              color = variable, 
                              linetype = variable,  
                              group = variable)) +
      geom_line(linewidth = 1) +  
      geom_text(data = . %>% filter(year == '2023/24'),
                aes(label = ifelse(percentage_increase > 0, 
                                   paste0("+", round(percentage_increase, 1), "%"), 
                                   paste0(round(percentage_increase, 1), "%"))),
                hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
    scale_y_continuous(limits = c(90, 140), breaks = seq(90, 140, by = 10)) +
      scale_color_manual(values = variable_colours,  
                         labels = variable_names) +
      scale_linetype_manual(values = c("ASC_RO" = "solid", 
                                       "CSC_RO" = "solid", 
                                       "SC_RO" = "solid", 
                                       "TSE" = "dotdash",  
                                       "GNSS" = "dotdash",  
                                       "CSP" = "dotdash"),  
                            labels = variable_names) +
      labs(title = "Indexed real terms social care and total LG spend: RO, 
  All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
           x = NULL,
           y = "2015/16 = 100",
           color = NULL,
           linetype = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank())
  
  
  ## SC as % of GNSS (figure 9) ----
  SC_perc_GNSS <- ggplot(Eng_data %>% 
                             filter(year >= '2015/16' & year <= '2023/24') %>%
                             mutate(
                                 ASC_perc_GNSS = ASC_RO / GNSS,  
                                 CSC_perc_GNSS = CSC_RO / GNSS,  
                                 SC_perc_GNSS = (ASC_RO + CSC_RO) / GNSS,
                             ) %>%
                             pivot_longer(cols = c(ASC_perc_GNSS, CSC_perc_GNSS),
                                          names_to = "variable", values_to = "value") %>%
                             mutate(variable = factor(variable, 
                                                      levels = c("ASC_perc_GNSS", 
                                                                 "CSC_perc_GNSS"))),  
                         aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1), labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +
      labs(title = "Social Care NCE as a proportion of General Net Service Spend: RO, 
  All principal LAs, 2015-16 to 2023-24",
           x = NULL,
           y = "% of GNSS",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank()) +
      geom_text(aes(label = paste0(round(value * 100), "%")),  
                position = position_stack(vjust = 0.5), 
                color = "white", 
                fontface = "bold") +
      geom_text(aes(x = year, y = SC_perc_GNSS, label = paste0(round(SC_perc_GNSS * 100), "%")),  
                vjust = -0.5,
                fontface = "bold")
  
  ## SC as % of TSE (figure 10) ----
  SC_perc_TSE <- ggplot(Eng_data %>% 
                             filter(year >= '2015/16' & year <= '2023/24') %>%
                             mutate(
                                 ASC_perc_TSE = ASC_RO / TSE,  
                                 CSC_perc_TSE = CSC_RO / TSE,  
                                 SC_perc_TSE = (ASC_RO + CSC_RO) / TSE,
                             ) %>%
                             pivot_longer(cols = c(ASC_perc_TSE, CSC_perc_TSE),
                                          names_to = "variable", values_to = "value") %>%
                             mutate(variable = factor(variable, 
                                                      levels = c("ASC_perc_TSE", 
                                                                 "CSC_perc_TSE"))),  
                         aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +
      scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1), labels = percent_format(accuracy = 1)) +  # Format y-axis labels as percentages
      labs(title = "Social Care NCE as a proportion of Total Service Expenditure: RO, 
  All principal LAs, 2015-16 to 2023-24",
           x = NULL,
           y = "% of TSE",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank()) +
      geom_text(aes(label = paste0(round(value * 100), "%")),  
                position = position_stack(vjust = 0.5), 
                color = "white", 
                fontface = "bold") +
      geom_text(aes(x = year, y = SC_perc_TSE, label = paste0(round(SC_perc_TSE * 100), "%")),  
                vjust = -0.5,
                fontface = "bold")
  
  
  ## SC as % of CSP (figure 11) ----
  SC_perc_CSP <- ggplot(Eng_data %>% 
                            filter(year >= '2015/16' & year <= '2023/24') %>%
                            mutate(
                                ASC_perc_CSP = ASC_RO / (CSP),  
                                CSC_perc_CSP = CSC_RO / (CSP),  
                                SC_perc_CSP = (ASC_RO + CSC_RO) / (CSP),
                            ) %>%
                            pivot_longer(cols = c(ASC_perc_CSP, CSC_perc_CSP),
                                         names_to = "variable", values_to = "value") %>%
                            mutate(variable = factor(variable, 
                                                     levels = c("ASC_perc_CSP", 
                                                                "CSC_perc_CSP"))),  
                        aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = variable_colours,  
                        labels = variable_names) +
      scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.1), labels = percent_format(accuracy = 1)) +  # Format y-axis labels as percentages
      labs(title = "Social Care NCE as a proportion of Core Spending Power, 
  All principal LAs, 2015-16 to 2023-24",
           x = NULL,
           y = "% of CSP",
           fill = NULL,  
           pattern = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank()) +
      geom_text(aes(label = paste0(round(value * 100), "%")),  
                position = position_stack(vjust = 0.5), 
                color = "white", 
                fontface = "bold") +
      geom_text(aes(x = year, y = SC_perc_CSP, label = paste0(round(SC_perc_CSP * 100), "%")),  
                vjust = -0.5,
                fontface = "bold")
  
  ## LG spend line for all services (figure NEW) ----
  LG_spend_line_all <- ggplot(Eng_data_LGF %>% 
                                  filter(year >= '2015/16' & year <= '2023/24') %>%
                                  mutate(
                                      ASC_RO = ASC_RO / deflator_ratio_2023_24,
                                      CSC_RO = CSC_RO / deflator_ratio_2023_24,
                                      GNSS = GNSS / deflator_ratio_2023_24, 
                                      Trans_RO = Trans_RO / deflator_ratio_2023_24,  
                                      Hous_RO = Hous_RO / deflator_ratio_2023_24,  
                                      Cult_RO = Cult_RO / deflator_ratio_2023_24,  
                                      Env_RO = Env_RO / deflator_ratio_2023_24,  
                                      Plan_RO = Plan_RO / deflator_ratio_2023_24,  
                                      Fire_RO = Fire_RO / deflator_ratio_2023_24,  
                                      Edu_RO = Edu_RO / deflator_ratio_2023_24,  
                                      Other_RO = Other_RO / deflator_ratio_2023_24,  
                                      PH_RO = PH_RO / deflator_ratio_2023_24
                                  ) %>%
                                  pivot_longer(cols = c(ASC_RO, 
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
                                                        PH_RO),
                                               names_to = "variable", values_to = "value") %>%
                                  group_by(variable) %>%
                                  mutate(indexed_value = value / value[year == '2015/16'] * 100,  
                                         percentage_increase = (indexed_value[year == '2023/24'] - 100)),
                              aes(x = year, 
                                  y = indexed_value, 
                                  color = variable, 
                                  linetype = variable,  
                                  group = variable,
                                  alpha = variable)) +  # Map alpha to variable
      geom_line(linewidth = 1) +  
      geom_text(data = . %>% filter(year == '2023/24'),
                aes(label = ifelse(percentage_increase > 0, 
                                   paste0("+", round(percentage_increase, 1), "% (", variable_names_LGF[variable], ")"), 
                                   paste0(round(percentage_increase, 1), "% (", variable_names_LGF[variable], ")"))),
                hjust = -0.1, vjust = 0.5, size = 3, show.legend = FALSE) +
      scale_color_manual(values = variable_colours_LGF) +
      scale_linetype_manual(values = c("ASC_RO" = "solid", 
                                       "CSC_RO" = "solid", 
                                       "GNSS" = "dotdash", 
                                       "Trans_RO" = "solid", 
                                       "Hous_RO" = "solid", 
                                       "Cult_RO" = "solid", 
                                       "Env_RO" = "solid", 
                                       "Plan_RO" = "solid", 
                                       "Fire_RO" = "solid", 
                                       "Edu_RO" = "solid", 
                                       "Other_RO" = "solid", 
                                       "PH_RO" = "solid")) +  
      scale_alpha_manual(values = c("ASC_RO" = 1, 
                                    "CSC_RO" = 1, 
                                    "GNSS" = 1, 
                                    "Trans_RO" = 0.3, 
                                    "Hous_RO" = 0.3, 
                                    "Cult_RO" = 0.3, 
                                    "Env_RO" = 0.3, 
                                    "Plan_RO" = 0.3, 
                                    "Fire_RO" = 0.3, 
                                    "Edu_RO" = 1, 
                                    "Other_RO" = 0.3, 
                                    "PH_RO" = 0.3)) +  # Adjust alpha values
      scale_y_continuous(limits = c(60, 220), breaks = seq(60, 220, by = 40)) +
      labs(title = "Indexed real terms local authority expenditure: RO, 
  All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
           x = NULL,
           y = "2015/16 = 100",
           color = NULL,
           linetype = NULL,
           alpha = NULL) +  # Remove alpha from legend
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",  # Remove legend
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank()) +
      scale_x_discrete(expand = expansion(mult = c(0.05, 0.25)))  # Add small space to the right
  
  
  
#### Saving graphs ####
ksp_graphs <- list(ASC_bar, 
               ASC_line, 
               CSC_bar,  
               CSC_line,  
               SC_bar,  
               SC_line,  
               SC_perc_CSP,  
               SC_perc_GNSS,  
               SC_perc_TSE,  
               LG_spend_line,  
               LG_spend_line_all)
ksp_filenames <- c("ASC_bar.svg", 
               "ASC_line.svg", 
               "CSC_bar.svg",  
               "CSC_line.svg",  
               "SC_bar.svg",  
               "SC_line.svg",  
               "SC_perc_CSP.svg",  
               "SC_perc_GNSS.svg",  
               "SC_perc_TSE.svg",  
               "LG_spend_line.svg",  
               "LG_spend_line_all.svg")

output_folder <- "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Graphs/"

for (i in seq_along(ksp_graphs)) {
    ggsave(paste0(output_folder, ksp_filenames[i]), ksp_graphs[[i]], device = "svg")
}


# Export the dataframes
write_xlsx(Eng_data, "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Graphs/Eng_data.xlsx")
write_xlsx(Eng_data_LGF, "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Graphs/Eng_data_LGF.xlsx")
write_xlsx(combined_data, "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Graphs/combined_data.xlsx")
