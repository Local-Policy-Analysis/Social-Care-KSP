###############################################################################
##                              *TABLES AND CHARTS*                          ##
###############################################################################
# Author:     Becky Foster
# Script Description:   Outputs for the KSP
# -----------------------------------------------------------------------------

financial_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
principal_authorities <- c("SC", "UA", "MD", "LB", "SD")

## CHARTS (2015 ref) ----
    ## 1)	Bar chart of real terms ASC NCE: ASC (RO), ASC (ASC-FR) and ASC (ASC-FR + NHS) ----
    plot(ASC_bar)
    ## 2)	Indexed real terms ASC NCE: ASC (RO), ASC (ASC-FR), ASC (ASC-FR + NHS) and ASC-FR per capita ----
    plot(ASC_line)
    ## 3)	Bar chart of real terms CSC NCE: CSC (RO) and CSC (S251) ----
    plot(CSC_bar)
    ## 4)	Indexed real terms CSC NCE: CSC (RO), CSC (S251) and CSC (S251) per capita ----
    plot(CSC_line)
    ## 5)	Compound bar chart of real terms Total Social Care NCE (RO) ----
    plot(SC_bar)
    ## 6)	Indexed real terms Total Social Care NCE (RO) ----
    plot(SC_line)
    ## 7)	Indexed real terms total social care expenditure, GNSS and TSE: TSC, ASC, CSC, TSE and GNSS ----
    plot(LG_spend_line)
    ## 8)	Indexed real terms social care expenditure and CSP: CSC, TSC, ASC and CSP ----
    plot()
    ## 9)	Compound bar chart of Social care NCE as a proportion of GNSS: ASC and CSC ----
    plot(SC_perc_GNSS)
    ## 10)	Compound bar chart of Social care NCE as a proportion of TSE: ASC and CSC ----
    plot(SC_per_TSE)
    ## 11)	Compound bar chart of Social care NCE as a proportion of CSP: ASC and CSC ----
    plot(SC_perc_CSP)



## TABLES (2010 ref) ----

# Initialise a blank excel workbook
wb <- createWorkbook()

    ## 1)	ASC NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 1")
       writeData(wb, "Table 1", df1)
    ## 2)	ASC NCE (ASC-FR): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 2")
    ## 3)	ASC NCE including NHS funding and BCF: nominal, real and annual real % change ----
       addWorksheet(wb, "Table 3")
    ## 4)	CSC NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 4")
    ## 5)	CSC S251: nominal, real and annual real % change ----
       addWorksheet(wb, "Table 5")
    ## 6)	Total Social Care NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 6")
 
 # Finalise the workbook
 saveWorkbook(wb, "KSP_tables.xlsx", overwrite = TRUE)