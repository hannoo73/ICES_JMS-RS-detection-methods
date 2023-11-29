

## ENVCPT ####

#Suppplementary info tables just send in html ! 

test <- OmniEnvCptCount_NA[,c(5,1,2,3,6)] %>%
  kbl(caption="Table 7: Summary of results from EnvCpt surrogate testing on all 32 available Norwegian Sea variables",
      format= "html",
      align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  test,
  "Tables/SuppInfo_EnvCptSurrogateModelpct_table7.html")

# additional research into the origin of the conflicts, the cases where no model is selected

SI_envcpt <- MM[, c(1, 2, 4,5,6)]%>%
  kbl(caption="Table 5: Summary of results from EnvCpt surrogate testing on all 32 available Norwegian Sea variables,
      including additional information into the origin of the conflicts",
                                     format= "html",
                                     align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_envcpt,
  "Tables/SuppInfo_EnvCptSurrogateModelpct_extraInfo_Table5.html")



### application result of conflicts if any 

SI_envcpt_applRes <- Menvcpt%>%
  kbl(caption="Table 6: Summary of results from EnvCpt application on all 32 available Norwegian Sea original time series,
      an x indicates that this model was selected according to its BIC score, if more than one X occurs for each variable/row, a conflict arose",
      format= "html",
      align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_envcpt_applRes,
  "Tables/SuppInfo_EnvCpt_ApplRes_table6.html")



#####

#### STRUCHANGE ####

Tidy_OmniSRes

SI_struc <- Tidy_OmniSRes[,c(3,1,2)] %>%
  kbl(caption="Table 4: Summary of results from Strucchange surrogate testing on all 32 available Norwegian Sea variables",
      format= "html",
      align="lcc", 
      col.names=c("Time series", "% of surrogates with changepoints", "% of surrogates without changepoints")) %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_struc,
  "Tables/SuppInfo_Strucchange_table4.html")


# application on all 32 time series

STes <- Tes[,c(3,1,2,4)]
STes$Breakdates <- unlist(STes$Breakdates)
STes$Pval <- unlist(STes$Pval)
STes$Pval <- round(STes$Pval, 4)

SI_struc2 <- STes%>%
  kbl(caption="Table 3: Summary of results from Strucchange method application on all 32 available Norwegian Sea original time series",
      format= "html",
      align="lcc") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_struc2,
  "Tables/SuppInfo_Strucchange_applRes_table3.html")



##### STARS #####

SI_stars <-Tidy_OmniSTARSres %>%
  pivot_wider(names_from = "Chgpt_count", values_from = "Percent_per_count",values_fill = 0)%>%
  kbl(caption="Table 2: Summary of results from STARS surrogate testing on all 32 available Norwegian Sea variables, each columns gives the number of changepoints found ",
      format= "html",
      align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_stars,
  "Tables/SuppInfo_STARS_table2.html")



SI_stars_appl <-Etoile3 %>%
  kbl(caption="Table 1: Summary of results from STARS application on all 32 available Norwegian Sea original time series, a 0 appears in the chgpt_years columns when no changepoints were found",
      format= "html",
      align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_stars_appl ,
  "Tables/SuppInfo_STARS_applRes_table1.html")



##### Clustering #####

Tidy_clusterRes_Omni[, 1] <- as.factor(Tidy_clusterRes_Omni[, 1])
t(Tidy_clusterRes_Omni)
colnames(Tidy_clusterRes_Omni)[1] <- "NumberOfClusters"
SI_clustering <- Tidy_clusterRes_Omni%>%
  kbl(caption="Table 8: Summary of results from Chronological clustering surrogate testing on 6 different groupings, giving here the percentage of each 1000 multivariate surrogate set
      finding between 1 and 10 significantly different clusters with the chclust method",
      format= "html",
      align="l") %>%
  kable_paper(full_width = F, html_font = "avenir")

save_kable(
  SI_clustering,
  "Tables/SuppInfo_Clustering_table8.html")
#####


