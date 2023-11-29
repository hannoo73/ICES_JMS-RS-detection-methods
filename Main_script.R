#############################################
#### Main script for surrogate testing ######
#############################################

# Make sure your environment is clean
rm(list=ls())

# CHANGE THIS !! 
setwd("")

# load functions files, please make sure they are in your current directory
source("Functions.R")
source("Libraries.R") 

## load data: this dataframe is formatted where each column is a different time-series
# where column one give the years as such each row, excluding the first row is a year from 1907 to 2021
# Only some columns were selected from the original data because of the ecological aim of this project 
# Warning, there will be some problems if you try to run this code on time-series with higher frequency than once per year (especially for STARS and struchange)

#save(NoS, file= "Data/NoS")
load(file ="Data/NoS")

# PHASE 1 : Generate and store surrogates for each time-series
# G_surrogates = x, column of data frame, Years= the column of df containing the time, ns , number of surrogates wanted

Surrogates <- lapply(NoS[,-1],function(x) G_surrogates(x, Years=NoS$Year, ns=1000))


#save(Surrogates, file = "Data/ts_surrogates")

# PHASE 2: START HERE, if you don't have my original data
## add a dot here to the file location to indicate the working directory ! 
load(file = "Data/ts_surrogates")

##### ENVCPT METHOD ####

Global_OmniresEnvCpt <- Surrogates%>%
  lapply(., function(x) lapply(x, omnibusQ))%>% # transform with omnibus ! 
  lapply(., function(x) lapply(x, function(x) x[,-c(1,2)])) %>% # formatting: remove Orank 
  lapply(., function(x) Apply_EnvCpt2(x, CM="NA")) # CM = "NA means to use only 8 models and not all 12, use "all" in that case
  
#save(Global_OmniresEnvCpt, file = "Result_data/Global_OmniresEnvcpt")

load("Result_data/Global_OmniresEnvcpt", verbose = T)
Global_OmniresEnvCpt
### Element 1: COUNT NA

OmniEnvCptCount_NA <- Global_OmniresEnvCpt%>%
  lapply(.,function(x) sapply(x, "[[", 1))%>% #takes the first element of each list and creates a new list with only those elements
  lapply(.,my_table2)%>%
  tidy_NA(.)
# SUPPLEMENTARY INFO table


OmniEnvCptCount_NA$Var1 <- as.character(OmniEnvCptCount_NA$Var1) 
OmniEnvCptCount_NA$pct <- OmniEnvCptCount_NA$Freq/10 # to get percentages instead frequencies out of 1000

#save(OmniEnvCptCount_NA, file = "Result_data/OmniEnvCpt_NA")

load(file = "Result_data/OmniEnvCpt_NA", verbose = T)

# Reformat the names for graphs
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="ArcticWater",5] <- "Arctic Water"
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="TempLanganes",5] <- "Temperature"
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="NAO2",5] <- "NAO"
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="NPPtotalC",5] <- "Primary Production"
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="ZooB",5] <- "Zooplankton"
OmniEnvCptCount_NA[OmniEnvCptCount_NA$Variable=="HerringB",5] <- "Herring"
OmniEnvCptCount_NA

# element 3: which conflicts

OmniEnvCpt_topmodels <- Global_OmniresEnvCpt%>%
  lapply(.,function(x) sapply(x, "[[", 3))%>% #takes the first element of each list and creates a new list with only those elements
  lapply(.,my_table2)%>%
  tidy_NA(.)
Global_OmniresEnvCpt$NAO2$V1
OmniEnvCpt_topmodels$NAO2
## Element 4: Model conflicts

## Exacting and condensing element 4: determine the nature of the conflict between the best fitting models
OmniEnvCptCount_Conflict <- Global_OmniresEnvCpt%>% 
  lapply(.,function(x) sapply(x, "[[", 4))%>%
  lapply(.,my_table3)%>% # attention aux noms des fonctions que tu crées ! 
  tidy_NA(.)%>%
  pivot_longer(c(1:4), names_to = "Conflict", values_to = "Count" )


OmniEnvCptCount_Conflict$Count <- replace_na(OmniEnvCptCount_Conflict$Count, 0 )
OmniEnvCptCount_Conflict$pct <- OmniEnvCptCount_Conflict$Count/10


#save(OmniEnvCptCount_Conflict, file = "Result_data/OmniEnvCpt_Conflict")
load(file = "Result_data/OmniEnvCpt_Conflict", verbose = T)



#### merge both dataframes  ####

#First data frame formatting
Merge <- OmniEnvCptCount_NA # this data frame has the information on the absolute, no conflict results
Merge$Conflict2 <- "NoConflict" # set all the rows to NoConflict
colnames(Merge)
Merge[Merge$Changepoint=="None",7] <- "Conflict"# set the row where No model was selected to Conflict 

# Second data frame formatting
OmniEnvCptCount_Conflict # data frame with 4 categories of conflict, No conflict corresponds to the same no
#conflict as in df above, Mixed = inconclusive, Only changepoint= conflict arose between changepoint models and same for Only base !

#change the names here cause I did above too 
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="ArcticWater",1] <- "Arctic Water"
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="TempLanganes",1] <- "Temperature"
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="NAO2",1] <- "NAO"
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="NPPtotalC",1] <- "Primary Production"
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="ZooB",1] <- "Zooplankton"
OmniEnvCptCount_Conflict[OmniEnvCptCount_Conflict$Variable=="HerringB",1] <- "Herring"
Merge2 <- OmniEnvCptCount_Conflict

Merge2$Conflict2 <- "Conflict" # create same column as above but here state Conflict
Merge2[Merge2$Conflict=="NoConflict",5] <- "NoConflict" # set only the ones with noconflict to no conflict

# check column number and order to rbind
colnames(Merge)
colnames(Merge2)


Merge2$Var1 <- Merge2$Conflict # duplicate columns to fit to larger df, could of done it the other way around 
Merge2$Model <- Merge2$Conflict
Merge2$Changepoint <-  Merge2$Conflict

Merge <- Merge[, c(5, 2, 1, 3, 6, 7)]
Merge$Conflict <- Merge$Conflict2
Merge2 <- Merge2[, c(1,7, 6, 8, 4, 5, 2)]

#check again
head(Merge)
head(Merge2)

Merge <- Merge[!Merge$Model=="NoModelSelected",] # remove the inconclusive results from this df

Merge2 <- Merge2[!Merge2$Model=="NoConflict",] # remove all the definite results from this df

# rbind the two df so that the extra detail of what happens in the conflict, or inconclusive categories is included
MM <- rbind(Merge, Merge2)

#save(EnvCptCount_Conflict, file = "Result_data/EnvCpt_Conflict")
load("Result_data/EnvCpt_Conflict")

MM$new <- "No changepoint"
           
MM$new <- ifelse(MM$Changepoint=="Changepoint", "Changepoint",MM$new)
MM$new <- ifelse(MM$Changepoint=="Mixed", "Mixed", MM$new)
MM$new <- ifelse(MM$Changepoint=="OnlyChgpt", "Changepoint", MM$new)
MM$new <- ifelse(MM$Changepoint=="OnlyBase", "No changepoint", MM$new)
           
TM <- MM[MM$Variable %in% c('Arctic Water',"NAO", "Temperature", "Primary Production","Zooplankton","Herring"),c(1,8,5)]%>%
             group_by(Variable, new)%>%
             summarise(pct=sum(pct))

colnames(TM)[2] <- "Model"
TM1 <-TM

# for pretty tables
TMall <- MM[,c(1,8,5)]%>%
  group_by(Variable, new)%>%
  summarise(pct=sum(pct))%>%
  pivot_wider(names_from = "new", values_from = "pct")

mean(TMall$Changepoint)
mean(TMall$Changepoint[c(1,6,14,19,27,28)])        
mean(TMall$Mixed)
mean(TMall$Mixed[c(1,6,14,19,27,28)])        

       
#save(TM, file = "Result_data/TM")
#load(file = "Result_data/TM", verbose=T)



##### STRUCCHANGE #####

# create a list of small data frames that contain the start years and end each time series
YearsSF <- lapply(NoS[,-1],function(x) List_yearSF(x, Years=NoS$Year))

#save(YearsSF, file = "Strucc_SFts")
load(file = "Data/Strucc_SFts")

NotTidy_OmniStrucRes <- Surrogates%>%
  lapply(., function(x) lapply(x, omnibusQ))%>% # transform with omnibus ! 
  lapply(., function(x) lapply(x, function(x) x[,-c(1,2)])) %>% # remove Orank ! 
  mapply(Apply_Strucchange,., SFts=YearsSF)%>%
  as.data.frame()
  
Tidy_OmniSRes <- lapply(NotTidy_OmniStrucRes, SignBreaks)%>%
  do.call(rbind, .)
  
# a bit of housekeeping
Tidy_OmniSRes$Variable <- rownames(Tidy_OmniSRes)
rownames(Tidy_OmniSRes) <- NULL
colnames(Tidy_OmniSRes) <- c("Yes", "No","Variable")

save(Tidy_OmniSRes, file = "Result_data/Strucchange_Omniresults_YN")
load(file = "Result_data/Strucchange_Omniresults_YN", verbose = T)

#####

##### STARS #####

NotTidy_OmniSTARSres <- Surrogates%>%
  lapply(., function(x) lapply(x, omnibusQ))%>% # Normalise surrogates: transform with omnibus 
  lapply(., function(x) lapply(x, function(x) x[,-c(1,2)])) %>% # remove Orank from output  
  mapply(Apply_STARS,Surrogate=., SFts=YearsSF)%>% # Apply STARS to each Surrogate taking into account the start/end of series (YearsSF)
  as.data.frame()

# tidy previous output
Tidy_OmniSTARSres <- lapply(NotTidy_OmniSTARSres, Countbreaks)%>% # Count number of changepoints found per surrogate
  unlist()%>% # formatting 
  as.data.frame() #formatting

Tidy_OmniSTARSres$Variable <- rownames(Tidy_OmniSTARSres) # formatting

Tidy_OmniSTARSres <- Tidy_OmniSTARSres%>% #extract the number of changepoint from variable column
  separate(col = "Variable", into = c("Variable", "Chgpt_count"), sep ="\\.(?=[^\\.]+$)", remove = T)

rownames(Tidy_OmniSTARSres)<- NULL  
colnames(Tidy_OmniSTARSres)[1] <- "Percent_per_count"
Tidy_OmniSTARSres <- Tidy_OmniSTARSres[,c(2,3,1)] # reorder

# Tidy_OmniSTARSres = percentage of surrogates where STARS found 0 to 13 significant changepoints

#save(Tidy_OmniSTARSres, file = "Result_data/STARS_Omniresults_details")
load(file = "Result_data/STARS_Omniresults_details", verbose = T)

# Transform previous file to just get false positives and ture negatives (Yes/No changepoints)

Tidy_OmniSTARSres_YN <- Tidy_OmniSTARSres%>%
  pivot_wider(names_from = "Chgpt_count",values_from = "Percent_per_count")%>%
  mutate(Surrogate_N=`0`,
         Surrogate_Y = rowSums(.[,c(3:15)],na.rm=T))%>%
  select(c(Variable,Surrogate_N,Surrogate_Y))

#save(Tidy_OmniSTARSres_YN, file = "Result_data/STARS_Omniresults_YN")
load(file = "Result_data/STARS_Omniresults_YN", verbose = T)

mean(Tidy_OmniSTARSres_YN$Surrogate_Y)
NoS

##### CLUSTERING #####


# cluster grouping
PhyLong <- c("NAO2", "NLGyre", "SvinCoreT", "SviCoreS", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes") #1976 --> start 70
PhyMed <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes")#1995 start 89
BioMed <- c("ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB" ) #1995 start 89
BioShort <- c("ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB", "NPPtotalC", "NPPpeak") # 2003 start 97

CombiMed <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes", 
              "ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB")#1995 start 89

CombiShort <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes", 
              "ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB", "NPPtotalC", "NPPpeak") #2003 start 97


#### Create surrogates for different cluster groupings ! 
# transform surrogates

Omni_surrogates <- Surrogates%>%
  lapply(., function(x) lapply(x, omnibusQNorm))%>%
  lapply(., as.data.frame)
  
Omni_surrogates[[1]]  

# PhyLong

ClusterSurro1_Omni <- list() # create a list of 1000 dataframes whose columns correspond each to a different surrogate time series

for (i in (1:length(Surrogates$NAO2))){ # loop over the number of surrogates per series
  ClusterSurro1_Omni[[i]] <- ClusterSurrogates(Omni_surrogates, PhyLong, i) %>% # select the surrogates corresponding to each cluster grouping
    do.call(rbind, .) %>% # rbind across the different surrogate series
    t() #transpose the dataframe 
}

# PhyMed
ClusterSurro2_Omni <- list()

for (i in (1:length(Surrogates$NAO2))){
  ClusterSurro2_Omni[[i]] <-ClusterSurrogates(Omni_surrogates, PhyMed, i)%>%
    do.call(rbind, .)%>%
    t()
}

# BioMed
ClusterSurro3_Omni <- list()

for (i in (1:length(Surrogates$NAO2))){
  ClusterSurro3_Omni[[i]] <-ClusterSurrogates(Omni_surrogates, BioMed, i)%>%
    do.call(rbind, .)%>%
    t()
}

# BioShort
ClusterSurro4_Omni <- list()

for (i in (1:length(Surrogates$NAO2))){
  ClusterSurro4_Omni[[i]] <-ClusterSurrogates(Omni_surrogates, BioShort, i)%>%
    do.call(rbind, .)%>%
    t()
}

# CombiMed
ClusterSurro5_Omni <- list()

for (i in (1:length(Surrogates$NAO2))){
  ClusterSurro5_Omni[[i]] <-ClusterSurrogates(Omni_surrogates, CombiMed, i)%>%
    do.call(rbind, .)%>%
    t()
}

# CombiShort
ClusterSurro6_Omni <- list()

for (i in (1:length(Surrogates$NAO2))){
  ClusterSurro6_Omni[[i]] <-ClusterSurrogates(Omni_surrogates, CombiShort, i)%>%
    do.call(rbind, .)%>%
    t()
}


## Apply the clustering function to each grouping 



Tidy_clusterRes_Omni <- list()

Tidy_clusterRes_Omni$PhyLong <- lapply(ClusterSurro1_Omni, FindClusters) %>% # where ClusterSurro1 a list of 1000 dataframe each containing one column of each variable in the grouping to be tested
  unlist()%>%
  table()%>%
  as.data.frame()

Tidy_clusterRes_Omni$PhyMed <- lapply(ClusterSurro2_Omni, FindClusters) %>% 
  unlist()%>%
  table()%>%
  as.data.frame()


Tidy_clusterRes_Omni$BioMed <- lapply(ClusterSurro3_Omni, FindClusters) %>% 
  unlist()%>%
  table()%>%
  as.data.frame()

Tidy_clusterRes_Omni$BioShort <- lapply(ClusterSurro4_Omni, FindClusters) %>% 
  unlist()%>%
  table()%>%
  as.data.frame()

Tidy_clusterRes_Omni$CombiMed <- lapply(ClusterSurro5_Omni, FindClusters) %>% 
  unlist()%>%
  table()%>%
  as.data.frame()


Tidy_clusterRes_Omni$CombiShort <- lapply(ClusterSurro6_Omni, FindClusters) %>% 
  unlist()%>%
  table()%>%
  as.data.frame()

list_names <- names(Tidy_clusterRes_Omni)# Get the list names from the original list

Tidy_clusterRes_Omni <- Reduce(function(x, y) merge(x, y, by = ".", all.x=T, all.y=T), Tidy_clusterRes_Omni) # reduce the list into a dataframe ! 

# Assign the list names to the column names of the new dataframe
colnames(Tidy_clusterRes_Omni)[-1] <- list_names
Tidy_clusterRes_Omni[is.na(Tidy_clusterRes_Omni)] <- 0 # replace the NAs

Tidy_clusterRes_Omni[,-1] <- Tidy_clusterRes_Omni[,-1]/1000 # switch to percentages instead of counts ! 

save(Tidy_clusterRes_Omni, file="Result_data/Clustering_detail_Omni")




