##########################################################################################
#### Main script for testing for changepoints/regime shift in the original time series ###
##########################################################################################

# Make sure your environment is clean
rm(list=ls())

# CHANGE THIS !! 
setwd("~/Desktop/Tromsø/R_tromso/Github_versions")

source("Functions.R")
source("Libraries.R") 

load(file ="Data/NoS", verbose=T)
load(file = "Data/Strucc_SFts", verbose=T)


## need to normalise the time series 

X1 <- list()

for(i in 2:ncol(NoS)){
  X1[[i-1]] <- NoS[,c(1,i)]
  X1[[i-1]] <- X1[[i-1]][c((YearsSF[[i-1]]$Start-1907):(YearsSF[[i-1]]$End-1907)+1),]
  X1[[i-1]][,2] <- na.approx(X1[[i-1]][,2]) # fill in the blanks 
  X1[[i-1]][,3] <- omnibusQNorm(X1[[i-1]][,2])
  colnames(X1[[i-1]])[3] <- colnames(X1[[i-1]])[2]
  X1[[i-1]] <- X1[[i-1]][,c(1,3)]
}

NoS_Omni <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), X1) # omnibused version of ts in similar format to NoS
X1 # list of each ts

#save(NoS_Omni, file="Data/NoS_omni_wide")
#save(X1, file="Data/NoS_omni_list")

load(file="Data/NoS_omni_wide", verbose=T)
load(file="Data/NoS_omni_list", verbose=T)


#### Envcpt #####

x = NoS_Omni[, 2]

envcpt <- list()

envcpt <- lapply(X1, App_envcpt)

names(envcpt) <- names(NoS_Omni)[-1]



for (i in 1: length(envcpt)){
  envcpt[[i]]$Variable <- names(envcpt[i])
}

Menvcpt <- do.call(rbind, envcpt)
Menvcpt$X <- "x"

Menvcpt <- Menvcpt[,c(2,4,5)]%>%
  pivot_wider(names_from ="Model", values_fill = "0", values_from = "X"  )

# zoom for my 6 time series
# investigating the timing of the changepoints

Herring <- envcpt(NoS_Omni$HerringB,
              models=c("mean","meancpt","meanar1","meanar1cpt",
                       "trend","trendcpt","trendar1","trendar1cpt"))

plot(Herring, type="fit")

BIC(Herring) 

ggplot(data=NoS_Omni, aes(x=Year, y=TempLanganes))+
  geom_point()+
  geom_line()


ZooB <- envcpt(NoS_Omni$ZooB[-c(0:88)],
                  models=c("mean","meancpt","meanar1","meanar1cpt",
                           "trend","trendcpt","trendar1","trendar1cpt"))
plot(ZooB, type="fit")
BIC(ZooB)

AW <- envcpt(NoS_Omni$ArcticWater[-c(0:88)],
               models=c("mean","meancpt","meanar1","meanar1cpt",
                        "trend","trendcpt","trendar1","trendar1cpt"))
plot(AW, type="fit")
BIC(AW)

#####


##### STARS #####

load(file = "Data/Strucc_SFts", verbose=T)

Etoile <- list()
for (i in 2: ncol(NoS_Omni)){
  Etoile[[i-1]] <- my_STARS2(NoS[,i],SFts=YearsSF[[i-1]])

}

#clean up results 

for (i in 1: length(Etoile)){
  if(nrow(Etoile[[i]])==0){
    Etoile[[i]][1,] <- c(0,0,0)
  }
  Etoile[[i]]$Variable <- colnames(Etoile[[i]])[1]
  Etoile[[i]] <- Etoile[[i]][,c(4,2)]
}

Etoile2 <- do.call(rbind, Etoile)

Etoile3 <- Etoile2 %>%
  group_by(Variable) %>%
  summarize(Chgpt_years = paste(Year, collapse = ","))


##### STRUCCHANGE ####

load(file = "Data/Strucc_SFts", verbose=T)

Tes <- mapply(my_Strucchange2,NoS_Omni[,-1], SFts=YearsSF )

Tes <- as.data.frame(t(Tes))

Tes$Variable <- rownames(Tes)
rownames(Tes) <- NULL
Tes$Significant <- "No"

Tes$Significant <- ifelse(Tes$Pval<0.05,"Yes","No")

save(Tes, file = "Result_data/Struc_applRes")


#####
###### CLustering #####

# cluster grouping
PhyLong <- c("NAO2", "NLGyre", "SvinCoreT", "SviCoreS", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes") #1976 --> start 70
PhyMed <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes")#1995 start 89
BioMed <- c("ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB" ) #1995 start 89
BioShort <- c("ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB", "NPPtotalC", "NPPpeak") # 2003 start 97

CombiMed <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes", 
              "ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB")#1995 start 89

CombiShort <- c("NAO2", "SPGIndexWinter","NLGyre", "SvinCoreT", "SviCoreS","ArcticWater", "RHeatContent", "RFreshWaterContent", "TempLanganes", "SalLanganes", 
                "ZooB","HerringR.age2","BluewhitingR.age1", "MackerelR.age2","HerringB","BluewhitingB","MackerelB", "NPPtotalC", "NPPpeak") #2003 start 97



### create new dataframe inside a list 

Recom <- list()

Recom$PhyLong <- NoS%>% # 1976
  select(Year,PhyLong)%>%
  slice(70:n())%>%
  mutate_all(na.approx) # a few missing years in Temp and Sal

Recom$PhyMed <- NoS%>% #1995
  select(Year,PhyMed)%>%
  slice(89:n())%>%
  mutate_all(na.approx)

Recom$BioMed <- NoS%>% #1995
  select(Year,BioMed)%>%
  slice(89:n())%>%
  mutate_all(na.approx)

Recom$BioShort <- NoS%>% #2003
  select(Year,BioShort)%>%
  slice(97:n())%>%
  mutate_all(na.approx)

Recom$CombiMed <- NoS%>% #1995
  select(Year,CombiMed)%>%
  slice(89:n())%>%
  mutate_all(na.approx)

Recom$CombiShort <- NoS%>% #2003
  select(Year,CombiShort)%>%
  slice(97:n())%>%
  mutate_all(na.approx)
  

# transform the ts with omnibus 

# empty dataframes
NPhyL <- as.data.frame(array(NA, dim = c(46,8)))
NPhyM <- as.data.frame(array(NA, dim = c(27,10)))
NBioM <- as.data.frame(array(NA, dim = c(27,7)))
NBioS <- as.data.frame(array(NA, dim = c(19,9)))
NCM <- as.data.frame(array(NA, dim = c(27,17)))
NCS <- as.data.frame(array(NA, dim = c(19,19)))

# keep a record of the years
row.names(NPhyL) <- Recom$PhyLong$Year
row.names(NPhyM) <- Recom$PhyMed$Year
row.names(NBioM) <- Recom$BioMed$Year
row.names(NBioS) <- Recom$BioShort$Year
row.names(NCM) <- Recom$CombiMed$Year
row.names(NCS) <- Recom$CombiShort$Year


for (i in 2:ncol(NPhyL)+1){
  NPhyL[,i-1]<- omnibusQ(Recom$PhyLong[,i])$norm
}
colnames(NPhyL) <- colnames(Recom$PhyLong)[-1]


for (i in 2:ncol(NPhyM)+1){
  NPhyM[,i-1]<- omnibusQ(Recom$PhyMed[,i])$norm
}
colnames(NPhyM) <- colnames(Recom$PhyMed)[-1]


for (i in 2:ncol(NBioM)+1){
  NBioM[,i-1] <- omnibusQ(Recom$BioMed[,i])$norm
}
colnames(NBioM) <- colnames(Recom$BioMed)[-1]


for (i in 2:ncol(NBioS)+1){
  NBioS[,i-1]<- omnibusQ(Recom$BioShort[,i])$norm
}
colnames(NBioS) <- colnames(Recom$BioShort)[-1]


for (i in 2:ncol(NCM)+1){
  NCM[,i-1] <- omnibusQ(Recom$CombiMed[,i])$norm
}
colnames(NCM) <- colnames(Recom$CombiMed)[-1]


for (i in 2:ncol(NCS)+1){
  NCS[,i-1]<- omnibusQ(Recom$CombiShort[,i])$norm
}
colnames(NCS) <- colnames(Recom$CombiShort)[-1]

## get distance matrixes


dist_NPhyL <- dist(NPhyL, "euclidean",  diag = TRUE, upper = TRUE)
dist_NPhyM <- dist(NPhyM, "euclidean",  diag = TRUE, upper = TRUE)
dist_NBioM <- dist(NBioM, "euclidean",  diag = TRUE, upper = TRUE)
dist_NBioS <- dist(NBioS, "euclidean",  diag = TRUE, upper = TRUE)
dist_NCM <- dist(NCM, "euclidean",  diag = TRUE, upper = TRUE)
dist_NCS <- dist(NCS, "euclidean",  diag = TRUE, upper = TRUE)

# apply chclust
ch_NPhyL <- chclust(dist_NPhyL, method="coniss")
ch_NPhyM <- chclust(dist_NPhyM, method="coniss")
ch_NBioM <- chclust(dist_NBioM, method="coniss")
ch_NBioS <- chclust(dist_NBioS, method="coniss")
ch_NCM <- chclust(dist_NCM, method="coniss")
ch_NCS <- chclust(dist_NCS, method="coniss")


# compare to broken stick and get significant number of clusters 
g1 <-bstick(ch_NPhyL, plot=F)
g2 <-bstick(ch_NPhyM, plot=F)
g3 <-bstick(ch_NBioM, plot=F) 
g4 <-bstick(ch_NBioS, plot=F)
g5 <-bstick(ch_NCM, plot=F)
g6 <-bstick(ch_NCS, plot=F)


# loop to determine min value for bstick
for (i in 1:nrow(g1)){
  if (g1$dispersion[i]>g1$bstick[i]){
    km1 <- g1$nGroups[i]
    break
  }
  else{
    km1 <- 1
  }
}

PhyL <- as.data.frame(cutree(ch_NPhyL, k=km1))# cuts at 2002/03
PhyL$Year <- rownames(PhyL)

for (i in 1:nrow(g2)){
  if (g2$dispersion[i]>g2$bstick[i]){
    km2 <- g2$nGroups[i]
    break
  }
  else{
    km2 <- 1
  }
}
PhyM <- as.data.frame(cutree(ch_NPhyM, k=km2))# cuts in 2013/14
PhyM$Year <- rownames(PhyM)

for (i in 1:nrow(g3)){
  if (g3$dispersion[i]>g3$bstick[i]){
    km3 <- g3$nGroups[i]
    break
  }
  else{
    km3 <- 1
  }
}
BioM <-as.data.frame(cutree(ch_NBioM, k=km3))# No cuts !
BioM$Year <- rownames(BioM)

for (i in 1:nrow(g4)){
  if (g4$dispersion[i]>g4$bstick[i]){
    km4 <- g4$nGroups[i]
    break
  }
  else{
    km4 <- 1
  }
}
BioS <- as.data.frame(cutree(ch_NBioS, k=km4))# 10 cuts LOL !!, too short I think, 
BioS$Year <- rownames(BioS)

for (i in 1:nrow(g5)){
  if (g5$dispersion[i]>g5$bstick[i]){
    km5 <- g5$nGroups[i]
    break
  }
  else{
    km5 <- 1
  }
}
CM <-as.data.frame(cutree(ch_NCM, k=km5))# cuts 2002/03 and 2013/14 
CM$Year <- rownames(CM)

for (i in 1:nrow(g6)){
  if (g6$dispersion[i]>g6$bstick[i]){
    km6 <- g6$nGroups[i]
    break
  }
  else{
    km6 <- 1
  }
}
CS<-as.data.frame(cutree(ch_NCS, k=km6))# 10 cuts, ridiculous reflets the choas of bioMed, too short
CS$Year <- rownames(CS)

pan6 <- merge(PhyL, PhyM, by="Year", all.x = T)

pan6 <- merge(pan6,BioM,by="Year", all.x = T)

pan6 <- merge(pan6,BioS,by="Year", all.x = T)
pan6 <- merge(pan6,CM,by="Year", all.x = T)
pan6 <- merge(pan6,CS,by="Year", all.x = T)
colnames(pan6)[-1] <- c("PhyLong", "PhyMed", "BioMed", "BioShort", "CombiMed", "CombiShort")


result <- pan6 %>%
  pivot_longer(cols = -c(Year), names_to = "Grouping", values_to = "Cluster")%>%
  arrange(Grouping, Year) %>%
  group_by(Grouping, Cluster) %>%
  summarise(Start = min(Year), End = max(Year))

result$Start <- as.numeric(result$Start)
result$End <- as.numeric(result$End)
result$End2 <- result$End+1
result$Start4 <- result$Start-0.4
result$End4 <- result$End+0.4
result$rep <- seq(1:nrow(result))
result$rep <- ifelse(is.na(result$Cluster), result$rep==NA, result$rep)
result$transp <- "No"
result$transp <- ifelse(is.na(result$Cluster), result$transp=="Yes", result$transp)

# crezte a alpha column binaary alpha ! transparent and not 

ggplot(result, aes(x = Start4, xend = End4, y = factor(Grouping, levels = c("CombiShort", "BioShort", "PhyMed", "CombiMed", "BioMed", "PhyLong")),
                   yend = factor(Grouping, levels = c("CombiShort", "BioShort", "PhyMed", "CombiMed", "BioMed", "PhyLong")), 
                   color = factor(rep), alpha=transp)) +
  geom_segment(size = 5) +
  scale_color_manual(values = rep(c("#1B998B","#A5BE00","#3C65FA","#5C2751","#E63946", 
                                "#A8DADC","#457B9D","#1D3557","#ffd60a"),4)) +
  scale_alpha_manual(values = c("No"=1, "FALSE"=0))+
  labs(x = "Year", y="") +
  guides(color = "none", alpha="none")+
  theme_minimal()
  


 
#####


