### Time-series plots ####

rep(c("#1B998B","#A5BE00","#3C65FA","#5C2751","#E63946", 
      "#A8DADC","#457B9D","#1D3557","#ffd60a"),4)

A <- list()

A$AW <- ggplot(NoS,aes(x=Year, y=ArcticWater))+
  geom_line(color="#E63946")+ #green
  geom_point(color="#E63946", size=0.7)+
  xlim(1910, 2022)+
  labs(title="Arctic Water index", x="", y=expression(paste(10^4,km^3, sep = "")))+
  #annotate("text", x = 1910, y=0.95*max(NoS$ArcticWater, na.rm = T), label = "(a)")+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(0,0.5, 1, 1.5),
             color = "grey", size =0.5, linetype="dotted")+
  theme(plot.margin = unit(c(1,1,0.5,1), 'lines'),
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),)


A$NAO <- ggplot(NoS,aes(x=Year, y=NAO2))+
  geom_line(color="#FFCB1F")+
  geom_point(color="#FFCB1F", size=0.7)+
  xlim(1910, 2022)+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(-2,-1, 0, 1,2),
             color = "grey", size =0.5, linetype="dotted")+
  theme(plot.margin = unit(c(-1,1,0.5,1), 'lines'),
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),)+
  labs(title="NAO index", x=NULL, y="")#+




test <- NoS[-c(1:66), c(1,10)]

test$TempLanganes <- na.approx(test$TempLanganes)

A$Temp <- ggplot(test,aes(x=Year, y=TempLanganes))+
  geom_line(color="#3BCEAC")+
  geom_point(color="#3BCEAC", size=0.7)+
  xlim(1910, 2022)+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(0, 1, 2),
             color = "grey", size =0.5, linetype="dotted")+
  theme(plot.margin = unit(c(-0.5,1,0.5,1), 'lines'),
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.text.x = element_blank(),
    axis.line.x = element_blank())+
  labs(title ="Temperature (Langanes)",y="°C", x="")#+
  #annotate("text", x = 1910, y=0.95*max(NoS$TempLanganes, na.rm = T), label = "(c)")


A$PP <- ggplot(NoS,aes(x=Year, y=NPPtotalC))+
  geom_line(color="#FF7F11")+
  geom_point(color="#FF7F11", size=0.7)+
  xlim(1910, 2021)+
  ylim(0, NA)+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(50, 100,150, 200),
             color = "grey", size =0.5, linetype="dotted")+
  theme(plot.margin = unit(c(-1,1,0.5,1), 'lines'),
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.text.x = element_blank(),
    axis.line.x = element_blank())+
  labs(title="Total Primary Production", y=expression(paste("NPP (gC.",m^-2,.yr^-1,")",sep="")),x="")#+
  #annotate("text", x = 1910, y=0.95*max(NoS$NPPtotalC, na.rm = T), label = "(d)")  


A$Zoo <- ggplot(NoS,aes(x=Year, y=ZooB))+
  geom_line(color="#6EA4BF")+
  geom_point(color="#6EA4BF", size=0.7)+
  xlim(1910, 2021)+
  ylim(0,NA)+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(0,5, 10),
             color = "grey", size =0.5, linetype="dotted")+
  theme(plot.margin = unit(c(-1,1,0.5,1), 'lines'),
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.text.x = element_blank(),
    axis.line.x = element_blank())+
  labs(title="Total Zooplankton biomass",y=expression(paste("dWg.",m^-2,sep="")), x="")#+
  #annotate("text", x = 1910, y=0.95*max(NoS$ZooB, na.rm = T), label = "(e)")


A$H <- ggplot(NoS,aes(x=Year, y=HerringB))+
  geom_line(color="#B5838D")+
  geom_point(color="#B5838D", size=0.7)+
  xlim(1910, 2021)+
  theme_classic2()+
  geom_vline(xintercept =c(1925, 1950, 1975, 2000),
             color = "grey", size =0.8, linetype="dashed")+
  geom_hline(yintercept =c(5, 10, 15),
             color = "grey", size =0.5, linetype="dotted")+
  labs(title ="Total Herring biomass",y = expression(paste(10^6, "t",sep="")), x="")+
  #annotate("text", x = 1910, y=0.95*max(NoS$HerringB, na.rm = T), label = "(f)")+
  theme(plot.margin = unit(c(-1,1,0.5,0), 'lines'),
        axis.text=element_text(size=14))


Time1 <-ggarrange(plotlist = A, ncol=1, nrow=6, align = "v")

pdf(file ="Figures/Time-series6.pdf",
    width = 9,
    height = 11)
Time1


dev.off()

#####

##### Strucchange ! #####

OrTidy_OmniSRes <- Tidy_OmniSRes

Tidy_OmniSRes[6,3] <- "Arctic Water"
Tidy_OmniSRes[9,3] <- "Temperature"
Tidy_OmniSRes[1,3] <- "NAO"
Tidy_OmniSRes[28,3] <- "Primary Production"
Tidy_OmniSRes[11,3] <- "Zooplankton"
Tidy_OmniSRes[19,3] <- "Herring"


pdf(file ="Figures/Strucchange_selected_omnibus_fin.pdf",
    width = 7,
    height =6)

Tidy_OmniSRes[c(1,9,6,11,19,28),]%>%
  pivot_longer(cols = c(1,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  ggplot(aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")),`Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_bar(position ="stack",stat="identity", colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#CDD5D1"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=1)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")
dev.off()


pdf(file ="Figures/Strucchange_6Omnifin.pdf",
    width = 7,
    height =6)

Tidy_OmniSRes[c(1,9,6,11,19,28),]%>%
  pivot_longer(cols = c(1,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  ggplot(aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")), `Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_chicklet(position ="stack",
                width = 0.9,
                radius = grid::unit(5, 'mm'),
                colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#DEE3E0"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=0.6)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")

dev.off()

#####
## STARS ####

Tidy_OmniSTARSres_YN

colnames(Tidy_OmniSTARSres_YN) <- c("Variable", "No", "Yes")
Tidy_OmniSTARSres_YN[6,1] <- "Arctic Water"
Tidy_OmniSTARSres_YN[9,1] <- "Temperature"
Tidy_OmniSTARSres_YN[1,1] <- "NAO"
Tidy_OmniSTARSres_YN[28,1] <- "Primary Production"
Tidy_OmniSTARSres_YN[11,1] <- "Zooplankton"
Tidy_OmniSTARSres_YN[19,1] <- "Herring"
Tidy_OmniSTARSres_YN[19,2] <- 0

pdf(file ="Figures/STARS_6Omnifin.pdf",
    width = 7,
    height =6)

Tidy_OmniSTARSres_YN[c(1,9,6,11,19,28),]%>%
  pivot_longer(cols = c(3,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  ggplot(aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")), `Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_bar(position ="stack",stat="identity", colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#CDD5D1"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=1)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")
dev.off()


# version 2 
pdf(file ="Figures/STARS_6Omnifin.pdf",
    width = 7,
    height =6)

Tidy_OmniSTARSres_YN[c(1,9,6,11,19,28),]%>%
  pivot_longer(cols = c(3,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  ggplot(aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")), `Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_chicklet(position ="stack",
                width = 0.9,
                radius = grid::unit(5, 'mm'),
                colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#DEE3E0"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=0.6)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")
dev.off()


#####

### clustering ####


load(file="Result_data/Clustering_detail_Omni") #Tidy_clusterRes_Omni

pdf(file ="Figures/Clustering_Omni_detail.pdf",
    width = 7,
    height =5)

Tidy_clusterRes_Omni%>%
  pivot_longer(cols = -1, names_to = "Grouping", values_to = "Percentage")%>%
  ggplot(aes(x=factor(NumberOfClusters, levels = c("1","2", "3", "4", "5", "6", "7", "8", "9", "10")), Percentage, fill=NumberOfClusters)) + 
  geom_bar(position ="stack",stat="identity", colour="black") +
  scale_fill_manual(values=c("1" = "#CDD5D1",
                             "2" ="#3C65FA","3"="#3C65FA","4"="#3C65FA","5"="#3C65FA","6"="#3C65FA","7"="#3C65FA","8"="#3C65FA","9"="#3C65FA","10"="#3C65FA"))+
  facet_wrap(.~factor(Grouping, levels = c("PhyLong","BioMed","CombiMed", "PhyShort","BioShort", "CombiShort")), ncol=3)+
  scale_y_continuous(labels = label_percent(scale = 100))+
  theme_bw()+
  theme(legend.position = "none",
        #axis.title.y = element_blank(), axis.title.x = element_blank()
  )+
  labs(fill = "Changepoint detected")+
  xlab("Number of significant clusters")+
  ylab("Percentage of surrogates")


dev.off() 


TC_YN <- Tidy_clusterRes_Omni%>%
  pivot_longer(cols = -1, names_to = "Grouping", values_to = "Percentage")%>%
  pivot_wider(names_from = NumberOfClusters,values_from = "Percentage")%>%
  mutate(No=`1`,
         Yes = rowSums(.[,c(3:11)],na.rm=T))%>%
  select(c(Grouping,No,Yes))
  

pdf(file ="Figures/Clustering_6Omni_CHECKDATA.pdf",
    width = 7,
    height =6)

TC_YN%>%
  pivot_longer(cols = c(3,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  mutate(`Percentage of Surrogates`=`Percentage of Surrogates`*100)%>%
  ggplot(aes(x=factor(Grouping, levels=c("CombiShort","BioShort", "PhyMed","CombiMed","BioMed", "PhyLong")), `Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_bar(position ="stack",stat="identity", colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#CDD5D1"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=0.45)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")

dev.off()

# version 2 
pdf(file ="Figures/Clustering_fin.pdf",
    width = 7,
    height =6)

TC_YN%>%
  pivot_longer(cols = c(3,2),names_to = "Changepoint",values_to = "Percentage of Surrogates")%>%
  mutate(`Percentage of Surrogates`=`Percentage of Surrogates`*100)%>%
  ggplot(aes(x=factor(Grouping, levels=c("CombiShort","BioShort","PhyMed","CombiMed","BioMed","PhyLong")), `Percentage of Surrogates`, fill=factor(Changepoint,levels=c("No","Yes")))) + 
  geom_chicklet(position ="stack",
                width = 0.9,
                radius = grid::unit(5, 'mm'),
                colour="black") +
  scale_fill_manual(values=c("Yes" = "#3C65FA",
                             "No" = "#DEE3E0"), breaks = c('Yes', 'No'))+
  coord_flip()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=0.6)+
  scale_y_continuous(labels = label_percent(scale = 1))+
  
  theme_bw()+
  theme(legend.position = "top", axis.title.y = element_blank())+
  labs(fill = "Changepoint detected")
dev.off()


##### Envcpt ####


# install.packages("ggchicklet", repos = "https://cinc.rud.is")
#remotes::install_git("https://git.rud.is/hrbrmstr/ggchicklet.git")


TM1 %>% mutate(Tpct=pct+1)
TM$Model <- as.factor(TM$Model)
TM$Variable <- as.factor(TM$Variable)

TMp <- pivot_wider(TM,names_from = Model,values_from = pct) %>%
  mutate(CP=Changepoint+Mixed,noCP=`No changepoint`+Mixed) %>%
  mutate(xCP=100-CP,xnoCP=100-noCP)

TM2 <- TMp %>% 
  select(Variable,CP,noCP,xCP,xnoCP) %>%
  pivot_longer(cols = c(CP,noCP,xCP,xnoCP),values_to = "perct") %>%
  mutate(Variable=as.character(Variable))

TM2a <- TM2 %>% filter(name%in%c("noCP","xnoCP")) %>%
  mutate(name=recode(name,"xnoCP"="other")) %>%
  mutate(name=factor(name,levels=c("other","noCP")))
TM2b <- TM2 %>% filter(name%in%c("CP","xCP")) %>%
  mutate(name=recode(name,"xCP"="other")) %>%
  mutate(name=factor(name,levels=c("CP","other")))

colnames(TM2a)[3] <- "Percentage of Surrogates"
colnames(TM2b)[3] <- "Percentage of Surrogates"


MainPalette <- c("#3C65FA","#DEE3E0","#FFFFFF")
names(MainPalette) <- c("CP","noCP","other")

pdf(file ="Figures/Grace1.pdf",
    width = 7,
    height =6)

ggplot() +
  coord_flip()+
  geom_chicklet(data=TM2a,aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")),y=`Percentage of Surrogates`,fill=name),
                alpha=1,
                width = 0.9,
                radius = grid::unit(5, 'mm'),
                colour="black") +
  geom_chicklet(data=as.data.frame(TM2b),aes(x=factor(Variable, levels=c("Herring","Zooplankton", "Primary Production", "Temperature", "NAO", "Arctic Water")),
                                             y=`Percentage of Surrogates`, fill=name),
                alpha=0.99,
                width = 0.9,
                radius = grid::unit(5, 'mm'),
                colour="black") +
  scale_fill_manual(values = MainPalette, name="Selected Model", labels=c("Changepoint ", "Inconclusive",  "No Changepoint"))+
  scale_y_continuous(labels = label_percent(scale = 1))+
  theme_bw()+
  geom_hline(yintercept=5,  linetype="dashed", color = "#F58A07", linewidth=0.4)+
  theme(axis.title.y = element_blank(), 
        legend.position = "top")

dev.off()

# NB: Multiple version of this plot were created and 
#subsequent esthetic modifications were done wtih Inkscape 

#####


##### Clustering application plot #####

pdf(file ="Figures/Clustering_Application.pdf",
    width = 6,
    height =3)

ggplot(result, aes(x = Start4, xend = End4, y = factor(Grouping, levels = c("CombiShort", "BioShort", "PhyMed", "CombiMed", "BioMed", "PhyLong")),
                   yend = factor(Grouping, levels = c("CombiShort", "BioShort", "PhyMed", "CombiMed", "BioMed", "PhyLong")), 
                   color = factor(rep), alpha=transp)) +
  geom_segment(size = 10) +
  scale_color_manual(values = rep(c("#1B998B","#A5BE00","#3C65FA","#5C2751","#E63946", 
                                    "#A8DADC","#457B9D","#1D3557","#ffd60a"),4)) +
  scale_alpha_manual(values = c("No"=1, "FALSE"=0))+
  labs(x = "Year", y="") +
  guides(color = "none", alpha="none")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),
        aspect.ratio = 3/6)

dev.off()
#####










