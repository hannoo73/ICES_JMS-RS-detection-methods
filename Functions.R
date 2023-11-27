#' AIM: Write pure functions that will be able to be use in conjunction with map() type functions
#' and applied without using a loop on all the column of a dataframe

#'* Definition of a pure function (According to RAP book)
#' A function is pure if the function does not interact in any way with the global 
#' environment. It does not write anything to the global environment, 
#' nor requires anything from the global environment.

### Omnibus transformation ####
omnibusQ <- function(tsdata){
  local({ # make the function pure*
    tsdata <- as.data.frame(tsdata) # needs to be a dataframe!
    tsdata$Orank <- rank(tsdata[[1]],ties.method = "first") # ranks of original data, preserve order
    Norm <- as.data.frame(qnorm(seq(1/(nrow(tsdata)+1),1-(1/(nrow(tsdata)+1)), 1/(nrow(tsdata)+1)))) # create sequence of qnorm data
    colnames(Norm)<-"norm" # rename column
    Norm$Orank <- rank(Norm$norm,ties.method = "first") # get ranks of normal random generated data
    tsdata <- merge(tsdata,Norm, by="Orank", sort=F)
    return(tsdata)
  })
}


omnibusQNorm <- function(tsdata){
  local({ # make the function pure*
    tsdata <- as.data.frame(tsdata) # needs to be a dataframe!
    tsdata$Orank <- rank(tsdata[[1]],ties.method = "first") # ranks of original data, preserve order
    Norm <- as.data.frame(qnorm(seq(1/(nrow(tsdata)+1),1-(1/(nrow(tsdata)+1)), 1/(nrow(tsdata)+1)))) # create sequence of qnorm data
    colnames(Norm)<-"norm" # rename column
    Norm$Orank <- rank(Norm$norm,ties.method = "first") # get ranks of normal random generated data
    tsdata <- merge(tsdata,Norm, by="Orank", sort=F)
    return(tsdata$norm)
  })
}


### Revervse Omnibus transformation ####


deomnibus <- function(surrogate,Orank){
  local({
    NewRanks <- as.data.frame(rank(surrogate,ties.method = "first"))
    colnames(NewRanks)<-"Orank"
    # get only the two column needed from reference time series
    Deomnibused <- merge(NewRanks,Orank, by="Orank",sort=F)
    return(Deomnibused[[2]])
  })
}

#####

#### Generate surrogates for each time-series ####
# and store them in a list of dataframes
# this function needs to be used in conjuction with a lapply type of function to avoid loops


# ns = number of surrogates to generate
# Years = column in x that has the years or time data in it
G_surrogates <- function(x,Years, ns){
  local({
    # STEP 1: extract the proper length of the time-series, 
    #removing missing row from before and after the time-series
    Subset <- x[c(min(which(is.na(x)==F)):max(which(is.na(x)==F)))]
    # keep a record of the actual years of the time-series, 
    # WARNING: code needs years to be in first column of df
    Year <- as.data.frame(Years[c(min(which(is.na(x)==F)):max(which(is.na(x)==F)))])
    
    # STEP 2: complete any interior missing values too
    Subset <- na.approx(Subset)
    
    # STEP 3: Normalise the data with OmnibusQ
    Omnibused <- omnibusQ(Subset)$norm
    Orank <- omnibusQ(Subset) # keep a record of the original order of rankings
    
    # STEP 4: Calculate and remove trend from Subset
    Trend <-lm(Omnibused~Year[[1]]) 
    
    #extract the intercept and the slope
    Intercept <- summary(Trend)$coefficients[1, 1]
    ax <- summary(Trend)$coefficients[2, 1]
    
    # create detrended data
    Res_omnibused <- Trend$residuals
    
    Otrend <- Year*ax + Intercept 
  
    # STEP 5 : Generate surrogates from the detrended omnibused data
    Surrogates <- surrogate(Res_omnibused, ns=ns, fft=T, amplitude = T)
    
    # STEP 6 : Add back the trend to the surrogates and backtransform the omnibus
    # calculate a matrix with the trend, all rows are the same
    trend_matrix <- as.matrix(Otrend)%*%t(rep(1,ns)) 
    
    # add the trend back onto the surrogate time series
    Trend_surrogates <-as.data.frame(Surrogates + trend_matrix)
    
    # backtransform the omnibus 
    #Nsurro <- as.data.frame(array(NA, dim = dim(Trend_surrogates)))
    
    Nsurro <- as.data.frame(lapply(Trend_surrogates,function(x) deomnibus(surrogate=x, Orank=Orank)))
    
    return(Nsurro) # return data frame with trended, backtransformed surrogates
  })
}
#####

Omnibus_ts <- function(x,Years){
  local({
    # STEP 1: extract the proper length of the time-series, 
    #removing missing row from before and after the time-series
    Subset <- x[c(min(which(is.na(x)==F)):max(which(is.na(x)==F)))]
    # keep a record of the actual years of the time-series, 
    # WARNING: code needs years to be in first column of df
    Year <- as.data.frame(Years[c(min(which(is.na(x)==F)):max(which(is.na(x)==F)))])
    colnames(Year) <-"Year"
    # STEP 2: complete any interior missing values too
    Subset <- na.approx(Subset)
    
    # STEP 3: Normalise the data with OmnibusQ
    Norm <- omnibusQ(Subset)$norm
    Orank <- omnibusQ(Subset) # keep a record of the original order of rankings
    return <- as.data.frame(cbind(Year,Norm))
    return(return)
  })
  }

    

##### ENVCPT on surrogates####
# Extract relevant info! 


Apply_EnvCpt <- function(x){ # I give you a list ==dataframe
  local({
    lapply(x, my_EnvCpt) # you take the dataframe and apply my_envcpt on each column
  })
}

my_EnvCpt <- function(x){ #
  local({
    Env_surro <- envcpt(x,
                        models=c("mean", "meancpt","meanar1","meanar1cpt",
                                 "trend","trendcpt","trendar1","trendar1cpt"))
    # store the best model
    res <- which.min(BIC(Env_surro)) 
    return(res)
  })
}


##takes the output of Apply EnvCpt and turns into readable and exploitable results tables and graphs

tidy_EnvCpt <- function(x){
  local({
    # tidy the list of all the results into a list of tables for each variable
    x <- as.data.frame(table(unlist(x)))
    
    #Create a small dataframe with the corresponding meaning of each EnvCpt model type number
    ModelTypes = as.data.frame(c("Mean", "Mean_Cpt","Mean_AR1","Mean_AR2",
                                 "Mean_AR1_Cpt","Mean_AR2_Cpt",
                                  "Trend","Trend_Cpt","Trend_AR1","Trend_AR2",
                                   "Trend_AR1_Cpt","Trend_AR2_Cpt"))
    colnames(ModelTypes) <- "Model"
    ModelTypes$Var1 <- seq(1,12,1)
    ModelTypes$Changepoint <- c("No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint",
                                "No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint")

    x <- merge(ModelTypes,x, by="Var1")
    
    x$pct <- x$Freq/sum(x$Freq)*100 # written like this so that you can change the number of surrogates if needed
    Eres_pct <- as.data.frame(array(NA, dim = c(1,2)))
    colnames(Eres_pct)<- c("Surrogate_Y", "Surrogate_N")
    
    Eres_pct$Surrogate_Y[1] <- sum(x$pct[x$Changepoint=="Yes"])
    Eres_pct$Surrogate_N[1] <- sum(x$pct[x$Changepoint=="No"])
    
    x <- list(x, Eres_pct)
    return(x)
  })
}


#####



##### STRUCCHANGE on surrogates ####

# create a list of dataframes with the min and max years for each NoS variable
List_yearSF <- function(x,Years){
  local({
    Year <- as.data.frame(array(NA, dim = c(1,2)))
    colnames(Year) <-c("Start", "End")
    Year$Start <- Years[min(which(is.na(x)==F))]
    Year$End <- Years[max(which(is.na(x)==F))]                
    return(Year)
  })
}

Apply_Strucchange <- function(Surrogates, SFts){ # I give you a list ==dataframe
  local({
    lapply(Surrogates, function(x) my_Strucchange(x, SFts)) # you take the dataframe and apply my_strucchange on each column
  })
}



my_Strucchange <- function(Surrogate, SFts){
  local({
    # transform the surrogate into the right format ! 
    x <- as.data.frame(Surrogate)
    x <- ts(x, 
            start= SFts$Start, 
            end = SFts$End, 
            frequency = 1)
    
    # apply the Fstats function
    x <- Fstats(x ~ 1)
    #extract the breakdates and corresponding p-values and save them
    #Breakdates <- breakdates(breakpoints(x))
    Pval <- sctest(x)$p.value
    return(Pval)
  })
}



### slightly different version in order to apply directly on the orginal data: Application section 
my_Strucchange2 <- function(Surrogate, SFts){
  local({
    # transform the surrogate into the right format ! 
    x <- as.data.frame(Surrogate)
    x <- x[-c(1:(SFts$Start-1907)),]
    x <- na.approx(x)
    x <- ts(x, 
            start= SFts$Start, 
            end = SFts$End, 
            frequency = 1)
    
    # apply the Fstats function
    x <- Fstats(x ~ 1)
    #extract the breakdates and corresponding p-values and save them
    Res <- list()
    Res$Breakdates <- breakdates(breakpoints(x))
    Res$Pval <- sctest(x)$p.value
    return(Res)
  })
}


# Determine among the different identified breakdates which are significant based on their associated p-value, calculate a % of all surrogates with a significant changepoint 
# and those without
SignBreaks <- function(df) {
  local({
    count <- as.data.frame(array(NA, dim = c(1,2)))
    colnames(count) <-c("Surrogate_Y", "Surrogate_N")
    count$Surrogate_Y <-sum(df < 0.05)/length(df)*100
    count$Surrogate_N <-100-count$Surrogate_Y 
    return(count)
  })
}

#####


##### STARS on surrogates #####

# create a list of dataframes with the years for each NoS variable
List_years <- function(x,Years){
  local({
    Years <- as.data.frame(Years[c(min(which(is.na(x)==F)):max(which(is.na(x)==F)))])
    colnames(Years) <- "Year"
    return(Years)
  })
}



Apply_STARS <- function(Surrogates, SFts){ # I give you a list == dataframe
  local({
    lapply(Surrogates, function(x) my_STARS(x, SFts)) # you take the dataframe and apply my_STARS on each column
  })
}

my_STARS <- function(Surrogate, SFts){
  local({
    # transform the surrogate into the right format ! 
    x <- data.frame(Surrogate) # has to be a data frame
    x$Year <- seq(from=SFts$Start,to=SFts$End, by=1) # add a year column ! 
    x <- Rodionov(x, 1,"Year", 10, merge = FALSE)
    return(x)
  })
}


# slightly different version for the Stars application phase
my_STARS2 <- function(x, SFts){
  local({
    # transform the surrogate into the right format !
    if ((SFts$Start-1907)==0){ # for NAO2 and other series that don't need cutting down
      x==x
    }else{
      x <- x[-c(1:(SFts$Start-1907)),]
    }
    # because there are missing values at the end of the series ! Not very universel but the best I can do in a hurry, not very elegant
    if (is.na(x[nrow(x),])==T){ 
      x <- x[-nrow(x),]
    }else{
      x==x
    }
    x$Year <- seq(from=SFts$Start,to=SFts$End, by=1) # add a year column ! 
    x[1] <- na.approx(x[1])
    x <- Rodionov(x, 1,"Year", 10, merge = FALSE)
    return(x)
  })
}


Countbreaks <- function(df){
  local({
    count <- lapply(df, nrow)
    count <- table(unlist(count))/length(df)*100
  })
}

#####


##### CLUSTERING #####


# fucntion to create clustered surrogates ! 


ClusterSurrogates <- function(AllS, ListOfVar, i){
  local({
    list <- lapply(ListOfVar, function(x)  AllS[[x]][i])
    lengthC <- lapply(list, nrow)%>%
      unlist()%>%
      min()
    lengthC<- lengthC-1
    l <- map(list, function(x) x[c((nrow(x)-lengthC):nrow(x)), ])
    return(l) 
  })
}


#####


##### ggplot functions ####

# envcpt detail different version of the functions

# this one is the keeper for now ! 
plot_those_hist <- function(x){
    x <- x%>%
    as.data.frame()%>%
    ggplot(aes(x=factor(Var1, levels=c("1","2","3","4","5","6","7","8","9","10","11","12")), pct, fill=factor(Changepoint,levels=c("Changepoint","No changepoint")))) + 
    geom_bar(stat="identity", colour="black") +
    scale_fill_manual(values=c("Changepoint" = "#3C65FA",
                               "No changepoint" = "#CDD5D1"))+
    facet_wrap(.~factor(Changepoint, levels=c("Changepoint", "No changepoint")), ncol=2, scales = "free_x")+
    theme_bw()+
    theme(legend.position = "none",
          #axis.title.y = element_blank(), axis.title.x = element_blank(),
          plot.title = element_text(size=9), 
          axis.title.y = element_blank(),axis.title.x = element_blank())+
    scale_y_continuous(labels =label_percent(scale = 1), limits = c(0,60))+
    labs(fill = "Changepoint detected")#+
   # ylab("% of surrogates")+
   # xlab("EnvCpt models")
  
  return(x)
}
##### REDO envcpt function: Functions2 ####


my_EnvCpt2 <- function(x, CM){ #CM: chosen models either all or NotAR2
  local({
    #apply envcpt function with all models
    if(CM=="all"){
      Env_surro <- envcpt(x,
                          models=c("mean","meancpt","meanar1","meanar2","meanar1cpt","meanar2cpt",
                                   "trend","trendcpt","trendar1","trendar2","trendar1cpt","trendar2cpt"))
    }else{ # only Vollset models, no order 2 models 
      Env_surro <- envcpt(x,
                          models=c("mean","meancpt","meanar1","meanar1cpt",
                                   "trend","trendcpt","trendar1","trendar1cpt"))
      
    }
    
    x <- as.data.frame(unlist(BIC(Env_surro))) # extract BIC score, unlist, transform into
    colnames(x)<-c("BICs")
    x$Var1 <- seq(1,12,1)
    ModelTypes = as.data.frame(c("Mean", "Mean_Cpt","Mean_AR1","Mean_AR2",
                                 "Mean_AR1_Cpt","Mean_AR2_Cpt",
                                 "Trend","Trend_Cpt","Trend_AR1","Trend_AR2",
                                 "Trend_AR1_Cpt","Trend_AR2_Cpt"))
    colnames(ModelTypes) <- "Model"
    ModelTypes$Var1 <- seq(1,12,1)
    ModelTypes$Changepoint <- c("No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint",
                                "No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint")
    x <- merge(ModelTypes,x, by="Var1")
    y <- x[order(x$BICs), ] # all the BIC score and model information in asecnding BIC score order
    rownames(y)<-seq(1,12,1) # change back rownames for clarity with first column
    
    # 1. Res: create variable with either top model or NA if no top model found
    # 2. y: reccord of all BIC scores, just in case !
    # 3: models: if res = NA, 
    if (y[2,4]-y[1,4] <= 3){ # if the top two models have BIC score different by less than 3 points
      res = NA # no top model chosen
      if (y[3,4]-y[2,4] <= 3 & y[3,4]-y[1,4]<= 3){# if the 2nd model and 3rd are indistinguisable AND 3rd and 1st 
        models = y[c(1:3),c(1:3)]# Top three indistinguisable models
        
      } else{ # 1st and third are distinguisable and I have decided to select only the top 2 
        models = y[c(1:2),c(1:3)]# Top 2 models: only the first two models are indistinguishable
      }
    } else{ # the difference between the top 2 model is more than 3 
      res=y[1,1]
      models =y[1,c(1:3)]
    }
    ## 4: Conflict: create another variable to contense the top models, to determine if the conflict arises within the models with chgpt, with chgpt or between the two groups 
    if(nrow(models)>1){ # if more than one model selected
      if (length(unique(models$Changepoint))==1){ # if there is only one type of model in changepoint column
        if(unique(models$Changepoint)=="Changepoint"){ 
          Conflict = "OnlyChgpt"
        } else{
          Conflict = "OnlyBase"
        }
      }else{
        Conflict = "Mixed"}
    }else{
      Conflict = "None"
    }
    
    Res <- list(res,y, models, Conflict) 
    return(Res)
  })
}



### slight modification of above function to suit the application on the original time series
App_envcpt <- function(x){ 
  local({
    
    Env_surro <- envcpt(as.numeric(unlist(x[,2])),
                        models=c("mean","meancpt","meanar1","meanar1cpt",
                                 "trend","trendcpt","trendar1","trendar1cpt"))
    
    x <- as.data.frame(unlist(BIC(Env_surro))) # extract BIC score, unlist, transform into
    colnames(x)<-c("BICs")
    x$Var1 <- seq(1,12,1)
    ModelTypes = as.data.frame(c("Mean", "Mean_Cpt","Mean_AR1","Mean_AR2",
                                 "Mean_AR1_Cpt","Mean_AR2_Cpt",
                                 "Trend","Trend_Cpt","Trend_AR1","Trend_AR2",
                                 "Trend_AR1_Cpt","Trend_AR2_Cpt"))
    colnames(ModelTypes) <- "Model"
    ModelTypes$Var1 <- seq(1,12,1)
    ModelTypes$Changepoint <- c("No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint",
                                "No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint")
    x <- merge(ModelTypes,x, by="Var1")
    y <- x[order(x$BICs), ] # all the BIC score and model information in asecnding BIC score order
    rownames(y)<-seq(1,12,1) # change back rownames for clarity with first column
    
    # 1. Res: create variable with either top model or NA if no top model found
    # 2. y: reccord of all BIC scores, just in case !
    # 3: models: if res = NA, 
    if (y[2,4]-y[1,4] <= 3){ # if the top two models have BIC score different by less than 3 points
      res = NA # no top model chosen
      if (y[3,4]-y[2,4] <= 3 & y[3,4]-y[1,4]<= 3){# if the 2nd model and 3rd are indistinguisable AND 3rd and 1st 
        models = y[c(1:3),c(1:3)]# Top three indistinguisable models
        
      } else{ # 1st and third are distinguisable and I have decided to select only the top 2 
        models = y[c(1:2),c(1:3)]# Top 2 models: only the first two models are indistinguishable
      }
    } else{ # the difference between the top 2 model is more than 3 
      res=y[1,1]
      models =y[1,c(1:3)]
    }
    ## 4: Conflict: create another variable to contense the top models, to determine if the conflict arises within the models with chgpt, with chgpt or between the two groups 
    if(nrow(models)>1){ # if more than one model selected
      if (length(unique(models$Changepoint))==1){ # if there is only one type of model in changepoint column
        if(unique(models$Changepoint)=="Changepoint"){ 
          Conflict = "OnlyChgpt"
        } else{
          Conflict = "OnlyBase"
        }
      }else{
        Conflict = "Mixed"}
    }else{
      Conflict = "None"
    }
    
    Res <- list(models, Conflict) #res,y,
    return(Res[[1]])
  })
}

# add an another variable disbuted models with or without changepoint ! something like 

# convert list into data frame so that the BIC scores are actually associated



Apply_EnvCpt2 <- function(x, CM){ # I give you a list ==dataframe
  local({
    lapply(x, function(x) my_EnvCpt2(x,CM)) # you take the dataframe and apply my_envcpt on each column
  })
}

my_table <- function(x){
  local({
    table_result <- as.data.frame(table(unlist(x),useNA = "always"))
    table_result$Var1 <- as.numeric(table_result$Var1)
    ModelTypes = as.data.frame(c("Mean", "Mean_Cpt","Mean_AR1","Mean_AR2",
                                 "Mean_AR1_Cpt","Mean_AR2_Cpt",
                                 "Trend","Trend_Cpt","Trend_AR1","Trend_AR2",
                                 "Trend_AR1_Cpt","Trend_AR2_Cpt", "NoModelSelected"))
    colnames(ModelTypes) <- "Model"
    ModelTypes$Var1 <- seq(1,13,1)
    ModelTypes$Var1[13] <- NA
    ModelTypes$Changepoint <- c("No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint",
                                "No changepoint","Changepoint","No changepoint","No changepoint",
                                "Changepoint","Changepoint","None")
    x <- merge(ModelTypes,table_result, by="Var1", all.x = T)
    x$Freq <- replace_na(x$Freq,0)
    return(x)
  })
}

my_table2 <- function(x){
  local({
    table_result <- as.data.frame(table(unlist(x),useNA = "always"))
    table_result$Var1 <- as.numeric(table_result$Var1)
    ModelTypes = as.data.frame(c("Mean", "Mean_Cpt","Mean_AR1",
                                 "Mean_AR1_Cpt",
                                 "Trend","Trend_Cpt","Trend_AR1",
                                 "Trend_AR1_Cpt", "NoModelSelected"))
    colnames(ModelTypes) <- "Model"
    ModelTypes$Var1 <- seq(1,9,1)
    ModelTypes$Var1[9] <- NA
    ModelTypes$Changepoint <- c("No changepoint","Changepoint","No changepoint",
                                "Changepoint",
                                "No changepoint","Changepoint","No changepoint",
                                "Changepoint","None")
    x <- merge(ModelTypes,table_result, by="Var1", all.x = T)
    x$Freq <- replace_na(x$Freq,0)
    return(x)
  })
}





# function to extract and count NA versus model selection from first element returned by apply_envcpt2
tidy_NA <- function(x){
  local({
    merged_df <- data.frame()
    for (i in 1:length(x)) {
      # Extract the dataframe and name from the list
      df <- x[[i]]
      name <- names(x)[i]
      
      # Add a new column with the name
      df$Variable <- name
      # Append the dataframe to the merged dataframe
      merged_df <- rbind(merged_df, df)
    }
    return(merged_df)
  })
}  


my_table3 <- function(x){ # no is.na 
  local({
    table_result <- table((unlist(x)))
    df <- data.frame(Mixed = ifelse(is.numeric(table_result["Mixed"])==T,table_result["Mixed"], 0),
                     OnlyChgpt = ifelse(is.numeric(table_result["OnlyChgpt"])==T,table_result["OnlyChgpt"], 0),
                     OnlyBase = ifelse(is.numeric(table_result["OnlyBase"])==T, table_result["OnlyBase"], 0),
                     NoConflict = ifelse(is.numeric(table_result["None"])==T, table_result["None"], 0)) 
    rownames(df) <- NULL
    return(df)
  })
}

#####



