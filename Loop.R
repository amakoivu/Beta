setwd("D:/Satan")
getwd()

library(readxl)
library(reshape2)
library(dplyr)
library(betapart)
######### Script for Species with min samp area of 2 transect#########
Resampling <- read_xlsx("NEW_beta.xlsx", sheet = "Resampling")

minimal_sampling_area <- tapply (Resampling$Area, list(Resampling$Code), sum)
msa <- min (minimal_sampling_area)

Resampling$Local_census <- paste (Resampling$Dive_Site, Resampling$Depth_class, Resampling$Local_census, sep = "_")
Resampling$tra <- 1
sub1 <- dcast (Dive_Site + Depth_class + Code + Area + Local_census ~ ., fun.aggregate = sum, value.var =  "Area", data = Resampling)
sub1 <- droplevels (sub1[,-ncol(sub1)])
sub1$Depth_class <- as.factor(sub1$Depth_class) 

#################################################
####gora function som du anvander senare
choosing_transec <- function(Resampling, minimal_sampling_area=msa){
  resu <- unlist (lapply (split (Resampling, Resampling$Code), function(x){
    x   <- droplevels(x)
    y   <- split (x,x$Depth_class)
    tab <- rep(1:length(y),1000)
    transec <- numeric();  Area=0; i=1
    while (Area<msa){ ## Define the limite for each Mesophotic zone
      pool <- y[[ tab[i] ]][,"Local_census"]; pool
      pool <- pool[!pool%in%transec];pool
      if (length(pool)==0)  {i <- i+1} else{   
        transec[i] <- sample(pool,1,replace=F)
        rm(pool)
        i <- i+1
        Area <- sum (x$Area [x$Local_census%in%transec]);
      }
    }
    transec
  }))
  resu=resu[!is.na(resu)]
  names(resu)=NULL
  return(resu)
}
######Chose random transects from min samp area########
chosen_transec  <- choosing_transec(sub1, msa)
sub2 <- droplevels (sub1 [sub1$Local_census %in% chosen_transec, ])
database_red <- droplevels (Resampling [Resampling$Local_census %in% sub2$Local_census, ])
dim(database_red)
length(levels(database_red$Name))


######Merge the same code together############

(out <- aggregate(.~ Code, database_red, toString)) 


out[-1] <- case_when(out[-1] == "0, 0" ~ 0L,
                     out[-1] == "0, 1" ~ 1L,
                     out[-1] == "1, 0" ~ 1L,
                     out[-1] == "1, 1" ~ 1L,
                     TRUE ~ 2L)

########Beta part- Pairwise#########
BETA_S<-betapart.core(out[,6:63])
S_pair<-beta.pair(BETA_S)

View(BETA_S)
S_pair<-beta.pair(S_pair)
View(S_pair)

####Making a Matrix######

s_sor <- melt(as.matrix(S_pair$beta.sor), varnames = c("site1", "site2"))
s_sim <- melt(as.matrix(S_pair$beta.sim), varnames = c("site1", "site2"))
s_nes <- melt(as.matrix(S_pair$beta.sne), varnames = c("site1", "site2"))

names(s_sor)[3]<-"sor_fam"
names(s_sim)[3]<-"sim_fam"
names(s_nes)[3]<-"nes_fam"

beta_sp<- merge.data.frame(x=s_sor, y=s_sim, all = TRUE)
beta_species<- merge.data.frame(x=beta_sp, y=s_nes, all = TRUE)

write.table(beta_family,'clipboard', sep = '\t')
write.csv(beta_family,"beta_family.csv")
