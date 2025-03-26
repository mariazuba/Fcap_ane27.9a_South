
# SS3


library(icesTAF)

# document the SS3 model
dir <- "boot/initial/software"       # Define the directory for the software
mkdir(dir)
# Optionally, download the SS3 executable for a specific version
r4ss::get_ss3_exe("boot/data/Lastyear", version = "v3.30.23")


# SS3


wd<-"boot/data/Lastyear"
cp("boot/software/ss3",wd)
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)


output <- r4ss::SS_output(dir = wd,forecast=FALSE)


output$fatage

fatage_subset <- subset(output$fatage,  Era == "TIME")
fatage_subset <- fatage_subset[rowSums(fatage_subset[, c("0", "1", "2", "3")]) != 0, ]
fatage_subset <- fatage_subset[order(fatage_subset$Yr), ]


subset(fatage_subset,Yr=="2024")
mean(subset(fatage_subset, Yr == "2024")[,11])
mean(subset(fatage_subset, Yr == "2023")[,11])
mean(subset(fatage_subset, Yr == "2021")[,11])



# Load packages

library(FLBRP)
library(FLasher)
library(FLSRTMB)
library(ss3om)
library(ss3diags)
library(ggpubr)
library(ggplot2)
library(FLCore)
library(ggplotFL)
library(mse)
library(FLRef)
library(ggpubr)
library(mseviz)
library(r4ss)

# Build FLStock

# SS3 outputs are loaded with the `readFLSss3()` into an `FLStock` object. 
# The folder that contains the model outputs has to be specified.
# 
# In the following, the area outside is evaluated first.

dir01 <- "boot/data/Lastyear"
run = "ane.27.9aS"
stk = ss3om::readFLSss3(dir01)
#stk = simplify(stk)
# Fill NAs
stk@m.spwn[] = 0
stk@harvest.spwn[] = 0 
sr = ss3om::readFLSRss3(dir01,run) #A function to read the stock-recruit relationships from an SS3 run into an FLSR object
stk@name = run
stk@desc = "2024, ICES, SS3"



metrics(stk)
## Plot SS3 Stock Dynamics

#Seasonal  stock trajectories
plot(stk)+
  theme_bw()+
  ylab("F")+
  xlab("Year")+
  facet_wrap(~qname,scales="free_y")


plot(stk,
     metrics=list(SSB=function(x)unitSums(ssb(x)[,,,2]),
                  F=function(x)unitMeans(fbar(x)),
                  Catch=function(x)unitSums(catch(x)),
                  Rec=function(x)unitSums(rec(x)[,,,3])))+
  theme_bw()+
  ylab("F")+
  xlab("Year")+
  facet_wrap(~qname,scales="free_y")


metrics=list(SSB=function(x)unitSums(ssb(x)[,,,2]),
             F=function(x)unitMeans(fbar(x)),
             Catch=function(x)unitSums(catch(x)),
             Rec=function(x)unitSums(rec(x)[,,,3]))

#Stock assessment trajectories at age
plotdyn(stk)+
  theme_bw()+
  scale_color_viridis_d()

#Stock biology trajectories at age
plotbioyr(stk)+
  ggtitle(paste0(stk@name))

#Annual stock quanties at age
plotbioage(stk)+
  theme(legend.position = "none")





out = ss3om::readOutputss3(dir01)


rdata_path <- file.path(getwd(), "rdata")

if (!dir.exists(rdata_path)) {
  dir.create(rdata_path)
}

save(stk,  sr, out, file = file.path(rdata_path, paste0(run, ".rdata")))



ss3rep = SS_output(dir01)
idxs =ss3om::readFLIBss3(dir01) # FLIndexBiomass


## Retune Stock-Recruitment

run = "ane.27.9aS"
load(file=file.path("rdata",paste0(run,".rdata")),verbose = T)
stka = stk

# Extract SR pars
s = params(sr)[1, 2, drop=TRUE]
R0 = params(sr)[2, 2, drop=TRUE]
B0 = params(sr)[3, 2, drop=TRUE]

# single sex
sr1 = srrTMB(as.FLSR(stka,
                     model=bevholtSV),
             #spr0=mean(spr0y(stk)),
             r0.pr=c(R0,0.0001),
             s=s,
             s.est=F)


