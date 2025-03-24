
#SS3 assessment summary for ane27.9aSouth

library(r4ss)
library(FLRef)
library(ss3om)
library(ss3diags)

# 1.1 Step 1: Set up file paths and folders structure for loading and saving the SS3 model ----
# Set up the file path to the folder where the SS3 model folder with run is located.
# Define name of reference model folder with the SS3 model outputs

model = "boot/data/Lastyear"

#Create .rds stock file name
stock.file = paste0("ane.27.9aS_", ".rds")

#Load reference model
ss3rep = SS_output(file.path(model))

#Create an rdata folder in the assessment model subdirectory.
rdata = file.path("rdata")
dir.create(rdata, showWarnings = FALSE)

#Save the model as rdata file
saveRDS(ss3rep, file = file.path("rdata", stock.file))

#...or load directly as .rdata if these had been saved already
ss3rep = readRDS(file.path(rdata, stock.file))

#1.2 Specify benchmarks ----
Fmsy = NA
Fpa = NA
Fupper = NA
Flower = NA
Fp0.5 = NA
Btrigger = NA
Blim = 4721
Bpa = 6561
TAC = 7266 # 2025

#Make data.frame that is compatible with FLPar in FLR

benchmarks = data.frame(params = c("Fmsy", "Fpa", "Fupper", "Flower",
                                   "Fp0.5", "Btrigger", "Blim", "Bpa"), 
                        data = c(Fmsy, Fpa, Fupper, Flower, Fp0.5, Btrigger, Blim, Bpa))


#Convert to FLPar
bms = as(benchmarks, "FLPar")
bms

# 1.3 Step 2: Convert SS3 to FLStockR ----
# First, the ssmvln() from FLRef is used to generate the stock trajectories with uncertainty using a Monte-
#   Carlo to generate a large number of iterations from multivariate log-normal approximation of the variance-
#   covariance estimates.
# For the conventional ICES advice, it is important to extend the assessment horizon to the reference year+1
# to plot SSB and recruitment one-step-ahead (y + 1) of F and Catch.
years = ss3rep$startyr:(ss3rep$endyr + 1)
mvn = FLRef::ssmvln(ss3rep, Fref = "Btgt", verbose = F, years = years)


# The option Fref=Btgt, and not Fref=MSY is chosen because the reference points were based on Btgt = B40,
# with a corresponding Ftgt = F40.
# This can be checked by
mvn$Btgtref

# Next the mvn object can easily converted into the FLStockR object
stk = ss2FLStockR(mvn)
# By default, the reference points for Ftgt and Btgt are extracted together with MSY, B0 and R0.
stk@refpts

# However, for the final advice plot only the agreed benchmarks should be shown. This can be done by
# specifying stk@refpts as the FLPar object bms
# Here, thereferencepointsFM SY , Fpa = Fp0.5, M SY Btrigger (Btrigger ),Bpa andBlim areselectedforplotting
stk@refpts = bms[c("Fmsy", "Fpa", "Btrigger", "Bpa", "Blim")]
# The option osa=TRUE allows to plot the one-step-ahead forecast for SSB and recruitment through 2023,
# while omitting 2023 for F and Catch.
plotAdvice(stk, osa = T)

rec(stk)
ssb(stk)

# Next make a FLStockR with iterations generated MVLN Monte-Carlo (default nsim = 1000) to depict
# uncertainty
# with uncertainty
stki = ss2FLStockR(mvn, output = "iters", thin = 1)
# assign benchmark reference points
stki@refpts = stk@refpts

#Check
plotAdvice(stki, osa = TRUE)
#caption: Uncertainty of estimated stock status trajectories with associated reference points, with solid line
# depicting the median


#1.4 Step 3: Make Advice plot of stock status indicators with uncertainty ----
# The final advice plot seeks to provide a standard format for presenting stock status indicators that shows the
# exact maximum likelihood estimates from the model (stk) and depicts the uncertainty around those from
# the Monte-Carlo approach (stki).
# The plotting code allows to specify the years shown along the x-axis by adjusting the option
# break=c(seq(years[1],tail(years,1),5),tail(years,1)) depending on the length of the time
# series (here every 5 years and the last year)
# Name plot padv
padv = plotAdvice(FLStocks(CIs = stki, Fit = stk), osa = TRUE) +
  scale_fill_manual(values = c("salmon", "black")) + 
  scale_color_manual(values = c(0,"black")) + theme(legend.position = "none") 
padv                                   
                                    
  
# 1.4.1 Save FLStockR objects in .rds format to rdata

# It is adviced to specify additional information in the FLStockR object before saving it.
# Label the FLStockR object properly

stk2=stk
stk2@name="ane.27.9aS"
stk2@desc="2025, SS3, WGHANSA"
sr = ss3om::readFLSRss3(model,stk2)

# Note that stk@name will be used through this script to label file names!

saveRDS(stk2, file = file.path(rdata, paste0(stk2@name, "_stk2.rds")))
saveRDS(stki, file = file.path(rdata, paste0(stk2@name, "_stki.rds")))

# 1.5 Step 5: Generate output trajectories for the ICES advice template ----
# First a folder òutput is created to save the outputs (Tables, Figures).
                                 
output.dir = file.path("output")
dir.create(output.dir, showWarnings = FALSE)

# The output from ssmvln can now be directly converted in the ICES time series compatible format from SS3
# model by

icests = ss2ices(mvn)
knitr::kable(icests, "pipe", align = "lccccc", caption = " Assessment summary. High and Low refer to 95%")

# The timeseries can be saved as .csv files
write.csv(icests, file = file.path(output.dir, paste0(stk2@name,
                                                      "_ts.csv")), row.names = F)



#'*===========================================================*
# - Stock-recruitment relationship(s) ----
#'*===========================================================*

#'*PARA CAMBIAR A data.R despues!!!*
# Beverton-Holt Model

# Extract SR pars
s = params(sr)[1, 2, drop=TRUE]
R0 = params(sr)[2, 2, drop=TRUE]
B0 = params(sr)[3, 2, drop=TRUE]


sr1 = srrTMB(as.FLSR(stk,
                     model=bevholtSV),
             spr0=mean(spr0y(stk)),
             r0.pr=c(R0,0.0001),
             s=s,
             s.est=F)

params(sr1)
plot(sr1)
plotsrs(sr1,b0=T)+
  geom_hline(yintercept = R0,linetype=2)




idxs =ss3om::readFLIBss3(model)
# idx1=idxs$polyvalent_sp_cpue
# idx1@range[c("startf","endf")] = c(6/12,7/12)

plot(idxs)+theme_bw()+
  ylab("Biomass")+xlab("Year")


plotdyn(stk)+
  ggtitle(paste0(stk@name))


plotbioyr(stk)+
  ggtitle(paste0(stk@name))

plotbioage(stk)+
  ggtitle(paste0(stk@name))


## Add uncertainty


# Read `ssmvln` [(Kell et al. 2023)](https://www.iccat.int/Documents/CVSP/CV080_2023/n_4/CV080040837.pdf) to generate random recruitment trajectories (see https://github.com/laurieKell/SCRS-papers/blob/main/mvln) 

