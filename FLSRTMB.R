

#Example data are loaded for North Sea from FLCore
dir01 <- "boot/data/Lastyear"
stk <- readFLSss3(dir01)
stk@m.spwn[] = 0
stk@harvest.spwn[] = 0 
srr = ss3om::readFLSRss3(dir01,birthseas = 2) #A function to read the stock-recruit relationships from an SS3 run into an FLSR object
plot(srr)



#The Beverton and Holt model is parameterized as a function of steepness (s) and unfished spawning potential ratio SPR0. To create the FLSR input object, the model=bevholtSV is selected.

# Beverton-Holt Model
s = params(srr)[1, 2, drop=TRUE]
R0 = params(srr)[2, 2, drop=TRUE]
B0 = params(srr)[3, 2, drop=TRUE]


sr <- as.FLSR(stk,model=bevholtSV)
spr0 <- yearMeans(spr0y(stk))

bh = srrTMB(sr,spr0=spr0,
            r0.pr=c(R0,0.0001),
            s=s,
            s.est=F)

#'*=====================================*
#'#https://github.com/flr/FLSRTMB
data(ple4)
sr_1 <- as.FLSR(ple4,model=bevholtSV)
spr0_1 <- yearMeans(spr0y(ple4))
bh_1 = srrTMB(sr_1,s.est=T, spr0=spr0_1)
plot(bh_1)
params(srrTMB(sr_1,s.est=T, spr0=spr0_1,report.sR0 = TRUE))
plot(spr0y(ple4))+ylab("spr0")
bh.y = srrTMB(sr_1,s.est=T, spr0=spr0y(ple4))
bh.y3 = srrTMB(sr_1,s.est=T, spr0=spr0y(ple4),nyears=3)
plot(FLSRs(spr0=bh_1,spr0y=bh.y,spr0y3 = bh.y3))+theme(legend.position="right")
bh.prior = srrTMB(sr_1,s.est=T,s=0.7,s.logitsd=0.4, spr0=spr0y(ple4))
plot(FLSRs(spr0=bh_1,spr0y=bh.y,s.prior = bh.prior))+theme(legend.position="right")
s = c(0.75,0.8,0.85,0.9,0.95)

bhs <- FLSRs(sapply(s, function(x) { return( srrTMB(sr_1,s=x,s.est=F, spr0=spr0y(ple4)))}))

bhs@names = c(paste("s =",round(s,3)))

plot(bhs)+theme(legend.position="right")

hs = srrTMB(as.FLSR(ple4,model=segreg),s.est=T, spr0=spr0y(ple4))
hs.b01 = srrTMB(as.FLSR(ple4,model=segreg),s.est=T, spr0=spr0y(ple4),plim=0.1)
plot(FLSRs(hs=hs,hs.b01=hs.b01))
#'*======================================*
#'#https://flr-project.org/doc/Modelling_stock_recruitment_with_FLSR.html
# Create an empty FLSR object.
sr1 <- FLSR()

# Create an  FLSR object using the existing SR models.
sr2 <- FLSR(model = bevholtSV)
sr2@model
sr2@initial
sr2@logl

ssb <- FLQuant(ssb(stk)[, , , season = 2]) # FLQuant(runif(10, 10000, 100000))
rec <- FLQuant(rec(stk)[, , , season = 3]) # 10000 + 2*ssb + rnorm(10,0,1)
sr5 <- FLSR(model = bevholtSV, ssb = ssb, rec = rec)
sr5.mle <- fmle(sr5)
plot(sr5.mle)
summary(sr5.mle)
ssb(sr5.mle)[,1]
rec(sr5.mle)[,1]


# Fit a bevholtSV model with fixed steepness at 0.8
model(sr5.mle) <- bevholtSV
p4sr <- fmle(sr5.mle, fixed = list(s = 0.8))

par(mfrow=c(1,1))
plot(p4sr)
params(p4sr)

# SPR0
spr0=mean(spr0y(stk))
plot(spr0y(stk))+theme_bw()+
  ylab(expression(SPR[0]))+xlab("Year")+
  geom_hline(yintercept = mean(spr0y(stk)),linetype="dashed")

bh = srrTMB(p4sr,s.est=T, spr0=spr0)

plot(bh)
bh@SV

srpars <- bootstrapSR(p4sr, iters=1,
                      models=c("bevholt"), method="best")


#'*======================================*
#'*OTRA PRUEBA*
#'
#'*======================================*
#'
data(ple4)
plot(ple4)
# Plot the SSB-Recruits graph
ggplot(aes(ssb, rec), data=model.frame(FLQuants(ple4, "ssb", "rec"))) +
  geom_point() + geom_smooth(method="loess")

sr1 <- FLSR() # objeto vacio
p4sr <- as.FLSR(ple4) # crea un objeto FLSR convirtiendo directamente un objeto FLStock
summary(p4sr)
# Outputs the contents of the first year of the rec and ssb slots of the FLSR object
ssb(p4sr)[,1]
rec(p4sr)[,1]
# You can set a different recruitment age, e.g. age 2, 
# by trimming the FLStock object as follows:
p4sr2 <-as.FLSR(ple4[-1])
ssb(p4sr2)[,1]
rec(p4sr2)[,1]

# Assign a Ricker SR model and fit it with fmle (which uses logl and R's optim model fitting through MLE)
model(p4sr) <- ricker()
p4sr<-fmle(p4sr)
## model formula
# model(p4sr)
## log-likelihood
# logl(p4sr)
# initial values for the optimiser
initial(p4sr)
# lower and upper limits for the parameters
lower(p4sr)
upper(p4sr)

# Diagnostic plots
plot(p4sr)
