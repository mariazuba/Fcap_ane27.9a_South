
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
     