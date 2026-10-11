##########################
##########################
# run the xogen function
# to adjust monthly data
# for Volcanic aerosils,
# ENSO, and solar activity
##########################
##########################
#
# change this line to point to the directory
# containing the program and data file
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling/scripts/AdjustData")

####################
# load the functions
####################
source("Xogen.r")

###############
# load the data
###############
zobs.all = read.csv("ObsData.csv",as.is=T)
zobs = zobs.all

#########################
# example: data from NASA
#########################
t = zobs$t           # times (must be monthly data)
volc = zobs$volc     # volcanic AOD
volc[!is.finite(volc)] = 0    # set missing volc to zero
soi = zobs$SOI       # SOI (southern oscillation index)
nino = zobs$NINO34   # NINO3.4
ssn = zobs$ssn       # sunspot number
##########
# pick one
##########
#x = zobs$NASA; tit = "NASA"        # temperature data (NASA GISS)
#x = zobs$NOAA; tit = "NOAA"        # temperature data (NOAA)
#x = zobs$HadCRU; tit = "HadCRU"    # temperature data (HadCRU)
#x = zobs$Berk; tit = "Berk"        # temperature data (Berkeley)
#x = zobs$ERA5; tit = "ERA5"        # temperature data (ERA5)
#x = zobs$Berk.land; tit = "BerkLand"  # temperature data (Berkeley land-only)
#x = zobs$rssTLT; tit = "rssTLT"  # temperature data (RSS TLTv4)
x = zobs$tmean; tit = "LHI"  # temperature data (RSS TLTv4)

##############################
# do it!
# it will take a while
# the number on the left
# of the title line
# will cycle from 0 to 24
# and it will do so five times
##############################
zback = xogen(t,x,volc,nino,ssn,t.2fit=c(1950,2025))

zout = zback$zout    # main output data array

plot(zout$t,zout$raw,type="l",
    xlab="Year",ylab="Temp.Anom. (deg.C)",main=tit)
lines(zout$t,zout$adj,col="red")

##################
# save the results
##################
varblist = c("varblist","zback")
fsav = paste(tit,".rad",sep=""); fsav
fout = paste(tit,"_adj.csv",sep=""); fout
#######################
# un-comment next two
# lines to save to disk
#######################
save(list=varblist,file=fsav)
write.csv(zout,file=fout,row.names=F,quote=F)

#########
#########
#########
# Remove superfluous values, functions and datasets
rm(list = ls()[!sapply(ls(), function(x) is.data.frame(get(x)))])
rm(zobs, zobs.all, zout)
