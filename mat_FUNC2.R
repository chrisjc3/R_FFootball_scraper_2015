




#####################CMeanTIER################################

#x1<-CMeanTIER(x1, which(colnames(b)=="EXPEREINCE"), x, "YEARS EXP TIER")

CMeanTIER<-function(p, x, a, b){
x1<-subset(p, p[,4] == a) #PULL POSITION
x<-which(colnames(p)==x)
if(x==9){x1[,x] <- as.numeric(as.character(gsub("R", "1", x1[,x])))
} else if(x==15){x1[,x] <-as.numeric(as.character(x1[,x]))
} else {x1[,x]<-as.numeric(as.character(x1[,x]))}
x2<-length(x1[,1]) #FIND PARAMETERS
colno<-length(unique(x1)) #FIND PARAMETERS
x1<- cbind(x1, "NA") #CREATE PLACEHOLDER
x0<-as.matrix(x1[1:x2,x])
x1[,colno+1] <- colMeans(x0, na.rm=TRUE) #MEAN 
x1<- cbind(x1, "NA")#CREATE PLACEHOLDER 
x1[,colno+2] <- round(as.numeric(x1[,x] - x1[,colno+1]), 2) #'MEAN TIER' #SUBTRACT MEAN FROM VALUE
colno<-length(unique(x1))
names(x1)[colno]<-b #NAME NEW COLUMN
x1[,colno-1]<-NULL #DELETE COLMEAN
x1<-x1
}

############################V2_CompileByPOS#######################
V2_CompileByPOS<-function(x, b){
x1<-b
#BASIC
x1<-CMeanTIER(x1, "EXPEREINCE", x, "YEARS EXP TIER")
x1<-CMeanTIER(x1, "WEIGHT", x, "WEIGHT TIER")
x1<-CMeanTIER(x1, "VSPTS", x, "VS/PTS TIER")
x1<-CMeanTIER(x1, "SPREAD", x, "SPREAD TIER")

#DRIVES
x1<-CMeanTIER(x1, "DR/SCORE%", x, "DR/SCR% TIER")
x1<-CMeanTIER(x1, "DR/PLAYSperDRIVE", x, "DR/PLYperDR TIER")
x1<-CMeanTIER(x1, "DR/YARDSperDRIVE", x, "DR/YRDperDR TIER")
x1<-CMeanTIER(x1, "AVG DRIVE POINTS", x, "DR/POINTS TIER")
x1<-CMeanTIER(x1, "OPP/DR/SCORE%", x, "OPPDR/SCR% TIER")
x1<-CMeanTIER(x1, "OPP/DR/PLAYSperDRIVE", x, "OPPDR/PLYperDR TIER")
x1<-CMeanTIER(x1, "OPP/DR/YARDSperDRIVE", x, "OPPDR/YRDperDR TIER")
x1<-CMeanTIER(x1, "OPP/AVG DRIVE POINTS", x, "OPPDR/POINTS TIER")

#PASS
x1<-CMeanTIER(x1, "PASS COMPLETIONS", x, "PASS COMPS TIER")
x1<-CMeanTIER(x1, "PASS ATTEMPTS", x, "PASS ATTS TIER")
x1<-CMeanTIER(x1, "PASS COMP%", x, "PASS COMP% TIER")
x1<-CMeanTIER(x1, "PASS YARDS", x, "PASS YARDS TIER")
x1<-CMeanTIER(x1, "QB TDS", x, "PASS TD TIER")
x1<-CMeanTIER(x1, "INTS", x, "PASS INT TIER")
x1<-CMeanTIER(x1, "QB-RATING", x, "PASS RATING TIER")
x1<-CMeanTIER(x1, "QB YARDS/GAME", x, "PASS YARDSperGAME TIER")

#RUSH
x1<-CMeanTIER(x1, "RUSHING ATTEMPTS", x, "RUSH ATTS TIER")
x1<-CMeanTIER(x1, "RUSHING YARDS", x, "RUSH YARDS TIER")
x1<-CMeanTIER(x1, "RUSHING AVG/YARD", x, "RUSH AVERAGE RUSH TIER")
x1<-CMeanTIER(x1, "RUSHING 20+ CT", x, "RUSH 20+ CT TIER")
x1<-CMeanTIER(x1, "RUSH TDS", x, "RUSH TDS TIER")
x1<-CMeanTIER(x1, "RUSHING YARDS/GAME", x, "RUSH YARDSperGAME TIER")
x1<-CMeanTIER(x1, "RUSHING FUMBLES", x, "RUSH FUMBLES TIER")
x1<-CMeanTIER(x1, "RUSHING FIRST DOWNS", x, "RUSH FIRST DOWN TIER")

#REC
x1<-CMeanTIER(x1, "RECEPTIONS", x, "REC RECEPTIONS TIER")
x1<-CMeanTIER(x1, "TARGETS", x, "REC TARGETS TIER")
x1<-CMeanTIER(x1, "REC YARDS", x, "REC YARDS TIER")
x1<-CMeanTIER(x1, "REC AVG/YARD", x, "REC AVERAGE REC TIER")
x1<-CMeanTIER(x1, "REC TDS", x, "REC TDS TIER")
x1<-CMeanTIER(x1, "REC 20+ CT", x, "REC 20+ CT TIER")
x1<-CMeanTIER(x1, "REC FUMBLES", x, "REC FUMBLES TIER")
x1<-CMeanTIER(x1, "YARDS AFTER CATCH", x, "REC YARDS AFTER CATCH TIER")
x1<-CMeanTIER(x1, "REC FIRST DOWNS", x, "REC FIRST DOWN TIER")
x1<-x1
}

####################V1_RidOld###################

V1_RidOld<-function(x){
x[,"HEIGHT"]<-NULL;x[,"AGE"]<-NULL;x[,"AGE"]<-NULL;x[,"WEIGHT"]<-NULL;x[,"EXPERIENCE"]<-NULL;x[,"COLLEGE"]<-NULL;

x[,"DR/DRIVES"]<-NULL;x[,"DR/PLAYS"]<-NULL;x[,"DR/SCORE%"]<-NULL;x[,"DR/PLAYSperDRIVE"]<-NULL;x[,"DR/YARDSperDRIVE"]<-NULL;x[,"AVG DRIVE POINTS"]<-NULL;
x[,"VSPTS"]<-NULL;x[,"SPREAD"]<-NULL;x[,"EXPEREINCE"]<-NULL;x[,"NUMBER"]<-NULL;

x[,"OPP/DR/DRIVES"]<-NULL;x[,"OPP/DR/PLAYS"]<-NULL;x[,"OPP/DR/SCORE%"]<-NULL;x[,"OPP/DR/PLAYSperDRIVE"]<-NULL;x[,"OPP/DR/YARDSperDRIVE"]<-NULL;x[,"OPP/AVG DRIVE POINTS"]<-NULL;

x[,"PASS COMPLETIONS"]<-NULL;x[,"PASS ATTEMPTS"]<-NULL;x[,"PASS COMP%"]<-NULL;x[,"PASS YARDS"]<-NULL;x[,"PASS AVG/YARDS"]<-NULL;x[,"PASS LONG"]<-NULL;x[,"QB TDS"]<-NULL;
x[,"INTS"]<-NULL;x[,"SACKS"]<-NULL;x[,"INTS"]<-NULL;x[,"QB-RATING"]<-NULL;x[,"QB YARDS/GAME"]<-NULL;

x[,"RUSHING ATTEMPTS"]<-NULL;x[,"RUSHING YARDS"]<-NULL;x[,"RUSHING AVG/YARD"]<-NULL;x[,"RUSHING LONG"]<-NULL;x[,"RUSHING 20+ CT"]<-NULL;x[,"RUSH TDS"]<-NULL;x[,"RUSHING YARDS/GAME"]<-NULL;
x[,"RUSHING FUMBLES"]<-NULL;x[,"RUSHING FIRST DOWNS"]<-NULL;

x[,"RECEPTIONS"]<-NULL;x[,"TARGETS"]<-NULL;x[,"REC YARDS"]<-NULL;x[,"REC AVG/YARD"]<-NULL;x[,"REC TDS"]<-NULL;x[,"REC LONG"]<-NULL;x[,"REC 20+ CT"]<-NULL;
x[,"REC YARDS/GAME"]<-NULL;x[,"REC FUMBLES"]<-NULL;x[,"YARDS AFTER CATCH"]<-NULL;x[,"REC FIRST DOWNS"]<-NULL;

x[,"DR/PLYperDR TIER"]<-NULL;x[,"DR/SCR% TIER"]<-NULL;x[,"DR/POINTS TIER"]<-NULL;
x[,"OPPDR/SCR% TIER"]<-NULL;x[,"OPPDR/PLYperDR TIER"]<-NULL;x[,"OPPDR/POINTS TIER"]<-NULL;
x1<-x
}

#x<-x6
#t<-c("RC/TDPROJ")
#p<-"RB"

###############################HI#################################################
HITierCompile<-function(x, t, p){
rbt<-subset(x , x [,c("POSITION")] == p)
i<-1
z<-0
trigger<-NULL
mxst<-NULL
mnst<-NULL
lr<-length(rbt[,1])
while(i<lr+1){
	if(is.na(rbt[i,t]) == FALSE){
	trigger <- as.numeric(as.character(rbt[i,t]))
	if(z==0){mxst<-trigger;mnst<-trigger;
	}else if(trigger > mxst){mxst<-trigger
	}else if(trigger < mnst){mnst<-trigger
	}
	z<-1
	}
i<-i+1
}
if(is.null(mxst) == FALSE){
t1m<-mxst-(mxst*.20);t2m<-mxst-(mxst*.40);t3m<-mxst-(mxst*.60);
t4m<-mxst-(mxst*.80);t5m<-mnst-(mnst*.20);
rbt<-as.matrix(rbt)
i<-1
lr<-length(rbt[,1])
while(i<lr+1){
	if(is.na(rbt[i,t]) == FALSE){
	trigger <- as.numeric(as.character(rbt[i,t]))
	if (trigger >= t1m) {
	rbt[i,t]<-"1"
	}else if(trigger >= t2m & trigger < t1m){
	rbt[i,t]<-"2"
	}else if(trigger >= t3m & trigger < t2m){
	rbt[i,t]<-"3"
	}else if(trigger >= t4m & trigger < t3m){
	rbt[i,t]<-"4"
	}else if(trigger >= t5m & trigger < t4m){
	rbt[i,t]<-"5"
	}else{rbt[i,t]<-"6"}
	}
	i<-i+1
}
}
rbt<-rbt
}

####################################LO######################################
LOTierCompile<-function(x, t, p){
rbt<-subset(x , x [,c("POSITION")] == p)
i<-1
z<-0
trigger<-NULL
mxst<-NULL
mnst<-NULL
lr<-length(rbt[,1])
while(i<lr+1){
	if(is.na(rbt[i,t]) == FALSE){
	trigger <- as.numeric(as.character(rbt[i,t]))
	if(z==0){mxst<-trigger;mnst<-trigger;
	}else if(trigger > mxst){mxst<-trigger
	}else if(trigger < mnst){mnst<-trigger
	}
	z<-1
	}
i<-i+1
}
if(is.null(mxst) == FALSE){
t1m<-mxst-(mxst*.20);t2m<-mxst-(mxst*.40);t3m<-mxst-(mxst*.60);
t4m<-mxst-(mxst*.80);t5m<-mnst-(mnst*.20);
rbt<-as.matrix(rbt)
i<-1
lr<-length(rbt[,1])
while(i<lr+1){
	if(is.na(rbt[i,t]) == FALSE){
	trigger <- as.numeric(as.character(rbt[i,t]))
	if (trigger >= t1m) {
	rbt[i,t]<-"6"
	}else if(trigger >= t2m & trigger < t1m){
	rbt[i,t]<-"5"
	}else if(trigger >= t3m & trigger < t2m){
	rbt[i,t]<-"4"
	}else if(trigger >= t4m & trigger < t3m){
	rbt[i,t]<-"3"
	}else if(trigger >= t5m & trigger < t4m){
	rbt[i,t]<-"2"
	}else{rbt[i,t]<-"1"}
	}
	i<-i+1
}
}
rbt<-rbt
}

###########HILOCompiler########################

HILOCompiler<-function(x,a){
x<-HITierCompile(x, c("RC/TDPROJ"), a)
x<-HITierCompile(x, c("VS/PTS TIER"), a)
x<-HITierCompile(x, c("PASS COMPS TIER"), a)
x<-HITierCompile(x, c("PASS ATTS TIER"), a)
x<-HITierCompile(x, c("PASS COMP% TIER"), a)
x<-HITierCompile(x, c("PASS YARDS TIER"), a)
x<-LOTierCompile(x, c("PASS INT TIER"), a)
x<-HITierCompile(x, c("PASS YARDSperGAME TIER"), a)
x<-HITierCompile(x, c("RUSH ATTS TIER"), a)
x<-HITierCompile(x, c("RUSH YARDS TIER"), a)
x<-HITierCompile(x, c("RUSH AVERAGE RUSH TIER"), a)
x<-HITierCompile(x, c("RUSH 20+ CT TIER"), a)
x<-HITierCompile(x, c("RUSH YARDSperGAME TIER"), a)
x<-HITierCompile(x, c("RUSH FIRST DOWN TIER"), a)
x<-LOTierCompile(x, c("RUSH FUMBLES TIER"), a)
x<-HITierCompile(x, c("REC RECEPTIONS TIER"), a)
x<-HITierCompile(x, c("REC TARGETS TIER"), a)
x<-HITierCompile(x, c("REC YARDS TIER"), a)
x<-HITierCompile(x, c("REC AVERAGE REC TIER"), a)
x<-HITierCompile(x, c("REC 20+ CT TIER"), a)
x<-HITierCompile(x, c("REC YARDS AFTER CATCH TIER"), a)
x<-HITierCompile(x, c("REC FIRST DOWN TIER"), a)
x<-LOTierCompile(x, c("REC FUMBLES TIER"), a)
x<-x
}








####################FINAL SCORE########################

#FFSTS<-c("PASS COMPS TIER","PASS ATTS TIER","PASS COMP% TIER","PASS YARDS TIER","PASS TD TIER","PASS INT TIER","PASS RATING TIER","PASS YARDSperGAME TIER",
#"RUSH ATTS TIER", "RUSH YARDS TIER", "RUSH AVERAGE RUSH TIER", "RUSH 20+ CT TIER", "RUSH YARDSperGAME TIER", "RUSH FIRST DOWN TIER",
#"RUSH FUMBLES TIER", "REC RECEPTIONS TIER", "REC TARGETS TIER", "REC YARDS TIER", "REC AVERAGE REC TIER", "REC 20+ CT TIER",
#"REC YARDS AFTER CATCH TIER", "REC FIRST DOWN TIER", "REC FUMBLES TIER")
#b<-FFSTS
#x<-x6


PFACTbyPOS<-function(x,b){
xtt<-as.matrix(x)
xtt<-cbind(xtt, "NA")
colno<-length(unique(as.data.frame(xtt)))
xtt<-as.data.frame(xtt)
names(xtt)[colno]<-"PFACTOR"
xtt<-as.matrix(xtt)
z<-length(b)
lr<-length(xtt[,1])
i<-1
j<-1

while(i<lr+1){
	while(j<z+1){
	if(is.na(xtt[i,unlist(b[j])]) == TRUE){
	xtt[i,unlist(b[j])]<-"6"; j<-j+1} else {j<-j+1}
	}
j<-1
xtt[i,colno]<-sum(as.numeric(xtt[i,unlist(b)]), na.rm=TRUE)
i<-i+1
}
xtt<-xtt
}










####################SAVE FOR LOAD############################
save(CMeanTIER, V1_RidOld, HITierCompile, LOTierCompile, PFACTbyPOS,
HILOCompiler, V2_CompileByPOS,
file = "MFUN.Rdata")












