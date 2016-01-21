
library(XML)
library(stringr)
library(googlesheets)
load("FUNCTS.Rdata")
load("NMFUN.Rdata")
load("MFUN.Rdata")
#USE package.skeleten(name="cusudf", code_files="cusudf.R")
#ONCE MORE FUNTIONS ARE NECESSARY

#########ESPN.GO PLAYER MINING###########
##########GATHER TEAM LIST#############
psrc <- GetSRC(3,2)

#############################IMPORT BC TIERS####################
bqb<-FixBC("bcqb", "BCQB", "QB", "PLAYER")
brb<-FixBC("bcrb", "BCRB", "RB", "PLAYER")
bte<- FixBC("bcte", "BCTE", "TE", "PLAYER")
bwr<-FixBC("bcwr", "BCWR", "WR", "PLAYER")
bdst<-FixBC("bcdst", "BCDST", "DST", "TEAM")

########		VEGAS ODDS			#############
vegas<-GetVegas("http://www.donbest.com/nfl/odds/")

###############	WEATHER	###################
weather<-GetWeather("http://www.nflweather.com/")
weather<-FixTMNM(weather)

###############	ROTO CURVE TD PROJECTIONS	##############
rctd<-GetRCTD("http://www.rotocurve.com/nfl-touchdown-predictor/")
rctd<-FixRCNM(rctd)

##############################POINTS AGAINST POSITION############################
paqb<-GetPAP(1)
paqb<-FixTMNM(paqb)
parb<-GetPAP(2)
parb<-FixTMNM(parb)
pawr<-GetPAP(3)
pawr<-FixTMNM(pawr)
pate<-GetPAP(4)
pate<-FixTMNM(pate)
padst<-GetPAP(16)
padst<-FixPADNM(padst)
padst <- gsub("/", "D", padst) 

########		FANTASY PROS RANKINGS			##########################
fpqb<-GetFP("qb", 15, 11) 
fprb<-GetFP("rb", 13, 9) 
fpwr<-GetFP("wr", 13, 9)
fpte<-GetFP("te", 10, 6)

###########		TEAM DRIVE STATS			############
tdr<-GetDRST("http://www.pro-football-reference.com/years/2015/")

###########		ESPN.GO STATS	###############
goqb<-GetGOSTAT("passing", 14, 15, 2)
goqb<-FixGOST(goqb)
goqb<-FixGOTM(goqb)
goqb<-FixGOTNM(goqb)
goqb<-goqb[,2:14]
colnames(goqb) <- as.list(c("PLAYER", "TEAM", "PASS COMPLETIONS", "PASS ATTEMPTS", "PASS COMP%", "PASS YARDS", "PASS AVG/YARDS",
"PASS LONG", "QB TDS", "INTS", "SACKS", "QB-RATING", "QB YARDS/GAME" ))
goqb<-FixFPNM(goqb)

gorb<-GetGOSTAT("rushing", 12, 13, 6)
gorb<-FixGOST(gorb)
gorb<-FixGOTM(gorb)
gorb<-FixGOTNM(gorb)
gorb<-gorb[,2:12]
colnames(gorb) <- as.list(c("PLAYER", "TEAM", "RUSHING ATTEMPTS", "RUSHING YARDS", "RUSHING AVG/YARD",
"RUSHING LONG", "RUSHING 20+ CT", "RUSH TDS", "RUSHING YARDS/GAME", "RUSHING FUMBLES", "RUSHING FIRST DOWNS"))
gorb<-FixFPNM(gorb)

gowr<-GetGOSTAT("receiving", 14, 15, 10)
gowr<-FixGOST(gowr)
gowr<-FixGOTM(gowr)
gowr<-FixGOTNM(gowr)
gowr<-gowr[,2:14]
colnames(gowr) <- as.list(c("PLAYER", "TEAM", "RECEPTIONS", "TARGETS", "REC YARDS", "REC AVG/YARD",
"REC TDS", "REC LONG", "REC 20+ CT", "REC YARDS/GAME", "REC FUMBLES", "YARDS AFTER CATCH", "REC FIRST DOWNS"))
gowr<-FixFPNM(gowr)

################################################ MERGING	#################################
#####################SRC ---- COMPLETE -----psrc -------ATTACHED	############################
#####################ROTOCURVE TDPROJ ---- COMPLETE -----rctd -------ATTACHED	################

x1<-NULL
psrc <- FixSRCPOS(psrc)
x1<-merge(psrc, rctd, by=c("PLAYER", "POSITION"), all = TRUE)
x1<-subset(x1 , x1 [,3] != "NA") #CHECK PERIODICALLY FOR OTHER PLAYER NAME ISSUES
x1[,c("RC/TDPROJ")] <- as.numeric(as.character(x1[,c("RC/TDPROJ")]))

#######################DRAW OPPONENENTS#############################################################
x1<-FindOPP(x1, vegas)

#####################BORIS CHEN	---- COMPLETE----bqb, brb, bte, bwr, bdst -------ATTACHED####
bqb<-FixFPNM(bqb)
brb<-FixFPNM(brb)
bte<-FixFPNM(bte)
bwr<-FixFPNM(bwr)
BC<-NULL
BC<-rbind(bqb, brb, bte, bwr)
x1<-merge(x1, BC, by=c("PLAYER", "POSITION"), all = TRUE)
x1<-merge(x1, bdst, by=c("TEAM", "POSITION"), all = TRUE)
x1<-BCDFIX(x1)

#####################FANTASY PROS ---- COMPLETE ---- fpqb, fprb, fpwr, fpte -------ATTACHED####
fpqb<-FixFPNM(fpqb)
fprb<-FixFPNM(fprb)
fpte<-FixFPNM(fpte)
fpwr<-FixFPNM(fpwr)
FP<-NULL
FP<-rbind(fpqb, fprb, fpwr, fpte)
x1<-merge(x1, FP, by=c("PLAYER", "POSITION"), all = TRUE)
x1<-subset(x1 , x1 [,3] != "NA")

#####################POINTS AGAINST POSITION ---- COMPLETE ----- paqb, parb, pawr, pate, padst	####

PA<-rbind(paqb, parb, pawr, pate, padst)
x1<-merge(x1, PA, by=c("OPPONENT", "POSITION"), all = TRUE)
####CLEAR BYE WEEK TEAMS (OPPONENTS) ##########
x1<-subset(x1 , x1 [,3] != "NA")
x1<-subset(x1 , x1 [,1] != "NA")

#####################VEGAS ---- COMPLETE-----vegas----ATTACHED#####################################
x1<-merge(x1, vegas, by=c("TEAM"), all = TRUE)

#####################WEATHER ---- COMPLETE----weather----ATTACHED####################################
x1<-merge(x1, weather, by=c("TEAM"), all = TRUE)

#####################DRIVE STATS ---- COMPLETE -----tdr----------ATTACHED######################
x1<-MrgDRSTATS(tdr, x1)
####CLEAR BYE WEEK TEAMS (OPPONENTS) ##########
x1<-subset(x1 , x1 [,3] != "NA")
x1<-subset(x1 , x1 [,1] != "NA")

####################STATS(ESPN.GO) ----COMPLETE ----goqb, gorb, gowr-----ATTACHED############################
x2<-merge(goqb, gorb, by=c("PLAYER","TEAM"), all = TRUE)
x2<-merge(x2, gowr, by=c("PLAYER","TEAM"), all = TRUE)
x1<-merge(x1,x2, by=c("PLAYER","TEAM"), all = TRUE)


####CLEAR PLAYERS I DO NOT GIVE A SHIT ABOUT (CHECK PERIODICALLY)#########
x1<-subset(x1 , x1 [,3] != "NA")
pBase <- x1

###################################################################################
###################################################################################
############################		TIERING SYSTEM		#######################
############################		MATHS BEGIN!!!		#######################
###################################################################################
###################################################################################

############################IMPLEMENT DST###########################################################

###DERIVE MEANS --- V2 ###
x1<-V2_CompileByPOS("RB",pBase)
x2<-V2_CompileByPOS("QB",pBase)
x3<-V2_CompileByPOS("WR",pBase)
x4<-V2_CompileByPOS("TE",pBase)
x5<-V2_CompileByPOS("DST",pBase)
###BIND IT TOGETHER###
x6<-rbind(x1, x2, x3, x4, x5)
###REMOVE OLD VARIABLES###
x6<-V1_RidOld(x6)
xdst<-subset(x6, x6[,c("POSITION")] == "DST")
###COMPILE DIFFERENCE OF MEANS AND TIER###











#################COMPILE INTO A FUNTION TO CALL THESE FUNCTIONS###########
#############FOR EACH POSITION################
##########TierCompiler(x,a)###################
##########x 	=	dataset###############
##########a		=	position##############
#####x=x6, a="RB"#############################

xQB<-HILOCompiler(x6,"QB")
xRB<-HILOCompiler(x6,"RB")
xWR<-HILOCompiler(x6,"WR")
xTE<-HILOCompiler(x6,"TE")

########PFACTbyPOS<-function(x,a,b){}	####
####	x	=	dataset			####
####	b	=	list of variables		####
###########LOWEST SCORE IS BEST ##############



FFSTS<-c("PASS COMPS TIER","PASS ATTS TIER","PASS COMP% TIER","PASS YARDS TIER","PASS INT TIER","PASS YARDSperGAME TIER",
"RUSH ATTS TIER", "RUSH YARDS TIER", "RUSH AVERAGE RUSH TIER", "RUSH 20+ CT TIER", "RUSH YARDSperGAME TIER", "RUSH FIRST DOWN TIER",
"RUSH FUMBLES TIER", "REC RECEPTIONS TIER", "REC TARGETS TIER", "REC YARDS TIER", "REC AVERAGE REC TIER", "REC 20+ CT TIER",
"REC YARDS AFTER CATCH TIER", "REC FIRST DOWN TIER", "REC FUMBLES TIER")

FFQ<-c("PASS COMPS TIER","PASS ATTS TIER","PASS COMP% TIER","PASS YARDS TIER","PASS INT TIER","PASS YARDSperGAME TIER",
"RUSH ATTS TIER", "RUSH YARDS TIER", "RUSH AVERAGE RUSH TIER", "RUSH 20+ CT TIER", "RUSH YARDSperGAME TIER", "RUSH FIRST DOWN TIER",
"RUSH FUMBLES TIER")

FFR<-c("RUSH ATTS TIER", "RUSH YARDS TIER", "RUSH AVERAGE RUSH TIER", "RUSH 20+ CT TIER", "RUSH YARDSperGAME TIER", "RUSH FIRST DOWN TIER",
"RUSH FUMBLES TIER", "REC RECEPTIONS TIER", "REC TARGETS TIER", "REC YARDS TIER", "REC AVERAGE REC TIER", "REC 20+ CT TIER",
"REC YARDS AFTER CATCH TIER", "REC FIRST DOWN TIER", "REC FUMBLES TIER")

FFW<-c("RUSH ATTS TIER", "RUSH YARDS TIER", "RUSH AVERAGE RUSH TIER", "RUSH 20+ CT TIER", "RUSH YARDSperGAME TIER", "RUSH FIRST DOWN TIER",
"RUSH FUMBLES TIER", "REC RECEPTIONS TIER", "REC TARGETS TIER", "REC YARDS TIER", "REC AVERAGE REC TIER", "REC 20+ CT TIER",
"REC YARDS AFTER CATCH TIER", "REC FIRST DOWN TIER", "REC FUMBLES TIER")

p1<-PFACTbyPOS(xQB,FFQ)
p2<-PFACTbyPOS(xRB,FFR)
p3<-PFACTbyPOS(xWR,FFW)
p4<-PFACTbyPOS(xTE,FFW)
p1<- p1[,!colnames(p1) %in% FFSTS]
p2<- p2[,!colnames(p2) %in% FFSTS]
p3<- p3[,!colnames(p3) %in% FFSTS]
p4<- p4[,!colnames(p4) %in% FFSTS]
p5<-rbind(p1, p2, p3, p4)

##ATTACH DEFENSE
xdst<-subset(x6, x6[,c("POSITION")] == "DST")
xdst<-xdst[,!colnames(xdst) %in% FFSTS]
xdst<-cbind(xdst, "NA")
xdst<-as.data.frame(xdst)
colno<-length(unique(xdst))
names(xdst)[colno]<-"PFACTOR"
xdst<-as.matrix(xdst)

###########COMPILED###############
FFsnapshot<-rbind(p5,xdst)

###################################################################################
###################################################################################
############################		COMPILE TO DRAFTDAY		#################
############################						#######################
###################################################################################
###################################################################################


####WRAP COMPILE DRAFT DAY####
DD <- read.csv(file="C:\\Users\\C\\Desktop\\FOOTBALL_SEP15\\SRC\\snapshot.csv", header=TRUE, sep=",")
Dlst <- c("Player.ID", "Opponent", "PPG")
DD<-DD[,!colnames(DD) %in% Dlst]
Nlst <- c("POSITION", "PLAYER", "TEAM", "SALARY")
colnames(DD)<-as.list(Nlst)
DD<-subset(DD, DD[,c("POSITION")] != "N/A")
DD<-subset(DD, DD[,c("POSITION")] != "FLEX")
DD<-as.matrix(DD)
DD<-FixTMNM(DD)
DD[,c("POSITION")]<-gsub("D", "DST", DD[,c("POSITION")])
DD1<-subset(DD, DD[,c("POSITION")] == "QB")
DD2<-subset(DD, DD[,c("POSITION")] == "RB")
DD3<-subset(DD, DD[,c("POSITION")] == "WR")
DD4<-subset(DD, DD[,c("POSITION")] == "TE")
DD5<-rbind(DD1,DD2,DD3,DD4)

x1<- merge(FFsnapshot, DD5, by=c("PLAYER", "POSITION", "TEAM"), all=TRUE)
x1<-subset(x1, x1[,c("OPPONENT")] != "NA")

DDD<-subset(DD, DD[,c("POSITION")] == "DST")
DDD<-as.data.frame(DDD)
DDD[,c("PLAYER")]<-NULL
x1<-as.matrix(x1)
lr1<-length(DDD[,2])
lr2<-length(x1[,2])
i<-1
j<-1
while(i<lr2+1){
	while(j<lr1+1){
		if(as.character(DDD[j,1])==as.character(x1[i,2]) & as.character(DDD[j,2])==as.character(x1[i,3])){
			x1[i,c("SALARY")]<-as.character(DDD[j,3]); j<-j+1;
	}else{j<-j+1}
	}
	j<-1
	i<-i+1
}


###WRITE TABLE#######
write.table(FFsnapshot, "C:/Users/C/Desktop/FOOTBALL_SEP15/SRC/FFsnapshot.csv",
row.names=FALSE, sep=",")

write.table(x1, "C:/Users/C/Desktop/FOOTBALL_SEP15/SRC/x1.csv",
row.names=FALSE, sep=",")














