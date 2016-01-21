
##########GET_SRC FUNCTION##############
#######EXAMPLE CALL: src<-GetSRC(i(3), i1(2))########
###########FOR ESPN.GO PLAYER SOURCE#############

GetSRC <- function(i, i1) {
url <- "http://espn.go.com/nfl/players" 
doc <- htmlParse(url, useInternal = TRUE)
aqhSRC <- sapply(getNodeSet(doc, "//*//a"), function(x) xmlValue(x))
i <- 3
hldSRC <- aqhSRC[1]
while(i<100){
if (" " %in% aqhSRC[i]) {
	x <- cbind(aqhSRC[i+1])
	hldSRC <- rbind(hldSRC, x)
	i <- i+2} else {
	i <- i+1
	}
} 

i1 <- 2
while(i1<35){
	x <- str_replace_all(hldSRC[i1], fixed(" "), "-")
	x <- str_to_lower(x, locale = "")
	b <- x
	if("green-bay-packers" %in% b) {b <- "gb"} else
	if("san-francisco-49ers" %in% b) {b <- "sf"} else 
	if("san-diego-chargers" %in% b) {b <- "sd"} else  
	if("st.-louis-rams" %in% b) {b <- "stl"} else 
	if("jacksonville-jaguars" %in% b) {b <- "jax"} else 
	if("kansas-city-chiefs" %in% b) {b <- "kc"} else 
	if("washington-redskins" %in% b) {b <- "wsh"} else 
	if("new-england-patriots" %in% b) {b <- "ne"} else 
	if("new-york-giants" %in% b) {b <- "nyg"} else 
	if("tampa-bay-buccaneers" %in% b) {b <- "tb"} else 
	if("new-orleans-saints" %in% b) {b <- "no"} else 
	if("new-york-jets" %in% b) {b <- "nyj"} else 
	{b <- str_sub(b, 1, 3)}
	url <- paste("http://espn.go.com/nfl/team/roster/_/name/", b, "/", x, sep="")
	doc <- htmlParse(url, useInternal = TRUE)
	aqh <- sapply(getNodeSet(doc,"//*//td"), function(x) xmlValue(x))
	#fix height
	aqh <- str_replace_all(aqh, fixed("-"), "'")
		lsta <- length(aqh)
		hld <- aqh[2:9] #HEADERS
		i <- 10
		#PULL OFFENSE#
			while(i<lsta) {
			if ("Defense" %in% aqh[i]) break
			x <- cbind(aqh[i], aqh[i+1], aqh[i+2], aqh[i+3] ,aqh[i+4], aqh[i+5], aqh[i+6], aqh[i+7], hldSRC[i1])
			hld <- rbind(hld, x)
			endn <- i
			i <- i + 8
		}
		#PULL DEFENSE
		i <- endn+17
			while(i<lsta) {
			if ("Special Teams" %in% aqh[i]) break
			x <- cbind(aqh[i], aqh[i+1], aqh[i+2], aqh[i+3] ,aqh[i+4], aqh[i+5], aqh[i+6], aqh[i+7], hldSRC[i1])
			hld <- rbind(hld, x)
			endn <- i
			i <- i + 8
		}
		#PULL SPECIAL TEAMS
		i <- endn+17
			while(i<lsta) {
			if ("Special Teams" %in% aqh[i]) break
			x <- cbind(aqh[i], aqh[i+1], aqh[i+2], aqh[i+3] ,aqh[i+4], aqh[i+5], aqh[i+6], aqh[i+7], hldSRC[i1])
			hld <- rbind(hld, x)
			endn <- i
			i <- i + 8
		}

	if(i1<3) {hldF <- hld} else {hldF <- rbind(hldF,hld)}
	i1 <- i1+1
}

#save/clean source list for binding
	PLAYERS <- hldF
colnames(PLAYERS) <- as.list(c("NUMBER", "PLAYER", "POSITION", "AGE", "HEIGHT", "WEIGHT", "EXPEREINCE", "COLLEGE", "TEAM"))
PLAYERS <- subset(PLAYERS, PLAYERS[,1] != "NO")
}

############FIXBC FUNCTION##################################
####EXAMPLE CALL: bqb<-FixBC("bcqb", "BCQB", "QB")###########
###########FOR BORIS CHEN RANKINGS##########################

FixBC <- function(x, a, b, c) {
bas<-a
cs1<-".csv"
cst<-"C:/Users/C/Desktop/FOOTBALL_SEP15/FFDATA/"
path<-paste(cst,bas,cs1, sep="")
x <- read.csv(file=as.character(path),header=FALSE, sep=",")

x1<-str_split(as.matrix(x), ":")
x1<-unlist(x1)
lr<-length(x1)
i<-1
c1<-c("TIER", c, "POSITION")
xe<-c()
xo<-c()
while(i<lr+1) {
	if(i %% 2 == 0){
		xe<-rbind(xe, x1[i])
	} else {
		xo<-rbind(xo, x1[i])
	}
i<-i+1
}
x2<-cbind(xo,xe)
x3<-str_split(x2[,2], ",")

p<-1
lr1<-length(x3)
x70<-c("TIER", c, "POSITION")
while (p<lr1) {
x0<-unlist(x3[p])
lr<-length(x0)
b1 <- c()
i<-1 
while (i<lr*lr1) {
b1 <- rbind(b1, x0[i])	
i<-i+1
}
x60<-cbind(x2[p,1], b1, b)
c1<-rbind(c1, x60)
p<-p+1
}

x1<- str_trim(c1[,2])
x2<- c1[,1]
x3<- c1[,3]
x4<- cbind(x2, x1, x3)
colnames(x4) <- as.list(c("BC/TIER", c, "POSITION"))
x4 <- subset(x4 , x4 [,1] != "TIER")
x4 <- subset(x4 , x4 [,2] != "NA")
a <- x4
}


#########################DONBEST VEGAS ODDS#######################
GetVegas <- function(x){
url <- x #######CHECK PAGE, LOOKS LIMITED SOMETIMES
doc <- htmlParse(url, useInternal = TRUE)
aqh <- sapply(getNodeSet(doc, "//table//tr//th"), function(x) xmlValue(x))
hld <- aqh[2:3] #HEADER DATA
aqb <- sapply(getNodeSet(doc, "//table//tr//td//a//nobr"), function(x) xmlValue(x))
aqc <- sapply(getNodeSet(doc, "//table//tr//td[@class='alignRight oddsOpener']//div//text()[1]"), function(x) xmlValue(x))
aqd <- sapply(getNodeSet(doc, "//table//tr//td[@class='alignRight oddsOpener']//div//text()[2]"), function(x) xmlValue(x))
###PULL ALL TERRIBLY FORMATTED DATA###

hld <- NULL
y <- NULL
i <- NULL
od <- 1
i <- 1 #INTERVAL COUNT
HA <- cbind("HOME", "AWAY")
while(i<33){
	x <- cbind(aqc[od], aqb[i], HA[1])
	y <- cbind(aqd[od], aqb[i+1], HA[2])
z <- rbind(x,y)
hld <- rbind(hld, z)
i <- i + 2
od <- od + 1
}

#Gets spread for -
#make seperate column for - signs
#if - exists then that line will be compiled with the next or prev spread

l <- "-"
hld <- data.frame(hld)
y <- as.numeric(rownames(hld))
hld <- cbind(hld, y)
hld <- as.matrix(hld)
hldVEG <- NULL
i <- 1
p <- 0

while(i<33){
if (grepl(l, hld[i,1]) == FALSE && grepl("HOME", hld[i,3]) == TRUE) {
	x1 <- as.numeric(as.vector(hld[i+1,1]))
	x2 <- as.numeric(as.vector(hld[i,1]))
	xh <- x2/2 - x1
	xa <- x2/2 + x1
	g1 <- subset(hld, y == i)
	g2 <- subset(hld, y == i+1)
	th1 <- cbind(as.matrix(g1), xa)
	th2 <- cbind(as.matrix(g2), xh)
	th <- rbind(th1,th2)
	if (p == 0) {hldVEG <- th} else {hldVEG <- rbind(hldVEG, th)}
} else {
if (grepl(l, hld[i,1]) == TRUE && grepl("HOME", hld[i,3]) == TRUE) {
	x1 <- as.numeric(as.vector(hld[i+1,1]))
	x2 <- as.numeric(as.vector(hld[i,1]))
	xa <- x1/2 - x2
	xh <- x1/2 + x2
	g1 <- subset(hld, y == i)
	g2 <- subset(hld, y == i+1)
	th1 <- cbind(as.matrix(g1), xa)
	th2 <- cbind(as.matrix(g2), xh)
	th <- rbind(th1,th2)
	if (p == 0) {hldVEG <- th} else {hldVEG <- rbind(hldVEG, th)}
}}
	i=i+2
	p=p+1
}
h1 <- hldVEG[,2:3]
h2 <- hldVEG[,5]
hldVEG <- cbind(h1,h2)
colnames(hldVEG) <- as.list(c("TEAM", "H/A", "SPREAD"))
hldVEG<- subset(hldVEG, hldVEG[,1] != "NA")
}



#####################	GET WEATHER	###########
########GetWthr<-function(x #url)#########

GetWeather <- function(x){
url <- x
doc <- htmlParse(url, useInternal = TRUE)

t1 <- sapply(getNodeSet(doc, "//tr//td[@class='team-name text-center']//a"), function(x) xmlValue(x))
t2 <- sapply(getNodeSet(doc, "//tr//td[@class='team-name  text-center']//a"), function(x) xmlValue(x))
w1 <- sapply(getNodeSet(doc, "//tr//td[@class='text-center']"), function(x) xmlValue(x))

hld <- cbind("Away", "Home", "Forecast", "Wind")
i <- 1
x <- 4
while(i<20){
w2 <- cbind(t1[i], t2[i], w1[x], w1[x+1])
hld <- rbind(hld, w2)
i <- i + 1
x <- x + 5
}
hld[, 3]<-str_trim(hld[, 3])

x1<- hld[,1]
f1<- hld[,3:4]
x2<- hld[,2]
x4<- cbind(x2,f1)
x3<- cbind(x1,f1)
x3 <- subset(x3 , x3 [,1] != "Away")
x4 <- subset(x4 , x4 [,1] != "Home")
x5<- rbind(x3,x4)
colnames(x5) <- as.list(c("TEAM", "FORECAST", "WIND"))
x5<-x5
}

#####################	ROTOCURVE TD PROJ	#########
######GetRCTD<-function(x)########

GetRCTD<-function(x){
url <- x
doc <- htmlTreeParse(url, useInternalNodes = TRUE)
aqh <- getNodeSet(doc, "//iframe")

###PULL URL FOR TD PROJECTION TABLE STORED IN HTML###
a2 <- sapply(aqh, function(x) xmlGetAttr(x, "src"))
url <- a2
####GOOGLESHEETS DEMO#####
gc <- gs_url(url,visibility="public")
x <- gc$ws$row_extent[2]
y <- gc$ws$col_extent[2]
gtbl <- gs_read(gc, ws=2, range=cell_limits(c(1,1), c(x-1, y-1)))
###MAKE IT FIT###
hld <- as.matrix(gtbl)
x1<- hld[,2:3]
x2<- hld[,6]
x3<- cbind(x1,x2)
colnames(x3) <- as.list(c("PLAYER", "POSITION", "RC/TDPROJ"))
x3<-x3
}

##############################POINTS AGAINST POSITION############################

#x = #QB "http://games.espn.go.com/ffl/pointsagainst?positionId=1"
	#RB "http://games.espn.go.com/ffl/pointsagainst?positionId=2"
	#WR "http://games.espn.go.com/ffl/pointsagainst?positionId=3"
	#TE "http://games.espn.go.com/ffl/pointsagainst?positionId=4"
	#DST "http://games.espn.go.com/ffl/pointsagainst?positionId=16"

GetPAP<-function(x){
url <- paste("http://games.espn.go.com/ffl/pointsagainst?positionId=", x, sep="")
doc <- htmlTreeParse(url, useInternal = TRUE)
tms <- sapply(getNodeSet(doc, "//a[@instance='_ppc']"), function(x) xmlValue(x))
pts <- sapply(getNodeSet(doc, "//td[@class='playertableStat appliedPoints']"), function(x) xmlValue(x))
pts <- pts[2:length(pts)]
x1 <- cbind(tms, pts)

pnms <- c()
lst <- length(x1[,1])
i <- 1
while (i<lst+1) {
x3 <- str_length(x1[i,1])
x2 <- str_sub(x1[i,1], 1, x3-7)
x2 <- str_trim(x2)
x4 <- str_sub(x1[i,1], x3-2, x3)
x4 <- str_trim(x4)
x5 <- cbind(x2, x4, x1[i,2])
pnms <- rbind(pnms, x5)
i <- i+1
colnames(pnms) <- as.list(c("OPPONENT", "POSITION", "VSPTS"))
}
pnms <- pnms
}


########		FANTASY PROS RANKINGS			##########################

GetFP<-function(x, a, b){
url1<-"http://www.fantasypros.com/nfl/projections/"
url2<-".php"
url<- paste(url1,x,url2, sep="")
doc <- htmlParse(url, useInternal = TRUE)

hld <- NULL
aqh <- sapply(getNodeSet(doc, "//th"), function(x) xmlValue(x))
hld <- rbind(aqh[5:a])#HEADER CHANGE
aqb1 <- sapply(getNodeSet(doc, "//tr[@class]//*[name()='td']"), function(x) xmlValue(x))
aqb2 <- sapply(getNodeSet(doc, "//tr[@class]//*[name()='td']//div"), function(x) xmlValue(x))

df <- data.frame(aqb2) #Convert High/Low to frame for seperation
y <- as.numeric(rownames(df)) #Make Row #s	
df <- cbind(df, y)	#Bind row numbers
df.hi <- subset(df, y %% 2 == 0) #SEPERATE BY EVEN
df.lo <- subset(df, y %% 2 != 0) #SEPERATE BY ODD
df.hi <- df.hi[,1] #drop rownum
df.lo <- df.lo[,1] #drop rownum

####match to character###
df.hi <- data.frame(lapply(df.hi, as.character), stringsAsFactors=FALSE)
df.hi <- unlist(df.hi)
df.lo <- data.frame(lapply(df.lo, as.character), stringsAsFactors=FALSE)
df.lo <- unlist(df.lo)

HA <- cbind(toupper(x))
lr<- length(aqb1)
y <- NULL
i <- NULL
i <- b #INTERVAL COUNT
y <- 1
while(i<lr){
hld <- rbind(hld, aqb1[y:i])
hld <- rbind(hld, df.hi[y:i])
hld <- rbind(hld, df.lo[y:i])
y = i + 1
i = i + b #INTERVAL CONTROL
}

hld <- subset(hld, hld[,1] != "Player" & hld[,1] != "low" & hld[,1] != "high")
x1 <- hld
pnms <- c()
lst <- length(x1[,1])
i <- 1
while (i<lst+1) {
x3 <- str_length(x1[i,1])
x2 <- str_sub(x1[i,1], 1, x3-4)
x2 <- str_trim(x2)
pnms <- rbind(pnms, x2)
i <- i+1
}
x1 <- cbind(pnms, hld[,b])
hld <- x1
hld<-cbind(hld, HA[,1])
colnames(hld) <- as.list(c("PLAYER", "FP-PROPTS", "POSITION"))
hld<-hld
}


############	TEAM DRIVE STATS	###################

GetDRST<-function(x){
x<-"http://www.pro-football-reference.com/years/2015/"
url <- x
doc <- htmlParse(url, useInternal = TRUE)
aqh <- sapply(getNodeSet(doc, "//tr[@class]//*[name()='th']"), function(x) xmlValue(x))
aqh <- sapply(getNodeSet(doc, "//tr//th"), function(x) xmlValue(x))
a1<-length(aqh)
a2<-a1-11
aqh <- aqh[a2:a1] 
aqb1 <- sapply(getNodeSet(doc, "//tr[@class]//*[name()='td']"), function(x) xmlValue(x))
la<- length(aqb1)
aqb1<-aqb1[(la-395):(la)]
y <- NULL
lstrw <- length(aqb1)
i <- NULL
hld <- NULL
hld <- rbind(aqh) 
i <- 12
y <- 1
while(i<lstrw+1){
hld <- rbind(hld, aqb1[y:i])
y = i + 1
i = i + 12
}

hld <- subset(hld , hld [,1] != "Rk")
x1 <- hld[,2]
x2 <- hld[,4:12]
hld <- cbind(x1,x2)
colnames(hld) <- as.list(c("TEAM", "DR/DRVS", "DR/PLAYS", "DR/SCR%",
"DR/TO%", "DR/PLYpDR", "DR/YDSpDR", "DR/STRT", "DR/TIME", "DR/PTS"))
hld<-hld
}


###########	ESPN GO STATS	#####################

GetGOSTAT<-function(x, a, b, p){
x1 <- 1
hld<-NULL

while(x1<p+1){
if(x1==1){
url1<-"http://espn.go.com/nfl/statistics/player/_/stat/"
url2<-"/qualified/false"
url<- paste(url1,x,url2,sep="")
doc <- htmlParse(url, useInternal = TRUE)
aqh <- sapply(getNodeSet(doc, "//td"), function(x) xmlValue(x))
aqh1 <- aqh[1:a] 
lr<-length(aqh)
aqh2 <- aqh[b:lr]
hld<-rbind(aqh1)
} else {
x1<-x1-1
x2<-(x1*40)+1
url1<-"http://espn.go.com/nfl/statistics/player/_/stat/"
url2<-"/sort/"
url3<-"Yards/qualified/false/count/"
url<- paste(url1,x,url2,x,url3,x2,sep="")
doc <- htmlParse(url, useInternal = TRUE)
aqh <- sapply(getNodeSet(doc, "//td"), function(x) xmlValue(x))
lr<-length(aqh)
aqh2 <- aqh[b:lr]
x1<-x1+1
}

i<-1
y<-a
lr<-length(aqh)
while(i<lr){
	hld<-rbind(hld, aqh2[i:y])
	i<-i+a
	y<-i+a
}
x1<-x1+1
}
hld<-hld
}


######### TUNE UP ESPN OUTPUTS############

FixGOST<-function(b){
b<- subset(b, b[,2] != "PLAYER")
b<- subset(b, b[,2] != "NA")
lst<-length(b[,2])
i<-1
while(i<lst+1){
x1<-str_length(b[i,2])
x2<-str_sub(b[i,2], 1, x1-2)
x2<-str_trim(x2)
b[i,2]<-x2
b<-gsub(",", "", b)  
i<-i+1
}
b<-b
}

FixGOTM<-function(x){
lst<-length(x[,2])
i<-1
hld<-x
	while(i<lst+1){
		if (str_length(hld[i,3])>4){
			x1<-str_length(hld[i,3])
			x2<-str_sub(hld[i,3], x1-2, x1)
			hld[i,3]<-as.character(x2)
		}
	i<-i+1
	}
hld<-hld
}

###################OPPONENT FIND######################
##########BASED ON DONBEST VEGAS ODDS AVAILABILITY####

FindOPP<-function(x,b){
lst1<-length(x[,1])
lst2<-length(b[,1])
colno<-length(unique(x)) 
i<-1
j<-1
hld1<-as.data.frame(x)
hld2<-as.matrix(b)
hld1 <- cbind(hld1, "NA")
names(hld1)[colno+1]<-"OPPONENT"
hld1<-as.matrix(hld1)
	while(i<lst1+1){
		tm<-as.character(hld1[i,c("TEAM")])
		while(j<lst2+1){
		c2<-as.character(hld2[j,1])
			if (tm == c2){
				if(hld2[j,2] == "HOME"){
					c1<-as.character(hld2[j+1, 1])
					hld1[i,colno+1] <- c1
				} else {
					c1<-as.character(hld2[j-1, 1])
					hld1[i,colno+1] <- c1
				}
			j<-j+1
			} else {
			j<-j+1
			}
		}
		i<-i+1
		j<-1
	}
hld1<-hld1
}


###########################BC DST FIX######################
BCDFIX<-function(x){
i<-1
lst<-length(x[,1])
colno<-length(unique(x)) 
hld1 <- x
hld1 <- cbind(hld1, "NA")
names(hld1)[colno+1]<-"BC/TIER"
hld1<-as.matrix(hld1)
while(i<lst+1){
	c1<-as.character(hld1[i,c("BC/TIER.y")])
	c2<-as.character(hld1[i,c("BC/TIER.x")])
	if(is.na(c1) == FALSE){
		hld1[i,colno+1]<-c1
	} else {
		hld1[i,colno+1]<-c2
	}	
	i<-i+1
}
hlda<-hld1[,1:(colno-2)]
hldb<-hld1[,colno+1]
hldc<-cbind(hlda,hldb)
hldc<-as.data.frame(hldc)
names(hldc)[colno-1]<-"BC/TIER"
hldc<-as.matrix(hldc)
}


#######################DRIVE STATES MERGING FOR OPPONENT & TEAM#################
x1<-MrgDRSTATS(tdr, x1)
x<-tdr
b<-x1
MrgDRSTATS<-function(x,b){
b<-x1
x<-tdr
##DISECT DRIVE STATS##
hlda<-x[,1:4]
hldb<-x[,6:7]
hldc<-x[,9:10]
xt<-cbind(hlda,hldb,hldc)
colnames(xt)<-as.list(c("TEAM", "DR/DRIVES", "DR/PLAYS", "DR/SCORE%", "DR/PLAYSperDRIVE",
"DR/YARDSperDRIVE", "AVG DRIVE TIME", "AVG DRIVE POINTS"))
xo<-cbind(hlda,hldb,hldc)
colnames(xo)<-as.list(c("OPPONENT", "OPP/DR/DRIVES", "OPP/DR/PLAYS", "OPP/DR/SCORE%", 
"OPP/DR/PLAYSperDRIVE", "OPP/DR/YARDSperDRIVE", "OPP/AVG DRIVE TIME", "OPP/AVG DRIVE POINTS"))
b[1:10,]
xt[1:10,]
b<-merge(b, xt, by=c("TEAM"), all = TRUE)
b<-merge(b, xo, by=c("OPPONENT"), all = TRUE)
}


####################SAVE FOR LOAD############################
save(GetSRC, FixBC, GetVegas, GetWeather, GetRCTD, GetPAP, GetFP, GetDRST, GetGOSTAT, FixGOST,
FindOPP, FixGOTM, MrgDRSTATS, BCDFIX,
file = "FUNCTS.Rdata")
























