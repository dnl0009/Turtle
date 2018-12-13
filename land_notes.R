####### LANDSCAPE COMPARISONS #########

#### datamod_notes.R is a complement to this RScript ####

# SET THE STAGE #
library(foreign)
setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Landscape/") # For work computer
# For laptop setwd("G:/My Drive/Survey Data/_THESIS/Landscape/")
RP_landuse <- read.dbf("RP_1km.dbf") # read in files
FPR3_ls <- read.dbf("FPR3_1km.dbf")
FPR3_ls$PERCENT_LA <- (FPR3_ls$PERCENT_LA)*100 # I forgot to do this in GIS
#######################################

PLUS <- cbind(RP_landuse[,c(1,4)],FPR3_ls[,4]); PLUS   ## Bind percent values as one
## Don't forget the value column because this will be needed for merging other data together
##### Make sure the value columns are the EXACT SAME as well
colnames(PLUS) <- c("VALUE","RP","FPR_3") 
class(PLUS) # Matrix columns cannot be called using $
PLUS <- as.data.frame(PLUS,row.names= 
                        c("Forested","Grass/Past/Ag","Barren/Dev",
                          "OpenWater","MineGrass","MineBarren",
                          "SMCRAForest","PreSMCRAGrass","PreSMCRABarren",
                          "PreSMCRAForested","HerbaceousWetland",
                          "WoodyWetland","Road")) # Landscape types per row 
PLUS$RP <- round(PLUS$RP,3) # Round values
PLUS$FPR_3 <- round(PLUS$FPR_3,3)
PLUS # TADA!

##### READ IN ALL THE DBF?! ######
myFiles <- list.files(pattern="*.dbf$") # the $ is important because there 
## are some files that end in .dbf.xml and read.dbf can't read those
ldf <- {}
for (k in 1:length(myFiles)) {
  ldf[[k]] <- read.dbf(myFiles[k])
} # this brings all files in as one object with k values
# Separate files.. I bet I can loop this somehow... 
Anderson <- ldf[[1]]
Born1 <- ldf[[1+1]]
Born2L <- ldf[[2+1]]
Born3L <- ldf[[3+1]]
Born4L <- ldf[[4+1]]
Farm1L <- ldf[[5+1]]
Farm2L <- ldf[[6+1]]
Farm3L <- ldf[[7+1]]
Farm4L <- ldf[[8+1]]
FPR1L <- ldf[[9+1]]
FPR2L <- ldf[[10+1]]
FPR4L <- ldf[[12+1]]
FPR5L <- ldf[[13+1]]
FPR6L <- ldf[[14+1]]
FPR7L <- ldf[[15+1]]
FPR8L <- ldf[[16+1]]
FPR9L <- ldf[[17+1]]
HPL <- ldf[[18+1]]
JesseL <- ldf[[19+1]]
Keener1L <- ldf[[20+1]]
Keener2L <- ldf[[21+1]]
LaurelL <- ldf[[22+1]]
ReservoirL <- ldf[[23+1]]
RRL <- ldf[[26+1]]
UDC1L <- ldf[[27+1]]
UDC2L <- ldf[[28+1]]
WPL <- ldf[[29+1]]

####################################
## I'll combine the data with similar lengths.

PLUS13 <- cbind(PLUS,FPR1L[,4],FPR2L[,4],FPR4L[,4],FPR5L[,4],FPR6L[,4],FPR7L[,4],FPR9L[,4],RRL[,4],UDC1L[,4]); PLUS13
colnames(PLUS13) <- c("VALUE","RP","FPR3","FPR1","FPR2","FPR4","FPR5","FPR6","FPR7","FPR9","RR","UDC1"); PLUS13
PLUS13[,4:12] <- round(PLUS13[,4:12],3); PLUS13 ## This works

PLUS12 <- cbind(Farm2L[,c(1,4)],HPL[,4],UDC2L[,4],WPL[,4])
colnames(PLUS12) <- c("VALUE","Farm2","HPL","UDC2","WPL")
PLUS12[,2:5] <- round(PLUS12[,2:5],3)
PLUS12

PLUS1 <- merge(PLUS13,PLUS12,by="VALUE",all=TRUE)

PLUS9 <- cbind(Anderson[,c(1,4)],Farm1L[,4],Keener1L[,4],LaurelL[,4],Keener2L[,4],Born1[,4],Born2L[,4],Born3L[,4],Born4L[,4])
colnames(PLUS9) <- c("VALUE","Anderson","Farm1","Keener1","Laurel","Keener2","Born1","Born2","Born3","Born4")
PLUS9[,2:9] <- round(PLUS9[,2:10],3) # Don't forget the ,3; otherwise it'll round to the nearest 1
PLUS9

PLUS1 <- merge(PLUS1,PLUS9,by="VALUE",all=TRUE); PLUS1 # Don't forget the ALL=TRUE
#Otherwise, it'll cut off the data to the shortest dataframes

PLUS10 <- cbind(FPR8L[,c(1,4)],JesseL[,4])
colnames(PLUS10) <- c("VALUE","FPR8","Jesse")
PLUS10[,2:3] <- round(PLUS10[,2:3],3)
PLUS10

PLUS1 <- merge(PLUS1,PLUS10,by="VALUE",all=T); PLUS1

PLUS8 <- cbind(Farm3L[,c(1,4)],Farm4L[,4],ReservoirL[,4])
colnames(PLUS8) <- c("VALUE","Farm3","Farm4","Reservoir")
PLUS8[,2:4] <- round(PLUS8[,2:4],3)
PLUS8

PLUS1 <- merge(PLUS1,PLUS8,by="VALUE",all=T); PLUS1
PLUS <- PLUS1[,c(1,4,5,3,6,7,8,9,26,10,11,2,30,12,15,27,14,16,22,23,24,25,17,19,21,20,18,13,28,29)]; PLUS
colnames(PLUS) <- c("VALUE","FPR1","FPR2","FPR3","FPR4","FPR5","FPR6","FPR7","FPR8","FPR9","RR","RP","Reservoir","UDC1","UDC2","Jesse","Hodges","Wolfe","Anderson","Born1","Born2","Born3","Born4","Keener1","Keener2","Laurel","Farm1","Farm2","Farm3","Farm4")
rownames(PLUS) <- c("Forested","Grass/Past/Ag","Barren/Dev","OpenWater",
                    "MineGrass","MineBarren","SMCRAForest","PreSMCRAGrass",
                    "PreSMCRABarren","PreSMCRAForested","HerbaceousWetland",
                    "WoodyWetland","Road")
PLUS <- PLUS[,2:30] # Don't need the value column anymore 

###################################################################
###################################################################
#################### MOVING ON ####################################
####################  TO THE   ####################################
#################### GRAPHING  ####################################
###################################################################
###################################################################

install.packages("colorRamps") # Make sure package is in quotes
library(colorRamps) # Call package
PLUS <- as.matrix(PLUS) # Need to get rid of the NA values I worked so hard to get -- HA!
PLUS[is.na(PLUS)] <- 0; PLUS # Because the bars cannot be drawn with NA values

plot1 <- barplot(prop.table(PLUS,2),main="Landscape Mosaics within 1 km of Each Site",
                 font.main=4,xlab="Site",ylab="Proportion of Hectares (%)",
                 col=colorRamps::primary.colors(13),las=2,cex.names = 0.8,font.lab=2)
legend("bottomleft",bg="white",legend=c("Forested","Grass/Past/Ag","Barren/Dev",
                                        "OpenWater","MineGrass","MineBarren",
                                        "SMCRAForest","PreSMCRAGrass","PreSMCRABarren",
                                        "PreSMCRAForested","HerbaceousWetland",
                                        "WoodyWetland","Road"),cex=0.75, inset= c(0.05,0.02), fill=colorRamps::primary.colors(13))

## EXACTEMENT! :D
## cex = 0.5, inset = c(0.175,0.01) for laptop 
## I think I have different versions on my different computers.

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

survey <- PLUS[,c(1:3,12:14)] # Pick out sites that have been surveyed for presab comparison
survey <- t(survey) # Flip data frame for adding presab column (for both CP and CS)
## T stands for TRANSPOSE, which means "to trade places"
survey <- as.data.frame(survey[,1:2]) # I think I only want to look at forested and ag 
## These are the primary landscape drivers
## I am making it a dataframe so I can add the third presab column $$$

### Bringing in the presab information ###
rownames(survey) <- NULL
survey$site <- c("FPR_1","FPR_2","FPR_3","RUBY_RUN","RUBY_POND","RESERVOIR"); survey<- survey[,c(3,1,2)]
survey$CSpres <- ifelse(survey$site %in% CSpres,1,0)
survey$CPpres <- ifelse(survey$site %in% CPpres,1,0)

### Occupancy may not be the best way to go about it since these species are fairly common

abundance <- survey[,1:3] # Don't need the occupancy (abundance tells us if they're there or not)
colnames(abundance) <- c("Locality","Forested","Grass/Past/Ag") # Need to be consistent with names for merge
CSabun <- abundanturtle[1:5,c(1,4)] # Splitting abundanturtle
CPabun <-abundanturtle[6:9,c(1,4)]

abundance <- merge(abundance,CSabun,by="Locality",all.x=TRUE,all.y=TRUE)
colnames(abundance) <- c("Locality","Forested","Grass/Past/Ag","CSabun")
abundance <- merge(abundance,CPabun,by="Locality",all.x=TRUE,all.y=TRUE)
colnames(abundance) <- c("Locality","Forested","Grass/Past/Ag","CSabun","CPabun")
abundance[is.na(abundance)] <- 0; abundance

################

# Fragmentation may be a better thing to look at than landscape influences because landscape
# might not have a massive impact on movement in the literature... 

RPLU <- PLUS[13,]
plot2 <- barplot(RPLU,main="Roads within 1 km of Sites",
                 font.main=4,xlab="Site",ylab="Proportion of Hectares (%)", 
                 ylim=c(0,6),col="black",las=2,cex.names = 0.8,font.lab=2)

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

##### TIME TO COMPARE LANDSCAPE BETWEEN DIFFERENT BUFFER SIZES (250, 500, and 1000 m) ######

setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Landscape500/")
fivehundoFiles <- list.files(pattern="*.dbf$")
fhf <- {}
for (k in 1:length(fivehundoFiles)) {
  fhf[[k]] <- read.dbf(fivehundoFiles[k])
}
# H for halvesies

APH <- fhf[[1]]
Born1H <- fhf[[2]]
Born2H <- fhf[[3]]
Born3H <- fhf[[4]]
Born4H <- fhf[[5]]
Farm1H <- fhf[[6]]
Farm2H <- fhf[[7]]
Farm3H <- fhf[[8]]
Farm4H <- fhf[[9]]
FPR1H <- fhf[[10]]
FPR2H <- fhf[[11]]
FPR3H <- fhf[[12]]
FPR4H <- fhf[[13]]
FPR5H <- fhf[[14]]
FPR6H <- fhf[[15]]
FPR7H <- fhf[[16]]
FPR8H <- fhf[[17]]
FPR9H <- fhf[[18]]
HPH <- fhf[[19]]
JesseH <- fhf[[20]]
Keener1H <- fhf[[21]]
Keener2H <- fhf[[22]]
LaurelH <- fhf[[23]]
ReservoirH <- fhf[[24]]
RPH <- fhf[[25]]
RRH <- fhf[[26]]
UDC1H <- fhf[[27]]
UDC2H <- fhf[[28]]
WPH <- fhf[[29]]

############################

setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Landscape250/")
twofiftyFiles <- list.files(pattern="*.dbf$")
tff <- {}
for (k in 1:length(twofiftyFiles)) {
  tff[[k]] <- read.dbf(twofiftyFiles[k])
}

# Q for quarter

APQ <- tff[[1]]
Born1Q <- tff[[2]]
Born2Q <- tff[[3]]
Born3Q <- tff[[4]]
Born4Q <- tff[[5]]
Farm1Q <- tff[[6]]
Farm2Q <- tff[[7]]
Farm3Q <- tff[[8]]
Farm4Q <- tff[[9]]
FPR1Q <- tff[[10]]
FPR2Q <- tff[[11]]
FPR3Q <- tff[[12]]
FPR4Q <- tff[[13]]
FPR5Q <- tff[[14]]
FPR6Q <- tff[[15]]
FPR7Q <- tff[[16]]
FPR8Q <- tff[[17]]
FPR9Q <- tff[[18]]
HPQ <- tff[[19]]
JesseQ <- tff[[20]]
Keener1Q <- tff[[21]]
Keener2Q <- tff[[22]]
LaurelQ <- tff[[23]]
ReservoirQ <- tff[[24]]
RPQ <- tff[[25]]
RRQ <- tff[[26]]
UDC1Q <- tff[[27]]
UDC2Q <- tff[[28]]
WPQ <- tff[[29]]

############################

# Bring a couple together to compare and see how it looks.

UDC1COMPARE <- merge(UDC1Q[,c(1,4)],UDC1H[,c(1,4)],by="VALUE",all=TRUE)
UDC1COMPARE <- merge(UDC1COMPARE,UDC1L[,c(1,4)],by="VALUE",all=TRUE); UDC1COMPARE[is.na(UDC1COMPARE)] <- 0; UDC1COMPARE[,2:4] <- round(UDC1COMPARE[,2:4],3); UDC1COMPARE
colnames(UDC1COMPARE) <- c("VALUE","250M","500M","1000M"); UDC1COMPARE
rownames(UDC1COMPARE) <- c("Forested","Grass/Past/Ag","Barren/Dev","OpenWater",
                    "MineGrass","MineBarren","SMCRAForest","PreSMCRAGrass",
                    "PreSMCRABarren","PreSMCRAForested","HerbaceousWetland",
                    "WoodyWetland","Road")
UDC1COMPARE <- UDC1COMPARE[,2:4] # Don't need value column

UDC1COMPARE <- as.matrix(UDC1COMPARE)

plot3 <- barplot(prop.table(UDC1COMPARE,2),main="Landscape Mosaics within Different Buffer Sizes at UDC1",
                 font.main=4,xlab="Buffer Size",ylab="Proportion of Hectares (%)",
                 col=colorRamps::primary.colors(13),las=1,cex.names = 0.8,font.lab=2) 
# las rotates x labels
legend("bottomleft",bg="white",legend=c("Forested","Grass/Past/Ag","Barren/Dev",
                                        "OpenWater","MineGrass","MineBarren",
                                        "SMCRAForest","PreSMCRAGrass","PreSMCRABarren",
                                        "PreSMCRAForested","HerbaceousWetland",
                                        "WoodyWetland","Road"),cex=0.7, inset= c(0.05,0.02), fill=colorRamps::primary.colors(13))
# cex changes font size

########