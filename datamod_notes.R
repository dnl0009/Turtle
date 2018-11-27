####### SET THE STAGE #########
setwd("C:/Users/Darien Lozon/Desktop/") # Find trapdata18 file
turtle <- read.csv("trapdata18.csv",header=TRUE, na.strings = "") # Bring in file
# This file will be for all traps whether there was anything or not
# This will be important for occupancy
str(turtle)
turtle <- turtle[1:22]; str(turtle) # I don't need the additional comments for analysis

#### Remember when formatting databases that R does not like spaces in titles
colnames(turtle) <- c("Date","Survey_Day","Trap_Size_m","Time_Processed","State","County",
                      "Locality","Trap_Num","Collector_Num","Collector_Name","Recap","Mark",
                      "Genus","Species","Subspecies","Sex","CL_mm","CW_mm","PL_mm","PW_mm",
                      "BD_mm","WT_g");colnames(turtle) # Clean up column titles and check

#### Reformat variables
turtle$Date <- as.Date(turtle$Date, format = "%m/%d/%Y") # Fix Date from Factor to Date
turtle$Survey_Day <- as.factor(turtle$Survey_Day) # Fix Survey_Day from Integer to Factor
turtle$Trap_Num <- as.factor(turtle$Trap_Num) 
turtle$Collector_Num <- as.character(turtle$Collector_Num)

#### Turtle weight looks weird
unique(turtle$WT_g)
which(turtle$WT_g == "-")
turtle$WT_g[6] <- "NA" # Doesn't work because NA isn't a factor label
turtle$WT_g <- as.character(turtle$WT_g) # Weight shouldn't be a factor anyway 
turtle$WT_g[6] <- "NA" # Fix dash input
which(turtle$WT_g == "-") # Check to see change was implemented
BIG <- which(turtle$WT_g == ">10KG") # Look for the big mammy jammies who I couldn't accurately
# measure at the time because I didn't have a big enough Pesola hanging scale o_0
turtle$WT_g[BIG] <- "NA" # Over 10 kg doesn't really help when conducting weight-heavy metal
# comparative analyses/regressions
which(turtle$WT_g == ">10KG") # Double check
turtle$WT_g <- as.integer(turtle$WT_g) # Can now change class to integer like the rest of
# the measurements

## CSpres$WT_g[203] <- 1300 ??


#### Simplify trap size input values ####
turtle$Trap_Size_m <- as.character(turtle$Trap_Size_m) # Need to change for ifelse to work
turtle$Trap_Size_m <- ifelse(turtle$Trap_Size_m == "LRG/0.91M","0.91","0.76") # BOOM
turtle$Trap_Size_m <- as.factor(turtle$Trap_Size_m) # Back to the Factor!
head(turtle)

#### Separated data frames ##### DON'T FORGET COMMAS IN BRACKETS ##############
RR <- turtle[turtle$Locality == "RUBY_RUN", ] # For Ruby Run 
RP <- turtle[turtle$Locality == "RUBY_POND", ] # Ruby Pond
FPR_1 <- turtle[turtle$Locality == "FPR_1", ] # FPR 1
FPR_2 <- turtle[turtle$Locality == "FPR_2", ] # Makes sense
FPR_3 <- turtle[turtle$Locality == "FPR_3", ]

#### RECAPTURE DATA ####
recap <- unique(which(turtle$Recap == "Y")); recap <- turtle[recap,]; recap 


#### Foreshadowing ####
pres <- which(turtle$WT_g > 1) # Check list of rows that have turtles
### Curiosity... 
length(pres) # I'm lazy and don't want to count values
pres <- turtle[pres,] # Create new data frame with all occupied trap locations
CPpres <- unique(pres$Locality[pres$Genus == "Chrysemys"])
CSpres <- unique(pres$Locality[pres$Genus == "Chelydra"])

