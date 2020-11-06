#### FORMAT DATA ####

mammals <- read.csv('C:/Users/dnnin/Google Drive/Survey Data/Mammals/lozon_mammals.csv',header=TRUE)
mammals$Month <- as.factor(mammals$Month)
mammals$Year <- as.factor(mammals$Year)
mammals$SurveyDay <- as.factor(mammals$SurveyDay)
mammals$Row <- as.factor(mammals$Row)
mammals$Trapline <- as.factor(mammals$Trapline)
#mammals$Species <- as.factor(mammals$Species)


#### CPUE ####
target <- c('PERMAN','PERSPP','PERLEU','MV','STS','TAMSTR','CLEAR','ZAPHUD','FROG','SOSP')
critters <- c('PERMAN','PERSPP','PERLEU','MV','STS','TAMSTR','ZAPHUD') # seven total species over the four years

mammals$TrapNight <- ifelse(mammals$Species %in% target,1,0.5)
mammals$Capture <- ifelse(mammals$Species %in% critters,1,0)
trapnights <- aggregate(mammals$TrapNight,by=list(mammals$Month,mammals$Year,mammals$Trapline),FUN=sum)
colnames(trapnights) <- c('Month','Year','Trapline','TrapNights')

captures <- aggregate(mammals$Capture,by=list(mammals$Month,mammals$Year,mammals$Trapline),FUN=sum)
colnames(captures) <- c('Month','Year','Trapline','Captures')
mammal_cpue <- cbind(trapnights,captures[,4])
colnames(mammal_cpue)[5] <- 'Captures'
mammal_cpue$CPUE <- 100 * round(mammal_cpue$Captures / mammal_cpue$TrapNights,3)
mammal_cpue <- mammal_cpue[with(mammal_cpue,order(Year,Month,Trapline)),]

ggplot(mammal_cpue,aes(x=Month,y=CPUE,col=Year,group=Year)) + 
  geom_point() + geom_line() + facet_wrap(~Trapline) + 
  scale_x_discrete(labels= c('May','Jun','Jul','Aug','Sep')) +
  ylab('Number of captures per 100 trap nights')


#### RELATIVE ABUNDANCE ####

# I want to know the number of unique small mammal species (critters is a vector of all species 
# trapped in four years) for each trapline (1-12) for each month visited between 2017 and 2020

# I created a function to make the for loop a bit more tidy... and the function does work.

SMRA <- function(y,m,t){ 
  length(which(unique(mammals$Species[mammals$Year == y 
                                      & mammals$Month == m 
                                      & mammals$Trapline == t] %in% critters)))}


# I think my problem is I don't know how to index the empty vector for storing values..

RA <- numeric(240)    # 4 years * 5 months * 12 traplines (see below)
year <- c(2017:2020)
month <- c(5:9)       # May to September
trapline <- c(1:12)

for(d in 1:length(RA)){
for(i in 1:length(year)){
  for(j in 1:length(month)){
    for(k in 1:length(trapline)){
      
      RA[d] <- SMRA(year[i],month[j],trapline[k])
    }
  }
}
}
