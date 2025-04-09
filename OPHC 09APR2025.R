# OPHC 06 APR 2025

library(ggplot2)
library(tidyverse)
library(ggthemes)
library(viridis)
library(usmap)
library(ggplot2)
library(geojsonio)
library(broom)
library(sf)
library(osmdata)
library(dplyr)
library(ggnewscale)
library(data.table)
library(ggpubr)
library(gtools)
library(readxl)
# library(blscrapeR)
library(tictoc)
library(reshape2)
`%!in%` <- Negate(`%in%`)

dat<-read_xlsx('./data/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts-direct-31March2025.xlsx') # this is opioid data
dat<-dat[2:11]
dat2024<-subset(dat,dat$Month==6 & dat$Year==2024)
dat<-subset(dat,dat$Month==12)
dat<-rbind(dat,dat2024)

# imputing 1 since the blank cells have 1-9 overdose deaths. 
colnames(dat)[10]<-'Provisional.Drug.Overdose.Deaths'
dat$Provisional.Drug.Overdose.Deaths[is.na(dat$Provisional.Drug.Overdose.Deaths)] <- 1
dat$Provisional.Drug.Overdose.Deaths<-as.numeric(gsub(",", "", dat$Provisional.Drug.Overdose.Deaths))
dat$COUNTYNAME<-paste0(dat$COUNTYNAME, ' County')
dat$state_county<-paste0(dat$STATE_NAME,'-',dat$COUNTYNAME)

# creating new column names to assist in the wrangling and analysis of the data.
dat$state_county_year<-paste0(dat$STATE_NAME,'-',dat$COUNTYNAME,'-',dat$Year)
dat$Month<-as.numeric(dat$Month)
dat$Month <- sprintf("%02d", as.numeric(dat$Month))
dat$YearMonth<-paste0(dat$Year,'-',dat$Month)
dat$state_county_year_month<-paste0(dat$STATE_NAME,'-',dat$COUNTYNAME,'-',dat$Year,'-',dat$Month)


colnames(dat)[6]<-'fips'

dat$fips <-as.character(dat$fips)
dat$fips <- ifelse(nchar(as.character(dat$fips)) == 4,
                paste0("0", dat$fips),
                as.character(dat$fips))



library(tidycensus)
popdat2020<-get_acs(
    year = 2020,
    survey = "acs5",
    geography = "county",
    # state = "NY",
    variables = c("B01001_001",'S0101_C01_026') # Total population and median age
  )


popdat2020<-reshape2::dcast(data = popdat2020,
                            formula = GEOID ~ variable,
                            value.var = c("estimate"))
popdat2021<-get_acs(
    year = 2021,
    survey = "acs5",
    geography = "county",
    # state = "NY",
    variables = c("B01001_001",'S0101_C01_026') # Total population and median age
  )


popdat2021<-reshape2::dcast(data = popdat2021,
                            formula = GEOID ~ variable,
                            value.var = c("estimate"))

popdat2022<-get_acs(
    year = 2022,
    survey = "acs5",
    geography = "county",
    # state = "NY",
    variables = c("B01001_001",'S0101_C01_026') # Total population and median age
  )


popdat2022<-reshape2::dcast(data = popdat2022,
                            formula = GEOID ~ variable,
                            value.var = c("estimate"))

popdat2023<-get_acs(
    year = 2023,
    survey = "acs5",
    geography = "county",
    # state = "NY",
    variables = c("B01001_001",'S0101_C01_026') # Total population and median age
  )


popdat2023<-reshape2::dcast(data = popdat2023,
                          formula = GEOID ~ variable,
                          value.var = c("estimate"))
popdat2024<-get_acs(
    year = 2023,
    survey = "acs5",
    geography = "county",
    # state = "NY",
    variables = c("B01001_001",'S0101_C01_026') # Total population and median age
  )


popdat2024<-reshape2::dcast(data = popdat2024,
                            formula = GEOID ~ variable,
                            value.var = c("estimate"))

colnames(popdat2020)<-c('fips','Population','pop18plus')
colnames(popdat2021)<-c('fips','Population','pop18plus')
colnames(popdat2022)<-c('fips','Population','pop18plus')
colnames(popdat2023)<-c('fips','Population','pop18plus')
colnames(popdat2024)<-c('fips','Population','pop18plus')
popdat2020$fips_year<-paste0(popdat2020$fips,'-',2020)
popdat2021$fips_year<-paste0(popdat2021$fips,'-',2021)
popdat2022$fips_year<-paste0(popdat2022$fips,'-',2022)
popdat2023$fips_year<-paste0(popdat2023$fips,'-',2023)
popdat2024$fips_year<-paste0(popdat2024$fips,'-',2024)

popdatrbind<-rbind(
  popdat2020,
  popdat2021,
  popdat2022,
  popdat2023,
  popdat2024
)


dat$fips_year<-paste0(dat$fips,'-',dat$Year)

mergedat<-merge(dat,popdatrbind,by="fips_year",all.x = T)

mergedatwith18<-mergedat
# mergedatwith18$pop18plus<-mergedatwith18$Population*mergedatwith18$over18freq
mergedatwith18$deathsper100k<-(100000*(mergedatwith18$Provisional.Drug.Overdose.Deaths/mergedatwith18$pop18plus)) # death per 100k adults
mergedatwith18$pwid<-mergedatwith18$pop18plus * 0.015 # pwid is 1.5% of the adult population according to https://pubmed.ncbi.nlm.nih.gov/35791261/ 
mergedatwith18$injectionsperperson <-508.8 # Taken from https://journals.sagepub.com/doi/10.1177/0022042616679829 
# which cites https://pubmed.ncbi.nlm.nih.gov/25735468/ but I can't find it there. 
#Other places just cite 2-4 times per day which corresponds to 730-1460 times per year
mergedatwith18$totalinjectionseverywhere<-mergedatwith18$pwid * mergedatwith18$injectionsperperson

# copy pasting april 6th
freqsbyall<-seq(0,0.01,0.0025) # note that this was changed to 0-0.25% on Jan 13!!!!
namesbyall<-c(
  'zero',
  'Point twenty-five',
  'Point five',
  'Point seventy-five',
  'One'
)

# determining percent of total injections that could happen at OPHC
for (k in 1:length(freqsbyall)) {
  freq<-freqsbyall[k]
  newcolname<-namesbyall[k]
  mergedatwith18$newcol<-mergedatwith18$totalinjectionseverywhere*freq
  colnames(mergedatwith18)[length(colnames(mergedatwith18))]<-newcolname
}
# mergedatwith18$fips<-as.character(mergedatwith18$FIPS)
# melt the data
mergedatwith18melt<-melt(mergedatwith18)
# subset the data to make sure the only values there are number of injections corresponding to a percent for injections at OPHC
mergedatwith18meltpercentinjections<-subset(mergedatwith18melt,mergedatwith18melt$variable %in% namesbyall) 
# rename columns 
colnames(mergedatwith18meltpercentinjections)[which(colnames(mergedatwith18meltpercentinjections)=='value')]<-'injectionsatophc'
colnames(mergedatwith18meltpercentinjections)[which(colnames(mergedatwith18meltpercentinjections)=='variable')]<-'writtenpercents'

# add frequnecies to df that match the writen percents
howmanytorepeat<-dim(mergedatwith18meltpercentinjections)[1]/length(freqsbyall)
newfreqcol<-rep(freqsbyall, each=howmanytorepeat)
mergedatwith18meltpercentinjections$freqinjections<-newfreqcol



#####paste next block
# start subsetting the melted dat for columns we want to keep in our finalmerge data frame
mergedatwith18meltdeaths<-subset(mergedatwith18melt,mergedatwith18melt$variable=='Provisional.Drug.Overdose.Deaths')
colnames(mergedatwith18meltdeaths)[dim(mergedatwith18meltdeaths)[2]]<-'Provisional.Drug.Overdose.Deaths'
mergedatwith18meltdeaths<-mergedatwith18meltdeaths[-(dim(mergedatwith18meltdeaths)[2]-1)]

mergedatwith18meltpwid<-subset(mergedatwith18melt,mergedatwith18melt$variable=='pwid')
colnames(mergedatwith18meltpwid)[dim(mergedatwith18meltpwid)[2]]<-'pwid'
mergedatwith18meltpwid<-mergedatwith18meltpwid[-(dim(mergedatwith18meltpwid)[2]-1)]

mergedatwith18meltpopulation<-subset(mergedatwith18melt,mergedatwith18melt$variable=='Population')
colnames(mergedatwith18meltpopulation)[dim(mergedatwith18meltpopulation)[2]]<-'Population'
mergedatwith18meltpopulation<-mergedatwith18meltpopulation[-(dim(mergedatwith18meltpopulation)[2]-1)]

mergedatwith18meltdeathsper100k<-subset(mergedatwith18melt,mergedatwith18melt$variable=='deathsper100k')
colnames(mergedatwith18meltdeathsper100k)[dim(mergedatwith18meltdeathsper100k)[2]]<-'deathsper100k'
mergedatwith18meltdeathsper100k<-mergedatwith18meltdeathsper100k[-(dim(mergedatwith18meltdeathsper100k)[2]-1)]

mergedatwith18meltFrequency18plus<-subset(mergedatwith18melt,mergedatwith18melt$variable=='Frequency18plus')
colnames(mergedatwith18meltFrequency18plus)[dim(mergedatwith18meltFrequency18plus)[2]]<-'Frequency18plus'
mergedatwith18meltFrequency18plus<-mergedatwith18meltFrequency18plus[-(dim(mergedatwith18meltFrequency18plus)[2]-1)]

mergedatwith18meltinjectionsperperson<-subset(mergedatwith18melt,mergedatwith18melt$variable=='injectionsperperson')
colnames(mergedatwith18meltinjectionsperperson)[dim(mergedatwith18meltinjectionsperperson)[2]]<-'injectionsperperson'
mergedatwith18meltinjectionsperperson<-mergedatwith18meltinjectionsperperson[-(dim(mergedatwith18meltinjectionsperperson)[2]-1)]

mergedatwith18melttotalinjectionseverywhere<-subset(mergedatwith18melt,mergedatwith18melt$variable=='totalinjectionseverywhere')
colnames(mergedatwith18melttotalinjectionseverywhere)[dim(mergedatwith18melttotalinjectionseverywhere)[2]]<-'totalinjectionseverywhere'
mergedatwith18melttotalinjectionseverywhere<-mergedatwith18melttotalinjectionseverywhere[-(dim(mergedatwith18melttotalinjectionseverywhere)[2]-1)]

mergedatwith18meltpop18plus<-subset(mergedatwith18melt,mergedatwith18melt$variable=='pop18plus')
colnames(mergedatwith18meltpop18plus)[dim(mergedatwith18meltpop18plus)[2]]<-'pop18plus'
mergedatwith18meltpop18plus<-mergedatwith18meltpop18plus[-(dim(mergedatwith18meltpop18plus)[2]-1)]

mergedatwith18meltYear<-subset(mergedatwith18melt,mergedatwith18melt$variable=='Year')
colnames(mergedatwith18meltYear)[dim(mergedatwith18meltYear)[2]]<-'Year'
mergedatwith18meltYear<-mergedatwith18meltYear[-(dim(mergedatwith18meltYear)[2]-1)]

### paste next block

# cbind them all together back into one dataframe
finalmerge<-cbind(mergedatwith18meltdeaths,
                  mergedatwith18meltpercentinjections,
                  mergedatwith18meltpwid,
                  mergedatwith18meltdeathsper100k,
                  mergedatwith18meltpopulation,
                  # mergedatwith18meltpopulationPercent18plus,
                  # mergedatwith18meltFrequency18plus,
                  mergedatwith18meltinjectionsperperson,
                  mergedatwith18melttotalinjectionseverywhere,
                  mergedatwith18meltpop18plus,
                  mergedatwith18meltYear)




dupcolumns<-which(duplicated(colnames(finalmerge)) | colnames(finalmerge)=='STNAME' | colnames(finalmerge)=='CTYNAME')
finalmerge<-finalmerge[-dupcolumns]


# total analysis begins, paste next block
# calculate livessaved
finalmerge$livessaved<-((finalmerge$injectionsatophc*finalmerge$Provisional.Drug.Overdose.Deaths)/
                          (finalmerge$pwid*finalmerge$injectionsperperson))

# creating new column for grouping for the cumulative functions
finalmerge$countywrittenpercent <-
  paste0(finalmerge$STATE_NAME,'.',finalmerge$COUNTYNAME,'.',finalmerge$writtenpercents)

# make cumulative lives saved column. The cumulative is per percent and per county. 2022 is the cumulative saved from the beginning of the dataset (2020) through the end of the dataset (2022), for example.
finalmerge<-finalmerge[order(finalmerge$YearMonth,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativelivessaved <- ave(finalmerge$livessaved, finalmerge$countywrittenpercent, FUN=cumsum)

# make cumulative deaths. The cumulative is per percent and per county. 2022 is the cumulative saved from the beginning of the dataset (2020) through the end of the dataset (2022), for example.
finalmerge<-finalmerge[order(finalmerge$YearMonth,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativedeaths <- ave(finalmerge$Provisional.Drug.Overdose.Deaths, finalmerge$countywrittenpercent, FUN=cumsum)



# continue paste
## Human immunodeficiency virus
# HIV incidences per person years taken from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9278993/
finalmerge$newHIVlowrisk<-finalmerge$pwid*2/10000 # estimated incidence of low risk HIV
finalmerge$newHIVmediumrisk<-finalmerge$pwid*4.6/10000 # estimated incidence of medium risk HIV
finalmerge$newHIVhighrisk<-finalmerge$pwid*5.6/10000 # estimated incidence of high risk HIV

# making cumulative HIV stats
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionlow <- ave(finalmerge$newHIVlowrisk, finalmerge$countywrittenpercent, FUN=cumsum)

finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionmed <- ave(finalmerge$newHIVmediumrisk, finalmerge$countywrittenpercent, FUN=cumsum)

finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionhigh <- ave(finalmerge$newHIVhighrisk, finalmerge$countywrittenpercent, FUN=cumsum)

# new HIV transmissions averted
finalmerge$HIVavertedhighrisk<-finalmerge$newHIVhighrisk * (finalmerge$freqinjections)
finalmerge$HIVavertedmedrisk<-finalmerge$newHIVmediumrisk * (finalmerge$freqinjections)
finalmerge$HIVavertedlowrisk<-finalmerge$newHIVlowrisk * (finalmerge$freqinjections)

# making cumulative HIV transmissions averted columns
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionlowaverted <- ave(finalmerge$HIVavertedlowrisk, finalmerge$countywrittenpercent, FUN=cumsum)
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionmedaverted <- ave(finalmerge$HIVavertedmedrisk, finalmerge$countywrittenpercent, FUN=cumsum)
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHIVinfectionhighaverted <- ave(finalmerge$HIVavertedhighrisk, finalmerge$countywrittenpercent, FUN=cumsum)
# discounted lifetime cost of HIV taken from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4359630/
finalmerge$HIVdiscountedcostsavingshigh<-finalmerge$cumulativeHIVinfectionhighaverted*326500
finalmerge$HIVdiscountedcostsavingsmed<-finalmerge$cumulativeHIVinfectionmedaverted*326500
finalmerge$HIVdiscountedcostsavingslow<-finalmerge$cumulativeHIVinfectionlowaverted*326500
## Hepatitis C Virus transmissions
# HCV incidences
finalmerge$newHCVrisk<-finalmerge$pwid*10/100 # estimated incidence of HCV, taken from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4589282/ cited this article: https://pubmed.ncbi.nlm.nih.gov/17854721/

#making cumulative HCV stats
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHCVinfection <- ave(finalmerge$newHCVrisk, finalmerge$countywrittenpercent, FUN=cumsum)

# new HCV transmissions averted
finalmerge$HCVavertedrisk<-finalmerge$newHCVrisk * (finalmerge$freqinjections)

# cumulative HCV transmissions averted
finalmerge<-finalmerge[order(finalmerge$Year,finalmerge$countywrittenpercent, decreasing = FALSE), ]   
finalmerge$cumulativeHCVinfectionaverted <- ave(finalmerge$HCVavertedrisk, finalmerge$countywrittenpercent, FUN=cumsum)

# discounted lifetime cost of HCV taken from https://journals.sagepub.com/doi/pdf/10.1177/2381468316671946
# according to https://www.bls.gov/data/inflation_calculator.htm, adjusted for inflation from January 2016 to January 2023, 
# the lifetime discounted cost is $320,238
finalmerge$HCVdiscountedcost<-finalmerge$cumulativeHCVinfectionaverted*320328


# paste next block
# cost of infectious diseases over time using HIV medium
finalmerge$infectiousdiscountedcost<-finalmerge$HIVdiscountedcostsavingsmed + finalmerge$HCVdiscountedcost

# value of a statistical deaths averted 
# value of statistical life taken from FEMA bene-fit cost analysis toolkit 6.0, originally published July 31, 2020
# https://www.fema.gov/sites/default/files/2020-08/fema_bca_toolkit_release-notes-july-2020.pdf
finalmerge$valueoflivessaved<-finalmerge$cumulativelivessaved*7500000

# infectious discounted costs and values of statistical lives added
finalmerge$totalsavings<-finalmerge$infectiousdiscountedcost + finalmerge$valueoflivessaved


finalmerge$fips<-finalmerge$fips.x
finalmergesub<-subset(finalmerge,finalmerge$YearMonth=='2024-06' & finalmerge$writtenpercents=='Point twenty-five')
finalmergesubtwopointfive<-subset(finalmergesub,finalmergesub$writtenpercents=='Point twenty-five')

# cumulative deaths regardless of OPHCS:
# 0.25% of injections at OPHC
cat('cumulative deaths regardless of OPHCS:')
sum(finalmergesubtwopointfive$cumulativedeaths,na.rm = T)
sum(dat$Provisional.Drug.Overdose.Deaths,na.rm = T) # this is the other way of calculating it to check to make sure our math was correct.

# 0.25% of injections at OPHC
cat('Cumulative deaths averted if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$cumulativelivessaved,na.rm = T)

cat('Cumulative HCV infections averted if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$cumulativeHCVinfectionaverted,na.rm = T)

cat('Cumulative HIV infections (low risk) averted if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$cumulativeHIVinfectionlowaverted,na.rm = T)

cat('Cumulative HIV infections (medium risk) averted if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$cumulativeHIVinfectionmedaverted,na.rm = T)

cat('Cumulative HIV infections (high risk) averted if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$cumulativeHIVinfectionhighaverted,na.rm = T)

cat('Cumulative discounted infectious disease costs if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$infectiousdiscountedcost,na.rm = T)

cat('Cumulative statistical cost of life of if 0.25% of injections were to occur at the OPHC:')
sum(finalmergesubtwopointfive$valueoflivessaved,na.rm = T)

# 0.5% of injections at OPHC
finalmergesub<-subset(finalmerge,finalmerge$YearMonth=='2024-06' & finalmerge$writtenpercents=='Point five')
finalmergesubfive<-subset(finalmergesub,finalmergesub$writtenpercents=='Point five')
cat('Cumulative deaths averted if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$cumulativelivessaved,na.rm = T)

cat('Cumulative HCV infections averted if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$cumulativeHCVinfectionaverted,na.rm = T)

cat('Cumulative HIV infections (low risk) averted if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$cumulativeHIVinfectionlowaverted,na.rm = T)

cat('Cumulative HIV infections (medium risk) averted if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$cumulativeHIVinfectionmedaverted,na.rm = T)

cat('Cumulative HIV infections (high risk) averted if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$cumulativeHIVinfectionhighaverted,na.rm = T)

cat('Cumulative discounted infectious disease costs if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$infectiousdiscountedcost,na.rm = T)

cat('Cumulative statistical cost of life of if 0.5% of injections were to occur at the OPHC:')
sum(finalmergesubfive$valueoflivessaved,na.rm = T)

# 0.75% of injections at OPHC
finalmergesub<-subset(finalmerge,finalmerge$YearMonth=='2024-06' & finalmerge$writtenpercents=='Point seventy-five')
finalmergesubsevenpointfive<-subset(finalmergesub,finalmergesub$writtenpercents=='Point seventy-five')
cat('Cumulative deaths averted if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$cumulativelivessaved,na.rm = T)

cat('Cumulative HCV infections averted if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$cumulativeHCVinfectionaverted,na.rm = T)

cat('Cumulative HIV infections (low risk) averted if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$cumulativeHIVinfectionlowaverted,na.rm = T)

cat('Cumulative HIV infections (medium risk) averted if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$cumulativeHIVinfectionmedaverted,na.rm = T)

cat('Cumulative HIV infections (high risk) averted if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$cumulativeHIVinfectionhighaverted,na.rm = T)

cat('Cumulative discounted infectious disease costs if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$infectiousdiscountedcost,na.rm = T)

cat('Cumulative statistical cost of life of if 0.75% of injections were to occur at the OPHC:')
sum(finalmergesubsevenpointfive$valueoflivessaved,na.rm = T)

# 1% of injections at OPHC
finalmergesub<-subset(finalmerge,finalmerge$YearMonth=='2024-06' & finalmerge$writtenpercents=='One')
finalmergesubten<-subset(finalmergesub,finalmergesub$writtenpercents=='One')
cat('Cumulative deaths averted if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$cumulativelivessaved,na.rm = T)

cat('Cumulative HCV infections averted if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$cumulativeHCVinfectionaverted,na.rm = T)

cat('Cumulative HIV infections (low risk) averted if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$cumulativeHIVinfectionlowaverted,na.rm = T)

cat('Cumulative HIV infections (medium risk) averted if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$cumulativeHIVinfectionmedaverted,na.rm = T)

cat('Cumulative HIV infections (high risk) averted if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$cumulativeHIVinfectionhighaverted,na.rm = T)

cat('Cumulative discounted infectious disease costs if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$infectiousdiscountedcost,na.rm = T)

cat('Cumulative statistical cost of life of if 1% of injections were to occur at the OPHC:')
sum(finalmergesubten$valueoflivessaved,na.rm = T)


#PLOTTING
theme_set(theme(axis.text = element_text(size=8),
                axis.title = element_text(size=8),
                strip.text = element_text(size=8),
                axis.text.x = element_text(angle=0),
                strip.text.x = element_text(size = 8,margin = margin(.1, 0, .1, 0, "cm")),
                legend.text = element_text(size=7),
                legend.title = element_text(size=7),
                legend.position = 'bottom',
                panel.border=element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_line(color='light grey'))
)


us_states<-us_map(
  regions = c("states")
)
us_counties<-us_map(
  regions = c("counties")
)

us_counties$fips
# counties_with_merge<-us_counties |>
#   left_join(finalmergesubtwopointfive$fips, by = join_by(fips))


counties_with_merge<-merge(us_counties,
                           finalmergesubtwopointfive,
                            by = 'fips',
                            all.x = T)

# counties_with_merge$log2cumulativedeathsaverted<-log2(1+counties_with_merge$cumulativelivessaved)
# Get x-axis limits of the bounding box for the state data
xlim_current <- st_bbox(us_states)$xlim

# Add 540ish km (or 10% of the US) to the bounds (thus shifting the window over)
xlim_expanded <- c(
  xlim_current[1] + (0.1 * diff(xlim_current)), 
  xlim_current[2] + (0.1 * diff(xlim_current))
)

ggplot() +
  geom_sf(data = us_states, fill = "#0074D9", color = "white", linewidth = 0.25) +
  coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded)

interior_state_borders <- st_intersection(us_states) |>
  filter(n.overlaps > 1) |> 
  # Remove weird points that st_intersection() adds
  filter(!(st_geometry_type(geom) %in% c("POINT", "MULTIPOINT")))

ggplot() +
  geom_sf(data = us_states, fill = "#0074D9", linewidth = 0) +
  geom_sf(data = interior_state_borders, linewidth = 0.25, color = "white") +
  coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded)




library(viridis)
ggplot() +
  # Add counties filled with unemployment levels
  geom_sf(
    data = counties_with_merge, aes(fill = cumulativelivessaved), linewidth = 0
  ) +
  # Add interior state boundaries
  geom_sf(
    data = interior_state_borders, color = "white", linewidth = 0.25
  ) +
  scale_fill_viridis()+
   coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded) +
  theme(
    panel.background =  element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.86, 0.32),
    legend.direction = "vertical",

  )


library(legendry)

# Convert the county polygons into single points
counties_with_merge_points <- counties_with_merge |> 
  st_point_on_surface()

ggplot() +
  # Use a gray background
  geom_sf(data = us_states, fill = "gray90", linewidth = 0) +
  geom_sf(data = interior_state_borders, linewidth = 0.25, color = "white") +
  # Include semi-transparent points with shape 21 (so there's a border)
  geom_sf(
    data = counties_with_merge_points, 
    aes(size = deathsper100k, fill = cumulativelivessaved), 
    pch = 21, color = "white", stroke = 0.25, alpha = 0.5
  ) +
  scale_size_continuous(
    range = c(1, 9), labels = scales::label_comma(), 
    breaks = c(10000, 100000, 1000000),
    guide = guide_legend(override.aes = list(pch = 19, color = "black"))
  ) +
  scale_fill_viridis()+
  
  
  theme(
    panel.background =  element_blank(),
    legend.position.inside = c(0.86, 0.32),
    legend.direction = "vertical",
    legend.position = "right",
  )+
  scale_size_continuous(
    # range = c(1, 11), labels = scales::label_comma(),
    breaks = c(1, 200),
    guide = guide_circles(
      text_position = "right",
      override.aes = list(
        fill = "grey20", alpha = 0.65
      )
    )
  )





# rm(twopointfivelist)
# saving
twopointfivelist<-list(
  sum(finalmergesubtwopointfive$cumulativelivessaved,na.rm = T),
  sum(finalmergesubtwopointfive$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubtwopointfive$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubtwopointfive$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubtwopointfive$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubtwopointfive$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubtwopointfive$valueoflivessaved,na.rm = T)
)

fivelist<-list(
  sum(finalmergesubfive$cumulativelivessaved,na.rm = T),
  sum(finalmergesubfive$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubfive$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubfive$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubfive$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubfive$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubfive$valueoflivessaved,na.rm = T)
)

sevenpointfivelist<-list(
  sum(finalmergesubsevenpointfive$cumulativelivessaved,na.rm = T),
  sum(finalmergesubsevenpointfive$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubsevenpointfive$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubsevenpointfive$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubsevenpointfive$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubsevenpointfive$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubsevenpointfive$valueoflivessaved,na.rm = T)
)

tenlist<-list(
  sum(finalmergesubten$cumulativelivessaved,na.rm = T),
  sum(finalmergesubten$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubten$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubten$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubten$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubten$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubten$valueoflivessaved,na.rm = T)
)

SummaryDataFrame_twopointfive<-do.call(cbind.data.frame, twopointfivelist)
SummaryDataFrame_five<-do.call(cbind.data.frame, fivelist)
SummaryDataFrame_sevenpointfive<-do.call(cbind.data.frame, sevenpointfivelist)
SummaryDataFrame_ten<-do.call(cbind.data.frame, tenlist)

colnames(SummaryDataFrame_twopointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_five)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_sevenpointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_ten)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

SummaryDataFrame<-rbind(SummaryDataFrame_twopointfive,
                        SummaryDataFrame_five,
                        SummaryDataFrame_sevenpointfive,
                        SummaryDataFrame_ten)

rownames(SummaryDataFrame)<-c(
  '0.25',
  '0.5',
  '0.75',
  '1'
)

SummaryDataFrame$`Total costs`<-SummaryDataFrame$`Discounted lifetime cost infections`+SummaryDataFrame$`Cost of life`

SummaryDataFrame$`Total costs (billions)`<-SummaryDataFrame$`Total costs`/1000000000
write.csv(SummaryDataFrame,'SummaryTable-09Apr2025.csv')

transposedSummaryDataFrame<-as.data.frame(t(SummaryDataFrame))


write.csv(transposedSummaryDataFrame,'transposedSummaryTable-09Apr2025.csv')


####
finalmergesubtwopointfive_top50_pwid<-slice_max(finalmergesubtwopointfive,n = 50,order_by = pwid)
finalmergesubfive_top50_pwid<-slice_max(finalmergesubfive,n = 50,order_by = pwid)
finalmergesubsevenpointfive_top50_pwid<-slice_max(finalmergesubsevenpointfive,n = 50,order_by = pwid)
finalmergesubten_top50_pwid<-slice_max(finalmergesubten,n = 50,order_by = pwid)

top50pwid<-as.data.frame(unique(finalmergesubfive_top50_pwid$state_county))
colnames(top50pwid)<-'Counties with highest number of PWID'
write.csv(top50pwid,'top50pwid-counties-09Apr2025.csv',row.names = F)

twopointfivelist<-list(
  sum(finalmergesubtwopointfive_top50_pwid$cumulativelivessaved,na.rm = T),
  sum(finalmergesubtwopointfive_top50_pwid$cumulativeHCVinfectionaverted),
  sum(finalmergesubtwopointfive_top50_pwid$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_pwid$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_pwid$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_pwid$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubtwopointfive_top50_pwid$valueoflivessaved,na.rm = T)
)

fivelist<-list(
  sum(finalmergesubfive_top50_pwid$cumulativelivessaved,na.rm = T),
  sum(finalmergesubfive_top50_pwid$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubfive_top50_pwid$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubfive_top50_pwid$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubfive_top50_pwid$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubfive_top50_pwid$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubfive_top50_pwid$valueoflivessaved,na.rm = T)
)

sevenpointfivelist<-list(
  sum(finalmergesubsevenpointfive_top50_pwid$cumulativelivessaved,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_pwid$valueoflivessaved,na.rm = T)
)

tenlist<-list(
  sum(finalmergesubten_top50_pwid$cumulativelivessaved,na.rm = T),
  sum(finalmergesubten_top50_pwid$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubten_top50_pwid$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubten_top50_pwid$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubten_top50_pwid$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubten_top50_pwid$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubten_top50_pwid$valueoflivessaved,na.rm = T)
)

SummaryDataFrame_twopointfive<-do.call(cbind.data.frame, twopointfivelist)
SummaryDataFrame_five<-do.call(cbind.data.frame, fivelist)
SummaryDataFrame_sevenpointfive<-do.call(cbind.data.frame, sevenpointfivelist)
SummaryDataFrame_ten<-do.call(cbind.data.frame, tenlist)

colnames(SummaryDataFrame_twopointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_five)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_sevenpointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_ten)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

SummaryDataFrame<-rbind(SummaryDataFrame_twopointfive,
                        SummaryDataFrame_five,
                        SummaryDataFrame_sevenpointfive,
                        SummaryDataFrame_ten)

rownames(SummaryDataFrame)<-c(
  '0.25',
  '0.5',
  '0.75',
  '0.1'
)

SummaryDataFrame$`Total costs`<-SummaryDataFrame$`Discounted lifetime cost infections`+SummaryDataFrame$`Cost of life`

SummaryDataFrame$`Total costs (billions)`<-SummaryDataFrame$`Total costs`/1000000000
write.csv(SummaryDataFrame,'SummaryTable_top50_pwid-09Apr2025.csv')

transposedSummaryDataFrame<-as.data.frame(t(SummaryDataFrame))
write.csv(transposedSummaryDataFrame,'transposedSummaryTable_top50_pwid-09Apr2025.csv')


#### deaths per 100k saving

finalmergesubtwopointfive_top50_deathsper100k<-slice_max(finalmergesubtwopointfive,n = 50,order_by = deathsper100k)
finalmergesubfive_top50_deathsper100k<-slice_max(finalmergesubfive,n = 50,order_by = deathsper100k)
finalmergesubsevenpointfive_top50_deathsper100k<-slice_max(finalmergesubsevenpointfive,n = 50,order_by = deathsper100k)
finalmergesubten_top50_deathsper100k<-slice_max(finalmergesubten,n = 50,order_by = deathsper100k)

top50deathsper100k<-as.data.frame(unique(finalmergesubfive_top50_deathsper100k$state_county))
colnames(top50deathsper100k)<-'Counties with highest number of deathsper100k'
write.csv(top50deathsper100k,'top50deathsper100k-counties-09Apr2025.csv',row.names = F)

twopointfivelist<-list(
  sum(finalmergesubtwopointfive_top50_deathsper100k$cumulativelivessaved,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubtwopointfive_top50_deathsper100k$valueoflivessaved,na.rm = T)
)

fivelist<-list(
  sum(finalmergesubfive_top50_deathsper100k$cumulativelivessaved,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubfive_top50_deathsper100k$valueoflivessaved,na.rm = T)
)

sevenpointfivelist<-list(
  sum(finalmergesubsevenpointfive_top50_deathsper100k$cumulativelivessaved,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubsevenpointfive_top50_deathsper100k$valueoflivessaved,na.rm = T)
)

tenlist<-list(
  sum(finalmergesubten_top50_deathsper100k$cumulativelivessaved,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$cumulativeHCVinfectionaverted,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$cumulativeHIVinfectionlowaverted,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$cumulativeHIVinfectionmedaverted,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$cumulativeHIVinfectionhighaverted,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$infectiousdiscountedcost,na.rm = T),
  sum(finalmergesubten_top50_deathsper100k$valueoflivessaved,na.rm = T)
)

SummaryDataFrame_twopointfive<-do.call(cbind.data.frame, twopointfivelist)
SummaryDataFrame_five<-do.call(cbind.data.frame, fivelist)
SummaryDataFrame_sevenpointfive<-do.call(cbind.data.frame, sevenpointfivelist)
SummaryDataFrame_ten<-do.call(cbind.data.frame, tenlist)

colnames(SummaryDataFrame_twopointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_five)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_sevenpointfive)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

colnames(SummaryDataFrame_ten)<-c(
  'Deaths averted',
  'HCV averted',
  'HIV-low averted',
  'HIV-medium averted',
  'HIV-high averted',
  'Discounted lifetime cost infections',
  'Cost of life'
)

SummaryDataFrame<-rbind(SummaryDataFrame_twopointfive,
                        SummaryDataFrame_five,
                        SummaryDataFrame_sevenpointfive,
                        SummaryDataFrame_ten)

rownames(SummaryDataFrame)<-c(
  '2.5',
  '5',
  '7.5',
  '10'
)

SummaryDataFrame$`Total costs`<-SummaryDataFrame$`Discounted lifetime cost infections`+SummaryDataFrame$`Cost of life`

SummaryDataFrame$`Total costs (billions)`<-SummaryDataFrame$`Total costs`/1000000000
write.csv(SummaryDataFrame,'SummaryTable_top50_deathsper100k-09Apr2025.csv')

transposedSummaryDataFrame<-as.data.frame(t(SummaryDataFrame))
write.csv(transposedSummaryDataFrame,'transposedSummaryTable_top50_deathsper100k-09Apr2025.csv')

# save final merge
finalmerge$percentinjections<-finalmerge$freqinjections*100
finalmerge$cumulativelivessavedper100k<-finalmerge$cumulativelivessaved/finalmerge$pop18plus
# saveRDS(finalmerge,'finalmerge-09Apr2025.rds')
# finalmerge<-readRDS(file = 'finalmerge-09Apr2025.rds')
