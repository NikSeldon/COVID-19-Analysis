library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


if(.Platform$OS.type == "windows") {
     setwd("C:/Users/domin/OneDrive/covid/")} else {
         setwd("~/OneDrive/covid/")}

cwd<-getwd()

#URLS
github<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/"
confirmedURL_JHU <-paste(github,"csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv?accessType=DOWNLOAD", sep="")



#Detailed German data as provided by the Robert Koch Institut is obtained from the Github-repo of Norman Nexo  
#[https://github.com/normannexo/COVID-19-RKI](https://github.com/normannexo/COVID-19-RKI/blob/master/csv/rki_data.csv)
gitGermanyURL<-"https://github.com/normannexo/COVID-19-RKI/blob/master/csv/rki_data.csv?accessType=DOWNLOAD"

#select a Date
dateDownloaded<-as.Date("2020-03-24")
#or take current data
dateDownloaded<-Sys.Date()%>%as.Date("%d-%m-%y")


#DOWNLOAD
#pathGithubGermany<-paste("/data/CasesGermany",dateDownloaded,".csv",sep="")
pathConfirmed<-paste(cwd,"/data/confirmedCases_",dateDownloaded,".csv",sep="")
pathDeath<-paste(cwd,"/data/deathsCases_",dateDownloaded,".csv",sep="")

if(.Platform$OS.type == "windows") {
  download.file(confirmedURL_JHU,destfile = pathConfirmed)
  #download.file(deathURL_JHU,destfile = pathDeath)
  #download.file(gitGermanyURL,destfile = pathGithubGermany)
  } else { 
    download.file(confirmedURL_JHU,destfile = pathConfirmed, method = "curl")
    #download.file(deathURL_JHU,destfile = pathDeath, method = "curl")
    #download.file(gitGermanyURL,destfile = pathGithubGermany, method = "curl")
    }


#Read Saved File
confirmedCases_JHU <- read_csv(pathConfirmed)%>%
    as_tibble()%>%
    rename(province = "Province/State", country_region = "Country/Region")%>% 
    pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
    mutate(Date=as.Date(mdy(Date)))%>%
    select(-c( Lat, Long)) %>%
    arrange(country_region)

confirmedCasesTmp<-as_tibble()    
countries<-list("Italy", "Germany", "Switzerland", "Spain", "France", "Netherlands")
#countries<-order(countries)
confirmedCasesSelected<-as_tibble()

for(i in seq_along(countries)){
  confirmedCasesTmp<-confirmedCases_JHU%>%
    filter(country_region == countries[i], province==countries[i] | is.na(province))%>%
    mutate(incident_cases = c(0, diff(cumulative_cases)))
  confirmedCasesSelected<-rbind(confirmedCasesSelected,confirmedCasesTmp)
}

### Daily incident confirmed cases
#####################################
title<-paste("COVID-19 daily incident  confirmed cases\n data downloaded from Johns Hopkins University: ", dateDownloaded)
caption<-"Source: Johns Hopkins CSSE Novel coronavirus COVID-19 (2019-nCoV)\n data repository: https://github.com/CSSEGISandData/COVID-19"

ggplot(confirmedCasesSelected, aes(x = Date, y = incident_cases, colour=country_region))+ 
  geom_col() + 
  facet_wrap(country_region ~ ., scale = "free_y")+
  labs(y = "Daily incident confirmed cases", title = title, 
       subtitle = "(Note: different scales)", caption = caption)+
  theme(legend.position = "none", legend.title = element_blank())


### Cumulative confirmed cases
#####################################
title<-paste("COVID-19 daily cumulative confirmed cases\n data downloaded from Johns Hopkins University: ", dateDownloaded)
ggplot(confirmedCasesSelected, aes(x = Date, y = cumulative_cases, colour=country_region))+ 
  geom_line() + 
  geom_point()+
  facet_wrap(country_region ~ ., scale = "free_y")+
  labs(y = "Cumulative confirmed cases", title = title, 
       subtitle = "(Note: different scales)", caption = caption)+
  theme(legend.position = "none", legend.title = element_blank())

############LogPlot
ggplot(confirmedCasesSelected, aes(x = Date, y = cumulative_cases, colour=country_region))+ 
  geom_line() + 
  geom_point()+
  geom_smooth(data=filter(confirmedCasesSelected, Date >= "2020-03-01", Date <= "2020-03-15"), method = "lm",formula =y ~x, col="blue",linetype="dashed")+
  geom_smooth(data=filter(confirmedCasesSelected, Date >= "2020-03-15",Date <= "2020-03-26"), method = "lm",formula =y ~x, col="red",linetype="dashed")+
  facet_wrap(country_region ~ .)+
  labs(y = "Cumulative confirmed cases", title = title, 
       subtitle = "", caption = caption) +
  xlim(c(as.Date('2020-02-15 00:00:00', format = "%Y-%m-%d %H:%M:%S"),
         as.Date('2020-04-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")))+
  scale_y_log10(limits = c(1,1e5))+
  theme(legend.position = "none", legend.title = element_blank())

####FIT
#Beginning of March
for(i in seq_along(countries)){
fitdata<-filter(confirmedCasesSelected,country_region == countries[i], Date >= "2020-03-01", Date <= "2020-03-15")%>%
  mutate(logCases=log(cumulative_cases))
model<-lm(logCases ~ Date, data = fitdata)
print(paste("Doubling Time beginning of March",countries[i], ": " ,round(log(2)/coef(model)[2],1), "days"))
}

#End of March
for(i in seq_along(countries)){
  fitdata<-filter(confirmedCasesSelected,country_region == countries[i], Date >= "2020-03-16", Date <= "2020-03-26")%>%
    mutate(logCases=log(cumulative_cases))
  model<-lm(logCases ~ Date, data = fitdata)
  print(paste(countries[i], ": " ,round(log(2)/coef(model)[2],1), "days"))
}

infectedGer<-confirmedCasesSelected%>%
  filter(country_region=="Germany", Date >= ymd("2020-01-27"), Date <= ymd("2020-03-19"))%>%
  #mutate(Day=as.numeric(Date)-as.numeric(ymd("2020-01-27")))%>%
  mutate(logCases=log(cumulative_cases))

fitdata<-filter(infectedGer, Date >= "2020-03-01")
fit<-ggplot(infectedGer, aes(x=Date, y = logCases))+
  geom_line() + 
  geom_point()+
  stat_smooth(data=fitdata ,method = "lm")
fit  
  

geom_abline(data = fitGer , slope = lm$coefficients[2], intercept = lm$coefficients[1])+
  labs(y = "Cumulative confirmed cases", title = title, 
       subtitle = "(Note: )", caption = caption) +
  xlim(c(as.Date('2020-02-15 00:00:00', format = "%Y-%m-%d %H:%M:%S"),
         as.Date('2020-04-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")))




extrapolation<-as_tibble(seq(as.Date('2020-03-15'),as.Date('2020-04-01'),by = 1))
exp(predict(fitGer, data=extrapolation))


plot(infectedGer$Date ,infectedGer$logCases)

infectedGerFit<-filter(infectedGer, Date >= "2020-02-15")
fitGer<-lm(logCases ~ Date, data = infectedGerFit)
abline(fitGer)

coef(fitGer)
print(paste("T2 - Germany:",log(2)/coef(fitGer)[2]))







infectedSwi<-confirmedCasesSelected%>%
  filter(province=="Hubei", Date >= ymd("2020-01-22"), Date <= ymd("2020-02-12"))%>%
  mutate(Day=as.numeric(Date)-as.numeric(ymd("2020-01-22")))%>%
  mutate(logCases=log(cumulative_cases))

plot(infectedSwi$Day ,infectedSwi$logCases)

infectedSwiFit<-filter(infectedSwi, Day >= 3 & Day <= 15)
fitSwi<-lm(logCases ~ Day, data = infectedSwiFit )
abline(fitSwi)

coef(fitSwi)
print(paste("T2 - Switzerland:",log(2)/coef(fitSwi)[2]))






confirmedUS<-confirmedCases_JHU%>% filter(country_region == "US")%>%
  mutate(incident_cases = c(0, diff(cumulative_cases)))

title<-paste("COVID-19 daily incident  confirmed cases\n data downloaded from Johns Hopkins University: ", dateDownloaded)
caption<-"Source: Johns Hopkins CSSE Novel coronavirus COVID-19 (2019-nCoV)\n data repository: https://github.com/CSSEGISandData/COVID-19"

ggplot(confirmedUS, aes(x = Date, y = incident_cases, colour=country_region))+ 
  geom_col() + 
  facet_wrap(country_region ~ ., scale = "free_y")+
  labs(y = "Daily incident confirmed cases", title = title, 
       subtitle = "(Note: different scales)", caption = caption)+
  scale_y_log10(limits = c(1,1e5))+
  theme(legend.position = "none", legend.title = element_blank())


fitdata<-filter(confirmedUS,country_region == "US", Date >= "2020-03-01")%>%
  mutate(logCases=log(cumulative_cases))
  model<-lm(logCases ~ Date, data = fitdata)
  
  plot(fitdata$Date,fitdata$logCases)  
  abline(model)
  
print(paste("Doubling Time US", ": " ,log(2)/coef(model)[2], "days"))




















##########HUBEI
confirmedCasesHubei<- confirmedCases_JHU %>% 
  filter(province == "Hubei", Date<="2020-02-12")%>%
  mutate(incident_cases = c(0, diff(cumulative_cases)))
  confirmedCasesSelected<-rbind(confirmedCasesSelected,confirmedCasesHubei)

###############
deathCases_JHU <- read_csv(pathDeath)%>%
  as_tibble()%>%
  rename(province = "Province/State", country_region = "Country/Region")%>% 
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  mutate(Date=as.Date(mdy(Date)))%>%
  select(-c( Lat, Long)) %>%
  arrange(country_region)

deathCasesTmp<-as_tibble()    
countries<-list("Italy", "Germany", "Switzerland")
deathCasesSelected<-as_tibble()

for(i in seq_along(countries)){
  deathCasesTmp<-deathCases_JHU%>%
    filter(country_region == countries[i], province==countries[i] | is.na(province))%>%
    mutate(incident_cases = c(0, diff(cumulative_cases)))
  deathCasesSelected<-rbind(deathCasesSelected,deathCasesTmp)
}
title<-paste("Johns Hopkins University\nCOVID-19 daily incidence data up to: ",dateDownloaded)
caption<-"Sources: Johns Hopkins CSSE Novel coronavirus COVID-19 (2019-nCoV)\n data repository: https://github.com/CSSEGISandData/COVID-19"

ggplot(deathCasesSelected, aes(x = Date, y = incident_cases, colour=country_region))+ 
  geom_line() + 
  geom_point()+
  facet_grid(country_region ~ ., scale = "free_y")+
  labs(y = "Daily incremental death cases", title = title, 
       subtitle = "(Note: )", caption = caption) 


################

conf150<-confirmedCasesSelected%>%filter(cumulative_cases>=150)
separate(data=conf150, col=country_region, into = c(unique(country_region)))

conf150Onset<-as_tibble()
for(i in seq_along(countries)){
  tmp<-conf150%>%filter(country_region==countries[i])%>%head(1)
  conf150Onset<-rbind(conf150Onset,tmp)
    }
rm(tmp)
conf150Onset
conf150<-mutate(conf150, DateShift=)

####################################
onset <- as.Date(c("2017-02-04", "2017-02-12", "2017-02-15",
                   "2017-02-23", "2017-03-01", "2017-03-01",
                   "2017-03-02", "2017-03-03", "2017-03-03"))   
library(incidence)
i <- incidence(onset)
i

###########################MODEL

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dI <- beta * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# put the daily cumulative incidence numbers for Hubei from
# 15th Jan to 30th Jan into a vector called Infected
sir_start_date <- "2020-01-15"
N=59020000 #people in Hubei

Infected <- confirmedCases_JHU %>% 
  filter(province == "Hubei")

%>% 
  pull(cumulative_cases)

# Create an incrementing Day vector the same length as our
# cases vector
Day <- 1:(length(Infected))

# now specify initial values for S, I and R
init <- c(S = N - Infected[1], I = Infected[1], R = 0)

# define a function to calculate the residual sum of squares
# (RSS), passing in parameters beta and gamma that are to be
# optimised for the best fit to the incidence data
library(deSolve)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((Infected - fit)^2)
}

# now find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.
# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0
Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
# check for convergence
Opt$message




Opt$message

