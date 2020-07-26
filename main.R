source('function.R')
source('plot.R')
call_libs()

#Read csv data
state_info  <- read.csv("state_info_main.csv")
mobility_df <- read.csv("mobility.csv")
mobility_df2<- read.csv("mobility-21-05.csv")
labs_df     <- read.csv("labs.csv")
testing_details <- read.csv("testing_details.csv")

#Getting the JSON data and converting it to a dataframe
#-------------raw-data-------------
data0 <- raw_data_init()

#--------------------------------case-time------------
data1 <- fromJSON("https://api.covid19india.org/data.json")
tcasetime  <- get_casetime(data1)
tstatewise <- get_statewise(data1)
ttested    <- get_tested(data1)
top10 <- get_top10(data1)
top5 <- get_top5(data1)

#-----------------------state-district-data-----------
data2 <- fromJSON("https://api.covid19india.org/state_district_wise.json")

#----------------------state-wise-data----------------
data3 <- fromJSON("https://api.covid19india.org/states_daily.json")
tcase <- get_tcase(data3,'y')
acase <- get_acase(data3,'y')
dcase <- get_dcase(data3,'y')
dcase2 <- get_dcase2(data3,'y')
# for testing data
acaset <- get_acase(data3,'t')


#Params estimating functions
estimate_raw00 <- get_estimate(state_info,acase,top10,0.33,0.2,'s')
estimate_raw <- get_estimate(state_info,acase,top10,0.32,0.2,'sir')
estimate_rawt2 <- get_estimate3(state_info,acaset,top10,0.62,0.2,'y')
estimate_raw6 <- get_estimate2(mobility_df2, state_info,acase,top10,0.64,0.4,'n')

# dfkappa <-SIR1(N= 121924973,S0=N -I0, I0=active[1],beta = 0,alpha =0 ,t=length(days),kappa = 0.49 )
# days <- seq(1,length(active))
# comp003(beta =0.2770787,gamma= 0.04264412,c='Maharashtra',active,days,N=121924973,kappa=0.49) 

e <- estimate_raw[order(estimate_raw$state_code),]
m <- mobility_df[order(mobility_df$state_code),]
m$state_code == tolower(e$state_code)
m$Avgpos <- 1 + m$Avg
relation <- data.frame(state = m$state, code= e$state_code,beta= e$beta,gamma= e$gamma,r0 = e$Ro, avg = m$Avg, avgpos =m$Avgpos)
relation$avg2 <- -relation$avg
plot(relation$r0,relation$avgpos)


