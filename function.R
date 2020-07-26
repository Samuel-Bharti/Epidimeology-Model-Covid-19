#initialization functions
#----------------------------------------------------------
call_libs <- function(){
  require(jsonlite)
  require(plyr)
  require(plotly)
  require(reshape2)
  require(ggplot2)
  require(gridExtra)
  require(rlist)
  require(tidyverse)
  require(imputeTS)
}

raw_data_init <- function(){
  data01 <- fromJSON("https://api.covid19india.org/raw_data1.json")
  data01 <- data01$raw_data
  data02 <- fromJSON("https://api.covid19india.org/raw_data2.json")
  data02 <- data02$raw_data
  data001 <- rbind.data.frame(data01,data02)
  data001 <- data001[,c(1,10)]
  data03 <- fromJSON("https://api.covid19india.org/raw_data3.json")
  data03 <- data03$raw_data
  data04 <- fromJSON("https://api.covid19india.org/raw_data4.json")
  data04 <- data04$raw_data
  data002 <- rbind.data.frame(data03,data04)
  data002 <- data002[,c(1,9)]
  data0 <- rbind.data.frame(data001,data002)
  y <- raw_data_init2(data0)
  return(y)
}

raw_data_init2 <- function(x){
  x$agebracket[x$agebracket == ""] <- "0"
  x$agebracket[x$agebracket == "28-35"]<-"31"
  gender_table <- data.frame(Male = length(which(x$gender == "M")), Female =length(which(x$gender == "F")))
  age_table <- age_category(x)
  age_table <- data.frame(table(age_table[age_table != "Unknown"], dnn = "Age"))
  x[x == "NA" | x == "" | x == "Non-Binary" | x == "0"] <- NA
  age_gen <- na.omit(x)
  age_gen$agebracket<- age_category(age_gen)
  age_gen <- as.data.frame.matrix(table(age_gen))
  age_gen_list <- list(gender_table,age_table,age_gen)
  names(age_gen_list) <- c("Total_gender","Total_age","Common_freq")
  return(age_gen_list)
}




#----------------------------------------------------------
#takes input dataframe of raw_data3.json
age_category <- function(y){
  age_num   <- y$agebracket
  age_num   <- as.numeric(age_num)
  age_cat   <- age_num
  a <- 1:length(age_num)
  for (x in a){if(age_num[x] == 0){age_cat[x] <- "Unknown"} 
    else if(0 < age_num[x] & age_num[x] < 5){age_cat[x] <- "1-5"}
    else if(5<=age_num[x] & age_num[x]<=10){age_cat[x] <- "6-10"}
    else if(11<=age_num[x] & age_num[x]<=15){age_cat[x] <- "11-15"}
    else if(16<=age_num[x] & age_num[x]<=20){age_cat[x] <- "16-20"}
    else if(21<=age_num[x] & age_num[x]<=25){age_cat[x] <- "21-25"}
    else if(26<=age_num[x] & age_num[x]<=30){age_cat[x] <- "26-30"}
    else if(31<=age_num[x] & age_num[x]<=35){age_cat[x] <- "31-35"}
    else if(36<=age_num[x] & age_num[x]<=40){age_cat[x] <- "36-40"}
    else if(41<=age_num[x] & age_num[x]<=45){age_cat[x] <- "41-45"}
    else if(46<=age_num[x] & age_num[x]<=50){age_cat[x] <- "46-50"}
    else if(51<=age_num[x] & age_num[x]<=55){age_cat[x] <- "51-55"}
    else if(56<=age_num[x] & age_num[x]<=60){age_cat[x] <- "56-60"}
    else if(61<=age_num[x] & age_num[x]<=65){age_cat[x] <- "61-65"}
    else if(66<=age_num[x] & age_num[x]<=70){age_cat[x] <- "66-70"}
    else if(71<=age_num[x] & age_num[x]<=75){age_cat[x] <- "71-75"}
    else if(75<=age_num[x]){age_cat[x] <- "Above-75"}}
  x10 <- c("1-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60", "61-65", "66-70","71-75","Above-75")
  age_cat = factor(age_cat, levels = x10)
  return(age_cat)
}

#----------------------------------------------------------
#takes input dataframe of data.json
get_casetime <- function(x){
  x <- x$cases_time_series
  #initialization and correcting format ---
  
  # dates <- as.Date(casetime$date,format = '%d %B')
  # tconfirmed <- as.numeric(casetime$totalconfirmed)
  # tdeath <- as.numeric(casetime$totaldeceased)
  # trecover <- as.numeric(casetime$totalrecovered)
  # dconfirm <- as.numeric(casetime$dailyconfirmed)
  # ddeath  <- as.numeric(casetime$dailydeceased)
  # drecover <- as.numeric(casetime$dailyrecovered)
  x
}

#takes input dataframe of data.json
get_statewise <- function(x){
  x <- x$statewise
  x
}

#save top10 state_codes ----takes data.json
get_top10 <- function(x){
  x <- x$statewise
  #  x <- data.frame(total_cases = x$confirmed, name = x$state, code = x$statecode)
  x <- c(x$statecode)
  x <- x[1:12]
  x <- x[-1]
  x <- x[-9]
  
  return(x)
}

get_top5 <- function(x){
  x <- x$statewise
  #  x <- data.frame(total_cases = x$confirmed, name = x$state, code = x$statecode)
  x <- c(x$statecode)
  x <- x[1:6]
  x <- x[-1]
  return(x)
}
#takes input dataframe of data.json
get_tested <- function(x){
  x <- x$tested
  
}


#----------------------------------------------------------
#takes input dataframe of states_daily.json
get_tcase <- function(x,lock){
  x <- na_replace(x, 0)
  x <- x$states_daily
  dates3 <- x$date[x$status == "Confirmed"]
  dates3 <- as.Date(dates3, format = '%d-%b-%y')
  tcase <- list()
  i = 1
  while( i <= length(x)){
    if(i == 8 || i == 33){
      i = i + 1
    }
    else{
      x[x==""]<-"0"
      confirmed <- cumsum(as.numeric(x[i][x[33] == "Confirmed"]))
      recovered <- cumsum(as.numeric(x[i][x[33] == "Recovered"]))
      deceased  <- cumsum(as.numeric(x[i][x[33] == "Deceased"]))
      tempdf <- data.frame(confirmed, deceased, recovered,dates3)
      active <- confirmed - (recovered + deceased)
      tempdf$active <- active
      if(lock == 'y') {tempdf <- tempdf [-(1:8),]}
      tcase <- list.append(tcase, tempdf)
      i = i + 1
    }
  }
  names(tcase) <- names(x[,-c(8,33)]) 
  return(tcase)
}


get_acase <- function(x,lock){
  x <- na_replace(x, 0)
  x <- x$states_daily
  dates3 <- x$date[x$status == "Confirmed"]
  dates3 <- as.Date(dates3, format = '%d-%b-%y')
  acase <- list()
  i = 1
  while( i <= length(x)){
    if(i == 8 || i == 33){
      i = i + 1
    }
    else{
      x[x==""]<-"0"
      confirmed <- cumsum(as.numeric(x[i][x[33] == "Confirmed"]))
      recovered <- cumsum(as.numeric(x[i][x[33] == "Recovered"]))
      deceased  <- cumsum(as.numeric(x[i][x[33] == "Deceased"]))
      active <- confirmed - (recovered + deceased)
      acase <- list.append(acase, active)
      i = i + 1
    }
  }
  names(acase) <- names(x[,-c(8,33)]) 
  acase <- as.data.frame(acase)
  acase$date <- dates3
  if(lock == 'y') {acase <- acase [-(1:8),]}
  else if(lock == 't') {acase <- acase [-(1:45),]}
  return(acase)
}

get_dcase <- function(x,lock){
  x <- na_replace(x, 0)
  x <- x$states_daily
  #dates3 <- x$date[x$status == "Confirmed"]
  #dates3 <- as.Date(dates3, format = '%d-%b-%y')
  dcase <- list()
  i = 1
  while( i <= length(x)){
    if(i == 8 || i == 33){
      i = i + 1
    }
    else{
      x[x==""]<-"0"
      deceased  <- cumsum(as.numeric(x[i][x[33] == "Deceased"]))
      dcase <- list.append(dcase, deceased)
      i = i + 1
    }
  }
  names(dcase) <- names(x[,-c(8,33)]) 
  dcase <- as.data.frame(dcase)
  if(lock == 'y') {dcase <- dcase [-(1:8),]}
  return(dcase)
}

get_dcase2 <- function(x,lock){
  x <- na_replace(x, 0)
  x <- x$states_daily
  dcase <- list()
  i = 1
  while( i <= length(x)){
    if(i == 8 || i == 33){
      i = i + 1
    }
    else{
      x[x==""]<-"0"
      deceased  <- as.numeric(x[i][x[33] == "Deceased"])
      dcase <- list.append(dcase, deceased)
      i = i + 1
    }
  }
  names(dcase) <- names(x[,-c(8,33)]) 
  dcase <- as.data.frame(dcase)
  if(lock == 'y') {dcase <- dcase [-(1:8),]}
  return(dcase)
}

show_this <- function(x,y1,c,z){
  x <- x
  y1 <- y1
  # y2 <- y2
  c <- c
  title <- z
  fig <- plot_ly(x = ~x,y = ~y1, name = 'avgpos',mode = 'markers', color = ~c)
  # fig <- fig %>% add_trace(y = ~y2, name = 'avg',mode = 'lines') 
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Reproductive Number"),
                        yaxis = list (title = "Lockdown coeffecient"))
  fig
  # filen <- paste(c,".jpeg")
  # path <- paste(path,"/")
  # direct <- paste(path,filen)
  # orca(fig, direct)
}

normalizef <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# PART 1 functions

# SIR MODEL FUNCTION
SIR <- function(N, S0, I0, beta, alpha, t) {
  S <- numeric(t)
  I <- numeric(t)
  S[1] <- S0
  I[1] <- I0
  for (i in 2:t) {
    S[i] <- S[i-1] - beta*S[i-1]/N*I[i-1]
    I[i] <- I[i-1] + beta*S[i-1]/N*I[i-1] - alpha * I[i-1]
    if (I[i] < 1 || S[i] < 1)
      break
  }
  df <- data.frame(time=1:t, S=S, I=I, R=N-S-I)
  df <- df[S>1&I>1,]
  df$AR <- (df$I+df$R)/N
  nr <- nrow(df)
  rate <- df$I[2:nr]/df$I[1:(nr-1)]
  df$rate <- c(rate[1], rate)
  return(df)
}

# SQUARE SUM F1
ss <- function(beta, gamma,active,days,N) {
  I0 <- active[1]
  times <- days
  df <- SIR(N=N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) )
  activess <-active[1:length(df$time)]
  sum((df$I[-1] - activess[-1])^2)
}

# SQUARE SUM F2
ss2 <- function(x,a,b,c) {
  active=a
  days=b
  N=c
  ss(beta = x[1], gamma = x[2],active,days,N)
}

# FUNCTION TO PLOT GRAPH OF SIR AND ACTIVE
comp0 <- function(beta, gamma,c,active,days,N,path){
  I0 <- active[1]
  times <- days
  df <- SIR(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times))
  active <- normalizef(active)
  length(active) <- length(days)
  x <- df$time
  y1 <- normalizef(df$S)
  y2 <- normalizef(df$I)
  y3 <- normalizef(df$R)
  y4 <- active
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Susceptible',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Infected',mode = 'lines')
  fig <- fig %>% add_trace(y = ~y3, name = 'Recovered',mode = 'lines')
  fig <- fig %>% add_trace(y = ~y4, name = 'Live Active data',mode = 'lines',line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'))
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "S,I,R and Live active data"))
  fig
  filen <- paste(c,".jpeg")
  path <- paste(path,"-SIR/")
  direct <- paste(path,filen)
  orca(fig, direct)
  
}

comp1 <- function(beta, gamma,c,active,days,N) {
  I0 <- active[1]
  times <- days
  df <- SIR(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) )
  x <- df$time
  y1 <- df$I
  y2 <- active[1:length(df$time)]
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Predicted with SIR',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Trend from 22nd March till Today',mode = 'lines') 
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "Number of cases"))
  fig
  
}


# comp1 with save graph 
comp2 <- function(beta, gamma,c,active,days,N,path) {
  I0 <- active[1]
  times <- days
  df <- SIR(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) )
  x <- df$time
  y1 <- df$I
  y2 <- active[1:length(df$time)]
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Predicted with SIR',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Trend from 22nd March till Today',mode = 'lines') 
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "Number of Cases"))
  fig
  direct <- paste("27-05-20/",path,"/",c,".jpeg")
  orca(fig, direct)
}


#estimate auto
get_estimate <- function(y,z,top10,beta,gamma,plot){
  betax <- list()
  gammax <- list()
  for (p in top10) {
    state_name <- as.character(y$state_name[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, length(active))
    ss(beta, gamma,active,days,N)
    beta_val <- seq(from = 0.3, to = 0.4, le = 100) #beta_val
    ss_val <- sapply(beta_val, ss, gamma = gamma,active,days,N) #ss_val
    min_ss_val <- min(ss_val) #min_ss_val
    beta_hat <- beta_val[ss_val == min_ss_val]
    beta_hat # this is estimated beta
    gamma_val <- seq(from = 0.2, to = 0.3, le = 100) #gamma_val
    ss_val <- sapply(gamma_val, function(x) ss(beta_hat, x,active,days,N)) #ss_val
    (min_ss_val <- min(ss_val))
    (gamma_hat <- gamma_val[ss_val == min_ss_val])
    ss2(c(0.3, 0.2),a= active,b= days,c= N)
    starting_param_val <- c(0.4, 0.3)
    ss_optim <- optim(starting_param_val, ss2,a= active,b= days,c= N)
    betax <- append(betax,ss_optim$par[1])
    gammax <- append(gammax,ss_optim$par[2])
    
  }
  x <- data.frame(state_code=top10, beta = as.numeric(betax) ,gamma = as.numeric(gammax))
  x$Ro <- x$beta/x$gamma
  if (plot == 'y'){plot_estimate(x,y,z,pl=1)
    return(x)}
  else if(plot == 's'){plot_estimate(x,y,z,pl=2)}
  else if(plot == 'sir'){plot_estimate(x,y,z,pl=3)}
}

#plot estimates
# x = state_codes

plot_estimate <- function(x,y,z,pl){
  path <- as.numeric(Sys.time())
  code <- x$state_code
  for (p in code){
    state_name <- as.character(y$state_name[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, length(active))
    b <- x$beta[x$state_code == p]
    g <- x$gamma[x$state_code == p]
    if(pl == 1){fig <- comp1(b,g,state_name,active,days,N)
    return(fig)}
    else if(pl == 2){comp2(b,g,state_name,active,days,N,path)}
  }
}


plot_sir <- function(x,y,z,d,top10){
  path <- as.numeric(Sys.time())
  code <- top10
  for (p in code){
    state_name <- as.character(y$state_name[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, d)
    b <- x$beta[x$code == p]
    g <- x$gamma[x$code == p]
    comp0(b,g,state_name,active=active,days=days,N=N,path=path)}
}

#part 3 function

# SIR MODEL FUNCTION
SIR1 <- function(N, S0, I0, beta, alpha, t,kappa) {
  S <- numeric(t)
  I <- numeric(t)
  S[1] <- S0
  I[1] <- I0
  for (i in 2:t) {
    S[i] <- S[i-1] - kappa*beta*S[i-1]/N*I[i-1]
    I[i] <- I[i-1] + kappa*beta*S[i-1]/N*I[i-1] - alpha * I[i-1]
    if (I[i] < 1 || S[i] < 1)
      break
  }
  df <- data.frame(time=1:t, S=S, I=I, R=N-S-I)
  df <- df[S>1&I>1,]
  
  return(df)
}

# SQUARE SUM F1
ss_2 <- function(beta, gamma,kappa, active,days,N) {
  I0 <- active[1]
  times <- days
  df <- SIR1(N=N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, kappa = kappa,t= length(times) )
  activess <-active[1:length(df$time)]
  sum((df$I[-1] - activess[-1])^2)
}

# SQUARE SUM F2
ss2_2 <- function(x,a,b,c,kappa) {
  active=a
  days=b
  N=c
  ss_2(beta = x[1], gamma = x[2],active,days,N,kappa =kappa)
}

# FUNCTION TO PLOT GRAPH OF SIR AND ACTIVE
comp3 <- function(beta, gamma,c,active,days,N,kappa,path) {
  I0 <- active[1]
  times <- days
  df <- SIR1(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) ,kappa= kappa)
  x <- df$time
  active1 <- active
  active1 <- normalizef(active1)
  y2 <- active1[1:length(df$time)]
  y1 <- normalizef(df$I)
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Predicted with SIR',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Trend from 22nd March till Today',mode = 'lines') 
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "Number of cases"))
  fig
  direct <-paste("Mobility_graphs/",path,"/",c,".jpeg",sep = "")
  orca(fig, direct)
  
}

comp4 <- function(beta, gamma,c,active,days,N,kappa) {
  I0 <- active[1]
  times <- days
  df <- SIR1(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) ,kappa= kappa)
  x <- df$time
  active1 <- active
  active1 <- normalizef(active1)
  y2 <- active1[1:length(df$time)]
  y1 <- normalizef(df$I)
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Predicted with SIR',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Trend from 22nd March till Today',mode = 'lines') 
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "Number of cases"))
  fig
}

#estimate auto
get_estimate2 <- function(xx,y,z,top10,beta,gamma,plot){
  betax <- list()
  gammax <- list()
  kappax <- list()
  state <- list()
  density   <- list()
  for (p in top10) {
    kappa <- 1 + xx$Avg[xx$state_code== tolower(p)]
    state_name <- as.character(y$state_name[y$state_code == p])
    d <- as.numeric(y$density[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, length(active))
    ss_2(beta, gamma,active,days,N,kappa=kappa)
    beta_val <- seq(from = 0.2, to = 0.4, le = 100) #beta_val
    ss_val <- sapply(beta_val, ss_2, gamma = gamma,active,days,N,kappa=kappa) #ss_val
    min_ss_val <- min(ss_val) #min_ss_val
    beta_hat <- beta_val[ss_val == min_ss_val]
    beta_hat # this is estimated beta
    gamma_val <- seq(from = 0.2, to = 0.3, le = 100) #gamma_val
    ss_val <- sapply(gamma_val, function(x) ss_2(beta_hat, x,active,days,N,kappa=kappa)) #ss_val
    (min_ss_val <- min(ss_val))
    (gamma_hat <- gamma_val[ss_val == min_ss_val])
    ss2_2(c(0.2, 0.2),a= active,b= days,c= N,kappa=kappa)
    starting_param_val <- c(0.3, 0.2)
    ss_optim <- optim(starting_param_val, ss2_2,a= active,b= days,c= N,kappa = kappa)
    betax <- append(betax,ss_optim$par[1])
    gammax <- append(gammax,ss_optim$par[2])
    kappax <- append(kappax, kappa)
    state <- append(state, state_name)
    density <- append(density, d)
  }
  x <- data.frame(state_name = as.character(state), state_code=top10, 
                  beta = as.numeric(betax),gamma = as.numeric(gammax),
                  kappa = as.numeric(kappax),R= as.numeric(betax)/as.numeric(gammax),
                  density = as.numeric(density))
  if (plot == 'n'){return(x)}
  else if(plot == 'y'){plot_estimate(x,y,z,pl=1)}
  else if(plot == 's'){plot_estimate(x,y,z,pl=2)}
}

#plot estimates
# x = state_codes

#set_rho

set_rho <- function(x){
  rho = 1 - x/100
  rho
}

plot_estimate2 <- function(x,y,z,pl){
  path <- as.numeric(Sys.time())
  code <- x$state_code
  for (p in code){
    state_name <- as.character(y$state_name[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, length(active))
    b <- x$beta[x$state_code == p]
    g <- x$gamma[x$state_code == p]
    kappa <- x$kappa[x$state_code == p]
    if(pl == 1)
    {fig <- comp3(b,g,state_name,active,days,N,kappa=kappa,path = path)}
  }
}

get_estimate3 <- function(y,z,top10,beta,gamma,plot){
  betax <- list()
  gammax <- list()
  for (p in top10) {
    state_name <- as.character(y$state_name[y$state_code == p])
    N <- y$population[y$state_code == p]
    active <- z[,tolower(p)]
    days <- seq(1, length(active))
    ss(beta, gamma,active,days,N)
    beta_val <- seq(from = 0.4, to = 0.5, le = 100) #beta_val
    ss_val <- sapply(beta_val, ss, gamma = gamma,active,days,N) #ss_val
    min_ss_val <- min(ss_val) #min_ss_val
    beta_hat <- beta_val[ss_val == min_ss_val]
    beta_hat # this is estimated beta
    gamma_val <- seq(from = 0.3, to = 0.4, le = 100) #gamma_val
    ss_val <- sapply(gamma_val, function(x) ss(beta_hat, x,active,days,N)) #ss_val
    (min_ss_val <- min(ss_val))
    (gamma_hat <- gamma_val[ss_val == min_ss_val])
    ss2(c(0.4, 0.3),a= active,b= days,c= N)
    starting_param_val <- c(0.5, 0.4)
    ss_optim <- optim(starting_param_val, ss2,a= active,b= days,c= N)
    betax <- append(betax,ss_optim$par[1])
    gammax <- append(gammax,ss_optim$par[2])
    
  }
  x <- data.frame(state_code=top10, beta = as.numeric(betax) ,gamma = as.numeric(gammax))
  x$Ro <- x$beta/x$gamma
  if (plot == 'y'){plot_estimate(x,y,z,pl=1)
    return(x)}
  else if(plot == 's'){plot_estimate(x,y,z,pl=2)}
  else if(plot == 'sir'){plot_estimate(x,y,z,pl=3)}
}

comp007 <- function(beta,gamma,c,active,days,N,test,labs){
  I0 <- active[1]
  times <- days
  df <- SIR2(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times))
  # active <- normalizef(active)
  # ll <- normalizef(ll)
  # test <- normalizef(test)
  max <- labs * 200
  min <- labs * 150
  x1 <- length(active) 
  y5 <- 
    length(active) <- length(days)
  length(ll) <- length(days)
  length(test) <- length(days)
  x  <- df$time
  y1 <- df$rateS
  y2 <- test
  x2 <- df$time[y2 == 1]
  y3 <- df$I
  y4 <- active
  title <- list(text = paste("<b> Comparing Testing Rate with Predicted Infection", c, "</b>"),
                font = list(family = "sans-serif",size = 10))
  fig <- plot_ly(x = ~x,y = ~y3, name = 'Predicted Infection Cases',mode = 'lines', type = 'scatter',
                 line = list( width = 1.5),width = 500, height = 300)
  fig <- fig %>% add_trace(x = ~x1, name = '28-May-2020',mode = 'lines',line = list(color = 'rgba(0, 0, 0, 0.6)', width = 1))
  fig <- fig %>% add_trace(y = ~y2, name = 'Current Testing Rate',mode = 'lines',line = list(width = 2))
  # fig <- fig %>% add_trace(y = ~y3, name = 'Predicted Infection Cases',mode = 'lines',line = list(color = 'rgba(238, 145, 38,0.7)', width = 1.5))
  fig <- fig %>% add_trace(y = ~y4, name = 'Live Active Cases',mode = 'lines',line = list(color = 'rgba(152, 0, 0, .8)',width = 2))
  # fig <- fig %>% add_trace(y = ~max, name = 'Lab Cap max',mode = 'lines',line = list(color = 'rgba(152, 68, 68, 0.7)', width = 1.5))
  # fig <- fig %>% add_trace(y = ~min, name = 'Lab Cap min',mode = 'lines',line = list(color = 'rgba(152, 68, 68, 0.7)', width = 1.5))
  fig <- fig %>% layout(fig, title = title,font = t,
                        xaxis = list(title = "<b> Days Since 28-April-2020 </b>",
                                     autotick = FALSE,
                                     ticks = "outside",
                                     tick0 = 0,
                                     dtick = 5,
                                     ticklen = 5,
                                     tickwidth = 2),
                        yaxis = list (title = "<b> Cases and Tests Counts </b>"),
                        plot_bgcolor = 'rgb(243, 243, 243)',
                        paper_bgcolor = 'rgb(243, 243, 243)',
                        legend =l2,showlegend = T,
                        shapes = list(
                          list(type = "rect",
                               fillcolor = "rgb(90, 200, 75)", 
                               line = list(color = "rgb(90, 200, 75)"), opacity = 0.2,
                               x0 = 0 , x1 = x1, xref = "x",
                               y0 = min, y1 = max, yref = "y")))
  return(fig) 
}

SIR2 <- function(N, S0, I0, beta, alpha, t) {
  S <- numeric(t)
  I <- numeric(t)
  S[1] <- S0
  I[1] <- I0
  rateS <- numeric(t)
  for (i in 2:t) {
    S[i] <- S[i-1] - beta*S[i-1]/N*I[i-1]
    I[i] <- I[i-1] + beta*S[i-1]/N*I[i-1] - alpha * I[i-1]
    rateS[i] <- beta*S[i-1]/N*I[i-1]
    if (I[i] < 1 || S[i] < 1)
      break
  }
  df <- data.frame(time=1:t, S=S, I=I, R=N-S-I)
  df <- df[S>1&I>1,]
  df$rateS <- rateS
  df$AR <- (df$I+df$R)/N
  nr <- nrow(df)
  rate <- df$I[2:nr]/df$I[1:(nr-1)]
  df$rate <- c(rate[1], rate)
  return(df)
}



labs <- labs_df$total_labs[labs_df$state_code == 'MH']

active <- acaset$dl
N <- state_info$population[state_info$state_code == 'DL']
I0=active[1]
days <- seq(1,length(active))
dfn2 <- SIR2(N= N,S0=N -I0, I0=active[1],beta = 0.32,alpha =0.2 ,t=length(days))

comp002 <- function(beta, gamma,c,active,days,N) {
  I0 <- active[1]
  times <- days
  df <- SIR(N = N, S0 = N- I0, I0 = I0 , beta = beta, alpha = gamma, t= length(times) )
  x <- df$time
  y1 <- df$I
  y2 <- active[1:length(df$time)]
  x1 <- length(active)
  title <- paste("State - ", c)
  fig <- plot_ly(x = ~x,y = ~y1, name = 'Predicted with SIR',mode = 'lines', type = 'scatter')
  fig <- fig %>% add_trace(y = ~y2, name = 'Trend from 22nd March till Today',mode = 'lines') 
  fig <- fig %>% add_trace(x = ~x1, name = '28-May-2019',mode = 'lines',
                           line = list(color = 'rgb(219, 64, 82, 0.7)', width = 4))
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Days"),
                        yaxis = list (title = "Number of cases"),
                        showlegend =F)
  fig
}
