#plot
t <- list(
  family = "Times New Roman",
  size = 11)

#plot age, gender, age-gender group

x  <- c("Male", "Female") 
y <- c(data0$Total_gender$Male ,data0$Total_gender$Female)

title <- "Gender of Individuals <br> affected by COVID19 till <br> 28-May-2020"
fig <- plot_ly(labels = ~x,values = ~y, type= "pie",
               textinfo = 'label+percent', 
               insidetextorientation='radial',
               textposition = 'inside',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)),showlegend = FALSE)
fig <- plot_ly(labels = ~x, values = ~y,
               texttemplate="%{label}:<br>(%{percent})",
               insidetextfont = list(color = '#000000',font = 'sans serif'),
               marker = list(colors = c('rgb(124,205,246)','rgb(243,155,214)'),
                             line = list(color = 'rgb(8,48,107)', width = 1.5)),showlegend = F)
fig <- fig %>% add_pie(hole = 0.5)
fig <- fig %>% layout(title = list(text = title,font = t,y =0.55,x=0.525),
                      xaxis = list(automargin = TRUE,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(automargin = TRUE,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig
# plot_bgcolor = 'rgb(243, 243, 243)',
# paper_bgcolor = 'rgb(243, 243, 243)')
# fig <- plot_ly(x = ~x,y = ~y, type= "bar",
#                text = y, textposition = 'auto',
#                marker = list(color = 'rgb(158,202,225)',
#                line = list(color = 'rgb(8,48,107)', width = 1.5)))
# fig <- fig %>% layout(title = title,
#                       xaxis = list(showgrid = TRUE,title = "Gender"),
#                       yaxis = list (showgrid = TRUE,title = "Count"))
#                       # plot_bgcolor = 'rgb(243, 243, 243)',
#                       # paper_bgcolor = 'rgb(243, 243, 243)')
fig
if (!require("processx")) install.packages("processx")
orca(fig, "27-05-20/Part1-Graphs/Gender.jpeg")




c <- c('rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',
       'rgb(219, 64, 82, 0.7)','rgb(219, 64, 82, 0.7)','rgb(219, 64, 82, 0.7)','rgb(219, 64, 82, 0.7)',
       'rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',
       'rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)')

######age
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
x  <- data0$Total_age$Age
y <- data0$Total_age$Freq
title <- "Age Group of Individuals affected by COVID19 till 28-May-2020"
fig <- plot_ly(x = ~x,y = ~y, type= "bar",
               width = 500, height = 400,
               marker = list(color = c,
                             line = list(color = 'rgb(8,48,107)', 
                                         width = 1.5)))
fig <- fig %>% layout(title = title, font = t,
                      xaxis = list(showgrid = TRUE,title = "Age-Brackets"),
                      yaxis = list (showgrid = TRUE,title = "Count"))
# plot_bgcolor = 'rgb(243, 243, 243)',
# paper_bgcolor = 'rgb(243, 243, 243)')
fig
orca(fig, "27-05-20/Part1-Graphs/Age4.jpeg")



# age-gender
l <- list(
  font = list(
    family = "sans-serif",
    size = 10,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "rgb(0, 0, 0)",
  borderwidth = 2,
  x = 0.8,
  y = 0.9)

x  <- data0$Total_age$Age
y  <- data0$Common_freq$F
y1 <- data0$Common_freq$M
title <- "Gender-Age Categorization of Individual affected by COVID19 till 28-May-2020"
fig <- plot_ly(x = ~x,y = ~y, type= "bar",name = 'Female',
               width = 500, height = 400,
               marker = list(color = 'rgb(222,45,38,0.8)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% add_trace(y = ~y1, name = 'Male',
                         marker = list(color = 'rgb(58,200,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = list(text= title,font = list(size = 12)),font = t,
                      xaxis = list(showgrid = TRUE,title = "Age-Gender Groups"),
                      yaxis = list (showgrid = TRUE,title = "Count"),
                      barmode = "grouped",
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      legend =l)
fig







orca(fig, "27-5-20/Part1-Graphs/Gender-Age.jpeg")






#mobility data

y  <- estimate_raw2$kappa 
x <- estimate_raw2$Ro
x1 <- estimate_raw2$state_code

title <- "Mobility Impact on R0"
fig <- plot_ly(x = ~x,y = ~y, type= "scatter",mode="markers",
               text = y, textposition = 'auto', color = ~x1,
               marker = list(size = 35, opacity = 0.8,
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = title,
                      xaxis = list(showgrid = TRUE,title = "R0:Reproductive Number Estimated with Current Mobility"),
                      yaxis = list (showgrid = TRUE,title = "Current Mobility Ratio"),
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      legend =l)
fig

orca(fig, "Part3-Graphs/Mobility-R0.jpeg")

# to plot complete SIR prediction with live
plot_sir(relation,state_info,acase,90,'AP')

I0 <- active[1]
df <- SIR(N = N, S0 = N- I0, I0 = I0 , 7.465227, 7.331131, t= 100 )
plot(df$time,df$R)

active1 <- active
active1 <- normalizef(active1)
length(active1) <- 100
#df1 <- normalizef(df)
#df1$active <- active
y1 <- normalizef(df$S)
y2 <- normalizef(df$I)
y3 <- normalizef(df$R)
y4 <- active1
x1 <- df$time[df$S == df$I]

fig <- plot_ly(x = ~df$time,y = ~y1, name = 'Susceptible',mode = 'lines', type = 'scatter')
fig <- fig %>% add_trace(y = ~y2, name = 'Infected',mode = 'lines')
fig <- fig %>% add_trace(y = ~y3, name = 'Recovered',mode = 'lines')
fig <- fig %>% add_trace(y = ~y4, name = 'Live Active data',mode = 'lines',line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'))
fig <- fig %>% add_trace(x = ~x1, name = 'Peak',mode = 'lines')
fig <- fig %>% layout(title = title,
                      xaxis = list(title = "Days"),
                      yaxis = list (title = "Number of cases"))
fig


show_this(relation$r0,relation$avgpos,relation$code,"Lockdown vs spread Analysis")

# x <- x
# y1 <- y1
# # y2 <- y2
# c <- c
title <- "Lockdown vs spread Analysis"
fig <- plot_ly(data = relation, x = ~r0,y = ~avg2,mode = 'markers', type= "scatter",color = ~state,marker = list(size = 35, opacity = 0.8))
# fig <- fig %>% add_trace(y = ~y2, name = 'avg',mode = 'lines') 
fig <- fig %>% layout(title = title,
                      xaxis = list(showgrid = FALSE,title = "Reproductive Number"),
                      yaxis = list (showgrid = FALSE,title = "Lockdown coeffecient"),
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)')
fig

############

title <- "Lockdown vs spread Analysis"
fig <- plot_ly(data = relation, x = ~r0,y = ~code,mode = 'markers', type= "scatter",color = ~state,marker = list(size = 35, opacity = 0.8))
fig <- fig %>% add_trace(x = ~avg2, y = ~code, name = 'avg',mode = 'lines') 
fig <- fig %>% layout(title = title,
                      xaxis = list(showgrid = FALSE,title = "Reproductive Number"),
                      yaxis = list (showgrid = FALSE,title = "Lockdown coeffecient"),
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      showlegend = FALSE)
fig

# filen <- paste(c,".jpeg")
# path <- paste(path,"/")
# direct <- paste(path,filen)
# orca(fig, direct)

code <- top10
path <- as.numeric(Sys.time())
for (p in code){
  state_name <- as.character(state_info$state_name[state_info$state_code == p])
  active <- acase[,tolower(p)]
  days <- seq(1, length(active))
  x <- days
  y0  <- dcase[,tolower(p)] 
  y1  <- rep(state_info$public_beds[state_info$state_code== p],length(days)) 
  y11 <- rep(state_info$private_beds[state_info$state_code==p],length(days))
  y12 <- rep(state_info$private_icu[state_info$state_code==p],length(days))
  y13 <- rep(state_info$public_icu[state_info$state_code==p],length(days))
  y14 <- rep(state_info$total_beds[state_info$state_code==p],length(days))
  y15 <- rep(state_info$total_icu[state_info$state_code==p],length(days))
  y16 <- rep(state_info$total_ventilators[state_info$state_code==p],length(days))
  #y2 <- rep(state_info$public_icu[state_info$state_code=='MH'],length(days))
  #y3 <- rep(state_info$public_beds[state_info$state_code=='GJ'],length(days))
  #y4 <- rep(state_info$public_icu[state_info$state_code=='TN'],length(days))
  #y5 <- rep(state_info$public_icu[state_info$state_code=='MP'],length(days))
  title <- paste("Availability of Beds In", state_name)
  fig <- plot_ly(x = ~x,y = ~active,name= 'Active Cases', mode = 'lines',type = 'scatter')
  #fig <- fig %>% add_trace(y = ~mh, mode = 'lines', name='MH') 
  #fig <- fig %>% add_trace(y = ~gj, mode = 'lines', name='GJ') 
  #fig <- fig %>% add_trace(y = ~tn, mode = 'lines', name='TN') 
  #fig <- fig %>% add_trace(y = ~mp, mode = 'lines', name='MP')
  fig <- fig %>% add_trace(y = ~y0, mode = 'lines', name='Deceased/Required ICUs')
  # fig <- fig %>% add_trace(y = ~y1, mode = 'lines', name='Public_beds',line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot')) 
  # fig <- fig %>% add_trace(y = ~y11, mode = 'lines', name='Private_beds',line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
  # fig <- fig %>% add_trace(y = ~y12, mode = 'lines', name='Private_ICU',line = list(color = 'rgb(50,205,50)', width = 4, dash = 'dash')) 
  # fig <- fig %>% add_trace(y = ~y13, mode = 'lines', name='Public_ICU',line = list(color = 'rgb(255,215,0)', width = 4, dash = 'dot')) 
  fig <- fig %>% add_trace(y = ~y1, mode = 'lines', name='Public Beds Available',line = list(color = 'rgb(50,205,50)', width = 4, dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~y15, mode = 'lines', name='Total ICU Beds Available',line = list(color = 'rgb(255,215,0)', width = 4, dash = 'dot'))
  fig <- fig %>% add_trace(y = ~y16, mode = 'lines', name='Total Ventilators Available',line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'))
  #fig <- fig %>% add_trace(y = ~y2, mode = 'lines', name='MH-hb',line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot')) 
  #fig <- fig %>% add_trace(y = ~y3, mode = 'lines', name='GJ-hb',line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot'))
  #fig <- fig %>% add_trace(y = ~y4, mode = 'lines', name='TN-hb',line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot'))
  #fig <- fig %>% add_trace(y = ~y5, mode = 'lines', name='MP-hb',line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot'))
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Number of cases/beds"),
                        yaxis = list (title = "Days"))
  fig
  filen <- paste(state_name,".jpeg")
  path <- paste(path,"BEDS/")
  direct <- paste(path,filen)
  orca(fig, direct)
}



relation2 <- relation[1:5,]

#name ="Reduction in Mobility" name = "Calculated Reproductive Number"
title <- "Lockdown vs spread Analysis State-wise"
fig <- plot_ly(data = relation, x = ~code,y = ~avg2,type = 'bar',color = ~state)
#fig <- fig %>% add_trace(y = ~r0, mode = "lines",line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot')) 
fig <- fig %>% layout(title = title,
                      xaxis = list(title = "State"),
                      yaxis = list (title = "Count"))
fig


################ testing data graph ####################
ttested <- as.character(ttested)
x <- seq(1, nrow(ttested))
y <- ttested$
  df <- data.frame(x=x,y=y)
title <- "Lockdown vs spread Analysis State-wise"
fig <- plot_ly(x = ~x,y = ~yy,type = 'scatter', mode="lines",connectgaps = TRUE)
#fig <- fig %>% add_trace(y = ~r0, mode = "lines",line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dot'),connectgaps = TRUE) 
fig <- fig %>% layout(title = title,
                      xaxis = list(title = "Days"),
                      yaxis = list (title = "Count"))

fig

p = 'MH'

dcase3 <- get_dcase3(tolower(p),dcase)
bedplot(p)

bedplot2(p)

bedplot <- function(p){
  
  state_name <- as.character(state_info$state_name[state_info$state_code == p])
  active <- acase[,tolower(p)]
  days <- seq(1, length(active))
  x   <- days
  y2  <- active * 0.2 
  y1  <- rep(state_info$public_beds[state_info$state_code== p],length(days)) 
  y11 <- rep(state_info$private_beds[state_info$state_code==p],length(days))
  y14 <- rep(state_info$total_beds[state_info$state_code==p],length(days))
  
  title <- paste("Availability of Hospital Beds in", state_name)
  
  fig <- plot_ly(x = ~x,width = 450, height = 300)
  fig <- fig %>% add_trace(y = ~y1, mode = 'lines', name='Public Beds',line = list(color = 'rgb(176, 126, 233)', width = 4))
  fig <- fig %>% add_trace(y = ~y11, mode = 'lines', name='Private Beds',line = list(color = 'rgb(26, 209, 111)', width = 4))
  fig <- fig %>% add_trace(y = ~y14, mode = 'lines', name='Total Beds',line = list(color = 'rgb(232, 220, 52)', width = 4))  
  fig <- fig %>% add_trace(y = ~active, name= 'Active Cases', mode = 'markers',type = 'scatter',
                           marker = list(size = 4,
                                         color = 'rgba(49,130,189, .9)',
                                         line = list(color = 'rgba(67,67,67,.8)',
                                                     width = 1)))
  fig <- fig %>% add_trace(y = ~y2, mode = 'markers', name='Symptomatic Cases/<br>Needs Hospitalization',
                           marker = list(size = 4,
                                         color = 'rgba(238, 24, 24, .9)',
                                         line = list(color = 'rgba(152, 0, 0, .8)',
                                                     width = 1)))
  
  fig <- fig %>% layout(title = title,font = t,
                        yaxis = list(title = "Number of cases/beds"),
                        xaxis = list (title = "Days"),
                        plot_bgcolor = 'rgb(243, 243, 243)',
                        paper_bgcolor = 'rgb(243, 243, 243)',
                        legend = l2, showlegend = T)
  fig
}



bedplot2 <- function(p){
  
  state_name <- as.character(state_info$state_name[state_info$state_code == p])  
  days <- seq(1, length(dcase[,tolower(p)]))
  x   <- days
  # active <- acase[,tolower(p)]
  # y2  <- active * 0.2 
  y0  <- dcase3  
  y12 <- rep(state_info$private_icu[state_info$state_code==p],length(days))
  y13 <- rep(state_info$public_icu[state_info$state_code==p],length(days))  
  y15 <- rep(state_info$total_icu[state_info$state_code==p],length(days))
  y16 <- rep(state_info$total_ventilators[state_info$state_code==p],length(days))
  
  title <- paste("Availability of Hospital Beds in", state_name,"for Severe Symptoms" )
  
  fig <- plot_ly(x = ~x, width = 450, height = 300)
  fig <- fig %>% add_trace(y = ~y0, mode = 'markers', name='Deceased/Required ICUs',
                           type = "scatter",
                           marker = list(size = 4,
                                         color = 'rgba(0, 0, 0, .5)',
                                         line = list(color = 'rgba(0, 0, 0, 1)',
                                                     width = 1)))    
  fig <- fig %>% add_trace(y = ~y12, mode = 'lines', name='Private ICU Beds',line = list(color = 'rgb(176, 126, 233)', width = 4))
  fig <- fig %>% add_trace(y = ~y13, mode = 'lines', name='Public ICU Beds',line = list(color = 'rgb(26, 209, 111)', width = 4))
  fig <- fig %>% add_trace(y = ~y15, mode = 'lines', name='Total ICU Beds Available',line = list(color = 'rgb(232, 220, 52)', width = 4))
  fig <- fig %>% add_trace(y = ~y16, mode = 'lines', name='Total Ventilators Available',line = list(color = 'rgb(215,141,197)', width = 4))
  
  fig <- fig %>% layout(title = title,font = t,
                        yaxis = list(title = "Number of cases/beds"),
                        xaxis = list (title = "Days"),
                        plot_bgcolor = 'rgb(243, 243, 243)',
                        paper_bgcolor = 'rgb(243, 243, 243)',
                        legend = l2, showlegend = T)
  # fig <- fig %>% add_trace(y = ~y2, mode = 'markers', name='Symptomatic Cases/<br>Needs Hospitalization',
  #        marker = list(size = 4,
  #        color = 'rgba(238, 24, 24, .9)',
  #        line = list(color = 'rgba(152, 0, 0, .8)',
  #        width = 1)))
  fig
}

get_dcase3 <- function(p,dcase){
  dcase3 <- dcase[,p]
  while(i <= length(dcase3)){
    if(i > 15){
      dcase3[i] <- dcase3[i] - dcase3[(i-15)] 
    }
    i = i+1 }
  return(dcase3)
}

#testing work

x  <- seq(1,length(acaset$dl))
y  <- acaset$dl 
y2 <- dfn2$I
y3 <- testing_details$DL

# s  <- mobility_table$state_name

title <- "<b> Prediction rate </b>"
fig <- plot_ly(x = ~x, width = 450, height = 350)
fig <- fig %>% add_trace(y = ~y,name ="Active cases"  ,type= "scatter",mode="lines")
fig <- fig %>% add_trace(y = ~y2,name ="Infection", type= "scatter",mode="lines")
fig <- fig %>% add_trace(y = ~y3,name ="Testing Rate", type= "scatter",mode="lines")
fig <- fig %>% layout(title = title,font = t,
                      xaxis = list(showgrid = TRUE,title = "<b>DAYS </b>"),
                      yaxis = list(showgrid = TRUE,title = "<b> CASES </b>"),
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)')

fig


dd = 50

p  = "MP"

state_name <- as.character(state_info$state_name[state_info$state_code == p])
N <- state_info$population[state_info$state_code == p]
act <- acase[,tolower(p)]
act2 <- acaset[,tolower(p)]
d <- seq(1, dd)
b <- estimate_rawtt$beta[estimate_rawtt$state_code == p]
g <- estimate_rawtt$gamma[estimate_rawtt$state_code == p]
lab <- labs_df$total_labs[labs_df$state_code == p]
# ll  <- rep(lab*200,length.out=length(act2))

test = td2[,p]

comp007(b,g,state_name,act2,d,N,test,lab)


comp007(2.5278818,2.44708856,state_name,act2,d,N,test,lab)


y <- list()
i <- 1
for(x in top10){
  
  if (i <= length(top10) ) {
    y1 <- labs_df$total_labs[labs_df$state_code == x]
    y2 <- labs_df$govt_labs[labs_df$state_code == x]
    y3 <- labs_df$private_labs[labs_df$state_code == x]
    y4 <- labs_df$ï..state_name[labs_df$state_code == x]
    df_testing$name[i] <- y4
    df_testing$private[i] <- y3
    df_testing$govt[i] <- y2
    df_testing$total[i] <- y1
  }
  i = i+1
}
df_testing <- data.frame(x = top10, y = as.numeric(y) )


#'rgb(158,202,225)'
x <- df_testing$x
y <- df_testing$total
y2 <- df_testing$govt
y1 <- df_testing$private

title <- "<b>Active Labs for COVID19 testing as of 28-May-2020</b>"
fig <- plot_ly(x = ~x,y = ~y, type= "bar",name = "Total Labs",
               width = 650, height = 350,
               text = y, textposition = 'auto',
               marker = list(color = 'rgb(245,167,77)',
                             line = list(color = 'rgb(8,48,107)', width = 1.6)))
fig <- fig %>% add_trace(y = ~y2, name = 'Government Labs',
                         text = y2, textposition = 'auto',
                         marker = list(color = 'rgb(90,222,85)',
                                       line = list(color = 'rgb(8,48,107)', 
                                                   width = 1.6)))
fig <- fig %>% add_trace(y = ~y1, name = 'Private',
                         text = y1, textposition = 'outside',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.6)))
fig <- fig %>% layout(title = title, font = t,
                      xaxis = list(showgrid = TRUE,title = "<b>States Codes</b>"),
                      yaxis = list (showgrid = TRUE,title = "<b>No. of Labs</b>"),
                      plot_bgcolor = 'rgb(243, 243, 243)',
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      barmode = "grouped", legend = l)
fig