# install
  library(quantmod)
  library(tidyquant)
  library(purrr)
  library(berryFunctions)
  
  # import library dplyr 
  library(dplyr) 
  
 
   #fuction numdate dummy
  num.stock_test <- function(stock,date){
    return(stock_array[num.date(date),2,stock])
  }
  
  #fuction return date at number
  num.date = function(date){
    return(toString(as.numeric(as.Date(date))))
  }  
  
  #return number date of stock
  num.stock <- function(stock,date){
    for(i in c(1:1000)){
      if(is.error(num.stock_test(stock,date), tell = FALSE, force = FALSE) == TRUE){
        
        date = as.Date(date) + 1
      }
      else {break}
    }
    return(stock_array[num.date(date),2,stock])
  }
  
  
  #Cal return of stock by start and end date
  Cal_return_bydate = function(stock,date0,date1) {
    
    #price <- stock_array[c(num.stock(stock,date0):num.stock(stock,date1)),1,stock]
    price0 <- stock_array[num.stock(stock,date0),1,stock]
    price1 <- stock_array[num.stock(stock,date1),1,stock]
    return_value <- (price1 - price0) / price0 *100
    
    return(return_value)
  }
  
  start_date <- "2010-01-01"
# option
options("getSymbols.warning4.0"=FALSE)

#create data frame
stock_list <- c('2S.BK',
                'ADVANC.BK',
                'AI.BK',
                'AIE.BK',
                'ALL.BK',
                'ALLA.BK',
                'ALUCON.BK',
                'AMANAH.BK',
                'APURE.BK',
                'ARROW.BK',
                'AS.BK',
                'ASIAN.BK',
                'ASP.BK',
                'AU.BK',
                'AUCT.BK',
                'BCH.BK',
                'BFIT.BK',
                'BH.BK',
                'BKER.BK',
                'BOL.BK',
                'BROOK.BK',
                'CBG.BK',
                'CHAYO.BK',
                'CHG.BK',
                'COM7.BK',
                'CPNCG.BK',
                'CPW.BK',
                'CTARAF.BK',
                'DCC.BK',
                'DELTA.BK',
                'DOD.BK',
                'DRT.BK',
                'EA.BK',
                'FORTH.BK',
                'FSMART.BK',
                'FTE.BK',
                'GC.BK',
                'GLOBAL.BK',
                'HARN.BK',
                'HFT.BK',
                'HMPRO.BK',
                'HTC.BK',
                'HUMAN.BK',
                'ICN.BK',
                'IIG.BK',
                'INSET.BK',
                'IP.BK',
                'JKN.BK',
                'JMT.BK',
                'JUBILE.BK',
                'KAMART.BK',
                'KIAT.BK',
                'KTC.BK',
                'KWC.BK',
                'KWM.BK',
                'LALIN.BK',
                'LST.BK',
                'M.BK',
                'MAKRO.BK',
                'MBAX.BK',
                'MC.BK',
                'MCS.BK',
                'MEGA.BK',
                'MFC.BK',
                'MGT.BK',
                'MICRO.BK',
                'MTC.BK',
                'NER.BK',
                'NETBAY.BK',
                'NOBLE.BK',
                'NTV.BK',
                'NYT.BK',
                'OISHI.BK',
                'ORI.BK',
                'OSP.BK',
                'PATO.BK',
                'PDG.BK',
                'PHOL.BK',
                'PM.BK',
                'POPF.BK',
                'PRM.BK',
                'PT.BK',
                'PTL.BK',
                'PTTEP.BK',
                'PYLON.BK',
                'Q-CON.BK',
                'QHPF.BK',
                'RBF.BK',
                'RCI.BK',
                'RJH.BK',
                'RS.BK',
                'S11.BK',
                'SAAM.BK',
                'SABINA.BK',
                'SAK.BK',
                'SAPPE.BK',
                'SAUCE.BK',
                'SAWAD.BK',
                'SCM.BK',
                'SCP.BK',
                'SEAFCO.BK',
                'SF.BK',
                'SFLEX.BK',
                'SICT.BK',
                'SINGER.BK',
                'SIS.BK',
                'SMART.BK',
                'SMPC.BK',
                'SORKON.BK',
                'SPALI.BK',
                'SPCG.BK',
                'SPVI.BK',
                'SSF.BK',
                'STA.BK',
                'STARK.BK',
                'STI.BK',
                'SUN.BK',
                'SVH.BK',
                'SWC.BK',
                'TACC.BK',
                'TASCO.BK',
                'TC.BK',
                'TCCC.BK',
                'TEAMG.BK',
                'TFG.BK',
                'TFMAMA.BK',
                'THIP.BK',
                'TIGER.BK',
                'TKN.BK',
                'TMD.BK',
                'TMILL.BK',
                'TNH.BK',
                'TNP.BK',
                'TOA.BK',
                'TOPP.BK',
                'TPA.BK',
                'TPAC.BK',
                'TPIPP.BK',
                'TPLAS.BK',
                'TPS.BK',
                'TQM.BK',
                'TSR.BK',
                'TTLPF.BK',
                'TTW.BK',
                'TVO.BK',
                'UBIS.BK',
                'UPF.BK',
                'UVAN.BK',
                'VCOM.BK',
                'VIH.BK',
                'VNT.BK',
                'WICE.BK',
                'WINNER.BK',
                'XO.BK',
                'YGG.BK',
                'YUASA.BK')
i <- 1

pre_data <- tq_get('kbank.bk',
                     from = start_date,
                     to = Sys.Date() ,
                     get = "stock.prices")
#length data
L_date <- length(pre_data[[2]])
L_stock_list <- length(stock_list)

stock_array <- array(c(0 ),dim = c(L_date,2,L_stock_list))
for (stock in stock_list){

               try({
                 
                stock_data <- tq_get(stock,
                     from = start_date,
                     to = Sys.Date() ,
                     get = "stock.prices")


               
                stock_array[,1,i]<- stock_data[[8]]
                stock_array[,2,i]<- c(1:L_date)
}, silent=TRUE
)
i <- i +1
}

row.names(stock_array) <- pre_data[[2]]
colnames(stock_array) <- c("PRICE","NUM")  #to set up col names
dimnames(stock_array)[[3]] <- stock_list






(is.error(num.stock("2S.BK","2010-01-03"), tell = FALSE, force = FALSE) == TRUE)

num.stock("2S.BK","2010-01-05")





Cal_expectreturn <- function(stock,date){
  # create x
  x <- c(1:num.stock(stock,date))
  
  # create y = adjust 
  y <- c(stock_array[c(num.stock(stock,start_date):num.stock(stock,date)),1,stock])
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(x) + intercept
  
  #actual price
  actual_price <- y[length(y)]

  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  
  return(expect_return)
  
}

expect_return_list <- c()
i <- 1
for (stock in stock_list){
  try({
  expect_return_list[i] <- Cal_expectreturn(stock,"2021-02-01")
  }, silent=TRUE
  )
  i <- i+1
}
cbind(stock_list,expect_return_list)
df_stock_return <- data.frame(stock_list,expect_return_list)
df_stock_return <- df_stock_return[with(df_stock_return,order(-expect_return_list)),]
df_stock_return


############################################################


Cal_returnstock_date_period.D_N <- function(date,period,N){
  
  
  expect_return_list <- c()
  i <- 1
  for (stock in stock_list){
    try({
      expect_return_list[i] <- Cal_expectreturn(stock,date)
    }, silent=TRUE
    )
    i <- i+1
  }
  
  df_stock_return <- data.frame(stock_list,expect_return_list)
  df_stock_return <- df_stock_return[with(df_stock_return,order(-expect_return_list)),]
  df_stock_return <- df_stock_return[1:N,]
  
  actual_return_list <- c()
  
  for (i in c(1:N)) {
    
    stock <- df_stock_return[i,1]
    actual_return_list[i] <- Cal_return_bydate(stock,date,as.Date(date) + period) 
    
  }
  
  
  Total_return_expect <- c()
  percent_invest <- c()
  Total_return <- c()
  for ( i in c(1:N) ){
    
    percent_invest[i] <- df_stock_return[i,2] / sum(df_stock_return$expect_return_list)
    
    Total_return_expect[i] <- df_stock_return[i,2] * percent_invest[i]
    Total_return[i] <- actual_return_list[i] * percent_invest[i]
  
  }
  
  Total_return <- Total_return * sum(Total_return_expect) / 100
  
  df_total_return <- data.frame(df_stock_return[1:N,1],Total_return)
  Total_return
  return(df_total_return)
}
Cal_returnstock_date_period.D_N("2020-01-29",300,5)
sum(Cal_returnstock_date_period.D_N("2019-12-02",150,5)$Total_return)
summary_return



year_cal <- c("2015-10-01","2016-10-01","2017-10-01","2018-10-01","2019-10-01")

array_return <- array(c(0 ),dim = c(4,10,5))
year_count <- 1
for (year in year_cal){

  summary_return_N_period <- c()
  date_start0 <- year
  date_start <- date_start0
  date_final <- as.Date(date_start0) + 365
  for (period in c(5,10,15,30)){

summary_return_N <- c()
summary_return <- c()

i_N <-0

  for (N in seq(1, 10, by=1)){
 
  
   summary_return_N <- c(summary_return_N,summary_return)
   summary_return <- c()
   i_N <- i_N + 1
   
   
   i_date <- 1
   date_start <- date_start0


  while (  as.numeric(as.Date(date_start)) < as.numeric( as.Date(date_final)) ){

    summary_return <- c(summary_return,sum(Cal_returnstock_date_period.D_N(date_start,period,N)$Total_return))
    date_start <- as.Date(date_start) + period
    i_date <- i_date + 1
    
  }
    
}


summary_return_N_period <- c(summary_return_N_period,colSums(matrix(summary_return_N, ncol = i_N,byrow = TRUE)) ) 
}
  x <- matrix(summary_return_N_period, ncol = 10 , byrow = TRUE)
  array_return[,,year_count] <- x
  year_count <- year_count +1
}
min(array_return[1,1,])
find_min <- c()
for (i in c(1:10)){
  
  for (j in c(1:4)) {
    find_min <- c(find_min,min(array_return[j,i,]))
    
  }
  
}
min_return_mat <- matrix(find_min, ncol = 10)
min_return_mat
which.max(min_return_mat)
max(min_return_mat)

########################################################
date_start <- "2018-10-01"
date_final <- "2019-10-01"
period <-10
N <- 6
summary_return <- c()      
summary_return 
      
while (  as.numeric(as.Date(date_start)) < as.numeric( as.Date(date_final)) ){
        
summary_return <- c(summary_return,sum(Cal_returnstock_date_period.D_N(date_start,period,N)$Total_return))
date_start <- as.Date(date_start) + period

        
      }
sum(summary_return) 
x <- 100
for(i in c(summary_return)){
  
  x <- x * ((100 + i) /100 )
  
}    
x    
 
as.Date("2021-01-16") + 10

