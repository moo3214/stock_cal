# install

  library(quantmod)
  library(tidyquant)
  library(purrr)



# option
options("getSymbols.warning4.0"=FALSE)

# create function to cal return expect price and actual price
#to = Sys.Date( ) ,
Cal_Return <- function(stock_name) {
  
  # dowload stock data
  stock_data <- tq_get(stock_name,
                  from = "2010-01-01",
                  to = "2019-07-08",
                  get = "stock.prices")
  
  # create x
  x <- 1:length(stock_data[[1]])
  
  # create y = adjust price
  y <- stock_data[[8]]
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(stock_data[[1]]) + intercept
  
  #actual price
  actual_price <- y[length(y)]
  
  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  expect_return
  
  #return valve
  return_valve <- c(expect_price,actual_price,expect_return)
  return(return_valve)
  
}

# i for loop and list for stote value
i <- 0
new_stock_list <- c()
expect_price_list <- c()
actual_price_list <- c()
expect_return_list <- c()


stock_list <- c("BH.BK","DCC.BK","NTV.BK","PYLON.BK","SCP.BK","SEAFCO.BK")
for (stock_name in stock_list) {
  # dowload stock data
  stock_data <- tq_get(stock_name,
                       from = "2010-01-01",
                       to = Sys.Date() ,
                       get = "stock.prices")
  
  # create x
  x <- 1:length(stock_data[[1]])
  
  # create y = adjust price
  y <- stock_data[[8]]
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(stock_data[[1]]) + intercept
  
  #actual price
  actual_price <- y[length(y)]
  
  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  expect_return
  

  
  #select stock
  if (expect_return >= 0){
    i = i + 1
    new_stock_list[i] <- stock_name
    actual_price_list[i] <- actual_price
    expect_price_list[i] <- expect_price
    expect_return_list[i] <- expect_return
  }
}

# input money for invest
cold_money <- readline(prompt="Enter money: ")
cold_money <- as.integer(cold_money)
cold_money <- 150000
#find percent invest and return ratio
percent_invest <- c()
return_ratio <- c()
for (i in (1 : length(new_stock_list))){
  percent_invest[i] <- expect_return_list[i] / sum(expect_return_list)
  return_ratio[i] <- percent_invest[i] * expect_return_list[i] /100
  
}

#find return port
return_port <- sum(return_ratio)

#find invest money
invest_money = cold_money * return_port

#find each stock buy
stock_buy <- c()
volumn_buy <- c()
for (i in (1 : length(new_stock_list))){
  stock_buy[i] <- invest_money * percent_invest[i]
  volumn_buy[i] <- stock_buy[i] / actual_price_list[i]
}
invest_money
return_port
x <- data.frame(new_stock_list,actual_price_list,expect_price_list,expect_return_list,stock_buy,volumn_buy)
x
# x <- x[with(x,order(-expect_return_list)),]
# length(expect_return_list)
# x <- x[1:10,]

expect_return_list
# cal_date <- function(date,N){
# 
#   for (stock_name in stock_list) {
# 
#     skip_to_next <- FALSE
#     # dowload stock data
#     tryCatch(
#       stock_data <- tq_get(stock_name,
#                          from = "2010-01-01",
#                          to = date ,
#                          get = "stock.prices"),
# 
# 
# 
# 
#     # create x
#     x <- 1:length(stock_data[[1]]),
# 
#     # create y = adjust price
#     y <- stock_data[[8]],
# 
#     #find slope and intercept
#     equation <- lm(y ~ x) ,
#     intercept <- coef(equation)[1],
#     slope <- coef(equation)[2],
# 
#     #find expect price
#     expect_price <- slope * length(stock_data[[1]]) + intercept,
# 
#     #actual price
#     actual_price <- y[length(y)],
# 
#     #expect return
#     expect_return = (expect_price - actual_price) / actual_price *100,
# 
# 
#     #select stock
#     if (expect_return >= 0){
#       i = i + 1
#       new_stock_list[i] <- stock_name
#       actual_price_list[i] <- actual_price
#       expect_price_list[i] <- expect_price
#       expect_return_list[i] <- expect_return
#     }
#     ,error = function(e) { skip_to_next <<- TRUE})
# 
#     if(skip_to_next) { next }
#   }
#   # input money for invest
#   #cold_money <- readline(prompt="Enter money: ")
#   #cold_money <- as.integer(cold_money)
#   cold_money <- 150000
#   #find percent invest and return ratio
#   percent_invest <- c()
#   return_ratio <- c()
#   for (i in (1 : length(new_stock_list))){
#     percent_invest[i] <- expect_return_list[i] / sum(expect_return_list)
#     return_ratio[i] <- percent_invest[i] * expect_return_list[i] /100
# 
#   }
# 
#   #find return port
#   return_port <- sum(return_ratio)
# 
#   #find invest money
#   invest_money = cold_money * return_port
# 
#   #find each stock buy
#   stock_buy <- c()
#   volumn_buy <- c()
#   for (i in (1 : length(new_stock_list))){
#     stock_buy[i] <- invest_money * percent_invest[i]
#     volumn_buy[i] <- stock_buy[i] / actual_price_list[i]
#   }
#   invest_money
#   return_port
#   x <- data.frame(new_stock_list,actual_price_list,expect_price_list,expect_return_list,stock_buy,volumn_buy)
#   x <- x[with(x,order(-expect_return_list)),]
# 
#   x <- x[1:N,]
# 
#   return(x[[1]])
# }
# date1 <- "2020-01-01"
# 
# count <- 0
# 
# while (cal_date(date1,1) == cal_date(as.Date(date1) +1,1)){
#   date1 <- as.Date(date1) + 1
#   count <- count +1
# }
# date0 <- as.Date(date1) - count
# date0
# date1
# cal_date(date1,1)
# 
# Cal_return_bydate = function(stock_list,date0,date1) {
#   return_value <- c()
#   for (stock in stock_list){
#   stock_data <- tq_get(stock,
#                        from = date0 ,
#                        to = date1  ,
#                        get = "stock.prices")
#   price <- stock_data[[8]]
#   price0 <- price[1]
#   price1 <- price[length(price)]
#   return_value <- c(return_value,(price1 - price0) / price0 * 100)
#   }
#   return(return_value)
# }
# for (stock_name in stock_list) {
#   skip_to_next <- FALSE
# 
#   # dowload stock data
#   tryCatch(
#     stock_data <- tq_get(stock_name,
#                          from = "2010-01-01",
#                          to = Sys.Date( ) ,
#                          get = "stock.prices")
#     ,error = function(e) { skip_to_next <<- TRUE})
# 
#   if(skip_to_next) { next }
# 
# }
# cal_stock_by_return <- function(date,N){
# 
#   # create x
#   x <- 1:length(stock_data[[1]])
# 
#   # create y = adjust price
#   y <- stock_data[[8]]
# 
#   #find slope and intercept
#   equation <- lm(y ~ x)
#   intercept <- coef(equation)[1]
#   slope <- coef(equation)[2]
# 
#   #find expect price
#   expect_price <- slope * length(stock_data[[1]]) + intercept
# 
#   #actual price
#   actual_price <- y[length(y)]
# 
#   #expect return
#   expect_return = (expect_price - actual_price) / actual_price *100
#   expect_return
# 
# 
# 
#   #select stock
#   if (expect_return >= 0){
#     i = i + 1
#     new_stock_list[i] <- stock_name
#     actual_price_list[i] <- actual_price
#     expect_price_list[i] <- expect_price
#     expect_return_list[i] <- expect_return
#   }
# 
# 
#   # input money for invest
#   #cold_money <- readline(prompt="Enter money: ")
#   #cold_money <- as.integer(cold_money)
#   cold_money <- 150000
#   #find percent invest and return ratio
#   percent_invest <- c()
#   return_ratio <- c()
#   for (i in (1 : length(new_stock_list))){
#     percent_invest[i] <- expect_return_list[i] / sum(expect_return_list)
#     return_ratio[i] <- percent_invest[i] * expect_return_list[i] /100
# 
#   }
# 
#   #find return port
#   return_port <- sum(return_ratio)
# 
#   #find invest money
#   invest_money = cold_money * return_port
# 
#   #find each stock buy
#   stock_buy <- c()
#   volumn_buy <- c()
#   for (i in (1 : length(new_stock_list))){
#     stock_buy[i] <- invest_money * percent_invest[i]
#     volumn_buy[i] <- stock_buy[i] / actual_price_list[i]
#   }
#   invest_money
#   return_port
#   x <- data.frame(new_stock_list,actual_price_list,expect_price_list,expect_return_list,stock_buy,volumn_buy)
#   x <- x[with(x,order(-expect_return_list)),]
# 
#   x <- x[1:N,]
# 
#   return(x[[1]])
# }