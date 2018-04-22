
# power

rec_factorial <- function(n){
  if(n == 0){
    return(1)
  }else{
    return(n*rec_factorial(n-1))
  }
}
rec_factorial(100)
factorial(100)


for_loop_cumsum <- function(x){
  n <- 1:x
  s <- 0
  for(i in 1:length(n)){
    s <- s+n[i]
    print(s)
  }
}
for_loop_cumsum(10)
cumsum(1:10)


# 

system.time(sum(as.numeric(1:100000000))) -> t1
system.time(sum(as.numeric(1:1000000000))) -> t2 
n2 <- 1000000000
n1 <- 100000000

a <- log2(t2[3]/t1[3])/log2(n2/n1)

t2[3]/t1[3] 
(n2/n1)^a



# generate date and price
# calculate moving averages for various time periods 
# save the date and price in various data strucures -  
# zoo, data_frame, data.table
# increase the size of the data constantly 10,100...
# change the rolling period 20,30,200...


library(lubridate)
library(tidyquant)
library(data.table)


ggplot(df,aes(date,price))+
  geom_line(size = .01, alpha = .2)

# functions
generate_data <- function(size){
  d1 <<- seq(ymd('2018-01-01'), by = 'days', length.out = size)  
  set.seed(10)
  p1 <<- abs(rnorm(size, mean = 10,sd = 2)) 
}



benchmark_fun <- function(data_size, rolling_period_size){
  
  rolling_p <- rolling_period_size
  
  # generate random data
  generate_data(size = data_size) 
  
  data_frame(date = d1, price = p1) -> df
  zoo_time_series <- zoo(p1,d1)
  data.table(date= d1,price=p1) -> dt
  
  tidy_roll_mean <- function(){
    tq_mutate(df, price ,SMA, n = rolling_p)
  }
  
  zoo_roll_mean <- function(){
    rollmeanr(zoo_time_series, k=rolling_p, fill = NA) 
  }
  
  data_table_roll_mean <- function(){
    dt[, rollmean:= rollmeanr(price, k=rolling_p, fill=NA)]
  }
  
  microbenchmark::microbenchmark(
    tidy_roll_mean(),
    zoo_roll_mean(),
    data_table_roll_mean(),
    times = 10
  ) -> benchmark_data
  
  benchmark_data$size <- data_size
  benchmark_data$rolling_period <- rolling_p
  return(benchmark_data)
  
}

benchmark_fun(25000,20) %>%
  bind_rows(., benchmark_fun(50000,20)) %>%
  bind_rows(., benchmark_fun(75000,20)) %>%
  bind_rows(., benchmark_fun(100000,20)) %>%
  bind_rows(., benchmark_fun(250000,20)) %>%
  bind_rows(., benchmark_fun(500000,20)) %>%
  bind_rows(., benchmark_fun(1000000,20)) -> df_20

benchmark_fun(25000,200) %>%
  bind_rows(., benchmark_fun(50000,200)) %>%
  bind_rows(., benchmark_fun(75000,200)) %>%
  bind_rows(., benchmark_fun(100000,200)) %>%
  bind_rows(., benchmark_fun(250000,200)) %>%
  bind_rows(., benchmark_fun(500000,200)) %>%
  bind_rows(., benchmark_fun(1000000,200)) -> df_200


bind_rows(df_20,df_200) -> benchmark_data

rm(df_20,df_200)
fwrite(benchmark_data, "benchmark_data.csv")




