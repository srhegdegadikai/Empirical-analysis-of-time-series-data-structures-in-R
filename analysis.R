library(tidyverse)

read_csv("benchmark_data.csv") -> runtime_samples

# reduce the run time samples into single value formeach group - mean(run_time)
runtime_samples %>% 
  group_by(expr,rolling_period,size) %>% 
  summarise(run_time = mean(time)) -> df

# calculate the local order of growth - t2/t1
df %>%
  group_by(expr) %>% 
  mutate(growth_rate = lead(run_time,order_by = expr)/run_time) -> df


# order the data properly and recode missing values as NA's
# the first row in each group wont have a growth rate as it wont have a lagging 
# runtime to calculate t2/t1 
df %>%
  mutate(id = row_number())-> df

df$growth_rate[df$id==7|df$id==14] <- 0

df %>%
  mutate(growth_rate = lag(growth_rate)) -> df

df$growth_rate[df$id==1|df$id==8] <- NA


df %>%
  ggplot(., aes(size, growth_rate)) +
  geom_line(aes(color= expr), size = 1) +
  geom_point(aes(color = expr), size = 2)+
  facet_wrap(~rolling_period, nrow = 2) 


df %>%
  ggplot(., aes(size, run_time)) +
  geom_line(aes(color = expr), size =1) +
  geom_point(aes(color= expr), size = 2) + 
  facet_wrap(~rolling_period, nrow = 2) +
  scale_y_log10() +
  geom_line(aes(size, size*log(df$size))) +
  geom_line(aes(size, log(df$size))) +
  geom_line(aes(size, size)) +
  geom_line(aes(size, 10^size))

runtime_samples %>%
  ggplot(., aes(expr,time))+
  geom_boxplot(aes(fill = expr)) +
  coord_flip() +
  theme(legend.position = "bottom")

runtime_samples %>%
  filter(expr == 'zoo_roll_mean()'|expr == 'data_table_roll_mean()') %>%
  ggplot(., aes(expr,time))+
  geom_boxplot(aes(fill = as.factor(size))) +
  coord_flip() +
  theme(legend.position = "bottom") +
  facet_wrap(~size,scales = "free_y")



runtime_samples %>%
  filter(expr == 'zoo_roll_mean()'|expr == 'data_table_roll_mean()') %>%
  ggplot(., aes(time))+
  geom_density(aes(color= expr))
  
runtime_samples %>%
  filter(expr == 'zoo_roll_mean()'|expr == 'data_table_roll_mean()') %>%
  ungroup(.) %>%
  group_by(expr) %>%
  arrange(expr) %>%
  split(.$expr) -> df_list
wilcox.test(x = df_list$`zoo_roll_mean()`$time, 
            y = df_list$`data_table_roll_mean()`$time,
            paired = FALSE, alternative = "less")


runtime_samples %>%
  filter(expr == 'zoo_roll_mean()'|expr == 'data_table_roll_mean()') %>%
  ungroup(.) %>%
  group_by(expr) %>%
  arrange(expr) %>%
  split(.$size) -> df_list


final <- list()

for(i in 1:7){
  df_list[[i]] %>%
    ungroup(.) %>%
    group_by(expr) %>%
    arrange(expr) %>%
    split(.$expr) -> x_list
  wilcox.test(x = x_list$`zoo_roll_mean()`$time, 
              y = x_list$`data_table_roll_mean()`$time,
              paired = FALSE, alternative = "less") %>%
    broom::tidy(test_result) -> final[[i]] 
  
} 


as_data_frame(bind_rows(final, .id="id")) -> man_u_tests



man_u_tests$id <- unique(runtime_samples$size)
man_u_tests$test_result <- man_u_tests$p.value < .05

colnames(man_u_tests) <- c("input_size","statistic",
                           "p_value","method","alternative","median_runtime_zoo < median_runtime_data_table")


