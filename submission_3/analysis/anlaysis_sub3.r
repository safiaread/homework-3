#Analysis Workspace 

#Loading in cleaned dataframe
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest)
data <- readRDS("/Users/safiaread/Desktop/homework_3/data/output/TaxBurden_Data.rds")

#Finding changes in tax and prices of cigarettes, also putting in 2012 dollars

data <- data %>% group_by(state)%>%
arrange(state, Year)%>%
mutate(tax_change = tax_state-lag(tax_state), 
tax_change_d = ifelse(tax_change == 0,0,1), 
price_cpi_2022 = cost_per_pack*(cpi_2012/index), 
tax_cpi_2022 = tax_dollar*(cpi_2012/index),
ln_tax_2012 = log(tax_cpi_2022),
ln_sales = log(sales_per_capita),
ln_price = log(price_cpi_2022))

View(data)

#Question 1
q1 <- data %>%
filter(Year <= 1985)%>%
group_by(Year)%>%
summarise(proportion = mean(tax_change_d, na.rm = TRUE))

q1_graph <- ggplot(q1, aes(x = Year, y = proportion)) + 
geom_bar(stat = "identity") + 
labs(
    x="Year",
    y="Proportion",
    title="Annual Proportion of States with Changes in Cigarette Tax"
  )

# Question 2
#Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price 
#of a pack of cigarettes from 1970 to 2018.

q2 <- data %>%
filter(Year <= 2018)%>%
group_by(Year)%>%
summarise(mean_price = mean(price_cpi_2022, na.rm = TRUE), mean_tax = mean(total_tax_cpi_2022, na.rm = TRUE))

figure_q2 <- ggplot(q2, aes(x = Year))+
geom_line(aes(y = mean_price), color = "red")+
geom_line(aes(y = mean_tax), color = "blue")

#Question 3
#Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time 
#period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

changes_in_price <- data %>%
select(state, Year, price_cpi_2022)%>%
filter(Year <= 2018)%>%
pivot_wider(names_from = Year, values_from = price_cpi_2022)%>%
mutate(price_change = `2018`-`1970`)%>%
select(state, price_change)%>%
arrange(desc(price_change))

head(changes_in_price, n = 5)

target <- c("District of Columbia","New York","Rhode Island","Hawaii", "Massachusetts") %>% paste(collapse = "|")

q3 <- data %>%
filter(Year <= 2018)%>%
filter(str_detect(state, target))%>%
group_by(Year)%>%
summarise(mean_sales = mean(sales_per_capita, na.rm = TRUE))

figure_q3 <- ggplot(q3, aes(x = Year, y = mean_sales)) + 
geom_bar(stat = "identity")

# Question 4
arrange(changes_in_price, by = price_change)%>%
head(n=5)

target_2 <- c("Missouri","Tennessee","North Dakota","Alabama", "Georgia") %>% paste(collapse = "|")

q4 <- data %>%
filter(Year <= 2018)%>%
filter(str_detect(state, target_2))%>%
group_by(Year)%>%
summarise(mean_sales = mean(sales_per_capita, na.rm = TRUE))

figure_q4 <- ggplot(q4, aes(x = Year, y = mean_sales)) + 
geom_line()

#Question 6
q6 <- data %>%
filter(Year <= 1990)

summary(lm(ln_sales ~ ln_price, data = q6))

#Question 7
summary(feols(ln_sales ~ 1 | ln_price ~ ln_tax_2012, 
             data=q6))

#Question 8
step1 <- lm(ln_price ~ ln_tax_2012, data=q6)
pricehat <- predict(step1)
step2 <- lm(ln_sales ~ pricehat, data=q6)
summary(step2)

#Question 9

q9_1 <- data %>%
filter(Year >= 1991 & Year <= 2015)%>%
group_by(Year)%>%
summarise(count_tax_change = sum(tax_change_d, na.rm = TRUE))

ggplot(q9_1, aes(x = Year, y = count_tax_change)) + 
geom_bar(stat = "identity")

q9_2 <- data %>%
filter(Year >= 1991 & Year <= 2015)%>%
group_by(Year)%>%
summarise(mean_price = mean(price_cpi_2022, na.rm = TRUE), mean_tax = mean(total_tax_cpi_2022, na.rm = TRUE))

ggplot(q9_2, aes(x = Year))+
geom_line(aes(y = mean_price), color = "red")+
geom_line(aes(y = mean_tax), color = "blue")

changes_in_price_2 <- data %>%
select(state, Year, price_cpi_2022)%>%
filter(Year >= 1991 & Year <= 2015)%>%
pivot_wider(names_from = Year, values_from = price_cpi_2022)%>%
mutate(price_change = `2015`-`1991`)%>%
select(state, price_change)%>%
arrange(desc(price_change))

head(changes_in_price_2, n = 5)

target_3 <- c("New York","Massachusetts","Alaska","Hawaii","Rhode Island") %>% paste(collapse = "|")

q9_3 <- data %>%
filter(Year >= 1991 & Year <= 2015)%>%
filter(str_detect(state, target_3))%>%
group_by(Year)%>%
summarise(mean_sales = mean(sales_per_capita, na.rm = TRUE))

ggplot(q9_3, aes(x = Year, y = mean_sales)) + 
geom_line()

arrange(changes_in_price_2, by = price_change)%>%
head(n=5)

target_4 <- c("North Dakota","Missouri","Georgia","California","Tennessee") %>% paste(collapse = "|")

q9_4 <- data %>%
filter(Year >= 1991 & Year <= 2015)%>%
filter(str_detect(state, target_2))%>%
group_by(Year)%>%
summarise(mean_sales = mean(sales_per_capita, na.rm = TRUE))

ggplot(q9_4, aes(x = Year, y = mean_sales)) + 
geom_line()

#Question 9 alternate
q7 <- data %>%
filter(Year >= 1991 & Year <= 2015)

summary(lm(ln_sales ~ ln_price, data = q7))

summary(feols(ln_sales ~ 1 | ln_price ~ ln_tax_2012, 
             data=q7))
step1 <- lm(ln_price ~ ln_tax_2012, data=q7)
pricehat <- predict(step1)
step2 <- lm(ln_sales ~ pricehat, data=q7)
summary(step2)

rm(list=c("data","q1", "service",
           "penetration", "premiums", "full_service", "pen_full_serv", "state_rename", "all_data")) # nolint
save.image("submission3/Hwk3_workspace.Rdata")