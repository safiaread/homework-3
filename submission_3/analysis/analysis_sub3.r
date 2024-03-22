#Analysis Workspace 

#Loading in cleaned dataframe
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest)
data <- readRDS("submission_3/data-code/TaxBurden_Data.R")

#Finding changes in tax and prices of cigarettes, also putting in 2012 dollars

data <- data %>% group_by(state) %>%
arrange(state, Year) %>%
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
summarise(mean_price = mean(price_cpi_2022, na.rm = TRUE), mean_tax = mean(tax_cpi_2022, na.rm = TRUE))

q2_graph <- ggplot(q2, aes(x = Year))+
geom_line(aes(y = mean_price, color = "red"))+
geom_line(aes(y = mean_tax, color = "blue", linetype = "dashed"))+
labs(
    x="Year",
    y="Average Price",
    title="Average Price and Cigarette Tax Over Time") + scale_color_manual(values = c("red", "blue"), labels = c("Price in 2012 Dollars", "Average Tax"),name = " ") + 
    guides(linetype = "none")

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

q3_1 <- data %>%
filter(Year <= 2018)%>%
filter(str_detect(state, target))

q3_graph <- ggplot(q3_1, aes(x = Year, y = sales_per_capita, color = state)) + 
geom_line()+
labs(
    x="Year",
    y="Average Sales Per Capita",
    title="Average Sales for States with the 5 Highest Increases in Tax")

# Question 4
#arrange(changes_in_price, by = price_change)%>%
#head(n=5)

target_2 <- c("Missouri","Tennessee","North Dakota","Alabama", "Georgia") %>% paste(collapse = "|")

q4 <- data %>%
filter(Year <= 2018)%>%
filter(str_detect(state, target_2))%>%
group_by(Year)%>%
summarise(mean_sales = mean(sales_per_capita, na.rm = TRUE))

q4_2 <- data %>%
filter(Year <= 2018)%>%
filter(str_detect(state, target_2))

q4_graph <- ggplot(q4_2, aes(x = Year, y = sales_per_capita, color = state)) + 
geom_line()+
labs(
    x="Year",
    y="Average Sales Per Capita",
    title="Average Sales for States with the 5 Lowest Increases in Tax")

#Question 5
target_3 <- c("District of Columbia","New York","Rhode Island","Hawaii", "Massachusetts","Missouri","Tennessee","North Dakota","Alabama", "Georgia") %>% paste(collapse = "|")

q5 <- data %>%
filter(Year <= 2018) %>%
filter(str_detect(state, target_3)) %>%
mutate(tax_group = ifelse(str_detect(state, target), "highest", "lowest")) %>%
group_by(Year, tax_group) %>%
summarise(avg_sales = mean(sales_per_capita))

q5_graph <- ggplot(data = q5, aes(x = Year, y = avg_sales, color = tax_group))+
geom_line()+
labs(x = "Year",
    y = "Average Sales Per Capita",
    title = "Average Sales for States with the Highest and Lowest Tax Increases")

#Question 6
q6 <- data %>%
filter(Year <= 1990)

##q6_reg <- summary(lm(ln_sales ~ ln_price, data = q6))

q6_1 <- coef(feols(ln_sales ~ ln_price, data = q6))
q6_reg_coeff <- q6_1[2]
##q6_reg_coeff <- q6_reg$coefficients[1,1]

#Question 7
q7 <- coef(summary(feols(ln_sales ~ 1 | ln_price ~ ln_tax_2012, data=q6)))
q7_reg <- q7[2]

#Question 8
step1 <- feols(ln_price ~ ln_tax_2012, data = q6)
step1_1 <- coef(step1)
step1_reg <- step1_1[2]
##step1_reg <- summary(step1)$coefficients[1,1]
pricehat <- predict(step1)
step2 <- feols(ln_sales ~ pricehat, data=q6)
step2_1 <- coef(step2)
step2_reg <- step2_1[2]
##step2_reg <- summary(step2)$coefficients[1,1]

#Question 9

q9 <- data %>%
filter(Year >= 1991 & Year <= 2015)
#q9_reg1 <- summary(lm(ln_sales ~ ln_price, data = q9))$coefficients[1,1]
q9_1 <- coef(feols(ln_sales ~ ln_price, data = q9))
q9_reg1 <- q9_1[2]

q9_2 <- coef(feols(ln_sales ~ 1 | ln_price ~ ln_tax_2012, data=q9))
q9_reg2 <- q9_2[2]

q9_step1 <- feols(ln_price ~ ln_tax_2012, data=q9)
q9_step1_1 <- coef(q9_step1)
q9_step1_reg <- q9_step1_1[2]
pricehat <- predict(q9_step1)
q9_step2 <- feols(ln_sales ~ pricehat, data=q9)
q9_step2_1 <- coef(q9_step2)
q9_step2_reg <- q9_step2_1[2]

regression_table <- data.frame(
  time_period = c("1970-1980", "1991-2015"),
  ols_estimate = c(q6_reg_coeff, q9_reg1),
  IV_estimate = c(q7_reg, q9_reg2),
  IV_stage_1 = c(step1_reg, q9_step1_reg),
  IV_stage_2 = c(step2_reg, q9_step2_reg)
)

rm(list=c("data", "q1", "q2", "changes_in_price", "q3", "q3_1", "q4", "q4_2", "q5", "q6", "q9")) # nolint
save.image("/Users/safiaread/Desktop/homework_3/submission_3/Hwk3_workspace.Rdata")