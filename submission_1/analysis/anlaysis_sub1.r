group_by(state)%>%
arrange(state, Year)%>%
mutate(tax_change = tax_state-lag(tax_state), 
tax_change_d = ifelse(tax_change == 0,0,1), 
price_cpi_2022 = cost_per_pack*(cpi_2022/index), 
# for tax : price_cpi_2022 = cost_per_pack*(cpi_2022/index)
ln_tax_2012 = log(total_tax_cpi_2022),
ln_sales
ln_price
