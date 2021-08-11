#####################################################################
### 6/24  5/26 : 6월부터 시작 , 4/28, 4/15, 4/5, 3/ 22
### reshape month variables into a month
### 


d1 <- data.frame( read_excel("snop_input.xlsx", sheet = "demand_do_m" ) ) #   


# d2 <- d1 %>% select( SKU, Fix, m3, m4, m5, m6, m7, m8) %>%

d2 <- d1 %>% select( SKU, Fix, m7, m8, m9, m10, m11, m12) %>%
  filter( !is.na(SKU)) %>% group_by( SKU ) #  

# sales_do_m <- d2 %>% select( SKU, Fix, m3) %>% mutate ( month="3mon" , qty=m3 )  %>% select(SKU, Fix, month, qty )

sales_do_m <- d2 %>% select( SKU, Fix, m7) %>% mutate ( month="7mon" , qty=m7 )  %>% select(SKU, Fix, month, qty )

# sales_do_m <- rbind ( sales_do_m, d3 )

# d3 <- d2 %>% select( SKU, Fix, m5) %>% mutate ( month="5mon" , qty=m5 )  %>%  select(SKU, Fix, month, qty )
# sales_do_m <- rbind ( sales_do_m, d3 )

# d3 <- d2 %>% select( SKU, Fix, m6) %>% mutate ( month="6mon" , qty=m6 )  %>% select(SKU, Fix, month, qty )
# sales_do_m <- rbind ( sales_do_m, d3 )

# d3 <- d2 %>% select( SKU, Fix, m7) %>% mutate ( month="7mon" , qty=m7 )  %>% select(SKU, Fix, month, qty )
# sales_do_m <- rbind ( sales_do_m, d3 )

d3 <- d2 %>% select( SKU, Fix, m8) %>% mutate ( month="8mon" , qty=m8 )  %>% 
  select(SKU, Fix, month, qty )

sales_do_m <- rbind ( sales_do_m, d3 )

d3 <- d2 %>% select( SKU, Fix, m9) %>% mutate ( month="9mon" , qty=m9 )  %>% 
  select(SKU, Fix, month, qty )

sales_do_m <- rbind ( sales_do_m, d3 )

d3 <- d2 %>% select( SKU, Fix, m10) %>% mutate ( month="10mon" , qty=m10 )  %>% 
  select(SKU, Fix, month, qty )

sales_do_m <- rbind ( sales_do_m, d3 )

d3 <- d2 %>% select( SKU, Fix, m11) %>% mutate ( month="11mon" , qty=m11 )  %>% 
  select(SKU, Fix, month, qty )

sales_do_m <- rbind ( sales_do_m, d3 )

d3 <- d2 %>% select( SKU, Fix, m12) %>% mutate ( month="12mon" , qty=m12 )  %>% 
  select(SKU, Fix, month, qty )


sales_do_m <- rbind ( sales_do_m, d3 )

# table ( is.na( sales_do_m$qty ))

sales_do_m <-  sales_do_m %>% filter( !is.na ( qty)) 

### convert : month --> m3~ m7

sales_do_m$value = sales_do_m$qty 

sales_do_month <- cast( sales_do_m, SKU + Fix ~ month, sum)   # 46

####################################
### end domestic sales 
### start below : receipt_scheduled

d1 <- data.frame( read_excel("snop_input.xlsx", sheet = "receipt_scheduled" ) ) #   

# d2 <- d1 %>% select( SKU, m5, m6, m7, m8, m9, m10, m11, m12) %>%

d2 <- d1 %>% select( SKU, m7, m8, m9, m10, m11, m12) %>%
  filter( !is.na(SKU)) %>% group_by( SKU ) #  

# po_m <- d2 %>% select( SKU, m5) %>% mutate ( month="5mon" , qty=m5 )  %>% select(SKU, month, qty )

po_m <- d2 %>% select( SKU, m7) %>% mutate ( month="7mon" , qty=m7 )  %>% select(SKU, month, qty )

# d3 <- d2 %>% select( SKU, m4) %>% mutate ( month="4mon" , qty=m4 )  %>% select(SKU, month, qty )
# po_m <- rbind ( po_m, d3 )

# d3 <- d2 %>% select( SKU, m5) %>% mutate ( month="5mon" , qty=m5 )  %>% select(SKU, month, qty )

# po_m <- rbind ( po_m, d3 )

# d3 <- d2 %>% select( SKU, m7) %>% mutate ( month="6mon" , qty=m6 )  %>% select(SKU, month, qty )
# po_m <- rbind ( po_m, d3 )
# d3 <- d2 %>% select( SKU, m7) %>% mutate ( month="7mon" , qty=m7 )  %>% select(SKU, month, qty )
# po_m <- rbind ( po_m, d3 )

d3 <- d2 %>% select( SKU, m8) %>% mutate ( month="8mon" , qty=m8 )  %>% 
  select(SKU, month, qty )

po_m <- rbind ( po_m, d3 )
 
d3 <- d2 %>% select( SKU, m9) %>% mutate ( month="9mon" , qty=m9 )  %>% 
  select(SKU, month, qty )

po_m <- rbind ( po_m, d3 )

d3 <- d2 %>% select( SKU, m10) %>% mutate ( month="10mon" , qty=m10 )  %>% 
  select(SKU, month, qty )

po_m <- rbind ( po_m, d3 )

d3 <- d2 %>% select( SKU, m11) %>% mutate ( month="11mon" , qty=m11 )  %>% 
  select(SKU, month, qty )

po_m <- rbind ( po_m, d3 )

d3 <- d2 %>% select( SKU, m12) %>% mutate ( month="12mon" , qty=m12 )  %>% 
  select(SKU, month, qty )

po_m <- rbind ( po_m, d3 )

# table ( is.na( sales_do_m$qty ))

po_m <-  po_m %>% filter( !is.na ( qty)) 

### convert : month --> m3~ m7

po_m$value = po_m$qty 

po_month <- cast( po_m, SKU ~ month, sum)   # 46


### end 
####################################