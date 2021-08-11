########################################################################################################
### 7/22, 7/14, 7/8, 6/30, 6/24, 6/16, 6/9, 6/3, 5/20, 5/13, 5/6, 4/28,  4/23, 4/22, 4/15, 4/7, 3/26
### getwd()
getwd()

ls()

rm( list=ls() )

library( readxl) 
library(reshape)
library( dplyr)
library(writexl)

### initial condition check

k = 'y' # if need module_month : demand_do_m , receipt_scheduled
# k = n


##############################################################################
### SKU, product master : 재고관리 유형을 추가하여 재고관리 여부 구분 type
### category , SKU , P_C , Product, inv_type  # category 는 추후 관리
### 아래 start 1 
################################################################################

### start step-1 : 

# d1 <- read_excel("data_04/0. mater_product_SKU.xlsx" ) 
d1 <- read_excel("0. mater_product_SKU.xlsx" ) 

master_product_SKU <- d1 %>% select(SKU, Product, Color) %>%  filter( !is.na(SKU))   # 156

master_product_SKU$inv_type = "inventory"

master_product_SKU [is.na(master_product_SKU$Color ),'Color' ] <- "any"

########## 재고관리 비 대상  확인  ########

d1 <- read_excel("0. master_재고제외.xlsx" )  ###  inv_type =n_inv 

### 재고관리 유형 추가
### left outer join 후에 by='key', all.x=TRUE 후에 inv_type = 'n_inv' 가 없는 경우 "y" 지정 
###    mater_product_SKU 에서 inv_type=n_iv 가 없는 것만 추출

master_product_SKU <- merge( x= master_product_SKU , y= d1 , by =c("SKU", "Product"), all.x=TRUE )  # 156
master_product_SKU <- master_product_SKU %>% filter( is.na(inv_type.y)) %>% select( SKU, Product, Color) # 124


######################################################################  
## 국내영업 SKU 단위로 필요한 필드만 추출 
## 값이 없는 것 삭제 : flier ( !is.na) / na.omit / complete.cases
######################################################################

sales_do_raw <- read_excel("snop_input.xlsx", sheet = "demand_do" ) # domestic sales demand  # 76

sales_do <- sales_do_raw %>% select ( type, SKU, month, qty )  


####################################
### if need use module_month 
### run module_month.r
### demand_do_m , receipt_scheduled
 
if ( k == "y" ) {
  source( "module_month.R")
} else {
    return()
  } 
 
## module_month end  ###############
###################################

sales_do_m[ sales_do_m$month=='3mon', 'month'] = '3월'
sales_do_m[ sales_do_m$month=='4mon', 'month'] = '4월'
sales_do_m[ sales_do_m$month=='5mon', 'month'] = '5월'
sales_do_m[ sales_do_m$month=='6mon', 'month'] = '6월'
sales_do_m[ sales_do_m$month=='7mon', 'month'] = '7월'
sales_do_m[ sales_do_m$month=='8mon', 'month'] = '8월'
sales_do_m[ sales_do_m$month=='9mon', 'month'] = '9월'
sales_do_m[ sales_do_m$month=='10mon', 'month'] = '10월'
sales_do_m[ sales_do_m$month=='11mon', 'month'] = '11월'
sales_do_m[ sales_do_m$month=='12mon', 'month'] = '12월'

po_m[ po_m$month=='3mon', 'month'] = '3월'
po_m[ po_m$month=='4mon', 'month'] = '4월'
po_m[ po_m$month=='5mon', 'month'] = '5월'
po_m[ po_m$month=='6mon', 'month'] = '6월'
po_m[ po_m$month=='7mon', 'month'] = '7월'
po_m[ po_m$month=='8mon', 'month'] = '8월'
po_m[ po_m$month=='9mon', 'month'] = '9월'
po_m[ po_m$month=='10mon', 'month'] = '10월' 
po_m[ po_m$month=='11mon', 'month'] = '11월'
po_m[ po_m$month=='12mon', 'month'] = '12월' 


###  

x <- sales_do_m %>% select( SKU, Fix, month, qty)
x$type ="any"
x$color ="any"
x$Product ="any"
x$week =0

sales_do_raw <- rbind( sales_do_raw, x)

####################################

sales_do <- sales_do_raw %>% select ( SKU, Product, month, qty ) %>%
  group_by( SKU, Product, month ) %>% summarise( qty=sum( qty) ) # 76

sales_do$sales = "domestic"   

sum(is.na(sales_do$qty)) # 51   
sales_do <- sales_do %>% filter( !is.na(qty)) # 25

## 해외 영업 
## 국내영업과 해외 영업 cf :  rbind  vs  outer join
## 해외 영업 grouping by SKU month

# sales_gs_raw <- read_excel("C:/1. Work/iKamper/1. OE_S&OP/재고조사/snop_input.xlsx", sheet = "demand_gs" ) # global sales demand # 1300
sales_gs_raw <- read_excel("snop_input.xlsx", sheet = "demand_gs" ) # global sales demand # 1300
# sales_gs_raw <- read_excel("C:/Working/work_r/snop_input.xlsx", sheet = "demand_gs" ) # global sales demand # 1300

sales_gs_raw$Product = sales_gs_raw$Description

# sales_gs <- sales_gs_raw %>% filter( month !="11월" | month !="12월" ) %>%  
#  group_by( SKU, Product, month ) %>% summarise( qty=sum( qty) ) # 345

sales_gs <- sales_gs_raw %>% filter( month !="3월" & month !="4월" & month !="1월" & month !="2월") %>%  
  group_by( SKU, Product, month ) %>% summarise( qty=sum( qty) ) # 218

sales_gs$sales = "global"    

sum(is.na(sales_gs$qty)) # 0

sales_gs <- sales_gs %>% filter( !is.na(qty))  #  

## rbind 로 처리 : 국내 영업, 해외 영업 

sales_do_gs <- rbind( sales_do, sales_gs)  # 419

####################################################
### sum ; sales_do_gs_tot = sales_do + sales_gs 

d1 <- merge( x= sales_do , y= sales_gs , by = c("SKU", "Product", "month"), all=TRUE )  #  386

## na 값 0으로 대체 : 

d1[is.na(d1)] <- 0

d1$qty = d1$qty.x + d1$qty.y

sales_do_gs_tot <- d1 %>% select(SKU, Product, month, qty) %>% group_by(SKU, month) %>% 
  summarise(qty=sum(qty)) # 386  -> 364

length(which(is.na( sales_do_gs_tot$qty))) # 353 --> 0



##########################################
### later : product with BOM ########



#########################################


### inventory , month = 0


inventory <- read_excel("snop_input.xlsx", sheet = "inventory" ) # stock # 61

inventory <- inventory %>% select( SKU, qty ) %>% filter( !is.na(qty)) %>%
  group_by( SKU) %>% summarise( qty=sum( qty) ) # 30

inventory$month = "0월"

## sales_do_gs_inv <- merge( x= cast_sales , y= inventory , by = "SKU", all=TRUE )  #  135 

####################################################
### po
### option-1 : po_scheduled
### option-2 : receipt_scheduled / from module_month  
#####################################################

if ( k == "y" ) {
  po_plan <- po_m %>% select( SKU, month, qty ) %>% filter( !is.na( qty)) %>% 
    group_by(SKU, month) %>% summarise( qty=sum( qty) ) # 
  
} else {
  po_plan_raw <- read_excel("snop_input.xlsx", sheet = "po_scheduled" ) # stock # 59
  # po_plan_raw <- read_excel("C:/1. Work/iKamper/1. OE_S&OP/재고조사/snop_input.xlsx", sheet = "receipt_scheduled" ) # stock # 59
  # po_plan_raw <- read_excel("C:/Working/work_r/snop_input.xlsx", sheet = "po_scheduled" )
  
  # 값이 있는 것만 추출 , 중요 필드만 추출 
  po_plan <- po_plan_raw %>% select( SKU, month, qty ) %>% filter( !is.na( qty)) %>% 
    group_by(SKU, month) %>% summarise( qty=sum( qty) ) # 59
  
}

### end  

### report descending ascending order

sales_do$order = "1_do"
sales_gs$order = "2_gs"
sales_do_gs_tot$order = "3_tot"
sales_do_gs_tot$sales = "total"

inventory$order ="4_inventory"
inventory$sales = "on_hand"

po_plan$order = "5_po"
po_plan$sales = "receipt"

### sales_do_gs 

d1 <- rbind(sales_do, sales_gs, sales_do_gs_tot, inventory, po_plan)

sales_do_gs <- d1[ order(d1$SKU, d1$month, d1$order), ]


###########################################################
## melt + cast  : month 변수 값을 이용 칼럼으로 활용
## SKU   Product   sales  1월 ~ 12월 
###########################################################

### check-1

d1 <- melt( sales_do , id.vars = c ('SKU', 'Product', 'sales', 'month' , 'order' ), measure.vars = 'qty') # melt : variable, value

d2 <- cast( d1, SKU + Product + sales + order ~ month, sum)  # cast 

### 1-end

melt_sales <- melt( sales_do_gs , id.vars = c ('SKU', 'Product', 'sales', 'month' , 'order' ), measure.vars = 'qty')

# x1 <- melt( sales_do_gs , id.vars = c ('SKU', 'Product', 'sales', 'order' ), measure.vars = 'month')

cast_sales <- cast( melt_sales, SKU + Product + sales + order ~ month, sum)   # 128

cast_sales <- cast_sales [ order( cast_sales$SKU, cast_sales$order), ]  # order

cast_sales <- cast_sales %>% select ( -Product )

d1 <-  merge( x= cast_sales , y= master_product_SKU , by = "SKU", all.x=TRUE )  # 156

cast_sales <- d1

# cast_sales <- cast_sales[ , c( 1, 2, 3, 4, 8, 9, 10, 11, 12, 5, 6, 7, 13, 14)]

cast_sales <- cast_sales[ , c( 1, 2, 3, 4, 8, 9, 10, 11, 5, 6, 7, 12, 13)]

#############################################################
### program-1 end 


### start : net_required

sales_net_req_raw <- sales_do_gs_tot %>% select(SKU, month, qty) %>% rename( demand=qty)

t1 <- merge( x=sales_net_req_raw , y=inventory, by=c('SKU', 'month'), all = TRUE) %>% 
  rename( inventory_est=qty) %>% select( -order, -sales)

sales_net_req_raw <- merge( x=t1 , y=po_plan, by=c('SKU', 'month'), all = TRUE) %>% 
  rename( receipt_scheduled=qty) %>% select( -order, -sales)

sales_net_req_raw [is.na (sales_net_req_raw)] <- 0  # 282

## remove SKU=0

sales_net_req <- sales_net_req_raw %>% filter( sales_net_req_raw$SKU !="0")  # 281

# sales_net_req [ sales_net_req$month=='0', ]

################################################ 
### net_required
### net_required_raw - end
################################################
 