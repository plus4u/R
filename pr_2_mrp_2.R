############################################################################################
###7/22,  7/8, 6/30, 6/16, 6/9, 6/3, 5/20, 5/13, 5/6, 4/28, 4/22, 4/7, 4/5, 4/2, 3/ 26
### MRP- 2 
######################################################
 
#### program-1 start : receipt_scheduled 필요한 SKU 확인 ########
### distinct cf: group_by  /   dataset-1 : no, sku, moq , po_lt 
### df_moq_polt <- read_excel("C:/Working/work_r/bom_0208.xlsx", sheet='Sheet1' )  
### t1 <- sales_net_req %>% select(SKU ) %>% group_by(SKU) %>% mutate( no = row_number())

# t1 <- sales_net_req_raw %>% filter( month=="0월")  
# t1 <- sales_net_req %>% filter( month=="0월")  

sales_net_req <- sales_net_req_raw %>% filter( sales_net_req_raw$SKU !="0")  # 281

df_moq_polt <- sales_net_req %>% distinct(SKU) %>% mutate( no = row_number()) 

### add  inventory : month = "0월"

t1 <- df_moq_polt %>% select( SKU) %>% mutate( month ='0월')

sales_net_req <- merge( x= sales_net_req, y= t1, by=c('SKU', 'month'), all = TRUE ) # 

### add order

t1 <- sales_net_req 
t1$order = 0


## 1. within  
t1 <- within(t1, order[ month=="0월" ] <- ( 1 ))
t1 <- within(t1, order[ month=="1월" ] <- ( 2 ))
t1 <- within(t1, order[ month=="2월" ] <- ( 3 ))
t1 <- within(t1, order[ month=="3월" ] <- ( 4 ))
t1 <- within(t1, order[ month=="4월" ] <- ( 5 ))
t1 <- within(t1, order[ month=="5월" ] <- ( 6 ))
t1 <- within(t1, order[ month=="6월" ] <- ( 7 ))
t1 <- within(t1, order[ month=="7월" ] <- ( 8 ))
t1 <- within(t1, order[ month=="8월" ] <- ( 9 ))
t1 <- within(t1, order[ month=="9월" ] <- ( 10 ))
t1 <- within(t1, order[ month=="10월" ] <- ( 11 ))
t1 <- within(t1, order[ month=="11월" ] <- ( 12 ))
t1 <- within(t1, order[ month=="12월" ] <- ( 13 )) 

### https://mustlearning.tistory.com/10  
## 2. replace

# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '0월', 1))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '1월', 2))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '2월', 3))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '3월', 4))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '4월', 5))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '5월', 6))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '6월', 7))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '7월', 8))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '8월', 9))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '9월', 10))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '10월', 11))
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '11월', 12)) 
# t1 <- t1 %>% mutate_at("order", ~replace(.,month== '12월', 13))

sales_net_req  <- t1 

### make template : SKU * month 5 months : 3 ~ 7 

i_max <- nrow( df_moq_polt )  #df_moq_polt %>% count( SKU )

df <- 0

for ( i in 1: i_max ) {
  v_SKU <- df_moq_polt %>% select( SKU) %>%  filter (df_moq_polt$no==i )
  v_SKU <- as.character( v_SKU)
  v1 <- rep( v_SKU, 13 )
  v2 <- c ( 1:13)
  t1 <- data.frame( v1,v2)
  # print(i)
  df <- rbind ( df, t1 )
}


names( df) <- c ('SKU', 'order') 

df <- df %>% filter( SKU !=0 )

t1 <- merge( x=df, y= sales_net_req, by=c('SKU', 'order' ) , all = TRUE)

t1 [ is.na( t1 )] <- 0

### 3 ~ 7
# t1 <- t1 %>% filter( order != 2 & order != 3 & order != 4 & order != 8 & order != 9 & order != 10 & order != 11 & order != 12 & order != 13 )

# t1 <- t1 %>% filter( order != 1 & order != 2 & order != 3 & order != 4 & order != 10 & order != 11 & order != 12 & order != 13 )

t1 <- t1 %>% filter( order != 2 & order != 3 & order != 4  & order != 5 )


sales_net_req <- t1 %>% group_by( SKU) %>%  mutate( no = row_number())

sales_net_req$order = sales_net_req$no


##############################
### start mrp 
####################################

v_SKU ="A-AIO-ADPT"

SKU_count <- count(sales_net_req , SKU)

v_no_min = min( df_moq_polt$no) 
v_no_max = max( df_moq_polt$no)
v1 = nrow( df_moq_polt )

### dataset-2 : demand, inventory, receipt_scheduled 

# sales_net_req <- read_excel("C:/1. Work/iKamper/1. OE_S&OP/재고조사/bom_0208.xlsx", sheet='test' ) # 
# sales_net_req <- read_excel("C:/Working/work_r/bom_0208.xlsx", sheet='test' ) # 

sales_net_req [ is.na(sales_net_req )] <- 0

sales_net_req$net_required <- 0
# sales_net_req$po_planned <- 0 

##################################### 
### start 

dx <- 0
dt <- 0
j <- 0 
sku_dx <- 0
sku_df <- 0

###### check  #################
# filter (sales_net_req, SKU == v_SKU & month ==i )
# j = as.numeric( dx$inventory_est ) 

###########################################

# for ( ii in 1: 1 ) {    # for-1  : forecasting period 
for ( ii in 1: v_no_max ) {    # for-1  : forecasting period 
  
  
  # for ( ii in 1: 1 ) {    # for-1  : forecasting period 
  
  v_SKU = as.character( df_moq_polt [ ii, "SKU" ] )
  #  v_moq = as.numeric ( df_moq_polt [ ii, "moq" ] )
  
  #### program-2 start : receipt_scheduled ########
  ### to gain po_planned  ## with demand, receipt_scheduled , inventory_est , net_required 
  
  ### inventory_est  예상 재고 누계 확인
  j = 0   ## inventory_est
  z = 0   ## net_required = demand - ( receipt_scheduled + inventory_est )
  m = 0
  n = 0   ## demand
  nn = 0
  dt <- 0 
  dx <- sales_net_req %>% select( -no) %>% filter (SKU == v_SKU & order ==1 )
  j = as.numeric( dx$inventory_est )  # 앞선 데이터의 예상 재고
  k = 0   ## if net_required 
  
  # i_max = 0 ## n of by SKU
  
  i_max <- as.numeric( SKU_count[ SKU_count$SKU == v_SKU , 'n'] )
  
  for ( i in 2: i_max ) {    # repeat 1~6 (i_max) time bucket  
    
    dt <- sales_net_req %>% select( -no) %>% filter (SKU == v_SKU & order ==i )
    
    if ( j< 0 ) { j = 0}   # inventory_est
    
    dt$inventory_est = j
    
    n = as.numeric( dt$demand ) # 수요가 있는 경우
    z = as.numeric( dt$demand - ( dt$receipt_scheduled + dt$inventory_est ) )  # 순수요량 계산
    
    ####
    if ( n > 0 & z < 0 ) {   # 수요가 있으면서 재고가 여유 
      dt$net_required = 0 
      j= as.numeric( ( dt$receipt_scheduled + dt$inventory_est ) - dt$demand )  # 예정입고 + 예상재고 - 수요 = 남은 예상 재고
      dt$inventory_est = j
      
    } else if ( n > 0 & z > 0 ){   # 수요가 있으면서 재고가 여유 없음
      nn = dt$demand - ( dt$receipt_scheduled + dt$inventory_est )  #  순수요량 계산
      dt$net_required = nn  
      dt$inventory_est = 0
      
    } else if ( n <= 0 ){  # 수요가 없는 경우,  receipt_scheduled 고려 
      # dt$inventory_est = j
      dt$inventory_est = j + dt$receipt_scheduled
      j = dt$inventory_est
      
    } else { 
      return 
    }
    #### 추출한 데이터를 계속 누적 
    
    dx <- rbind (dx, dt)
    k=as.numeric( dt$net_required )    # 순수요가 있는 경우, MOQ 확인하여 조치 
    
    if ( k > 0 ) {      
      ## moq %%%%%%%%%%%%%
      ## dx[ i+1, "po_planned"] = v_moq
      #    m = dx[ i+1, "net_required"]
      m = as.numeric( dt$net_required ) 
      
      ## dx[ i+1, "inventory_est"] = v_moq - m
      #    dx[ i+1, "inventory_est"] = v_moq - dt$net_required
      j = as.numeric( dx[ i, "inventory_est"] ) # 앞선 데이터의 예상 재고  
    } else {
      k = 0
    }
    
  }       ## for-1 end, start to append 
  
  dx <- as.data.frame( dx )
  sku_dx <- rbind ( sku_dx , dx )
  
  t_dx <- as.data.frame( t( dx) )  ### transfer 
  sku_df <- rbind ( sku_df , t_dx )
  
  #### program-2 end ########
  
} 


#### start post  

sku_dx <- sku_dx %>% filter( sku_dx$SKU !=0)

melt_sku_dx <- melt( sku_dx , id.vars = c ('SKU', 'order'), 
                     measure.vars = c ( 'demand', 'inventory_est', 'receipt_scheduled', 'net_required' ) )

# cast_sku_dx <- cast( melt_sku_dx, SKU + order ~ variable, sum)   #  

cast_sku_dx <- cast( melt_sku_dx, SKU + variable ~ order, sum)   #  


#####################################
### post MRP operation
### change row name  
#####################################

sku_dx <- sku_dx %>% filter( sku_dx$SKU !=0)

melt_sku_dx <- melt( sku_dx , id.vars = c ('SKU', 'order'), 
                     measure.vars = c ( 'demand', 'inventory_est', 'receipt_scheduled', 'net_required' ) )

# cast_sku_dx <- cast( melt_sku_dx, SKU + order ~ variable, sum)   #  

cast_sku_dx <- cast( melt_sku_dx, SKU + variable ~ order, sum)   # 

### net_required -> sub_total = m2~m6

d1 <- cast_sku_dx

d2 <- d1 %>% filter( variable =='net_required' )

# d3 <- as.data.frame ( apply( d2[ , 4:7], 1, sum ) )
d3 <- as.data.frame ( apply( d2[ , 4:11], 1, sum ) )

colnames(d3) <- "sub_total"

net_required_total <- cbind(d2,d3)

net_required_total <- net_required_total %>% filter( sub_total > 0) %>% select( SKU, variable, sub_total)

net_required_total <- merge( d1, net_required_total, by=c( 'SKU', 'variable'), all=TRUE ) 

net_required_total [ is.na( net_required_total )] <- 0

### merge SKU + Product

d1 <-  merge( x= net_required_total , y= master_product_SKU , by = "SKU", all.x=TRUE )  # 156 

colnames(d1) <- c( 'SKU', 'Item', 'now', 'm5', 'm6', 'm7','m8', 'm9', 'm10','m11','m12', 'sub_total', 'Product', 'color' )

cast_sku_dx <- d1

cast_sku_dx$no =0

cast_sku_dx[is.na( cast_sku_dx)] <- 0

t1 <- cast_sku_dx

t1 <- within(t1, no [ Item=="demand" ] <- ( 1 ))
t1 <- within(t1, no [ Item=="receipt_scheduled" ] <- ( 2 ))
t1 <- within(t1, no [ Item=="inventory_est" ] <- ( 3 ))
t1 <- within(t1, no [ Item=="net_required" ] <- ( 4 ))

cast_sku_dx <- t1 [ order(t1$SKU, t1$no ), ]

########################################################
### final 
########################################################

## change column order
