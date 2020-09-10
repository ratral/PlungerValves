

library(tidyverse)
library(wcontrolvalve)


temp <- 15.00   # °C
elev <- 2700    # m 
flow <- 9396    # m3/h
d    <- 1.000   # m
d1   <- 1.4     # m (DN Pipe Upstream)
d2   <- 1.2     # m (DN Pipe Downstream)

# base_data Pressures (bar)
base_data <- tribble(
  ~measurement, ~p1,   ~p2, ~flow,
  "Min.",      1.720, 0.614,  9396,
  "Mean",      1.410, 0.684,  9396,
  "Max.",      1.370, 1.009,  9396
) 

# Cylinder parameter/Characteristics

cylinder <- tribble(
  ~typ,     ~kv.b,  ~kv.d,   ~kv.e,   ~zvs,  ~fls,
  "typ_01", -2.393,	1.389,  67.422,	 1.780,	0.617, # 0
  "typ_02", -3.893,	1.146,  61.038,  3.000,	0.598, # 20-30
  "typ_03", -2.755, 1.979,  99.215, 15.830, 0.775  # 40
) 

cylinder <- cylinder %>% 
  mutate( kvs  = kv_value(d, zvs)) %>% 
  mutate( fps  = fp(kvs, d*1000, d1*1000, d2*1000)) %>% 
  mutate( flps = flp(kvs, fls, d*1000, d1*1000, d2*1000)) %>% 
  select( typ, kv.b, kv.d, kv.e, kvs, zvs,  fls, fps, flps)

# Calculation characteristics

base_data <- base_data %>% 
  mutate( dp = (p1 - p2),
          kv = kv(p1, p2, flow, temp)) %>% 
  mutate( zeta  = zeta_vaule(d, kv),
          sig_1 = sigma_1(p1, p2, elev, temp)) 

data_analyse <- cylinder %>% 
  mutate(data = list(base_data)) %>% 
  unnest(data) %>% 
  mutate(kv_kvs = ifelse(kv > kvs, NA, kv/kvs), position = 0)

for(i in c(1:length(data_analyse$kv_kvs))){
  if(is.na(data_analyse$kv_kvs[i])){
    data_analyse$position[i] <- NA
  } else {
    data_analyse$position[i] <- inv_LL3(data_analyse$kv_kvs[i], 
                                        data_analyse$kv.b[i],
                                        data_analyse$kv.d[i],
                                        data_analyse$kv.e[i])
  }
}

data_analyse <- data_analyse %>% 
  mutate( flp    = ifelse(kv > kvs, NA, fl_function( position, kv.b, kv.d, kv.e, flps)),
          Sig_i  = ifelse(kv > kvs, NA, Sigma_i( position,  kv.b,  kv.d,  kv.e, flps)),  # Incipient Cavitation
          Sig_c  = ifelse(kv > kvs, NA, Sigma_c( position,  kv.b,  kv.d,  kv.e, flps)),  # Constant Cavitation
          Sig_mv = ifelse(kv > kvs, NA, Sigma_mv( position,  kv.b,  kv.d,  kv.e, flps)), #  Maximum Vibration Cavitation
          regime = ifelse(kv > kvs, NA, cavtation_regime(position,  kv.b,  kv.d,  kv.e, flps, sig_1)))


data_analyse <- data_analyse %>%
  group_by(typ, kv.b, kv.d, kv.e, zvs, kvs, fls, fps, flps) %>% 
  nest()


#---------------------------------------------
i <- 1

data_points <- data_analyse$data[[i]] %>%  
  select( kv_kvs, position) %>% 
  rename( x = position, y = kv_kvs )

segment_data <- tibble ( x    = data_points$x,
                         y    = data_points$y*0,
                         xend = data_points$x,
                         yend = data_points$y) %>% 
                add_row( x    = data_points$x*0,
                         y    = data_points$y,
                         xend = data_points$x,
                         yend = data_points$y)



i <- 1

data_points <- data_analyse$data[[i]] %>% 
  select( position, Sig_i) %>% 
  rename( x = position, y = Sig_i )  




