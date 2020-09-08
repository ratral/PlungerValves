
  # Load libraries
  library("tidyverse")
  library("wcontrolvalve")
  library("here")

  # Basic data
  temp <- 15.00   # °C
  elev <- 2700    # m 
  flow <- 9396    # m3/h
  d    <- 1.000   # m
  d1   <- 1.4     # m (DN Pipe Upstream)
  d2   <- 1.2     # m (DN Pipe Downstream)
  
  adfactor <- 1.3  # Kv additional factor (Zuschlagfaktor)
  
  # Read data Wiesner WTP
  base_data <- readRDS(here::here("data", "results.rds"))
  scenarios <- readRDS(here::here("data", "scenarios.rds")) %>%
    select(-taf_il)
  
  ## Select only the values with the pumps on.
  base_data <- base_data %>% # head(11592) %>%
    select( "valve",  "scenarios_id", "Timestamp", "level",
            "p1", "p2", "flow" = "Flow" ) %>%
    mutate( flow = flow * 3.6) %>%    # convert flow from  l/s to m3/h
    mutate( p1 = p1/10, p2 = p2/10)   # convert pressure from m to bar 

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
  
  
  # base_data %>% filter(sig_1 == min(sig_1))
  # base_data %>% filter(sig_1 >= mean(sig_1))
  # base_data %>% filter(sig_1 == max(sig_1))
  
  data_analyse <- cylinder %>% 
    mutate(data = list(base_data)) %>% 
    unnest(data) %>% 
    mutate(kv_kvs = ifelse(kv * adfactor > kvs, NA, kv/kvs), position = 0)
  
  
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
    mutate( fl     = ifelse(kv * adfactor > kvs, NA, fl_function( position, kv.b, kv.d, kv.e, flps)),
            Sig_i  = ifelse(kv * adfactor > kvs, NA, Sigma_i( position,  kv.b,  kv.d,  kv.e, flps)),  # Incipient Cavitation
            Sig_c  = ifelse(kv * adfactor > kvs, NA, Sigma_c( position,  kv.b,  kv.d,  kv.e, flps)),  # Constant Cavitation
            Sig_mv = ifelse(kv * adfactor > kvs, NA, Sigma_mv( position,  kv.b,  kv.d,  kv.e, flps)), #  Maximum Vibration Cavitation
            regime = ifelse(kv * adfactor > kvs, NA, cavtation_regime(position,  kv.b,  kv.d,  kv.e, flps, sig_1)))
  
  
  data_analyse <- data_analyse %>%
    group_by(typ, kv.b, kv.d, kv.e, zvs, kvs, fls, fps, flps) %>% 
    nest()
  
  