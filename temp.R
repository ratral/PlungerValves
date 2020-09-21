
temp <- 15.00   # Upstream water temperature in centigrade (°C)
elev <- 2700    # Elevation above sea level - in meter - 
flow <- 9396    # Flow in m3/h
d    <- 1.000    # Diameter of the valve in meter
d1   <- 1.400    # Diameter of the Pipe Upstream in meter
d2   <- 1.200    # Diameter of the Pipe Downstream in meter

base_data <- tribble(
  ~measurement, ~p1,   ~p2, ~flow,
  "Min.",      1.720, 0.614,  9396,
  "Mean",      1.410, 0.684,  9396,
  "Max.",      1.370, 1.009,  9396
) 

cylinder <- tribble(
  ~typ,         ~b,    ~d,      ~e,   ~zvs,  ~fls,
  "typ_01", -2.393,	1.389,  67.422,	 1.780,	0.617, # 0
  "typ_02", -3.893,	1.146,  61.038,  3.000,	0.598, # 20-30
  "typ_03", -2.755, 1.979,  99.215, 15.830, 0.775  # 40
) 


valve <- tribble(
  ~name,         ~b,    ~d,      ~e,   ~zvs,  ~fls,
  "valve_01", -2.393,	1.389,  67.422,	 1.780,	0.617, # 0
  "valve_02", -3.893,	1.146,  61.038,  3.000,	0.598, # 20-30
  "valve_03", -2.755, 1.979,  99.215, 15.830, 0.775  # 40
) 

cylinder <- cylinder %>% 
  mutate( kvs  = kv_value(d, zvs)) %>% 
  mutate( fps  = fp(kvs, d*1000, d1*1000, d2*1000)) %>% 
  mutate( flps = flp(kvs, fls, d*1000, d1*1000, d2*1000)) %>% 
  mutate( flps_fps = flps/fps) %>% 
  select( typ, b, d, e, kvs, zvs, fls, fps, flps, flps_fps)

valve <- valve %>% 
  mutate( kvs  = kv_value(d, zvs)) %>% 
  mutate( fps  = fp(kvs, d*1000, d1*1000, d2*1000)) %>% 
  mutate( flps = flp(kvs, fls, d*1000, d1*1000, d2*1000)) %>% 
  mutate( flps_fps = flps/fps) %>% 
  select( name, b, d, e, kvs, zvs,  fls, fps, flps, flps_fps)

((1*1000)^2)/sqrt(626.3*1.78)
((1*1000)^2)/sqrt(626.3*3.00)
((1*1000)^2)/sqrt(626.3*15.83)

