library(DataAnalysis)

DAS_con = dbConnect(
  duckdb(),
  dbdir = here::here("../../Database/ReportingData.duckdb"),
  read_only = T
)


RT = tbl(DAS_con, "ReportingTable") %>% 
  collect()

dbDisconnect(DAS_con, shutdown = TRUE)


EWMA <- function(x, a, n) {
  N = length(x)
  f <- rep(NA, N)
  f[1] <- x[1]
  u = rep(NA, n)
  l = rep(NA, n)
  
  if (N > 1) {
    for (i in 2:N) {
      f[i] = a * x[i] + (1 - a) * f[i-1]
      
      
    }
  }
  u = f[N+1-n] + 3*sd(x)*sqrt(a/(n*(2-a))) 
  l = f[N+1-n] - 3*sd(x)*sqrt(a/(n*(2-a)))
  
  X = tibble(
    f = f,
    u = rep(u, N),
    l = rep(l, N)
  )
  
  return(X)
}


ewma = RT %>% 
  filter(DateReturned >= "2012-01-01",
         DateReturned < "2024-11-01",
         LIMS_Group == "FENT") %>% 
  mutate(Month = floor_date(DateReturned, "month")) %>% 
  count(Month, LIMS_Group) %>% 
  group_by(LIMS_Group) %>% 
  mutate(Fit = round(EWMA(n, 0.6, 24)$f, 3),
         UCL = round(EWMA(n, 0.6, 24)$u, 3),
         LCL = round(EWMA(n, 0.6, 24)$l, 3),
         Warning = if_else(n > Fit, 1, 0),
         Warning = frollapply(Warning, 5, sum),
         Warning = if_else(Warning >= 5, 1, 0),
         Green = if_else(n < Fit, 1, 0),
         Green = frollapply(Green, 5, sum),
         Green = if_else(Green >= 5, 1, 0),
         Red = if_else(Warning == 1 & n > UCL, 1, 0),
         Blue = if_else(Green == 1 & n < LCL, 1, 0),
         Low = if_else(n < LCL, 1, 0),
         Hi = if_else(n > UCL, 1, 0)) %>% 
  ungroup() %>%
  mutate(LCL = if_else(LCL < 0, 0, LCL))

ewma %>% 
  filter(Month >= "2022-11-01") %>% 
  ggplot(aes(x = Month))+
  geom_line(aes(y = Fit),
            color = "purple")+
  geom_line(aes(y = UCL),
            color = "blue")+
  geom_line(aes(y = LCL),
            color = "blue")+
  theme_minimal()

ewma %>% filter(LIMS_Group == "FENT") %>% summarize(v = sd(n))

