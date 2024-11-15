library(rjags)
library(DataAnalysis)

DAS_con = dbConnect(
  duckdb(),
  dbdir = here::here("../../Database/ReportingData.duckdb"),
  read_only = T
)


RT = tbl(DAS_con, "ReportingTable") %>% 
  collect()

dbDisconnect(DAS_con, shutdown = TRUE)

ModelData = RT %>% 
  filter(LIMS_Group %in% c("FENT", "PFLFENT", "CARFENT"),
         DateReturned >= "2022-01-01",
         DateReturned < "2024-11-01") %>% 
  mutate(Month = floor_date(DateReturned, "month")) %>% 
  count(Month, LIMS_Group) %>% 
  pivot_wider(names_from = "LIMS_Group", values_from = n) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))


ModelFit <- jags.model(file = "BUGS/Pois_SS_MV.txt",
                       data = list(Y = t(ModelData %>% select(-Month)),
                                   n = length(ModelData)-1,
                                   N = length(ModelData$FENT),
                                   Y1 = ModelData[1,2]),
                       n.chains = 2,
                       inits = list(#inv.q = 1,
                                    #u = 0,
                                    inv.g = 1,
                                    v = rep(0, length(ModelData)-1)))

Samples = coda.samples(ModelFit, 
                       variable.names = c("g", "v", "EY"), 
                       n.iter = 100000)
coda::traceplot(Samples)

Fit = summary(window(Samples, start = 10000))


Fit_Q = as_tibble(Fit$quantiles, rownames = "Statistic")


ModelData_Out = full_join(
  
  ModelData %>% 
    pivot_longer(cols = where(is.numeric), names_to = "LIMS", values_to = "n"),
  
  Fit_Q %>% 
    filter(str_sub(Statistic,1,2) == "EY") %>% 
    mutate(LIMS = colnames((ModelData %>% select(-Month)))[as.numeric(str_sub(Statistic, 4, 4))]) %>% 
    mutate(Month = rep(seq(min(ModelData$Month), max(ModelData$Month), by = "month"), each = length(ModelData) - 1)) %>% 
    select(-Statistic)
  
) %>% 
  mutate(`2.5%` = if_else(`2.5%` < 0, 0, `2.5%`),
         `25%` = if_else(`25%` < 0, 0, `25%`))
  
ggplot(ModelData_Out)+
  geom_line(aes(x = Month,
                y = `50%`))+
  geom_point(aes(x = Month,
                 y = n))+
  geom_ribbon(aes(x = Month,
                  ymin = `25%`,
                  ymax = `75%`),
              color = "lightblue",
              fill = "lightblue",
              alpha = 0.7)+
  geom_ribbon(aes(x = Month,
                  ymin = `2.5%`,
                  ymax = `97.5%`),
              color = "lightblue",
              fill = "lightblue",
              alpha = 0.5)+
  facet_wrap(~LIMS, scales = "free_y")+
  theme_minimal()
