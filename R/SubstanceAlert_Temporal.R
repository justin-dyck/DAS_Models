library(DataAnalysis)

DAS_con = dbConnect(
  duckdb(),
  dbdir = here::here("../../Database/ReportingData.duckdb"),
  read_only = T
)


RT = tbl(DAS_con, "ReportingTable") %>% 
  collect()

dbDisconnect(DAS_con, shutdown = TRUE)