#make a df of acid rain history dates (CAA, etc.) #https://daattali.com/shiny/timevis-demo/
timeline_data <- data.frame(
  id = 1:8,
  content = c("Span of HBEF dataset",
              "Air Pollution Control Act",
              "Clean Air Act of 1963",
              "Air Quality Act of 1967",
              "EPA founded", 
              "Clean Air Act",
              "1977 Clean Air Act Amendments",
              "1990 Clean Air Act Amendments"),
  title = c("Watershed 6 is displayed in this story, as it is the control",
            "Research funding, first federal legislation on air pollution",
            "Research developing, national program made",
            "Expanded research, interstate pollution policies",
            "The EPA was founded to enforce the Clean Air Act",
            "The Clean Air Act of 1970",
            "National Ambient Air Quality standards improvement",
            "Amendment that more specifically addressed acid rain"),
  start = c("1957-06-01",
            "1955-01-01",
            "1963-01-01",
            "1967-01-01",
            "1970-12-02", 
            "1970-06-01",
            "1977-01-01",
            "1990-06-01"),
  end = c("2014-05-01", NA, NA, NA, NA, NA, NA, NA)
)

#save as RDS file
saveRDS(timeline_data, file="timeline_data.rds")
timeline_data <- readRDS("timeline_data.rds")
