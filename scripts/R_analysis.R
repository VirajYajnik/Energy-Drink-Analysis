install.packages("quantmod")
install.packages("openxlsx")
install.packages("tidyquant")
install.packages("alphavantager")
install.packages("httr")
install.packages("jsonlite")

library(quantmod)
library(openxlsx)
library(tidyquant)
library(alphavantager)
library(httr)
library(jsonlite)

#````````````````````````````````````````````````````````````````````````

getSymbols("MNST", src="yahoo", from="2014-10-01", to= "2024-10-02")
head(MNST)
View(MNST)

getSymbols("FIZZ", src="yahoo", from="2014-10-01", to= "2024-10-02")
head(FIZZ)
View(FIZZ)

getSymbols("PEP", src="yahoo", from="2014-10-01", to= "2024-10-02")
head(PEP)
View(PEP)

#``````````````````````````````````````````````````````````````````````````````

# Set the API key for Alpha Vantage
api_key <- "P9EFYGPPO5OC9WIT"
av_api_key(api_key)

# Function to get stock data from Yahoo Finance, sort by descending date, and save to Excel
get_stock_data <- function(symbol, from, to, filename) {
  getSymbols(symbol, src = "yahoo", from = from, to = to)
  stock_data <- get(symbol)
  stock_data_df <- data.frame(Date = index(stock_data), coredata(stock_data))
  
  # Sort the data from 2023 to 2014 (descending by Date)
  stock_data_df <- stock_data_df[order(stock_data_df$Date, decreasing = TRUE), ]
  
  write.xlsx(stock_data_df, file = filename)
  print(paste("Data has been successfully exported to", filename))
}

# Retrieve stock data for MNST, FIZZ, and PEP, sorted by date in descending order
get_stock_data("MNST", "2014-10-01", "2024-10-02", "MNST_data.xlsx")
get_stock_data("FIZZ", "2014-10-01", "2024-10-02", "FIZZ_data.xlsx")
get_stock_data("PEP", "2014-10-01", "2024-10-02", "PEP_data.xlsx")

# Function to get annual financial reports from Alpha Vantage, sort by descending date, and save to Excel
get_annual_reports <- function(symbol, filename) {
  url <- paste0("https://www.alphavantage.co/query?function=INCOME_STATEMENT&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  content_data <- content(response, as = "parsed")
  
  if (!is.null(content_data) && "annualReports" %in% names(content_data)) {
    annual_reports <- content_data$annualReports
    annual_reports_df <- do.call(rbind, lapply(annual_reports, as.data.frame))
    annual_reports_df$date <- as.Date(annual_reports_df$fiscalDateEnding)
    
    # Filter and sort data from 2023 to 2014
    last_10_years <- subset(annual_reports_df, date >= as.Date("2014-01-01") & date <= as.Date("2023-12-31"))
    last_10_years <- last_10_years[order(last_10_years$date, decreasing = TRUE), ]
    
    write.xlsx(last_10_years, file = filename)
    print(paste("Data has been successfully exported to", filename))
  } else {
    print(paste("No annual reports found in the response for", symbol))
  }
}

# Retrieve annual financial reports for MNST, FIZZ, and PEP, sorted by date in descending order
get_annual_reports("MNST", "MNST_annual_reports_last_10_years.xlsx")
get_annual_reports("FIZZ", "FIZZ_annual_reports_last_10_years.xlsx")
get_annual_reports("PEP", "PEP_annual_reports_last_10_years.xlsx")


#```````````````````````````````````````````````````````````

fizz_data <- read.xlsx("C://Users//viraj//OneDrive//Desktop//CAPSTONE RU//FIZZ_annual_reports_last_10_years.xlsx")
mnst_data <- read.xlsx("C://Users//viraj//OneDrive//Desktop//CAPSTONE RU//MNST_annual_reports_last_10_years.xlsx")
pep_data <- read.xlsx("C://Users//viraj//OneDrive//Desktop//CAPSTONE RU//PEP_annual_reports_last_10_years.xlsx")
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)  


# Add Company column to each dataset
fizz_data$Company <- "FIZZ"
mnst_data$Company <- "MNST"
pep_data$Company <- "PEP"

# Combine datasets
all_data <- bind_rows(fizz_data, mnst_data, pep_data)

# Convert relevant columns to numeric
numeric_columns <- c("totalRevenue", "grossProfit", "operatingIncome", "netIncome", "ebit", "ebitda")
all_data[numeric_columns] <- lapply(all_data[numeric_columns], as.numeric)

# Line Plot for Total Revenue Over Time
ggplot(all_data, aes(x = as.Date(fiscalDateEnding), y = totalRevenue, color = Company, group = Company)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Revenue Over Time", x = "Fiscal Date", y = "Total Revenue (in billions)") +
  scale_y_continuous(labels = function(x) paste0(comma(x / 1e9), "B")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heat map for correlation between Financial Metrics.
financial_data <- all_data %>%
  select(totalRevenue, grossProfit, operatingIncome, netIncome, ebit, ebitda) %>%
  mutate(across(everything(), as.numeric))
cor_matrix <- cor(financial_data, use = "complete.obs")
melted_cor <- melt(cor_matrix)
head(melted_cor)
                     
# Plotting Using color blind-friendly palette.
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradientn(colors = c("blue", "lightblue", "yellow", "orange", "red")) +
  labs(title = "Correlation Heatmap of Financial Metrics", x = "", y = "") +
  theme_minimal()
