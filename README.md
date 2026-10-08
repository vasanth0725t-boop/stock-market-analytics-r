# Stock Market Analytics & Forecasting Dashboard

## Overview
This project is a professional R Shiny dashboard designed for stock market analysis and simplified forecasting. It uses synthetic stock data to demonstrate how R can be used for interactive financial analytics without relying on external APIs or real-time market feeds.

## Features
- Interactive overview dashboard with KPI cards
- Stock explorer with date filtering and price analysis
- Performance analysis with return and risk comparisons
- Simplified 7-, 14-, and 30-day forecasting demo
- Dataset viewer with search, sorting, and download support
- Modern financial dashboard styling and responsive layout

## Dashboard Pages
- Dashboard
- Stock Explorer
- Performance Analysis
- Forecast
- Dataset
- About

## Technologies
- R
- Shiny
- bslib
- ggplot2
- dplyr
- plotly
- DT
- scales
- lubridate

## Dataset
The project uses simulated stock market data for five companies:
- TCS
- INFY
- RELIANCE
- HDFCBANK
- ITC

Columns in the dataset:
- Date
- Stock
- Open
- High
- Low
- Close
- Volume
- Daily_Return
- Market_Cap

## Forecasting Method
The forecast page uses a lightweight educational approach based on simple linear regression on recent price history. This is intended for demonstration purposes only and is not a financial advisory model.

## Project Structure
```text
stock-market-analytics-r/
├── data/
│   └── stock_data.csv
├── app.R
├── generate_data.R
├── requirements.R
├── README.md
└── www/
    └── style.css
```

## Installation
1. Open R or RStudio.
2. Install the required packages:
   ```r
   source("requirements.R")
   ```
3. Generate the synthetic data:
   ```r
   source("generate_data.R")
   ```

## How to Run
From the project folder:

```r
shiny::runApp()
```

## Screenshots Placeholder
Add screenshots here once the app is run locally.

## Limitations
- Uses synthetic data only
- Forecasting is intentionally simplified
- Not connected to live stock market feeds
- Intended for educational and portfolio analysis demonstration

## Future Improvements
- Add more stock symbols
- Add technical indicators
- Improve forecast modeling in a classroom-friendly way
- Add more interactive filters and export features

## Disclaimer
The dashboard is designed for educational demonstration and should not be treated as financial advice or a real trading system.

## Author
**Sanjeev Kumar**  
AI & Data Science Student
