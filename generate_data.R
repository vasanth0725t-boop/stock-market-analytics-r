set.seed(2024)

stocks <- list(
  TCS = list(base_price = 3200, mean_volume = 600000, volatility = 0.016),
  INFY = list(base_price = 1450, mean_volume = 520000, volatility = 0.018),
  RELIANCE = list(base_price = 2450, mean_volume = 680000, volatility = 0.019),
  HDFCBANK = list(base_price = 1550, mean_volume = 480000, volatility = 0.014),
  ITC = list(base_price = 420, mean_volume = 430000, volatility = 0.015)
)

start_date <- as.Date("2022-01-03")
all_dates <- seq(start_date, by = "day", length.out = 780)
all_dates <- all_dates[weekdays(all_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]

data_list <- list()

for (stock_name in names(stocks)) {
  stock_meta <- stocks[[stock_name]]
  price <- rep(NA_real_, length(all_dates))
  open_price <- rep(NA_real_, length(all_dates))
  high_price <- rep(NA_real_, length(all_dates))
  low_price <- rep(NA_real_, length(all_dates))
  close_price <- rep(NA_real_, length(all_dates))
  volume <- rep(NA_real_, length(all_dates))
  daily_return <- rep(NA_real_, length(all_dates))
  market_cap <- rep(NA_real_, length(all_dates))

  price[1] <- stock_meta$base_price
  open_price[1] <- stock_meta$base_price
  close_price[1] <- stock_meta$base_price
  high_price[1] <- stock_meta$base_price * (1 + runif(1, 0.002, 0.008))
  low_price[1] <- stock_meta$base_price * (1 - runif(1, 0.002, 0.008))
  volume[1] <- max(1000, round(rnorm(1, stock_meta$mean_volume, stock_meta$mean_volume * 0.25)))
  daily_return[1] <- 0
  market_cap[1] <- close_price[1] * sample(seq(5000000, 120000000, by = 500000), 1)

  for (i in 2:length(all_dates)) {
    previous_close <- close_price[i - 1]
    drift <- rnorm(1, mean = 0.0006, sd = stock_meta$volatility)
    new_close <- previous_close * (1 + drift)
    new_open <- previous_close

    spread <- runif(1, 0.003, 0.015)
    new_high <- max(new_open, new_close) * (1 + spread)
    new_low <- min(new_open, new_close) * (1 - spread)

    if (new_low <= 0) {
      new_low <- min(new_open, new_close) * 0.995
    }

    new_volume <- max(1000, round(rnorm(1, stock_meta$mean_volume, stock_meta$mean_volume * 0.3)))

    open_price[i] <- new_open
    close_price[i] <- new_close
    high_price[i] <- new_high
    low_price[i] <- new_low
    volume[i] <- new_volume
    daily_return[i] <- (new_close / new_open) - 1
    market_cap[i] <- new_close * sample(seq(5000000, 120000000, by = 500000), 1)
  }

  stock_df <- data.frame(
    Date = all_dates,
    Stock = stock_name,
    Open = round(open_price, 2),
    High = round(high_price, 2),
    Low = round(low_price, 2),
    Close = round(close_price, 2),
    Volume = as.integer(volume),
    Daily_Return = round(daily_return, 6),
    Market_Cap = round(market_cap, 2)
  )

  data_list[[stock_name]] <- stock_df
}

final_df <- do.call(rbind, data_list)
final_df <- final_df[order(final_df$Date, final_df$Stock), , drop = FALSE]
row.names(final_df) <- NULL

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

write.csv(final_df, file = file.path("data", "stock_data.csv"), row.names = FALSE)

message("Synthetic stock dataset generated successfully with ", nrow(final_df), " rows.")
