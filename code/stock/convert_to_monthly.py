import pandas as pd

# 1. Load the daily data
daily_path = r"KSE100_2000_2025_combined_sorted.xlsx"
df_daily = pd.read_excel(daily_path)

# 2. Ensure Date is datetime and sorted
df_daily["Date"] = pd.to_datetime(df_daily["Date"])
df_daily = df_daily.sort_values("Date").set_index("Date")

# 3. Resample to monthly (month-end) with OHLC-style aggregation
monthly = pd.DataFrame({
    "Open":  df_daily["Open"].resample("M").first(),
    "High":  df_daily["High"].resample("M").max(),
    "Low":   df_daily["Low"].resample("M").min(),
    "Close": df_daily["Price"].resample("M").last(),   # treat Price as close
    "Price": df_daily["Price"].resample("M").last(),   # keep a Price column too
    "Vol.":  df_daily["Vol."].resample("M").last(),    # simple choice for volume
})

# 4. Add monthly return based on Close
monthly["Monthly_Return"] = monthly["Close"].pct_change()

# 5. Bring Date back as a column
monthly = monthly.reset_index()

# 6. Save to Excel
output_path = r"KSE100_2000_2025_monthly.xlsx"
monthly.to_excel(output_path, index=False)
