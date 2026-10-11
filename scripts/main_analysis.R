# Практичне заняття №2
# VaR та Expected Shortfall для AAPL і S&P 500

# пакети
pkgs <- c("quantmod", "PerformanceAnalytics", "ggplot2")
for (p in pkgs) {
if (!require(p, character.only = TRUE)) {
install.packages(p)
library(p, character.only = TRUE)
}
}

# параметри
tickers <- c("AAPL", "^GSPC")
to_date <- Sys.Date()
from_date <- seq(to_date, by = "-3 years", length.out = 2)[2]

if (!dir.exists("data")) dir.create("data")

results <- data.frame()

for (tk in tickers) {

# 1. дані з Yahoo Finance
px <- getSymbols(tk, src = "yahoo", from = from_date, to = to_date,
auto.assign = FALSE)
prices <- Ad(px)

# 2. логарифмічні дохідності, прибираємо NA
ret <- CalculateReturns(prices, method = "log")
ret <- na.omit(ret)

# 3. VaR і ES (значення виходять від'ємними - це збиток)
var_h95 <- as.numeric(VaR(ret, p = 0.95, method = "historical"))
var_h99 <- as.numeric(VaR(ret, p = 0.99, method = "historical"))
var_n95 <- as.numeric(VaR(ret, p = 0.95, method = "gaussian"))
var_n99 <- as.numeric(VaR(ret, p = 0.99, method = "gaussian"))
es_h95 <- as.numeric(ES(ret, p = 0.95, method = "historical"))

results <- rbind(results, data.frame(
Актив = tk,
Спостережень = length(ret),
Hist_VaR_95 = round(var_h95 * 100, 2),
Hist_VaR_99 = round(var_h99 * 100, 2),
Param_VaR_95 = round(var_n95 * 100, 2),
Param_VaR_99 = round(var_n99 * 100, 2),
ES_95 = round(es_h95 * 100, 2)
))

# 4. графік
df <- data.frame(r = as.numeric(ret))
lines_df <- data.frame(
x = c(var_h95, var_h99),
level = c("VaR 95%", "VaR 99%")
)

g <- ggplot(df, aes(x = r)) +
geom_histogram(aes(y = after_stat(density)), bins = 60,
fill = "grey75", color = "white") +
geom_vline(data = lines_df, aes(xintercept = x, color = level),
linewidth = 1) +
scale_color_manual(values = c("VaR 95%" = "orange", "VaR 99%" = "red")) +
labs(title = paste("Розподіл дохідностей та історичний VaR:", tk),
x = "Логарифмічна денна дохідність", y = "Щільність",
color = NULL) +
theme_minimal()

fname <- paste0("data/var_chart_", gsub("\\^", "", tk), ".png")
ggsave(fname, g, width = 8, height = 6, dpi = 120)
}

# результати
print(results)
write.csv(results, "data/risk_results.csv", row.names = FALSE)

# таблиця у форматі markdown - скопіювати в README.md
cat("\n| Актив | Hist VaR 95% | Hist VaR 99% | Param VaR 95% | Param VaR 99% | ES 95% |\n")
cat("|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(results))) {
cat(sprintf("| %s | %s%% | %s%% | %s%% | %s%% | %s%% |\n",
results$Актив[i], results$Hist_VaR_95[i], results$Hist_VaR_99[i],
results$Param_VaR_95[i], results$Param_VaR_99[i], results$ES_95[i]))
}
