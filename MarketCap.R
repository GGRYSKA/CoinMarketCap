library(coinmarketcapr)
library(ggplot2)

market_today <- get_marketcap_ticker_all()

market_today$percent_change_24h <- as.numeric(market_today$percent_change_24h)
market_today$percent_change_7d <- as.numeric(market_today$percent_change_7d)
market_today$X24h_volume_usd <- as.numeric(market_today$X24h_volume_usd)
market_today$available_supply <- as.numeric(market_today$available_supply)
totalvol <- sum(na.omit(market_today$X24h_volume_usd))
market_today$volratio <-market_today$X24h_volume_usd/totalvol

df <- head(market_today,35)
df1 <- subset(market_today, select = c(symbol, volratio))
df1 <- df1[order(-df1$volratio),] 
df1 <- head(df1, 10)
df$X24h_volume_usd

g <- ggplot(df, aes(percent_change_7d, percent_change_24h)) +
  geom_point(aes(size=volratio),position = position_jitter(w = 0.1, h = 0)) + 
  geom_text(aes(label=symbol), hjust=-0.15, vjust=-0.15) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  ggtitle("Cryptocurrency Movers") +
  labs(x="7-day Pct Chg", y="24hr Pct Chg") 

plot(g)

b <-ggplot(data=df1, aes(x=reorder(df1$symbol, -df1$volratio),y=df1$volratio)) +
       geom_bar(stat="identity") + 
       ggtitle("Cryptocurrency 24hr Volume as Pct of Total") +
       labs(x="Cryptocurrency", y="Percentage of Total Volume") +
       scale_y_continuous(labels = scales::percent) +
       geom_text(aes(label=format(100*df1$volratio, digits = 2)),vjust=-0.5)

plot(b)
