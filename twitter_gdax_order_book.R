library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(httr)
library(jsonlite)
library(scales)
library(twitteR)

################################################################################
##################### GET & GRAPH ORDER BOOK DATA FROM GDAX ####################
################################################################################

# Get Current Time
time <- Sys.time()
time <- format(time, "%A %B %d %Y at %H:%M %Z")

# Call GDAX Order Book API
endpoint <- '/products/BTC-USD/book?level=3'
url <- paste0('https://api.gdax.com', endpoint)
raw_result <- GET(url)
raw_content <- rawToChar(raw_result$content)
content <- fromJSON(raw_content)

# Extract current order book from API data
bids <- data.frame(content$bids[,1:2])
colnames(bids) <- c('Price', 'Volume')
asks <- data.frame(content$asks[,1:2])
colnames(asks) <- c('Price', 'Volume')

# Convert all data types to nueric
bids <- mutate_all(bids, function(x) as.numeric(as.character(x)))
asks <- mutate_all(asks, function(x) as.numeric(as.character(x)))

# Aggregate all bids and asks with the same prices
bids <- bids %>% group_by(Price) %>% summarise(Volume = sum(Volume))
asks <- asks %>% group_by(Price) %>% summarise(Volume = sum(Volume))

# Calculate cumulative volume for orderbook
bids$Cum_Volume <- rev(cumsum(rev(bids$Volume)))
asks$Cum_Volume <- cumsum(asks$Volume)

# Filter to a reasonable range
max_bid <- max(bids$Price)
min_ask <- min(asks$Price)
bids_in_range <- filter(bids, Price >= 0.95 * max_bid)
asks_in_range <- filter(asks, Price <= 1.05 * min_ask)

# Graph orderbook
output <- '/home/michael/dev/twitter-gdax-orders/Current_Order_Book.png'
png(filename = output, width = 1600, height = 800, res = 200)
ggplot(bids_in_range) + 
    geom_area(aes(x = Price, y = Cum_Volume), color = '#88F56E', fill = '#88F56E', alpha = 0.25) +
    geom_area(data = asks_in_range, aes(x = Price, y = Cum_Volume), color = '#FC6A42', fill = '#FC6A42', alpha = 0.25) +
    geom_line(aes(x = Price, y = Cum_Volume), color = '#88F56E') +
    geom_line(data = asks_in_range, aes(x = Price, y = Cum_Volume), color = '#FC6A42') +
    scale_x_continuous(labels = dollar, expand = c(0, 0)) + 
    labs(title = 'BTC-USD Order Book',
         subtitle = paste0('GDAX Order Book on ', time),
         x = 'Price',
         y = 'Bitcoin Volume',
         caption = '@GDAX_Order_Book
@Michael_Toth') +
    theme_ipsum(axis_title_size = 14) +
    theme(panel.background=element_rect(fill = '#1F2B34', color = NA),
          plot.background=element_rect(fill = '#1F2B34', color = NA),
          axis.title = element_text(color = 'white'),
          axis.text = element_text(color = 'white'),
          title = element_text(color = 'white'),
          panel.grid.major = element_line(color = 'white'))
dev.off()

# Calculate Metrics
current_price <- dollar((max_bid + min_ask) / 2)
bid_ask_spread <- dollar(min_ask - max_bid)
liquidation_volume <- comma(sum(bids$Volume))


################################################################################
############################# CREATE AND SEND TWEET ############################
################################################################################

# Set up Twitter API
source('~/twitter_creds.R')
setup_twitter_oauth(consumer_key = gdax_order_book$consumer_key,
                    consumer_secret = gdax_order_book$consumer_secret,
                    access_token = gdax_order_book$access_token,
                    access_secret = gdax_order_book$access_token_secret)

time <- Sys.time()

# Create Tweet
tweet_text <- 
paste0('Current Bitcoin Price: ', current_price, '
Bid-Ask Spread: ', bid_ask_spread, '
Number of Bitcoin (Sales) Required to Liquidate Order Book: ', liquidation_volume)

# Send Tweet
tweet(tweet_text, mediaPath = output)
