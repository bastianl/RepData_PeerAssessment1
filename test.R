by_day = sapply(split(df, df$date), function(x) {
  sum(x$steps, na.rm=T)
})