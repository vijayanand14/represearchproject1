interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
  ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
  interval[which.max(interval$steps),]
  
