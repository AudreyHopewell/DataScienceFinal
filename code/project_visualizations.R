# visualizing the relationship between revenue and rating
ggplot(data = clean_movies, aes(x=vote_average, y=revenue)) +
  geom_jitter() +
  geom_hline(yintercept = mean_rev) +
  geom_vline(xintercept = mean_rating) +
  labs(x = "Average IMDb rating", y = "Box office revenue") +
  theme_light() +
  annotate(geom = "text", x=1, y=200000000, label="mean revenue") +
  annotate(geom = "text", x = 7.4, y = 2300000000, label = "mean rating")



