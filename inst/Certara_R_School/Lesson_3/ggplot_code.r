library(ggquickeda)
library(ggplot2)

sample_data <- as.data.frame(ggquickeda::sample_data)

ggplot(sample_data,
       aes(x = Time, y = Conc,
           color = cut_interval(Weight,2)
       )
) +
  geom_point(size = 2.4, alpha = 0.5) +
  facet_grid(cut_interval(Weight,2) ~ table1::eqcut(Age,2)) +
  scale_color_manual("Weight",values = tableau10)+
  theme_bw(base_size = 22) +
  theme(legend.position = "bottom")