# Reference: Lesson 4

# 3. Descriptive Statistics Tables ----

# * 3.1 Demographic summary table using gt ----
# the distinct function provides all unique rows of specified variables - in this case 1 row per individual.
# Note that a dataset with time varying covariates would require a different approach
# to isolate the unique rows to use for a demography summary table
dm_table <- finaldat %>%
  distinct(ID,AGE,SEX,RACE,WT) %>%
  select(AGE,SEX,RACE,WT) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})") %>%
  add_n() %>%
  as_flex_table() %>%
  bold(part="header")

save_as_image(dm_table, path = "./tables/dmtable.png")


# * 3.2 Concentration summary by time and dose group using flextable ----
# gtsummary allows only one grouping column, we'll use dplyr for more
# complex data summary operations, then convert summary data.frame to a flextable

mean_conc_dosegrp_time_tbl <- finaldat %>%
  select(TIME, CONC, DOSEGRP) %>%
  #filter(TIME > 0) %>%  #Generally would include in table if PK collected pre-dose
  group_by(DOSEGRP, TIME) %>%
  summarise(CONC_mean = signif(mean(CONC), 3),
            CONC_min = signif(min(CONC), 3),
            CONC_max = signif(max(CONC), 3)) %>%
  mutate(CONC_combined_stat = paste0(CONC_mean, " (", CONC_min, "-", CONC_max, ")")) %>%
  select(DOSEGRP, TIME, CONC_combined_stat) %>%
  pivot_wider(names_from = "DOSEGRP", values_from = "CONC_combined_stat") %>%
  flextable(cwidth = 1.5) %>%
  set_header_labels(`TIME` = "Time (hr)",
                    `5000` = paste0("5000  ","\U03BC","g"),
                    `10000` = paste0("10000 ","\U03BC","g"),
                    `20000` = paste0("20000 ","\U03BC","g")) %>%
  add_header_row(colwidths = c(1,3), values = c("", "Dose Group")) %>%
  set_caption("Mean Concentration by Dose Group and Time", ) %>%
  align(align = "center", part = "all") %>%
  bold(part="header")

save_as_image(mean_conc_dosegrp_time_tbl, path = "./tables/mean_conctable.png")


# 4. Exploratory Data Analysis ----

# Init custom ggplot2 theme
theme_certara <-
  function (base_size = 11,
            base_family = "",
            base_line_size = base_size / 22,
            base_rect_size = base_size / 22,
            grid = c("none", "horizontal",
                     "both"))
  {
    grid <- match.arg(grid)
    half_line <- base_size / 2
    theme_c <-
      theme(
        line = element_line(
          colour = "black",
          linewidth = base_line_size,
          linetype = 1,
          lineend = "butt"
        ),
        rect = element_rect(
          fill = "white",
          colour = "black",
          linewidth = base_rect_size,
          linetype = 1
        ),
        text = element_text(
          family = base_family,
          face = "plain",
          colour = "black",
          size = base_size,
          lineheight = 0.9,
          hjust = 0.5,
          vjust = 0.5,
          angle = 0,
          margin = margin(),
          debug = FALSE
        ),
        axis.line = element_blank(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = element_text(size = rel(0.9),
                                 colour = "grey40"),
        axis.text.x = element_text(margin = margin(t = 0.8 *
                                                     half_line /
                                                     2), vjust = 1),
        axis.text.x.top = element_text(margin = margin(b = 0.8 *
                                                         half_line /
                                                         2), vjust = 0),
        axis.text.y = element_text(margin = margin(r = 0.8 *
                                                     half_line /
                                                     2), hjust = 0.5),
        axis.text.y.right = element_text(
          angle = -90,
          margin = margin(l = 0.8 * half_line /
                            2),
          hjust = 0.5
        ),
        axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
        axis.ticks.length = unit(half_line, "pt"),
        axis.ticks.length.x = unit(half_line,
                                   "pt"),
        axis.ticks.length.x.top = unit(half_line,
                                       "pt"),
        axis.ticks.length.x.bottom = unit(half_line,
                                          "pt"),
        axis.ticks.length.y = unit(half_line, "pt"),
        axis.ticks.length.y.left = unit(half_line, "pt"),
        axis.ticks.length.y.right = unit(half_line,
                                         "pt"),
        axis.title = element_text(colour = "grey40"),
        axis.title.x = element_text(margin = margin(t = half_line /
                                                      2),
                                    vjust = 1),
        axis.title.x.top = element_text(margin = margin(b = half_line / 2),
                                        vjust = 0),
        axis.title.y = element_text(
          angle = 90,
          margin = margin(r = half_line /
                            2),
          vjust = 1
        ),
        axis.title.y.right = element_text(
          angle = -90,
          margin = margin(l = half_line /
                            2),
          vjust = 0
        ),
        legend.background = element_rect(colour = NA),
        legend.spacing = unit(2 * half_line, "pt"),
        legend.spacing.x = unit(half_line,
                                "pt"),
        legend.spacing.y = NULL,
        legend.margin = margin(half_line,
                               half_line, half_line, half_line),
        legend.key = element_rect(fill = "white",
                                  colour = "NA"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(
          size = rel(0.9),
          margin = margin(r = 2 * half_line, unit = "pt")
        ),
        legend.text.align = NULL,
        legend.title = element_text(size = rel(0.9),
                                    hjust = 0),
        legend.title.align = NULL,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.box = NULL,
        legend.box.margin = margin(0, 0, 0,
                                   0, "cm"),
        legend.box.background = element_blank(),
        legend.box.spacing = unit(2 * half_line, "pt"),
        panel.background = element_rect(fill = "white",
                                        colour = NA),
        panel.border = element_rect(
          fill = NA,
          colour = "grey60",
          linewidth = 0.3
        ),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(fill = "grey90",
                                        colour = "grey20"),
        strip.text = element_text(
          colour = "grey30",
          size = rel(0.8),
          margin = margin(0.3 * half_line,
                          0.3 * half_line, 0.5 * half_line, 0.3 * half_line)
        ),
        strip.text.x = NULL,
        strip.text.y = element_text(angle = -90),
        strip.placement = "inside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = unit(half_line / 2,
                                     "pt"),
        strip.switch.pad.wrap = unit(half_line / 2,
                                     "pt"),
        plot.background = element_rect(colour = "white"),
        plot.title = element_text(
          size = rel(1.2),
          hjust = 0,
          vjust = 1,
          margin = margin(b = half_line)
        ),
        plot.subtitle = element_text(
          hjust = 0,
          vjust = 1,
          margin = margin(b = half_line)
        ),
        plot.caption = element_text(
          size = rel(0.8),
          hjust = 1,
          vjust = 1,
          margin = margin(t = half_line)
        ),
        plot.tag = element_text(
          size = rel(1.2),
          hjust = 0.5,
          vjust = 0.5
        ),
        plot.tag.position = "topleft",
        plot.margin = margin(half_line,
                             half_line, half_line, half_line),
        complete = TRUE
      )
    if (grid == "horizontal") {
      theme_c <-
        theme_c %+replace% theme(
          panel.grid.major.y = element_line(colour = "grey90",
                                            linewidth = 0.3),
          panel.grid.minor.y = element_line(colour = "grey90",
                                            linewidth = 0.3)
        )
    }
    else if (grid == "both") {
      theme_c <-
        theme_c %+replace% theme(
          panel.grid.major = element_line(colour = "grey90",
                                          linewidth = 0.3),
          panel.grid.minor = element_line(colour = "grey90",
                                          linewidth = 0.3)
        )
    }
    theme_c
  }

# * 4.1 Time-Concentration by Subject ----
p1 <- finaldat %>%
  mutate(ID = as.factor(ID)) %>%
  ggplot(aes(TIME,CONC,group=ID)) +
  geom_line() +
  geom_point() +
  theme_certara()

ggsave("./plots/time_concentration_by_subject.png", plot = p1)

# * 4.2 Time-Concentration by Subject Faceted by Dose Group ----
p2 <- p1 + facet_wrap( ~ DOSEGRP) + theme_certara()

ggsave("./plots/time_concentration_by_subject_facet_dose_group.png", plot = p2)

# * 4.3 Time-Concentration by Gender & Dose Group ----
p3 <- p2 + aes(color=SEX) + theme_certara()

ggsave("./plots/time_concentration_facet_dose_group.png", plot = p3)

# * 4.4 Time-Concentration by Subject Faceted by Dose Group & Gender ----
p4 <- p1 + facet_wrap(DOSEGRP~SEX,ncol=2) + theme_certara()

ggsave("./plots/time_concentration_by_subject_facet_dose_group_gender.png", plot = p4)

# * 4.5 Time-Concentration by Subject Faceted by Dose Group & Race ----
p5 <- p1 + facet_wrap(DOSEGRP~RACE) + theme_certara() #facet synonymous with lattice

ggsave("./plots/time_concentration_by_subject_facet_dose_group_race.png", plot = p5)

# * 4.6 Time-Concentration by Subject Faceted by Equal WT Intervals ----
p6 <- p1 + facet_wrap(~cut(WT,3)) + theme_certara()  #group into 3 equal "cuts" of WT

ggsave("./plots/time_concentration_by_subject_by_wt_cut.png", plot = p6)

# * 4.7 Time-Concentration by Subject Faceted by Age Quantiles ----
p7 <- p1 + facet_wrap(~cut(AGE,quantile(AGE),include.lowest=TRUE)) + theme_certara()#group into quartile "cuts"

ggsave("./plots/time_concentration_by_subject_by_age_quantile.png", plot = p7)


