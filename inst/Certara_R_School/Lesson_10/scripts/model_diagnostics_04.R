# Reference: Lesson 4
# 5. Create Model Diagnostic Plots ----

# Init xpose_data object
xpdb <- xposeNlmeModel(model = models$OneCpt,
                       fitmodelOutput = fit_output$OneCpt)

# 5.1 DV PREDs vs IDV ----

p8 <- dv_preds_vs_idv(
  xpdb,
  facets = c("variable", "DOSEGRP"),
  type = "ps",
  smooth_color = "#D63636",
  point_color = "#757D8F",
  point_alpha = 0.5,
  point_size = 2,
  scales = "fixed"
) + theme_certara()

ggsave("./plots/one_cmpt_dv_preds_idv.png", plot = p8)

# 5.2 CWRES vs PRED ----
p9 <-
  res_vs_pred(
    xpdb,
    type = "ps",
    facets = "DOSEGRP",
    scales = "free_x",
    subtitle = "-2LL: @ofv, Eps shrink: @epsshk",
    smooth_color = "#D63636",
    point_color = "#757D8F",
    point_alpha = 0.5,
    point_size = 2,
    scales = "fixed"
  ) +
  theme_certara()

ggsave("./plots/one_cmpt_res_vs_pred.png", plot = p9)



