# 5. Create Model Diagnostic Plots ----

# Init xpose_data object
xpdb <- xposeNlmeModel(model = models$OneCpt,
                       fitmodelOutput = fit_output$OneCpt)

# 5.1 DV PREDs vs IDV ----

p8 <- dv_preds_vs_idv(
  xpdb,
  facets = c("variable", "DOSEGRP"),
  type = "ps",
  smooth_method = "loess",
  smooth_span = 0.75,
  smooth_linetype = "solid",
  smooth_color = "#D63636",
  smooth_size = 1.2,
  point_shape = 16,
  point_color = "#757D8F",
  point_alpha = 0.5,
  point_size = 1L,
  point_stroke = 1,
  line_color = "#000000",
  line_alpha = 0.5,
  line_size = 1L,
  line_linetype = "solid",
  scales = "fixed"
)

ggsave("./plots/one_cmpt_dv_preds_idv.png", plot = p8)

# 5.2 CWRES vs PRED ----
p9 <-
  res_vs_pred(
    xpdb,
    type = "ps",
    facets = "DOSEGRP",
    scales = "free_x",
    subtitle = "-2LL: @ofv, Eps shrink: @epsshk",
    smooth_method = "loess",
    smooth_span = 0.75,
    smooth_linetype = "solid",
    smooth_color = "#D63636",
    smooth_size = 1.2,
    point_shape = 16,
    point_color = "#757D8F",
    point_alpha = 0.5,
    point_size = 1L,
    point_stroke = 1,
    line_color = "#000000",
    line_alpha = 0.5,
    line_size = 1L,
    line_linetype = "solid"
  )

ggsave("./plots/one_cmpt_res_vs_pred.png", plot = p9)



