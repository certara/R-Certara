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

#ggsave("./plots/one_cmpt_dv_preds_idv", plot = p8)
save_files(p8, name = "one_cmpt_dv_preds_idv", type = "plot")
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

#ggsave("./plots/one_cmpt_res_vs_pred", plot = p9)
save_files(p9, name = "one_cmpt_res_vs_pred", type = "plot")


# * Theta Table ----
tabletheta <- xpdb %>%
  get_prmNlme() %>%
  filter(type == "the") %>%
  select(-type, -diagonal, -n) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "2.5% CI", "97.5% CI")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI")) %>%
  set_caption(caption = "Table Theta") %>%
  colformat_double(digits = 2L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

save_files(tabletheta, name = "one_cmpt_theta", type = "table")

# * Omega Table ----
tableomega <- xpdb %>%
  get_prmNlme() %>%
  filter(type == "ome") %>%
  select(-type) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  left_join(Certara.ModelResults::get_eta_shk(xpdb), by = "label") %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "fixed", "diagonal", "2.5% CI", "97.5% CI", "shrinkage%")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", fixed = "Fixed", diagonal = "Diagonal", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI", `shrinkage%` = "Shrinkage%")) %>%
  set_caption(caption = "Table Omega") %>%
  colformat_double(digits = 3L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

save_files(tableomega, name = "one_cmpt_omega", type = "table")

# * Sigma Table ----
tablesigma <- xpdb %>%
  get_prmNlme() %>%
  filter(type == "sig") %>%
  select(-type) %>%
  mutate(`rse%` = as.numeric(rse) * 100) %>%
  left_join(Certara.ModelResults::get_eps_shk(xpdb), by = "label") %>%
  flextable(col_keys = c("name", "label", "value", "se", "rse%", "fixed", "2.5% CI", "97.5% CI", "shrinkage%")) %>%
  set_header_labels(values = list(name = "Name", label = "Label", value = "Value", se = "SE", `rse%` = "RSE%", fixed = "Fixed", `2.5% CI` = "2.5% CI", `97.5% CI` = "97.5% CI", `shrinkage%` = "Shrinkage%")) %>%
  set_caption(caption = "Table Sigma") %>%
  colformat_double(digits = 3L) %>%
  align(align = "left", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

save_files(tablesigma, name = "one_cmpt_sigma", type = "table")



