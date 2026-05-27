#########################################
######    Deterministic Analysis   ######    
#########################################

# =======================================
#      National Health Service (NHS)
# =======================================

deterministic_nhs <- dec_tree(params = input_nhs, n_cohort = 1000)

cost_breakdown_nhs <- deterministic_nhs %>%
  dplyr::select(Strategy, contains("Cost")) %>%
  pivot_longer(
    cols = contains("Cost"),
    names_to = "Item",
    values_to = "Cost"
  ) %>%
  mutate(
    Item = case_match(
      Item,
      "Cost_Screening" ~ "Screening",
      "Cost_Biopsy"    ~ "Biopsy",
      "Total_Cost"     ~ "Total",
      .default = Item
    ) 
  ) %>%
  filter(Item %in% c("Screening", "Biopsy", "Total"))
  

cost_breakdown_nhs$Item <- factor(
  cost_breakdown_nhs$Item, 
  levels = c("Screening", "Biopsy", "Total")
)

plot_costs_nhs <- ggplot(cost_breakdown_nhs, aes(x = Item, y = Cost, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  geom_text(aes(label = dollar(Cost, prefix = "£", accuracy = 0.01)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 2, fontface = "bold", color = "#333333") +
  scale_fill_manual(name = NULL, values = c("Arm 1: Usual Care" = "#8C8781", "Arm 2: Lunit AI" = "#D98C75")) +
  scale_y_continuous(labels = dollar_format(prefix = "£"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Average Cost per Patient - NHS Perspective", y = "Cost (£)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#404040", size = 14),
    legend.position = "top",
    axis.text.x = element_text(face = "bold", color = "#404040", size = 8),
    panel.grid.major.x = element_blank()
  )

plot_costs_nhs


# =======================================
#         Healthcare Providers
# =======================================

deterministic_provider <- dec_tree(params = input_provider, n_cohort = 1000)

cost_breakdown_provider <- deterministic_provider %>%
  dplyr::select(Strategy, contains("Cost")) %>%
  pivot_longer(
    cols = contains("Cost"),
    names_to = "Item",
    values_to = "Cost"
  ) %>%
  mutate(
    Item = case_match(
      Item,
      "Cost_Screening" ~ "Screening",
      "Cost_Biopsy"    ~ "Biopsy",
      "Total_Cost"     ~ "Total",
      .default = Item
    ) 
  ) %>%
  filter(Item %in% c("Screening", "Biopsy", "Total"))


cost_breakdown_provider$Item <- factor(
  cost_breakdown_provider$Item, 
  levels = c("Screening", "Biopsy", "Total")
)

plot_costs_provider <- ggplot(cost_breakdown_provider, aes(x = Item, y = Cost, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  geom_text(aes(label = dollar(Cost, prefix = "£", accuracy = 0.01)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 2, fontface = "bold", color = "#333333") +
  scale_fill_manual(name = NULL, values = c("Arm 1: Usual Care" = "#8C8781", "Arm 2: Lunit AI" = "#D98C75")) +
  scale_y_continuous(labels = dollar_format(prefix = "£"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Average Cost per Patient - Provider Perspective", y = "Cost (£)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#404040", size = 14),
    legend.position = "top",
    axis.text.x = element_text(face = "bold", color = "#404040", size = 8),
    panel.grid.major.x = element_blank()
  )

plot_costs_provider

  geom_text(aes(label = dollar(Cost, prefix = "£", accuracy = 0.01)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 2, fontface = "bold", color = "#333333") +
  scale_fill_manual(name = NULL, values = c("Arm 1: Usual Care" = "#8C8781", "Arm 2: Lunit AI" = "#D98C75")) +
  scale_y_continuous(labels = dollar_format(prefix = "£"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Average Cost per Patient - NHS Perspective", y = "Cost (£)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#404040", size = 14),
    legend.position = "top",
    axis.text.x = element_text(face = "bold", color = "#404040", size = 8),
    panel.grid.major.x = element_blank()
  )

plot_costs_nhs


# =======================================
#         Healthcare Providers
# =======================================

deterministic_provider <- dec_tree(params = input_provider, n_cohort = 1000)

cost_breakdown_provider <- deterministic_provider %>%
  dplyr::select(Strategy, contains("Cost")) %>%
  pivot_longer(
    cols = contains("Cost"),
    names_to = "Item",
    values_to = "Cost"
  ) %>%
  mutate(
    Item = case_match(
      Item,
      "Cost_Screening" ~ "Screening",
      "Cost_Biopsy"    ~ "Biopsy",
      "Total_Cost"     ~ "Total",
      .default = Item
    ) 
  ) %>%
  filter(Item %in% c("Screening", "Biopsy", "Total"))


cost_breakdown_provider$Item <- factor(
  cost_breakdown_provider$Item, 
  levels = c("Screening", "Biopsy", "Total")
)

plot_costs_provider <- ggplot(cost_breakdown_provider, aes(x = Item, y = Cost, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  geom_text(aes(label = dollar(Cost, prefix = "£", accuracy = 0.01)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 2, fontface = "bold", color = "#333333") +
  scale_fill_manual(name = NULL, values = c("Arm 1: Usual Care" = "#8C8781", "Arm 2: Lunit AI" = "#D98C75")) +
  scale_y_continuous(labels = dollar_format(prefix = "£"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Average Cost per Patient - Provider Perspective", y = "Cost (£)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#404040", size = 14),
    legend.position = "top",
    axis.text.x = element_text(face = "bold", color = "#404040", size = 8),
    panel.grid.major.x = element_blank()
  )

plot_costs_provider

