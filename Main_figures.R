# Main text figures 

session_counts <- Session_behav_sat_no_state %>%
  group_by(Video_ID) %>%
  count(Session_display_rank) %>%
  pivot_wider(
    names_from = Session_display_rank,
    values_from = n,
    values_fill = 0
  )

# libraries -----
library(ggplot2)
library(tidyr)
library(dplyr)
library(jtools)
library(viridis)
library(sjPlot)
library(scales)
library(glmmTMB)
library(ggpubr)

# Session level figures --------------

## data ------
  
Session_behav_no_state <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Session_behav_no_state_aggression.csv")
Session_behav_sat_no_state <- Session_behav_no_state %>% filter(Condition == "Satellite")

#how many sessions 
length(unique(Session_behav_sat_no_state$Session_ID))
# 59 sessions 

# how many session with co-display
length(unique(Session_behav_sat_no_state$Video_ID[Session_behav_sat_no_state$Co_disp_session == "1"]))
# 53

length(unique(Session_behav_sat_no_state$Video_ID))
# 60

# Figure 3 ------------------
# these were later combined in photoshop 
## female visits numbers ------
# model 
# f visits nr (update with date and female compartment)
res_session_vis_nr_nb <- glmmTMB(Total_visit_nr_all_females ~ scale(Co.display_total_duration_s_session)  +  Total_nr_display + Fm_ID + Date2 + (1|Video_ID) + (1|Colour_code), data = Session_behav_sat_no_state,  family = nbinom2, zi = ~ 0)
summary(res_session_vis_nr_nb)

### female visits vs co-display duration ---------
visit_nr_session <- plot_model(res_session_vis_nr_nb, type = "pred", 
                              terms = c("Co.display_total_duration_s_session"), 
                              colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_nr_all_females), 
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black", fill = "#6e9e55") +
  labs(x = "Duration of co-display [s]", 
       y = "Number of female visits", 
       title = NULL) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "none") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("visit_nr_session.png", visit_nr_session, width = 7, height = 6, dpi = 600)

### female visit numbers colored by co-display time  -------------
# Create a new column to flag zeros vs special cases
Session_behav_sat_no_state$PointFlagVisits <- ifelse(
  Session_behav_sat_no_state$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat_no_state$Total_visit_nr_all_females > 0, "ZeroCoDisplay_Vis", 
  ifelse(Session_behav_sat_no_state$Total_visit_nr_all_females == 0, "Zero", "Non-zero")
)

table(Session_behav_sat_no_state$PointFlagVisits)
#Non-zero         Zero                ZeroCoDisplay_Cop 
#46               130                 4 

visit_nr_sess_zero <- plot_model(res_session_vis_nr_nb, type = "pred", 
                                 terms = c("Co.display_total_duration_s_session"), 
                                 colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_nr_all_females, fill = PointFlagVisits),   # map fill to new flag
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black") +
  scale_fill_manual(values = c(
    "Zero" = "gray",                      # no visits
    "ZeroCoDisplay_Vis" = "#9e558b",      # zero co-display but some visits
    "Non-zero" = "#6e9e55"               # normal points
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of female visits", title = NULL, fill = "") +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10)) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("visit_nr_session_zero.png", visit_nr_sess_zero, width = 7, height = 6, dpi = 600)

### female visit numbers colored by session display rank -----------
# make session display rank into alpha beta gamma
Session_behav_sat_no_state$Session_display_rank <- factor(
  Session_behav_sat_no_state$Session_display_rank,
  levels = c(1, 2, 3),
  labels = c("Alpha", "Beta", "Gamma")
)

visit_nr_hierarchy <- plot_model(res_session_vis_nr_nb, type = "pred", 
                                 terms = c("Co.display_total_duration_s_session"), 
                                 colors = "black") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_nr_all_females, fill = factor(Session_display_rank)),
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black") +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"                # gamma
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of female visits", title = NULL, fill = "") +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10)) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("visit_nr_hierarchy.png", visit_nr_hierarchy, width = 7, height = 6, dpi = 600)

# boxplot 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Total_visit_nr_all_females,
           fill = factor(Session_display_rank))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                  
    "Beta" = "#b366b3", 
    "Gamma" = "#a8902a" 
  )) +
  #scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
  labs(x = "Lek hierarchy", 
       y = "Number of female visits", title = NULL, fill = "") +
  theme_classic2(base_size = 20) +
  theme(legend.position = "none")

# violin 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Total_visit_nr_all_females,
           fill = factor(Session_display_rank))) +
  geom_violin(trim = FALSE, alpha = 0.5) + 
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                  
    "Beta" = "#b366b3", 
    "Gamma" = "#a8902a" 
  )) +
  #geom_jitter(width = 0.1, alpha = 0.6) +
  #scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
  theme_classic2(base_size = 20) +
  labs(x = "Lek hierarchy", 
       y = "Number of female visits", title = NULL, fill = "") +
  theme(legend.position = "none")

## visit duration -----------------
# model 
visit_dur_lognorm_hurdle_m <- glmmTMB(Total_visit_duration_all_females ~ Date2 + Fm_ID + scale(Co.display_total_duration_s_session)  +  Total_nr_display + (1|Video_ID), data=Session_behav_sat_no_state, family = glmmTMB::lognormal(link="log"), ziformula = ~ 1  +  Total_nr_display )
summary(visit_dur_lognorm_hurdle_m)

### duration of visits vs co-display duration -------
visit_dur_sess_final <- 
  plot_model(visit_dur_lognorm_hurdle_m, 
             type = "pred", 
             terms = c("Co.display_total_duration_s_session"), 
             colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_duration_all_females), 
             inherit.aes = FALSE,           # avoids alpha warning
             alpha = 0.7, shape = 21, size = 5, stroke = 1,
             color = "black", fill = "#6e9e55") +
  labs(x = "Duration of co-display [s]", 
       y = "Duration of female visits [s]", 
       title = NULL) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 100), 
                     breaks = c(0, 200, 1000, 3000),
                     labels = c("0","200", "1000", "3000")) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save
ggsave("visit_dur_session.png", visit_dur_sess_final, width = 7, height = 6, dpi = 600)

### duration of visits colored by co-display dration 
# Create a new column to flag zeros vs special cases
Session_behav_sat_no_state$PointFlagVisitsDur <- ifelse(
  Session_behav_sat_no_state$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat_no_state$Total_visit_duration_all_females > 0, "ZeroCoDisplay_Vis", 
  ifelse(Session_behav_sat_no_state$Total_visit_duration_all_females == 0, "Zero", "Non-zero")
)

table(Session_behav_sat_no_state$PointFlagVisitsDur)
#Non-zero         Zero                ZeroCoDisplay_Cop 
#46               130                 4 

visit_dur_sess_zero <- plot_model(visit_dur_lognorm_hurdle_m, type = "pred", 
                                  terms = c("Co.display_total_duration_s_session"), 
                                  colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_duration_all_females, fill = PointFlagVisitsDur),   
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1,
             color = "black") +
  scale_fill_manual(values = c(
    "Zero" = "gray",                      # no visits
    "ZeroCoDisplay_Vis" = "#9e558b",      # zero co-display but some visits
    "Non-zero" = "#6e9e55"               # normal points
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Duration of female visits [s]", title = NULL, fill = "") +
  scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 100),
                     breaks = c(0, 200, 1000, 3000),
                     labels = c("0","200", "1000", "3000")) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("visit_dur_session_zero.png", visit_dur_sess_zero, width = 7, height = 6, dpi = 600)

### female visit duration colored on session display rank -----------------
visit_duration_hierarchy <- plot_model(visit_dur_lognorm_hurdle_m, type = "pred", 
                                 terms = c("Co.display_total_duration_s_session"), 
                                 colors = "black") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_visit_duration_all_females, fill = factor(Session_display_rank)),
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black") +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"              # gamma
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Duration of female visits", title = NULL, fill = "") +
  scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 100), 
                     breaks = c(0, 200, 1000, 3000),
                     labels = c("0","200", "1000", "3000")) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("visit_dur_hierarchy.png", visit_duration_hierarchy, width = 7, height = 6, dpi = 600)

# boxplot 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Total_visit_duration_all_females,
           fill = factor(Session_display_rank))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  #geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 100), 
                     breaks = c(0, 200, 1000, 3000),
                     labels = c("0","200", "1000", "3000")) +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                  
    "Beta" = "#b366b3", 
    "Gamma" = "#a8902a" 
  )) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_classic2(base_size = 20) +
  labs(x = "Lek hierarchy", 
       y = "Duration of female visits", title = NULL, fill = "") +
  theme(legend.position = "none")

# violin 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Total_visit_duration_all_females,
           fill = factor(Session_display_rank))) +
  geom_violin(trim = FALSE, alpha = 0.5) + 
  # geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                  
    "Beta" = "#b366b3", 
    "Gamma" = "#a8902a" 
  )) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10, sigma = 100), 
                     breaks = c(0, 200, 1000, 3000),
                     labels = c("0","200", "1000", "3000")) +
  labs(x = "Lek hierarchy", 
       y = "Duration of female visits", title = NULL, fill = "") +
  theme_classic2(base_size = 20) +
  theme(legend.position = "none")

## copulation attempts -----------------
# model
cop_nr_nb_mm <- glmmTMB(Copulation_nr ~  scale(Co.display_total_duration_s_session) +  Total_nr_display + Date2 + Fm_ID + (1|Colour_code), data = Session_behav_sat_no_state, 
                        family = nbinom2, zi = ~ 0)
summary(cop_nr_nb_mm)

### copulation attempts vs co-display duration ------------------
# numbers are just rescaled visually 
cop_nr_sess_final <- plot_model(cop_nr_nb_mm, type = "pred", 
                                terms = c("Co.display_total_duration_s_session"), 
                                colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Copulation_nr), 
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1,
             color = "black", fill = "#6e9e55") +
  labs(x = "Duration of co-display [s]", 
       y = "Number of copulation attempts", title = NULL) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  scale_color_manual(values = "#6e9e55") +
  update_geom_defaults("line", list(size = 1.5))
#save
ggsave("cop_nr_sess.png", cop_nr_sess_final, width = 7, height = 6, dpi = 600)

### copulation attemps colored by co-display duration ---------------------
# Create a new column to flag zeros vs special cases
Session_behav_sat_no_state$PointFlag <- ifelse(
  Session_behav_sat_no_state$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat_no_state$Copulation_nr > 0, "ZeroCoDisplay_Cop", 
  ifelse(Session_behav_sat_no_state$Copulation_nr == 0, "Zero", "Non-zero")
)

table(Session_behav_sat_no_state$PointFlag)
# Non-zero         Zero           ZeroCoDisplay_Cop 
# 22               156                 2 

cop_nr_session_0 <- 
  plot_model(cop_nr_nb_mm, type = "pred", 
             terms = c("Co.display_total_duration_s_session"), 
             colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Copulation_nr, fill = PointFlag),   # map fill to new flag
             inherit.aes = FALSE,
             alpha = 0.7,shape = 21, size = 5, stroke = 1,
             color = "black") +
  scale_fill_manual(values = c(
    "Zero" = "gray",                      # no copulations
    "ZeroCoDisplay_Cop" = "#85559e",      # zero co-display but some copulations
    "Non-zero" = "#6e9e55"               # normal points
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of copulation attempts", title = NULL, fill = "") +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10)
  ) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save
ggsave("cop_nr_sess_zero.png", cop_nr_session_0, width = 7, height = 6, dpi = 600)

### copulation attempts colored by session display rank -------------------------
cop_att_hierarchy <- plot_model(cop_nr_nb_mm, type = "pred", 
                                       terms = c("Co.display_total_duration_s_session"), 
                                       colors = "black") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Copulation_nr, fill = factor(Session_display_rank)),
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black") +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"               # gamma
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of copulation attempts", title = NULL, fill = "") +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10)
  ) +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("cop_att_hierarchy.png", cop_att_hierarchy, width = 7, height = 6, dpi = 600)

# boxplot 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Copulation_nr,
           fill = factor(Session_display_rank))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"               # gamma
  )) +
  labs(x = "", 
       y = "Number of copulation attempts", title = NULL, fill = "") +
  theme_classic2(base_size = 20) +
  theme(legend.position = "none") +
  scale_y_continuous(
      trans = pseudo_log_trans(base = 10)
    )

# violin 
ggplot(Session_behav_sat_no_state,
       aes(factor(Session_display_rank),
           Copulation_nr,
           fill = factor(Session_display_rank))) +
  geom_violin(trim = FALSE, alpha = 0.5) + 
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"               # gamma
  )) +
  labs(x = "", 
       y = "Number of copulation attempts", title = NULL, fill = "") +
  # geom_jitter(width = 0.1, alpha = 0.6) +
  #scale_y_continuous(
  #  trans = pseudo_log_trans(base = 10)
  #) +
  theme_classic2(base_size = 20) +
  theme(legend.position = "none")

## successful copulations ----------------
# final model 
res_session_suc_cop_nr_nb <- glmmTMB(Total_successful_copulations ~ scale(Co.display_total_duration_s_session)  +  Total_nr_display + Date2 + Fm_ID + (1|Colour_code), data = Session_behav_sat_no_state,  family = nbinom2, zi = ~ 0)
summary(res_session_suc_cop_nr_nb)  

### successful copuations vs co-display duration ------------------------
succ_cop_sess <- plot_model(res_session_suc_cop_nr_nb, type = "pred", 
                            terms = c("Co.display_total_duration_s_session"), 
                            colors = "#6e9e55") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_successful_copulations), 
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1,
             color = "black", fill = "#6e9e55") +
  labs(x = "Duration of co-display [s]", 
       y = "Number of successful copulations", title = NULL)  +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  scale_color_manual(values = "#6e9e55") +
  update_geom_defaults("line", list(size = 1.5))
ggsave("succ_cop_nr_sess.png", succ_cop_sess, width = 7, height = 6, dpi = 600)

### successful copulations colored by co-display duration -----------------------
# zero co display but succ cop 
# Create a new column to flag zeros vs special cases
Session_behav_sat_no_state$PointFlagSC <- ifelse(
  Session_behav_sat_no_state$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat_no_state$Total_successful_copulations > 0, "ZeroCoDisplay_Cop", 
  ifelse(Session_behav_sat_no_state$Total_successful_copulations == 0, "Zero", "Non-zero")
)

table(Session_behav_sat_no_state$PointFlagSC)
#Non-zero              Zero ZeroCoDisplay_Cop 
#18               160                 2 

succ_cop_nr_session_0 <- 
  plot_model(res_session_suc_cop_nr_nb, type = "pred", 
             terms = c("Co.display_total_duration_s_session"), 
             colors = "darkgray") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_successful_copulations, fill = PointFlagSC),
             alpha = 0.7, shape = 21, size = 5, stroke = 1,
             color = "black") +
  scale_fill_manual(values = c(
    "Zero" = "gray",                      # no copulations
    "ZeroCoDisplay_Cop" = "#85559e",      # zero co-display but some copulations
    "Non-zero" = "#6e9e55"               # normal points
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of succcessful copulations", title = NULL, fill = "") +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 2.5))
ggsave("succ_cop_nr_sess_zero.png", succ_cop_nr_session_0, width = 7, height = 6, dpi = 600)

### succ copulations colored by session display rank -------------------------
succ_cop_hierarchy <- plot_model(res_session_suc_cop_nr_nb, type = "pred", 
                                terms = c("Co.display_total_duration_s_session"), 
                                colors = "darkgray") +
  geom_point(data = Session_behav_sat_no_state, 
             aes(x = Co.display_total_duration_s_session, 
                 y = Total_successful_copulations, fill = factor(Session_display_rank)),
             inherit.aes = FALSE,
             alpha = 0.7, shape = 21, size = 5, stroke = 1, 
             color = "black") +
  scale_fill_manual(values = c(
    "Alpha" = "#1a8d8d",                     # alpha
    "Beta" = "#b366b3",      # beta
    "Gamma" = "#a8902a"                # gamma
  )) +
  labs(x = "Duration of co-display [s]", 
       y = "Number of successful copulations", title = NULL, fill = "") +
  theme_classic2(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
ggsave("succ_cop_hierarchy.png", succ_cop_hierarchy, width = 7, height = 6, dpi = 600)


# Combo level figures --------
## data -----
Combo_behav_no_state <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Combo_behav_no_state.csv")

Combos_cop_sum <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Combo copulations summed.csv")


# Figure 4 --------------
## female visits numbers ------

## slope figure 
# filter the data to make figures
filtered_df <- Combo_behav_no_state %>% dplyr::select(Condition, Total_visit_nr_all_females, Combo_lek_week, Colour_code)
filtered_df$Combo_lek_week_ID <- paste(filtered_df$Combo_lek_week,filtered_df$Colour_code,sep="_")
# reshape the data frame for geom_segment
reshaped_df <- filtered_df %>%
  group_by(Combo_lek_week_ID, Condition, Colour_code) %>%
  summarise(Visits = sum(Total_visit_nr_all_females, na.rm = TRUE)) %>%
  pivot_wider(names_from = Condition, 
              values_from = Visits, 
              values_fill = 0) %>%
  rename(Marginal_Visits = Marginal,
         Satellite_Visits = Satellite)

zero_visits_both <- reshaped_df %>%
  filter(Marginal_Visits == 0 & Satellite_Visits == 0) %>%
  nrow()

zero_visits_both #33

# figure no difference in slopes 
Individ_response_cond_visits <-
  reshaped_df %>%
  # add a variable for when Marginal is more successful than Satellite (for highlighting the colors)
  mutate(more_vis_satellite = Marginal_Visits > Satellite_Visits) %>%
  ggplot() + 
  # add a line segment that goes from Marginal to Satellite for each individual
  geom_segment(aes(x = 1, xend = 2, 
                   y = Marginal_Visits, 
                   yend = Satellite_Visits,
                   group = Combo_lek_week_ID,
                   col = more_vis_satellite), 
               size = 1.2)  +
  # set the colors
  scale_color_manual(values = c("grey", "grey"))  +
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  geom_segment(aes(x = 1, xend = 1, 
                   y = min(c(Marginal_Visits, Satellite_Visits)) - 2,
                   yend = max(c(Marginal_Visits, Satellite_Visits)) + 1),
               col = "grey70", size = 0.8) +
  # add vertical lines that act as axis for Satellite
  geom_segment(aes(x = 2, xend = 2, 
                   y = min(c(Marginal_Visits, Satellite_Visits)) - 2,
                   yend = max(c(Marginal_Visits, Satellite_Visits)) + 1),
               col = "grey70", size = 0.8) +
  # Add "Satellite absent" label above the left vertical axis
  annotate("text", x = 1, y = max(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) + 2, 
           label = "Satellite absent", hjust = 0.5, vjust = 0, size = 4) +
  # Add "Satellite" label above the right vertical axis
  annotate("text", x = 2, y = max(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) + 2, 
           label = "Satellite present", hjust = 0.5, vjust = 0, size = 4) +
  # Adjust y-axis limits to accommodate the new labels
  coord_cartesian(xlim = c(0.5, 2.5), 
                  ylim = c(min(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) - 2, 
                           max(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) + 4)) + 
  # Adjust the x-axis limits to give some padding
  coord_cartesian(xlim = c(0.5, 2.5)) +
  # Add copulation numbers next to Marginal points in black
  geom_text(aes(x = 1 - 0.05, 
                y = Marginal_Visits,
                label = Marginal_Visits),
            hjust = "right", size = 2, color = "black") +
  # Add copulation numbers next to Satellite points in black
  geom_text(aes(x = 2 + 0.05, 
                y = Satellite_Visits,
                label = Satellite_Visits),
            hjust = "left", size = 2, color = "black") +
  # Set custom limits for the x-axis so labels are not cut off
  scale_x_continuous(limits = c(0.5, 2.5)) +
  # add the white outline for the points for Satellite absent cond
  geom_point(aes(x = 1, 
                 y = Marginal_Visits), size = 1.5,
             col = "black", shape = 19) +
  # add the white outline for the points at each rate for Satellite present
  geom_point(aes(x = 2, 
                 y = Satellite_Visits), size = 1.5,
             col = "black", shape = 19) +
  # add the actual points at session with Marginal copulations
  geom_point(aes(x = 1, 
                 y = Marginal_Visits), size = 1,
             col = "grey60", shape = 19, fill = "grey60") +
  # add the actual points at each session with Satellite copulations
  geom_point(aes(x = 2, 
                 y = Satellite_Visits), size = 1,
             col = "grey60", shape = 19, fill = "grey60")
# coord_cartesian(ylim = c(0, max(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) + 10))
# save 
ggsave("Individ_response_cond_visits.png",Individ_response_cond_visits , width = 8, height = 6, dpi = 300,
       bg = "transparent")

## merge visits and boxplots
# take the axes as you would for the intercep slope plot and make the boxplots
y_min <- min(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) - 2
y_max <- max(c(reshaped_df$Marginal_Visits, reshaped_df$Satellite_Visits)) + 2

# without zeros
subset_data1 <- Combos_cop_sum %>%
  filter(Condition == "Marginal", Total_visit_nr_all_females > 0)

# marginal boxplot no zeros
p1 <- ggplot(subset_data1, aes(x = Condition, y = Total_visit_nr_all_females, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
               outlier.size = 3, notch = FALSE, color = "black",
               width = 0.4,  # Adjust the boxplot width
               position = position_nudge(x = 0.25)) +
  scale_fill_manual(values = c("grey")) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  # Add vertical lines to easier align plots in inkscape
  geom_segment(aes(x = 0.15, xend = 0.15, 
                   y = y_min, yend = y_max),
               color = "grey70", size = 0.8)

# only for satellite condition no zeros 
subset_datas1 <- Combos_cop_sum %>%
  filter(Condition == "Satellite", Total_visit_nr_all_females > 0)

#boxplot with zeros 
p2<-ggplot(subset_datas1, aes(x = Condition, y = Total_visit_nr_all_females, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
               outlier.size = 3, notch = FALSE, color = "black",
               width = 0.4,  # Adjust the boxplot width
               position = position_nudge(x = 0.25)) +
  scale_fill_manual(values = c("grey")) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  # Add vertical line for the axis to easier align in inkscape
  geom_segment(aes(x = 1.75, xend = 1.75, 
                   y = y_min, yend = y_max),
               color = "grey70", size = 0.8)

## combine the boxplots with the intercept slope plot 
library(cowplot)
#combined_plot <- cowplot::plot_grid(visits_dens_m, Individ_response_cond_visits, visits_dens_s, ncol = 3, rel_widths = c(1, 3, 1))
#ggsave("Combined_violin_slope_plot_visits_zeros_trimmed.png", combined_plot, width = 8, height = 6, dpi = 300, bg = "transparent")
# from here take it to Inkscape to nicely combine everything
## copulation attempt nr -----
# filter the data for the plot
filtered_df <- Combo_behav_no_state %>% dplyr::select(Condition, Copulation_nr, Combo_lek_week, Colour_code)
filtered_df$Combo_lek_week_ID <- paste(filtered_df$Combo_lek_week,filtered_df$Colour_code,sep="_")

# reshape the data frame for geom_segment
reshaped_df <- filtered_df %>%
  group_by(Combo_lek_week_ID, Condition, Colour_code) %>%
  summarise(Copulations = sum(Copulation_nr, na.rm = TRUE)) %>%
  pivot_wider(names_from = Condition, 
              values_from = Copulations, 
              values_fill = 0) %>%
  rename(Marginal_Copulations = Marginal,
         Satellite_Copulations = Satellite)

zero_copulations_both <- reshaped_df %>%
  filter(Marginal_Copulations == 0 & Satellite_Copulations == 0) %>%
  nrow()

zero_copulations_both # 42

# slope plot
Individ_response_cond <-
  reshaped_df %>%
  # add a variable for when Marginal is more successful than Satellite (for highlighting the colors)
  mutate(more_cop_marginal = Marginal_Copulations > Satellite_Copulations) %>%
  ggplot() + 
  # add a line segment that goes from Marginal to Satellite for each individual
  geom_segment(aes(x = 1, xend = 2, 
                   y = Marginal_Copulations, 
                   yend = Satellite_Copulations,
                   group = Combo_lek_week_ID,
                   col = more_cop_marginal), 
               size = 1.2)  +
  # set the colors
  scale_color_manual(values = c("#7d7979", "#7d7979"))  + # #cba1be", "#62a8a8 prev cols
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) + 
  geom_segment(aes(x = 1, xend = 1, 
                   y = min(c(Marginal_Copulations, Satellite_Copulations)) - 2,
                   yend = max(c(Marginal_Copulations, Satellite_Copulations)) + 1),
               col = "grey70", size = 0.8) +
  # add vertical lines that act as axis for Satellite
  geom_segment(aes(x = 2, xend = 2, 
                   y = min(c(Marginal_Copulations, Satellite_Copulations)) - 2,
                   yend = max(c(Marginal_Copulations, Satellite_Copulations)) + 1),
               col = "grey70", size = 0.8) +
  # Add "Satellite absent" label above the left vertical axis
  annotate("text", x = 1, y = max(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) + 2, 
           label = "Satellite absent", hjust = 0.5, vjust = 0, size = 4) +
  # Add "Satellite" label above the right vertical axis
  annotate("text", x = 2, y = max(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) + 2, 
           label = "Satellite present", hjust = 0.5, vjust = 0, size = 4) +
  # Adjust y-axis limits to accommodate the new labels
  coord_cartesian(xlim = c(0.5, 2.5), 
                  ylim = c(min(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) - 2, 
                           max(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) + 4)) + 
  # Adjust the x-axis limits to give some padding
  coord_cartesian(xlim = c(0.5, 2.5)) +
  # Add copulation numbers next to Marginal points in black
  geom_text(aes(x = 1 - 0.05, 
                y = Marginal_Copulations,
                label = Marginal_Copulations),
            hjust = "right", size = 4, color = "black") +
  # Add copulation numbers next to Satellite points in black
  geom_text(aes(x = 2 + 0.05, 
                y = Satellite_Copulations,
                label = Satellite_Copulations),
            hjust = "left", size = 4, color = "black") +
  # Set custom limits for the x-axis so labels are not cut off
  scale_x_continuous(limits = c(0.5, 2.5)) +
  # add the white outline for the points for Satellite absent cond
  geom_point(aes(x = 1, 
                 y = Marginal_Copulations), size = 3.5,
             col = "black", shape = 19) +
  # add the white outline for the points at each rate for Satellite present
  geom_point(aes(x = 2, 
                 y = Satellite_Copulations), size = 3.5,
             col = "black", shape = 19) +
  # add the actual points at session with Marginal copulations
  geom_point(aes(x = 1, 
                 y = Marginal_Copulations), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # add the actual points at each session with Satellite copulations
  geom_point(aes(x = 2, 
                 y = Satellite_Copulations), size = 3,
             col = "grey60", shape = 19, fill = "grey60")
#ggsave("Individ_response_cond_cop.png", Individ_response_cond, width = 8, height = 6, dpi = 300, bg = "transparent")

# take the axes as you would for the intercep slope plot and make the boxplots
y_min <- min(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) - 2
y_max <- max(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations)) + 2

# boxplots 

# marginal box no zeros 
subset_data1 <- Combos_cop_sum %>%
  filter(Condition == "Marginal", Copulation_nr > 0)

p1.1 <- ggplot(subset_data1, aes(x = Condition, y = Copulation_nr, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
               outlier.size = 3, notch = FALSE, color = "black",
               width = 0.4,  # Adjust the boxplot width
               position = position_nudge(x = 0.25)) +
  scale_fill_manual(values = "#62a8a8") +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  )+
  # Add vertical lines
  geom_segment(aes(x = 0.75, xend = 0.75, 
                   y = y_min, yend = y_max),
               color = "grey70", size = 0.8)

# only for satellite condition
# remove zeros 
subset_data_s <- Combos_cop_sum %>%
  filter(Condition == "Satellite", Copulation_nr > 0)

p2<-ggplot(subset_data_s, aes(x = Condition, y = Copulation_nr, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
              outlier.size = 3, notch = FALSE, color = "black",
             width = 0.4,  # Adjust the boxplot width
            position = position_nudge(x = 0.15)) +
  scale_fill_manual(values = "#cba1be") +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  # Add vertical line fpr the axis
  geom_segment(aes(x = 1.75, xend = 1.75, 
                   y = y_min, yend = y_max),
               color = "grey70", size = 0.8)


## combine the boxplots with the intercept slope plot 
library(cowplot)
combined_plot <- cowplot::plot_grid(p1.1, Individ_response_cond, p2, ncol = 3, rel_widths = c(1, 3, 1))
ggsave("Combined_box_slope_plot_cop_0values.png", combined_plot, width = 8, height = 6, dpi = 300, bg = "transparent")
# from here take it to inkscape to nicely combine everything

## successful copulation nr ----
filtered_df2 <- Combo_behav_no_state %>% dplyr::select(Condition, Total_successful_copulations, Combo_lek_week, Colour_code)
filtered_df2$Combo_lek_week_ID <- paste(filtered_df2$Combo_lek_week,filtered_df2$Colour_code,sep="_")

# reshape the data frame for geom_segment
reshaped_df2 <- filtered_df2 %>%
  group_by(Combo_lek_week_ID, Condition, Colour_code) %>%
  summarise(Copulations = sum(Total_successful_copulations, na.rm = TRUE)) %>%
  pivot_wider(names_from = Condition, 
              values_from = Copulations, 
              values_fill = 0) %>%
  rename(Marginal_SUCC_Copulations = Marginal,
         Satellite_SUCC_Copulations = Satellite)

zero_succ_copulations_both <- reshaped_df2 %>%
  filter(Marginal_SUCC_Copulations == 0 & Satellite_SUCC_Copulations == 0) %>%
  nrow()

zero_succ_copulations_both #42

# slope plot 

## BEFORE RUNNING THIS CODE MAKE SURE THAT YOU RUN THE CODE FOR COPULATION ATTEMPTS SO
## THAT THE AXES ARE ALIGNED
## scale the axes to be the same as with the copulations plot 
other_max <- max(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations), na.rm = TRUE)
other_min <- min(c(reshaped_df$Marginal_Copulations, reshaped_df$Satellite_Copulations), na.rm = TRUE)

Individ_response_cond_succ_cop2 <-
  reshaped_df2 %>%
  # add a variable for when Marginal is more successful than Satellite (for highlighting the colors)
  mutate(more_cop_marginal = Marginal_SUCC_Copulations > Satellite_SUCC_Copulations) %>%
  ggplot() + 
  # add a line segment that goes from Marginal to Satellite for each individual
  geom_segment(aes(x = 1, xend = 2, 
                   y = Marginal_SUCC_Copulations, 
                   yend = Satellite_SUCC_Copulations,
                   group = Combo_lek_week_ID,
                   col = more_cop_marginal), 
               size = 1.2)  +
  # set the colors
  scale_color_manual(values = c("#7d7979", "#7d7979"))  +
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  # add vertical lines for marginal
  geom_segment(aes(x = 1, xend = 1, 
                   y = other_min - 2,
                   yend = other_max + 1),
               col = "grey70", size = 0.8) +
  # add vertical lines for satellite
  geom_segment(aes(x = 2, xend = 2, 
                   y = other_min - 2,
                   yend = other_max + 1),
               col = "grey70", size = 0.8) +
  # annotate text for M and Satellite
  annotate("text", x = 1, y = other_max + 2, 
           label = "Satellite absent", hjust = 0.5, vjust = 0, size = 4) +
  annotate("text", x = 2, y = other_max + 2, 
           label = "Satellite present", hjust = 0.5, vjust = 0, size = 4) +
  # Adjust y-axis limits to accommodate the new labels
  coord_cartesian(
    ylim = c(other_min - 2, other_max + 4),
    xlim = c(0.5, 2.5)
  ) + 
  # Adjust the x-axis limits to give some padding
  coord_cartesian(xlim = c(0.5, 2.5)) +
  # Add copulation numbers next to Marginal points in black
  geom_text(aes(x = 1 - 0.05, 
                y = Marginal_SUCC_Copulations,
                label = Marginal_SUCC_Copulations),
            hjust = "right", size = 4, color = "black") +
  # Add copulation numbers next to Satellite points in black
  geom_text(aes(x = 2 + 0.05, 
                y = Satellite_SUCC_Copulations,
                label = Satellite_SUCC_Copulations),
            hjust = "left", size = 4, color = "black") +
  # Set custom limits for the x-axis so labels are not cut off
  #scale_x_continuous(limits = c(0.5, 2.5)) +
  # add the white outline for the points for Satellite absent cond
  geom_point(aes(x = 1, 
                 y = Marginal_SUCC_Copulations), size = 3.5,
             col = "black", shape = 19) +
  # add the white outline for the points at each rate for Satellite present
  geom_point(aes(x = 2, 
                 y = Satellite_SUCC_Copulations), size = 3.5,
             col = "black", shape = 19) +
  # add the actual points at session with Marginal copulations
  geom_point(aes(x = 1, 
                 y = Marginal_SUCC_Copulations), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # add the actual points at each session with Satellite copulations
  geom_point(aes(x = 2, 
                 y = Satellite_SUCC_Copulations), size = 3,
             col = "grey60", shape = 19, fill = "grey60")

#ggsave("Individ_response_cond_succ_cop.png", Individ_response_cond_succ_cop2, width = 8, height = 6, dpi = 300, bg = "transparent")


# take the axes as you would for the intercep slope plot and make the boxplots
y_min2 <- other_min - 2
y_max2 <- other_max + 2

# marginal boxplot no zeros 
subset_data <- Combos_cop_sum %>%
  filter(Condition == "Marginal", Total_successful_copulations > 0)

p1 <- ggplot(subset_data, aes(x = Condition, y = Total_successful_copulations, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
               outlier.size = 3, notch = FALSE, color = "black",
               width = 0.4,  # Adjust the boxplot width
               position = position_nudge(x = 0.25)) +
  scale_fill_manual(values = c("#62a8a8")) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  )+
  # Add vertical lines
  geom_segment(aes(x = 0.25, xend = 0.25, 
                   y = y_min2, yend = y_max2),
               color = "grey70", size = 0.8)

# satellite no zeros 
subset_data_s <- Combos_cop_sum %>%
  filter(Condition == "Satellite", Total_successful_copulations > 0)

p2 <-ggplot(subset_data_s, aes(x = Condition, y = Total_successful_copulations, fill = Condition)) +
  geom_boxplot(outlier.colour = NULL, outlier.shape = 19,
               outlier.size = 3, notch = FALSE, color = "black",
               width = 0.4,  # Adjust the boxplot width
               position = position_nudge(x = 0.25)) +
  scale_fill_manual(values = c("#cba1be")) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  # Add vertical line for the axis
  geom_segment(aes(x = 1.75, xend = 1.75, 
                   y = y_min2, yend = y_max2),
               color = "grey70", size = 0.8)

## combine everything and then work further in Inkscape or Photoshop 
combined_plot <- cowplot::plot_grid(p1, Individ_response_cond_succ_cop2, p2, ncol = 3, rel_widths = c(1, 3, 1))
ggsave("Combined_violin_slope_plot_succ_cop_0values.png", combined_plot, width = 8, height = 6, dpi = 300, bg ="transparent")
