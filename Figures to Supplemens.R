
#### Resident benefits MS - Supplemental material figures ####

## data ------
Session_behav_no_state <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Session_behav_no_state_aggression.csv")

Session_behav_sat <- Session_behav_no_state %>% filter(Condition == "Satellite")

## Packages

library(dplyr)
library(tidybayes)
library(tidyr)
library(car)
library(stringr)
library(ggplot2)
library(tidyverse)
#library(hrbrthemes)
library(viridis)
library(hrbrthemes)
library(lme4)
library(lmtest)
library(MASS)
library(glmmTMB)
library(performance)
library(rstanarm)
library(brms)  # for models
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidybayes)
library(modelr)
library(pscl)
library(GLMMadaptive)
library(fitdistrplus)
library(AER)
library(tidyr)
library(car)
library(AICcmodavg)
library(VGAM) # tobit function
library(DHARMa)
library(readxl)
library(stringr)
library(sjPlot) # ploy_model
library(scales)
library(emmeans)
library(patchwork)
library(data.table)
library(DHARMa)
library(sjPlot)


## Figure S1 A&B: Display activity score ----

## A: Display activity score and measures display 

Session_behav_df <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Session_behav_df2.csv")

Session_behav_df_display <- Session_behav_df[!with(Session_behav_df,is.na(Co.display_total_duration_s_session) & is.na(Single_display_total_duration_s_session)),]

Session_behav_no_state_display <- Session_behav_df_display %>% group_by(Session_ID, Video_ID, Colour_code, Condition, Bird_desc, Sat_ID, Fm_ID, Time, Date, Date2, Week,Combo_lek_week,Combo_lek) %>% summarise(across(c(Female_max_id,Female_max_id,Total_visit_duration_all_females,Total_visit_nr_all_females,Total_nr_display,Total_nr_display_30, Interrupted_copulations_nr_OtherR,Interrupted_copulations_nr_OtherS,Interrupted_copulations_nr_PartnerR,Interrupted_copulations_nr_PartnerS,Successful_copulations_nr,Interrupted_total,Female_mount_max_id,Mount_nr,Female_max_crouching_id,Crouching_nr,Copulation_attempt, Copulation_nr, Total_successful_copulations, Co.display_total_nr_session,Total_copulation_nr,Co.display_total_duration_s_session,Single_display_total_duration_s_session, Single_display_to_satellite_total_nr_session,Single_display_to_both_total_nr_session, Single_display_to_both_total_duration_s_session, Paired_state_total_nr_session, Paired_state_total_duration_s_session, Lek_codisplay_nr, Lek_codisplay_dur_s, Lek_codisplay_occ, Cop_session), sum, na.rm = TRUE),across(c(Lek_hierarchy, Copulation_occurrence_successful,Copulation_occurrence,Total_copulation_occurrence, Single_display_duration_mean_s_session,Single_display_duration_std_dev_session,Single_display_to_satellite_duration_mean_s_session,Single_display_to_satellite_duration_std_dev_session,Single_display_to_both_duration_mean_s_session,Single_display_to_both_duration_std_dev_session,Paired_state_duration_mean_s_session,Paired_state_duration_std_dev_session), mean, na.rm = TRUE)) %>% arrange(Session_ID)

display_score <- Session_behav_no_state_display %>% dplyr::select(c(Session_ID, Video_ID, Colour_code, Condition, Total_nr_display, Total_nr_display_30, Co.display_total_duration_s_session, Single_display_total_duration_s_session))

display_score$Total_display_duration_s_session <- display_score$Co.display_total_duration_s_session + display_score$Single_display_total_duration_s_session 

display_score$Total_display_duration_s_session <- ifelse(display_score$Total_display_duration_s_session > 1800, 1800, display_score$Total_display_duration_s_session)


# For the whole session

ggplot(display_score, aes(as.factor(Total_nr_display), Total_display_duration_s_session, fill = as.factor(Total_nr_display))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  xlab("Display score")+
  ylab("Duration of display") +
  theme(legend.title = element_blank())

## enhanced 
ggplot(display_score, aes(
  x = as.factor(Total_nr_display),
  y = Total_display_duration_s_session,
  fill = as.factor(Total_nr_display)
)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, color = "gray30") +
  geom_jitter(
    color = "black",
    size = 0.7,
    alpha = 0.7,
    width = 0.2
  ) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.7, option = "A") +
  theme_classic(base_size = 18) +
  xlab("Display activity score") +
  ylab("Duration of display [s]") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", size = 0.6)
  )

# For only 30min

ggplot(display_score, aes(as.factor(Total_nr_display_30), Total_display_duration_s_session, fill = as.factor(Total_nr_display_30))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  xlab("Display activity score")+
  ylab("Duration of display 30min") +
  theme(legend.title = element_blank())

## enhanced 30 minutes 
FigS1A <- ggplot(display_score, aes(
  x = as.factor(Total_nr_display_30),
  y = Total_display_duration_s_session,
  fill = as.factor(Total_nr_display_30)
)) +
   geom_boxplot(outlier.shape = 21, width = 0.6, color = "gray30") +
#  geom_jitter(
#    color = "black",
#    size = 0.7,
#    alpha = 0.7,
#    width = 0.2
#  ) +
  scale_fill_manual(values = c("gray", "beige","orange","#a855ff")) +
  theme_classic(base_size = 20) +
  xlab("Display activity score") +
  ylab("Duration of display [s]") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
  )
ggsave("FigS1A_box.png", FigS1A, width = 7, height = 6, units = "in", dpi = 600)
# Hierarchy figures

ggplot(Session_behav_no_state[!is.na(Session_behav_no_state$Dominance_hierarchy), ], aes(as.factor(Dominance_hierarchy), y = Total_nr_display, fill = factor(Dominance_hierarchy))) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_fill_manual(values = c('#3ed9d8', "#a26ca6", "#c08f3c")) + 
  theme_ipsum() +
  xlab("Dominance hieararchy in session ")+
  ylab("Display activity score (DAS)") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size=16),
    axis.title.y = element_text(hjust = 0.5, size=16)
  )


ggplot(Session_behav_no_state[!is.na(Session_behav_no_state$Dominance_hierarchy), ], aes(as.factor(Dominance_hierarchy), Co.display_total_duration_s_session, fill = factor(Dominance_hierarchy))) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_fill_manual(values = c('#3ed9d8', "#a26ca6", "#c08f3c")) + 
  theme_ipsum() +
  xlab("Dominance hieararchy in session ")+
  ylab("Duration co-display (s)") +
  theme(  
    legend.title = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size=16),
    axis.title.y = element_text(hjust = 0.5, size=16)
  )


## B: Relationship between the Display activity score (Total_nr_display) and the Display rank 

Session_behav_no_state <- read.csv("Residents benefits ms data analysis Kriszti Jelena/Session_behav_no_state2.csv")

ggplot(Session_behav_no_state, aes(as.character(Session_display_rank_unique), Total_nr_display, fill = Session_display_rank_unique))+
  geom_violin(alpha = 0.5) +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  theme(legend.position = "none")

# cleaned 

FigS1B <- ggplot(Session_behav_no_state, aes(
  x = as.character(Session_display_rank_unique),
  y = Total_nr_display,
  fill = as.factor(Session_display_rank_unique)
)) +
  geom_boxplot(outlier.shape = 21, width = 0.6, color = "gray30") +
  #geom_violin() +
  #geom_jitter(
  #  position = position_jitter(seed = 1, width = 0.15),
  #  color = "black",
  #  size = 0.7,
  #  alpha = 0.7
  #) +
  scale_fill_manual(values = c("#a855ff", "orange", "beige")) +
  theme_classic(base_size = 20) +
  xlab("Session display rank") +
  ylab("Display activity score") +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave("FigS1B_box.png", FigS1B, width = 7, height = 6, units = "in", dpi = 600)


## Figure S2: Display and aggression of Resident towards other Resients in S session -----------

## chasing other residents 

# significant
chasings_res_zi_poi_mm <- glmmTMB(Chasings_res ~  scale(Co.display_total_duration_s_session) +  
                                    Total_nr_display  + Date2 + Fm_ID + Time + (1|Colour_code) +
                                    (1|Video_ID), data = Session_behav_sat, 
                                  family = poisson(link = "log"), zi = ~ 1 +  Total_nr_display)

chasings_Rs <- plot_model(chasings_res_zi_poi_mm, type = "pred", terms = c("Co.display_total_duration_s_session"), colors = "#59AC77") +
  geom_point(data = Session_behav_sat, 
             aes(x = Co.display_total_duration_s_session, y = Chasings_res), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill="#59AC77") +
  labs(x = "Total duration of co-display [s]", 
       y = "Number of chases towards other Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("chasings_R-sup.png", chasings_Rs, width = 7, height = 7, dpi = 300)

# significant 
chasings_totnr_disp <- plot_model(chasings_res_zi_poi_mm, type = "pred", terms = c("Total_nr_display"), colors = "#59AC77") +
  geom_point(data = Session_behav_sat, aes(x = Total_nr_display, y = Chasings_res), 
             alpha = 0.5, shape = 21, size = 7, stroke = 1, color="black", fill= "#59AC77") +
  labs(x = "Display activity score", 
       y = "Number of chases towards other Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("chasings_R-disp_activity-sup.png", chasings_totnr_disp, width = 7, height = 7, dpi = 300)

## attacks to other Rs
attacks_res_nb_mm <- glmmTMB(Aggression_res_attack_approach ~  scale(Co.display_total_duration_s_session) + 
                               Total_nr_display  + Date2 + Fm_ID + Time + (1|Video_ID) + 
                               (1|Colour_code), data = Session_behav_sat, family = nbinom2)
summary(attacks_res_nb_mm)

attacks_sess <- plot_model(attacks_res_nb_mm, type = "pred", terms = c("Co.display_total_duration_s_session"), colors = "#7c8472") +
  geom_point(data = Session_behav_sat, aes(x = Co.display_total_duration_s_session, y = Aggression_res_attack_approach),
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill= "#7c8472") +
  labs(x = "Total duration of co-display [s]", 
       y = "Number of attacks towards other Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("attacks_sess-sup.png", attacks_sess, width = 7, height = 7, dpi = 300)

# attacks vs total nr display # sig
attacks_sess_dispscore <- plot_model(attacks_res_nb_mm, type = "pred", terms = c("Total_nr_display"), colors = "#59AC77") +
  geom_point(data = Session_behav_sat, aes(x = Total_nr_display, 
                                                    y = Aggression_res_attack_approach),
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill= "#59AC77") +
  labs(x = "Display activity score", 
       y = "Number of attacks towards other Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("attacks_sess_dispscore-sup.png", attacks_sess_dispscore, width = 7, height = 7, dpi = 300)

## fights Rs 
figth_dur_mm_gamma <- glmmTMB(Fight_dur_s ~  scale(Co.display_total_duration_s_session) +  Total_nr_display  + Date2 + Fm_ID + Time + (1|Video_ID) + (1|Colour_code), data = Session_behav_sat, family = ziGamma(link="log"), ziformula = ~ 1 +  Total_nr_display)
summary(figth_dur_mm_gamma)
check_overdispersion(figth_dur_mm_gamma)

fights_sess <- plot_model(figth_dur_mm_gamma, type = "pred", 
                          terms = c("Co.display_total_duration_s_session"), colors = "#7c8472") +
  geom_point(data = Session_behav_sat, aes(x = Co.display_total_duration_s_session, y = Fight_dur_s), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill= "#7c8472") +
  labs(x = "Total duration of co-display [s]", 
       y = "Total duration of fights between Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("fights_sess-sup.png", fights_sess, width = 7, height = 7, dpi = 300)

# fights vs total nr display
fights_sess_dispscore <- plot_model(figth_dur_mm_gamma, type = "pred",
                                    terms = c("Total_nr_display"), colors = "#59AC77") +
  geom_point(data = Session_behav_sat, aes(x = Total_nr_display, y = Fight_dur_s), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill= "#59AC77") +
  labs(x = "Display activity score", 
       y = "Total duration of fights between Rs", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 25),
    axis.text = element_text(size = 25),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("fights_sess_dispscore-sup.png", fights_sess_dispscore, width = 7, height = 7, dpi = 300)

## Figure S3: aggression towards S in S session -----------
## attacks S 
attacks_sat_nb_mm <- glmmTMB(Attacks_sat ~  scale(Co.display_total_duration_s_session) + 
                               Total_nr_display  + Date2 + Fm_ID + (1|Colour_code), 
                             data = Session_behav_sat, family = nbinom2)
summary(attacks_sat_nb_mm)

attacks_sess_S <- plot_model(attacks_sat_nb_mm, type = "pred", terms = c("Co.display_total_duration_s_session"), colors = 	"#7c8472") +
  geom_point(data = Session_behav_sat, 
             aes(x = Co.display_total_duration_s_session, y = Attacks_sat), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill=	"#7c8472") +
  labs(x = "Total duration of co-display [s]", 
       y = "Number of attacks towards the S", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.text = element_text(size = 22),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  )+
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("attacks_sess_S-sup.png", attacks_sess_S, width = 7, height = 6, dpi = 300)

attacks_sess_SDAS <- plot_model(attacks_sat_nb_mm, type = "pred", terms = c("Total_nr_display"), colors = "#7c8472") +
  geom_point(data = Session_behav_sat, 
             aes(x = Total_nr_display, y = Attacks_sat), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill=		"#7c8472") +
  labs(x = "Display activity score", 
       y = "Number of attacks towards the S", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.text = element_text(size = 22),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  )+
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("attacks_sess_SDAS-sup.png", attacks_sess_SDAS, width = 7, height = 6, dpi = 300)

# chasings S 
chasigs_sat_nb_mm <- glmmTMB(Chasings_sat ~  
                               scale(Co.display_total_duration_s_session) +  
                               Total_nr_display  + Date2 + Fm_ID + (1|Video_ID) + 
                               (1|Colour_code), data = Session_behav_sat, 
                             family = nbinom2)
summary(chasigs_sat_nb_mm)

chasings_sess_S <- plot_model(chasigs_sat_nb_mm, type = "pred", terms = c("Co.display_total_duration_s_session"), 
                              colors ="#7c8472") +
  geom_point(data = Session_behav_sat, 
             aes(x = Co.display_total_duration_s_session, y = Chasings_sat), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill=	"#7c8472") +
  labs(x = "Total duration of co-display [s]", 
       y = "Number of chases towards S", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.text = element_text(size = 22),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  )+
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("chasings_sess_S-sup.png", chasings_sess_S, width = 7, height = 6, dpi = 300)

chasings_sess_SDAS<- plot_model(chasigs_sat_nb_mm, type = "pred", terms = c("Total_nr_display"), 
           colors = "#7c8472") +
  geom_point(data = Session_behav_sat, 
             aes(x = Total_nr_display, y = Chasings_sat), 
             alpha = 0.7, shape = 21, size = 7, stroke = 1, color="black", fill=	"#7c8472") +
  labs(x = "Display activity score", 
       y = "Number of chases towards S", title = NULL) +
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.text = element_text(size = 22),      # Increase tick label size
    axis.ticks = element_line(size = 1.2),    # Make tick marks thicker
    axis.ticks.length = unit(0.3, "cm"),      # Make tick marks longer
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3)))
ggsave("chasings_sess_SDAS-sup.png", chasings_sess_SDAS, width = 7, height = 6, dpi = 300)

## Figure S4A&B ---------------------
### Occurrence of female visits (A) and copulation attempts (B) for a Resident 
### in a session regarding its co-display performance (duration in seconds) and display activity (DAS).
# DAS == "Total_nr_display" in the R codes

# A: Co-display, Display activity and Visit occurrence for a Resident in a session
Session_behav_sat <- Session_behav_no_state %>% filter(Condition == "Satellite")

ggplot(Session_behav_sat, aes(Co.display_total_duration_s_session, Total_nr_display))+
  geom_jitter(aes( colour = as.character(Visit_occurrence)), position = position_jitter(seed = 1, width = 0.2))+
  geom_smooth(method = "lm", se = FALSE)


FigS4A <- ggplot(Session_behav_sat, aes(
  x = Co.display_total_duration_s_session,
  y = Total_nr_display,
  color = as.character(Visit_occurrence)
)) +
  geom_jitter(
    aes(fill = as.character(Copulation_occurrence)),
  shape = 21,
  color = "black",
  alpha = 0.8,
  size = 7,
  width = 0.15,
  stroke = 1.1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  scale_fill_manual(values = c("gray", "#a855ff"), 
                     name = "Female visits") +
  guides(fill=guide_legend(title="Female visits")) +
  theme_classic(base_size = 20) +
  xlab("Co-display total duration [s]") +
  ylab("Display activity score") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "black")
  )

ggsave("FigS4A.png", FigS4A, width = 7, height = 6, dpi = 300)

# B: Co-display, Display activity and Copulation attempt occurrence for a Resident in a session

ggplot(Session_behav_sat, aes(Co.display_total_duration_s_session, Total_nr_display))+
  geom_jitter(aes( colour = as.character(Copulation_occurrence)), position = position_jitter(seed = 1, width = 0.2))+
  geom_smooth(method = "lm", se = FALSE)

FigS4B <- ggplot(Session_behav_sat, aes(
  x = Co.display_total_duration_s_session,
  y = Total_nr_display
)) +
  geom_jitter(
    aes(fill = as.character(Copulation_occurrence)),
    shape = 21,
    color = "black",
    alpha = 0.8,
    size = 7,
    width = 0.15,
    stroke = 1.1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  scale_fill_manual(values = c("gray", "#a855ff"), 
                    name = "Copulation attempts") +
  theme_classic(base_size = 20) +
  xlab("Co-display total duration [s]") +
  ylab("Display activity score") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "black")
  )
ggsave("FigS4B.png", FigS4B, width = 7, height = 6, dpi = 300)

## Figure S5: Results of an individual slope model on Resident aggressive behaviour --------------

# towards Satellites and Marginals  
filtered_df <- Combo_behav_no_state %>% dplyr::select(Condition, Attacks_marg, Attacks_sat, Combo_lek_week, Colour_code)
filtered_df$Combo_lek_week_ID <- paste(filtered_df$Combo_lek_week, filtered_df$Colour_code,sep="_")

filtered_df$Attacks_marg_sat <- rowSums(filtered_df[, c("Attacks_sat", "Attacks_marg")], na.rm = TRUE)

# reshape the data frame for geom_segment
reshaped_df <- filtered_df %>%
  group_by(Combo_lek_week_ID, Condition, Colour_code) %>%
  summarise(Attacks = sum(Attacks_marg_sat, na.rm = TRUE)) %>%
  pivot_wider(names_from = Condition, 
              values_from = Attacks, 
              values_fill = 0) %>%
  rename(Attacks_Marginal = Marginal,
         Attacks_Satellite = Satellite)

combo_attacks_ms_slope <-  reshaped_df %>%
  # add a variable for when Marginal is more successful than Satellite (for highlighting the colors)
  mutate(more_att_mar = Attacks_Marginal > Attacks_Satellite) %>%
  ggplot() + 
  # add a line segment that goes from Marginal to Satellite for each individual
  geom_segment(aes(x = 1, xend = 2, 
                   y = Attacks_Marginal, 
                   yend = Attacks_Satellite,
                   group = Combo_lek_week_ID,
                   col = more_att_mar), 
               size = 1.2)  +
  # set the colors
  scale_color_manual(values = c("#a86262", "darkgrey"))  +
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
  # vertical line that acts as axis as a Marginal
  geom_segment(aes(x = 1, xend = 1, 
                   y = min(c(Attacks_Marginal, Attacks_Satellite)),
                   yend = max(c(Attacks_Marginal, Attacks_Satellite))),
               col = "grey70", size = 0.8) +
  # add vertical lines that act as axis for Satellite
  geom_segment(aes(x = 2, xend = 2, 
                   y = min(c(Attacks_Marginal, Attacks_Satellite)),
                   yend = max(c(Attacks_Marginal, Attacks_Satellite))),
               col = "grey70", size = 0.8) +
  # Adjust y-axis limits to accommodate the new labels
  coord_cartesian(xlim = c(0.5, 2.5), 
                  ylim = c(0, 28)) + 
  # add the white outline for the points for Satellite absent cond
  geom_point(aes(x = 1, 
                 y = Attacks_Marginal), size = 3.5,
             col = "black", shape = 19) +
  # add the white outline for the points at each rate for Satellite present
  geom_point(aes(x = 2, 
                 y = Attacks_Satellite), size = 3.5,
             col = "black", shape = 19) +
  # add the actual points at session with Marginal copulations
  geom_point(aes(x = 1, 
                 y = Attacks_Marginal), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # add the actual points at each session with Satellite copulations
  geom_point(aes(x = 2, 
                 y = Attacks_Satellite), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # Add copulation numbers next to Marginal points in black
  geom_text(aes(x = 1 - 0.05, 
                y = Attacks_Marginal,
                label = Attacks_Marginal),
            hjust = "right", size = 4, color = "black") +
  # Add copulation numbers next to Satellite points in black
  geom_text(aes(x = 2 + 0.05, 
                y = Attacks_Satellite,
                label = Attacks_Satellite),
            hjust = "left", size = 4, color = "black") 

ggsave("combo_attacks_M&S_slope-sup.png", combo_attacks_ms_slope, width = 8, height = 6, dpi = 300)

## attack towards other residents  
filtered_df <- Combo_behav_no_state %>% dplyr::select(Condition, Aggression_res_attack_approach, Combo_lek_week, Colour_code)
filtered_df$Combo_lek_week_ID <- paste(filtered_df$Combo_lek_week, filtered_df$Colour_code,sep="_")

# reshape the data frame for geom_segment
reshaped_df <- filtered_df %>%
  group_by(Combo_lek_week_ID, Condition, Colour_code) %>%
  summarise(Attacks = sum(Aggression_res_attack_approach, na.rm = TRUE)) %>%
  pivot_wider(names_from = Condition, 
              values_from = Attacks, 
              values_fill = 0) %>%
  rename(Attacks_Marginal = Marginal,
         Attacks_Satellite = Satellite)

combo_attacks_otherRs_slope <-  reshaped_df %>%
  # add a variable for when Marginal is more successful than Satellite (for highlighting the colors)
  mutate(more_att_mar = Attacks_Marginal > Attacks_Satellite) %>%
  ggplot() + 
  # add a line segment that goes from Marginal to Satellite for each individual
  geom_segment(aes(x = 1, xend = 2, 
                   y = Attacks_Marginal, 
                   yend = Attacks_Satellite,
                   group = Combo_lek_week_ID,
                   col = more_att_mar), 
               size = 1.2)  +
  # set the colors
  scale_color_manual(values = c("#a86262", "darkgrey"))  +
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
  # vertical line that acts as axis as a Marginal
  geom_segment(aes(x = 1, xend = 1, 
                   y = min(c(Attacks_Marginal, Attacks_Satellite)),
                   yend = max(c(Attacks_Marginal, Attacks_Satellite))),
               col = "grey70", size = 0.8) +
  # add vertical lines that act as axis for Satellite
  geom_segment(aes(x = 2, xend = 2, 
                   y = min(c(Attacks_Marginal, Attacks_Satellite)),
                   yend = max(c(Attacks_Marginal, Attacks_Satellite))),
               col = "grey70", size = 0.8) +
  # Adjust y-axis limits to accommodate the new labels
  coord_cartesian(xlim = c(0.5, 2.5), 
                  ylim = c(0, 28)) + 
  # add the white outline for the points for Satellite absent cond
  geom_point(aes(x = 1, 
                 y = Attacks_Marginal), size = 3.5,
             col = "black", shape = 19) +
  # add the white outline for the points at each rate for Satellite present
  geom_point(aes(x = 2, 
                 y = Attacks_Satellite), size = 3.5,
             col = "black", shape = 19) +
  # add the actual points at session with Marginal copulations
  geom_point(aes(x = 1, 
                 y = Attacks_Marginal), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # add the actual points at each session with Satellite copulations
  geom_point(aes(x = 2, 
                 y = Attacks_Satellite), size = 3,
             col = "grey60", shape = 19, fill = "grey60") +
  # Add copulation numbers next to Marginal points in black
  geom_text(aes(x = 1 - 0.05, 
                y = Attacks_Marginal,
                label = Attacks_Marginal),
            hjust = "right", size = 4, color = "black") +
  # Add copulation numbers next to Satellite points in black
  geom_text(aes(x = 2 + 0.05, 
                y = Attacks_Satellite,
                label = Attacks_Satellite),
            hjust = "left", size = 4, color = "black") 

ggsave("combo_attacks_res_slope.png", combo_attacks_otherRs_slope, width = 8, height = 6, dpi = 300)


### Figure S6: ------

## Calling data ------

# Data set prep: Only Resident behaviour data set

Session_behav_all_res <- read.csv("Resident cooperation MS Kupan Data/Session_behav_no_state_aggression.csv")


# Only Satellite condition

Session_behav_sat <- Session_behav_all_res %>% filter(Condition == "Satellite") 


# Figure S6A: female visit numbers colored by co-display time  -------------
# Create a new column to flag zeros vs special cases

# model: f visits nr (update with date and female compartment)
res_session_vis_nr_nb <- glmmTMB(Total_visit_nr_all_females ~  scale(Co.display_total_duration_s_session)  +  Total_nr_display + Fm_ID + Date2 + (1|Combo_lek) + (1|Colour_code), family = nbinom2, data = Session_behav_sat, zi = ~ 0)
summary(res_session_vis_nr_nb)

# Create a new column to flag zeros vs special cases
Session_behav_sat$PointFlagVisits <- ifelse(
  Session_behav_sat$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat$Total_visit_nr_all_females > 0, "ZeroCoDisplay_Vis", 
  ifelse(Session_behav_sat$Total_visit_nr_all_females == 0, "Zero", "Non-zero")
)

table(Session_behav_sat$PointFlagVisits)
#Non-zero         Zero                ZeroCoDisplay_Cop 
#46               130                 4 

visit_nr_sess_zero <- plot_model(res_session_vis_nr_nb, type = "pred", 
                                 terms = c("Co.display_total_duration_s_session"), 
                                 colors = "#6e9e55") +
  geom_point(data = Session_behav_sat, 
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
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save 
# ggsave("visit_nr_session_zero.png", visit_nr_sess_zero, width = 7, height = 6, dpi = 600)


# Figure S6B: visit duration -----------------

# model 
visit_dur_lognorm_hurdle_m <- glmmTMB(Total_visit_duration_all_females ~ Date2 + Fm_ID + 
                                        scale(Co.display_total_duration_s_session)  +  Total_nr_display + (1|Video_ID), 
                                      data=Session_behav_sat, family = glmmTMB::lognormal(link="log"), ziformula = ~ 1  +  Total_nr_display )
summary(visit_dur_lognorm_hurdle_m)

### duration of visits colored by co-display duration 
# Create a new column to flag zeros vs special cases
Session_behav_sat$PointFlagVisitsDur <- ifelse(
  Session_behav_sat$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat$Total_visit_duration_all_females > 0, "ZeroCoDisplay_Vis", 
  ifelse(Session_behav_sat$Total_visit_duration_all_females == 0, "Zero", "Non-zero")
)

table(Session_behav_sat$PointFlagVisitsDur)
#Non-zero         Zero                ZeroCoDisplay_Cop 
#46               130                 4 

visit_dur_sess_zero <- plot_model(visit_dur_lognorm_hurdle_m, type = "pred", 
                                  terms = c("Co.display_total_duration_s_session"), 
                                  colors = "#6e9e55") +
  geom_point(data = Session_behav_sat, 
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
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  update_geom_defaults("line", list(size = 1.5))
# save 
# ggsave("visit_dur_session_zero.png", visit_dur_sess_zero, width = 7, height = 6, dpi = 600)

# Figure S6C: copulation attempts -----------------
# model
cop_nr_nb_mm <- glmmTMB(Copulation_nr ~  scale(Co.display_total_duration_s_session) +  Total_nr_display + Date2 + Fm_ID + (1|Colour_code), data = Session_behav_sat, 
                        family = nbinom2, zi = ~ 0)
summary(cop_nr_nb_mm)


# Figure S6C: copulation attemps colored by co-display duration ---------------------

# model
cop_nr_nb_mm <- glmmTMB(Copulation_nr ~  scale(Co.display_total_duration_s_session) +  Total_nr_display + Date2 + Fm_ID + (1|Colour_code), data = Session_behav_sat, 
                        family = nbinom2, zi = ~ 0)
summary(cop_nr_nb_mm)

# Create a new column to flag zeros vs special cases
Session_behav_sat$PointFlag <- ifelse(
  Session_behav_sat$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat$Copulation_nr > 0, "ZeroCoDisplay_Cop", 
  ifelse(Session_behav_sat$Copulation_nr == 0, "Zero", "Non-zero")
)

table(Session_behav_sat$PointFlag)
# Non-zero         Zero           ZeroCoDisplay_Cop 
# 22               156                 2 

cop_nr_session_0 <- 
  plot_model(cop_nr_nb_mm, type = "pred", 
             terms = c("Co.display_total_duration_s_session"), 
             colors = "#6e9e55") +
  geom_point(data = Session_behav_sat, 
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
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 1.5))
# save
# ggsave("cop_nr_sess_zero.png", cop_nr_session_0, width = 7, height = 6, dpi = 600)


# Figure S6D: successful copulations ----------------

# final model 
res_session_suc_cop_nr_nb_zi <- glmmTMB(Total_successful_copulations ~ scale(Co.display_total_duration_s_session)  +  Total_nr_display + Date2 + Fm_ID + (1|Colour_code) + (1|Combo_lek), 
                                        data = Session_behav_sat,  family = nbinom2, zi = ~ Total_nr_display)
summary(res_session_suc_cop_nr_nb_zi)  

# zero co display but succ cop 
# Create a new column to flag zeros vs special cases
Session_behav_sat$PointFlagSC <- ifelse(
  Session_behav_sat$Co.display_total_duration_s_session == 0 & 
    Session_behav_sat$Total_successful_copulations > 0, "ZeroCoDisplay_Cop", 
  ifelse(Session_behav_sat$Total_successful_copulations == 0, "Zero", "Non-zero")
)

table(Session_behav_sat$PointFlagSC)
#Non-zero              Zero ZeroCoDisplay_Cop 
#18               160                 2 

succ_cop_nr_session_0 <- 
  plot_model(res_session_suc_cop_nr_nb_zi, type = "pred", 
             terms = c("Co.display_total_duration_s_session"), 
             colors = "darkgray") +
  geom_point(data = Session_behav_sat, 
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
  theme_classic(base_size = 20) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none" # make sure the axis lines are here 
  )  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  update_geom_defaults("line", list(size = 2.5))
# ggsave("succ_cop_nr_sess_zero.png", succ_cop_nr_session_0, width = 7, height = 6, dpi = 600)
