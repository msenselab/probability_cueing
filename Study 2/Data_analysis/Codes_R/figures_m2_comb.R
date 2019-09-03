library(tidyverse)
library(ez)
library(openxlsx)
library(BayesFactor)
# getMeans, getSummary, plotMeans
source('aux_functions.R')

# ---- Read data -----

raw_e3 <- readRDS('data/E3_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 1' )
raw_e4 <- readRDS('data/E4_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 2' )
raw_e5 <- readRDS('data/E5_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 3' )
raw_e6 <- readRDS('data/E6_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 4' )

raw_e3$sub <- factor(raw_e3$sub, levels = unique(raw_e3$sub), labels = paste0(1:30, '_e1'))
raw_e4$sub <- factor(raw_e4$sub, levels = unique(raw_e4$sub), labels = paste0(1:30, '_e2'))
raw_e5$sub <- factor(raw_e5$sub, levels = unique(raw_e5$sub), labels = paste0(1:30, '_e3'))
raw_e6$sub <- factor(raw_e6$sub, levels = unique(raw_e6$sub), labels = paste0(1:30, '_e4'))

N <- 30

old_subs <- trimws(apply(expand.grid(1:24,c('_e1', '_e2', '_e3', '_e4')),1,paste,collapse=""))

raw_e3$colswap <- "Swapping"
raw_e4$colswap <- "No swapping" 
raw_e5$colswap <- "Swapping"
raw_e6$colswap <- "No swapping"

raw_e3$density <- "Sparse"
raw_e4$density <- "Sparse"
raw_e5$density <- "Dense"
raw_e6$density <- "Dense"

raw_e3 <- mutate(raw_e3, subsession = if_else(tno>500, if_else(tno>1000, 3, 2), 1))
raw_e4 <- mutate(raw_e4, subsession = if_else(session=="s2", if_else(tno>480, 4, 3),
                                              if_else(tno>480, 2, 1)))
raw_e5 <- mutate(raw_e5, subsession = if_else(session=="s2", 
                                              if_else(tno>480, if_else(tno>960, 6, 5), 4),
                                              if_else(tno>480, if_else(tno>960, 3, 2), 1)))
raw_e6 <- mutate(raw_e6, subsession = if_else(session=="s2", if_else(tno>480, 4, 3), 
                                              if_else(tno>480, 2, 1)))

# combine two experiments together
raw_e3$session <- "s1"
raw_e3$number <- NA
raw_e4$color <- "fixed"
raw_e3$age <- "NA"
raw_e3$sex <- "NA"
raw_e4$age <- "NA"
raw_e4$sex <- "NA"
raw_e3_e4 <- rbind(raw_e3, raw_e4) %>%
  mutate(tar_cond = fct_recode(tar_cond, "Rare" = "Low", "Freq." = "High"),
         dist_cond = fct_recode(dist_cond, "Rare Distr." = "Low", "Freq. Distr." = "High", "No Dist." = "No"), 
         session = fct_recode(session, "Session 1"  = "s1", "Session 2" = "s2"))
distance <- c(1,2,3,4,3,2,1)
raw_e3_e4 <- mutate(raw_e3_e4, tar_dist_distance = 
                      if_else(dist_pos==0, 0, distance[abs(tar_pos-dist_pos)])) 
raw_e3_e4$tar_dist_distance <- factor(raw_e3_e4$tar_dist_distance, levels=0:4, 
                                      labels=c("No Dist.", "1", "2", "3", "4"))

raw_e6$color <- "fixed"
raw_e6$age <- "NA"
raw_e6$sex <- "NA"
raw_e5_e6 <- rbind(raw_e5, raw_e6) %>%
  mutate(tar_cond = fct_recode(tar_cond, "Rare" = "Low", "Freq." = "High"),
         dist_cond = fct_recode(dist_cond, "Rare Distr." = "Low", "Freq. Distr." = "High", "No Dist." = "No"), 
         session = fct_recode(session, "Session 1"  = "s1", "Session 2" = "s2"))
raw_e5_e6 <- mutate(raw_e5_e6, tar_dist_distance = 
                      if_else(dist_pos==0, 0, distance[abs(tar_pos-dist_pos)])) 
raw_e5_e6$tar_dist_distance <- factor(raw_e5_e6$tar_dist_distance, levels=0:4, 
                                      labels=c("No Dist.", "1", "2", "3", "4"))
raw_e5_e6$number <- NA

rawtot <- rbind(raw_e3_e4, raw_e5_e6)

# ---- Mean RTs and error -----
# add sd summary in mrt_tot
mrt_tot <- filter(rawtot, session=="Session 1", correct, !outlier, tno<=960, 
                  dist_cond=="No Dist.") %>% 
  group_by(colswap, density, sub) %>% summarize(mrt=mean(rt)*1000, sd = sd(rt)*1000)

mrt_summary <- mrt_tot %>% summarize( ci_rt=sd(mrt)/sqrt(N-1)*1.96, mrt=mean(mrt),
                                     msd = mean(sd), ci_sd = sd(sd)/sqrt(N-1)*1.96) 
pj = position_dodge(width = 0.2)
fig_mrt_summary <- mrt_summary %>% ggplot(aes(x=colswap, y=mrt, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mrt-ci_rt, ymax=mrt+ci_rt), width=0.2, position = pj) +
  labs(x="", y="Mean RT (ms)") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="none")
fig_mrt_summary

# add sd figure
fig_msd_summary <- mrt_summary %>% ggplot(aes(x=colswap, y=msd, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line( position = pj) +
  geom_errorbar(aes(ymin=msd-ci_sd, ymax=msd+ci_sd), width=0.2, position = pj) +
  labs(x="", y="Mean SD (ms)") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="none")
fig_msd_summary

fig_baseline = plot_grid(fig_mrt_summary, fig_msd_summary, nrow = 1, labels = 'auto' )
fig_baseline

ggsave('manuscript2/figs/fig_summary_base.png',fig_baseline, width = 7, height = 4)

err_tot <- filter(rawtot, session=="Session 1", dist_cond=="No Dist.", tno<=960) %>% 
  group_by(colswap, density, sub) %>% summarize(err=1-mean(correct))

ies_tot <- full_join(mrt_tot, err_tot) %>% mutate(ies = mrt/(1-err)) # Inverse efficiency score
ies_tot %>% ezANOVA(dv=ies, wid=sub, between=.(density, colswap))

write.csv(mrt_tot, './manuscript2/stats/mrt_tot.csv')

write.csv(ies_tot, './manuscript2/stats/ies_tot.csv')

err_summary <- err_tot %>% summarize(merr=mean(err), ci_err=sd(err)/sqrt(N-1)*1.96) 

fig_err_summary <- err_summary %>% ggplot(aes(x=colswap, y=merr, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=merr-ci_err, ymax=merr+ci_err), width=0.1, position = pj) +
  labs(x="Color", y="Error rate") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="bottom")
   
mrt_tot %>% ezANOVA(dv=mrt, wid=sub, between=.(density, colswap))
# add sd anova test
mrt_tot %>% ezANOVA(dv=sd, wid=sub, between=.(density, colswap))

err_tot %>% ezANOVA(dv=err, wid=sub, between=.(density, colswap))

fig_summary = plot_grid(fig_mrt_summary, fig_msd_summary, fig_err_summary, nrow = 2, ncol=2, rel_heights = c(5,3))
fig_summary

ggsave('manuscript2/figs/fig_summary.png',fig_summary, width = 9, height = 5)

# ---- Distractor interference and prob. cueing ----

dist_tot <- filter(rawtot, session=="Session 1", correct, !outlier, tno<=960) %>% 
  group_by(dist_cond, colswap, density, sub) %>% summarize(rt=mean(rt)*1000) %>% 
  spread(dist_cond, rt) %>% mutate(distraction = (`Rare Distr.` + `Freq. Distr.`)/2 - `No Dist.`,
                                   prob_cueing = (`Rare Distr.` - `Freq. Distr.`)) 

dist_summary <- dist_tot %>% summarize(mdist=mean(distraction), mpc=mean(prob_cueing), 
            ci_dist=sd(distraction)/sqrt(N-1)*1.96, ci_pc=sd(prob_cueing)/sqrt(N-1)*1.96) 

dist_tot %>% ezANOVA(dv=distraction, wid=sub, between=.(density, colswap))
dist_tot %>% ezANOVA(dv=prob_cueing, wid=sub, between=.(density, colswap))

write.csv(dist_tot, './manuscript2/stats/dist_tot.csv')

dist_err <- filter(rawtot, session=="Session 1", tno<=960) %>% 
  group_by(dist_cond, colswap, density, sub) %>% summarize(err=1-mean(correct)) %>% 
  spread(dist_cond, err) %>% mutate(distraction = (`Rare Distr.` + `Freq. Distr.`)/2 - `No Dist.`,
                                    prob_cueing = (`Rare Distr.` - `Freq. Distr.`)) 

dist_err_summary <- dist_err %>% summarize(mdist=mean(distraction), mpc=mean(prob_cueing), 
                                           ci_dist=sd(distraction)/sqrt(N-1)*1.96, 
                                           ci_pc=sd(prob_cueing)/sqrt(N-1)*1.96) 

fig_dist_interference <- dist_summary %>% 
  ggplot(aes(x=colswap, y=mdist, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mdist-ci_dist, ymax=mdist+ci_dist), width=0.1, position = pj) +
  labs(x="Color", y="Distractor interference (ms)") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="none")

fig_dist_interference_err <- dist_err_summary %>% 
  ggplot(aes(x=colswap, y=mdist, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mdist-ci_dist, ymax=mdist+ci_dist), width=0.1, position = pj) +
  labs(x="Color", y="Interference error") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="bottom", legend.justification = "top")

fig_interference = plot_grid(fig_dist_interference, fig_dist_interference_err, nrow=2, rel_heights = c(5,3))
fig_interference 

ggsave('manuscript2/figs/fig_dist_interference.png', fig_interference, width=7.5, height=7)


# Re-plot for ECVP conference
myTheme <- theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major = element_line(colour = "lightgrey"),
  strip.background = element_rect(fill = "steelblue"),
  strip.text = element_text(color = "white"),
  axis.text = element_text(color = "black", face='bold')
)

fig_dist_interference_con <- dist_summary %>% 
  ggplot(aes(x=colswap, y=mdist, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mdist-ci_dist, ymax=mdist+ci_dist), width=0.1, position = pj) +
  labs(x="Color", y="Distractor interference (ms)") + myTheme 
  #   scale_color_manual(values = c( 'black','grey')) + 
# theme(legend.position="bottom", legend.justification = "top")  

fig_dist_interference_err_con <- dist_err_summary %>% 
  ggplot(aes(x=colswap, y=mdist, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mdist-ci_dist, ymax=mdist+ci_dist), width=0.1, position = pj) +
  labs(x="Color", y="Interference error") +   myTheme 
  #     scale_color_manual(name="Density", values = c('black','grey')) + 
# theme(legend.position="bottom", legend.justification = "top")

fig_interference_con = plot_grid(fig_dist_interference_con, fig_dist_interference_err_con, nrow=2, rel_heights = c(5,3))
fig_interference_con 

ggsave('manuscript2/figs/fig_dist_interference_con.png', fig_dist_interference_con, width=7.5, height=5)


dist_tot %>% filter(colswap=="No swapping") -> no_swap_dist
dist_tot %>% filter(colswap=="Swapping") -> swap_dist

t.test(swap_dist$distraction)
ttestBF(swap_dist$distraction)
t.test(no_swap_dist$distraction)
ttestBF(no_swap_dist$distraction)

t.test(filter(dist_tot, colswap=="Swapping", density=="Dense")$distraction)
ttestBF(filter(dist_tot, colswap=="Swapping", density=="Dense")$distraction)

t.test(filter(dist_tot, colswap=="Swapping", density=="Sparse")$distraction)
ttestBF(filter(dist_tot, colswap=="Swapping", density=="Sparse")$distraction)

t.test(filter(dist_tot, colswap=="No swapping", density=="Dense")$distraction)
ttestBF(filter(dist_tot, colswap=="No swapping", density=="Dense")$distraction)

t.test(filter(dist_tot, colswap=="No swapping", density=="Sparse")$distraction)
ttestBF(filter(dist_tot, colswap=="No swapping", density=="Sparse")$distraction)

fig_dist_prob <- dist_summary %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Probability cueing effect (ms)") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="none")

fig_dist_prob_err <- dist_err_summary %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Prob. cueing error") + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="bottom", legend.justification = "top")

fig_prob_cueing = plot_grid(fig_dist_prob, fig_dist_prob_err, nrow=2, rel_heights = c(5,3))
fig_prob_cueing

ggsave('manuscript2/figs/fig_dist_prob.png', fig_prob_cueing, width=7.5, height=7)


# Re-plot for ECVP conference
fig_dist_prob_con <- dist_summary %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Probability cueing effect (ms)") + myTheme
  #  scale_color_manual(name="Density", values = c('black','grey')) + 
  # theme(legend.position="none")

fig_dist_prob_err_con <- dist_err_summary %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Prob. cueing error") + myTheme
  #   scale_color_manual(name="Density", values = c('black','grey')) + 
# theme(legend.position="bottom", legend.justification = "top")

fig_prob_cueing_con = plot_grid(fig_dist_prob_con, fig_dist_prob_err_con, nrow=2, rel_heights = c(5,3))
fig_prob_cueing_con

ggsave('manuscript2/figs/fig_dist_prob_con.png', fig_dist_prob_con, width=7.5, height=5)


t.test(filter(dist_tot, colswap=="Swapping", density=="Dense")$prob_cueing)
ttestBF(filter(dist_tot, colswap=="Swapping", density=="Dense")$prob_cueing)

t.test(filter(dist_tot, colswap=="Swapping", density=="Sparse")$prob_cueing)
ttestBF(filter(dist_tot, colswap=="Swapping", density=="Sparse")$prob_cueing)

t.test(filter(dist_tot, colswap=="No swapping", density=="Dense")$prob_cueing)
ttestBF(filter(dist_tot, colswap=="No swapping", density=="Dense")$prob_cueing)

t.test(filter(dist_tot, colswap=="No swapping", density=="Sparse")$prob_cueing)
ttestBF(filter(dist_tot, colswap=="No swapping", density=="Sparse")$prob_cueing)

# ---- Target prob. cueing ----

# Distractor absent trials

tar_tot_da <- group_by(rawtot, blkNo) %>% mutate(pos_inttrial=if_else(tar_pos==lag(dist_pos), 'Coincident', 'Non-coincident')) %>%
  filter(session=="Session 1", pos_inttrial=="Non-coincident", correct, !outlier, tno <= 960, dist_cond=="No Dist.") %>% 
  group_by(tar_cond, colswap, density, sub) %>% summarize(rt=mean(rt)*1000) %>% 
  spread(tar_cond, rt) %>% mutate(prob_cueing = Freq. - Rare) 

tar_summary_da <- tar_tot_da %>% summarize(mpc=mean(prob_cueing), ci_pc=sd(prob_cueing)/sqrt(N-1)*1.96)

tar_err_da <- group_by(rawtot, blkNo) %>% mutate(pos_inttrial=if_else(tar_pos==lag(dist_pos), 'Coincident', 'Non-coincident')) %>%
  filter(session=="Session 1", pos_inttrial=="Non-coincident", tno <= 960, dist_cond=="No Dist.") %>% 
  group_by(tar_cond, colswap, density, sub) %>% summarize(err=1-mean(correct)) %>% 
  spread(tar_cond, err) %>% mutate(prob_cueing = Freq. - Rare) 

tar_summary_err_da <- tar_err_da %>% summarize(mpc=mean(prob_cueing), ci_pc=sd(prob_cueing)/sqrt(N-1)*1.96)

tar_tot_da %>% filter(colswap=="Swapping") -> swap_pc
tar_tot_da %>% filter(colswap=="No swapping") -> no_swap_pc

t.test(swap_pc$prob_cueing)
ttestBF(swap_pc$prob_cueing)
t.test(no_swap_pc$prob_cueing)
ttestBF(no_swap_pc$prob_cueing)

t.test(filter(tar_tot_da, colswap=="Swapping", density=="Dense")$prob_cueing)
ttestBF(filter(tar_tot_da, colswap=="Swapping", density=="Dense")$prob_cueing)

t.test(filter(tar_tot_da, colswap=="Swapping", density=="Sparse")$prob_cueing)
ttestBF(filter(tar_tot_da, colswap=="Swapping", density=="Sparse")$prob_cueing)

t.test(filter(tar_tot_da, colswap=="No swapping", density=="Dense")$prob_cueing)
ttestBF(filter(tar_tot_da, colswap=="No swapping", density=="Dense")$prob_cueing)

t.test(filter(tar_tot_da, colswap=="No swapping", density=="Sparse")$prob_cueing)
ttestBF(filter(tar_tot_da, colswap=="No swapping", density=="Sparse")$prob_cueing)

fig_tar_prob <- tar_summary_da %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Target position effect (ms)")  + 
  scale_color_manual(name="Density", values = c('black','grey'))  + 
  theme(legend.position="none")

fig_tar_prob_err <- tar_summary_err_da %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.1, position = pj) +
  labs(x="Color", y="Target pos. error")  + 
  scale_color_manual(name="Density", values = c('black','grey')) + 
  theme(legend.position="bottom", legend.justification = "top")

fig_tar_prob_cueing = plot_grid(fig_tar_prob, fig_tar_prob_err, nrow=2, rel_heights = c(5,3))
fig_tar_prob_cueing

ggsave('manuscript2/figs/fig_tar_prob.png', fig_tar_prob_cueing, width=7.5, height=7)


# Re-plot for ECVP conference
myTheme <- theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major = element_line(colour = "lightgrey"),
  strip.background = element_rect(fill = "steelblue"),
  strip.text = element_text(color = "white"),
  axis.text.x = element_text(face='bold', size=18),
  axis.text.y = element_text(face='bold', size=15),
  axis.title = element_text(color = "black", face='bold', size=18),
  legend.title = element_text(face = "bold", size=18), 
  legend.text = element_text(face = "bold", size=18)
)

fig_tar_prob_con <- tar_summary_da %>% 
  ggplot(aes(x=colswap, y=mpc, color=density, group=density)) +
  geom_point(size=3, position = pj) + geom_line(size=1.5,position = pj) +
  geom_errorbar(aes(ymin=mpc-ci_pc, ymax=mpc+ci_pc), width=0.5, position = pj) +
  labs(x="Color", y="Target position effect (ms)")  + myTheme
# scale_color_manual(values = c("#20639B", "#3CAEA3"))+ myTheme
# theme(legend.position="none")

ggsave('manuscript2/figs/fig_tar_prob_cueing_con.png', fig_tar_prob_con , width=9, height=6)


tar_tot_da %>% ezANOVA(dv=prob_cueing, wid=sub, between=.(colswap, density))

write.csv(tar_tot_da, './manuscript2/stats/tar_tot.csv')


tar_tot_dp <- group_by(rawtot, blkNo) %>% mutate(pos_inttrial=if_else(tar_pos==lag(dist_pos), 'Coincident', 'Non-coincident')) %>%
  filter(session=="Session 1",  pos_inttrial=="Non-coincident", correct, !outlier, dist_cond!="No Dist.", tno<=960) %>% 
  group_by(tar_cond, colswap, density, sub) %>% summarize(rt=mean(rt)*1000) %>% 
  spread(tar_cond, rt) %>% mutate(prob_cueing = Freq. - Rare) 

tar_tot_dp$dist <- "Present"
tar_tot_da$dist <- "Absent"

tar_tot <- rbind(tar_tot_dp, tar_tot_da) %>% select(colswap, density, sub, dist, prob_cueing) %>% 
  spread(dist, prob_cueing)

t.test(filter(tar_tot, colswap=="Swapping", Absent > median(Absent))$Present,
       filter(tar_tot, colswap=="Swapping", Absent <= median(Absent))$Present)
ttestBF(filter(tar_tot, colswap=="Swapping", Absent > median(Absent))$Present,
        filter(tar_tot, colswap=="Swapping", Absent <= median(Absent))$Present)

t.test(filter(tar_tot, colswap=="No swapping", Absent > median(Absent))$Present,
       filter(tar_tot, colswap=="No swapping", Absent <= median(Absent))$Present)
ttestBF(filter(tar_tot, colswap=="No swapping", Absent > median(Absent))$Present,
        filter(tar_tot, colswap=="No swapping", Absent <= median(Absent))$Present)

# ---- Color swapping ----

# First check whether color swapping vs. repeat has an effect on interference or prob. cueing

group_by(rawtot, blkNo) %>% mutate(pcol=lag(color), 
                                   rcol=if_else(pcol==color, "Repeat", "Switch")) %>% 
  filter(!is.na(rcol), correct, !outlier, colswap=="Swapping", tno<=1440) %>% group_by(rcol, dist_cond,  density, sub) %>% 
  summarize(rt=mean(rt)*1000) %>% spread(dist_cond, rt) %>%
  mutate(interference = (`Rare Distr.` + `Freq. Distr.`)/2 - `No Dist.`, 
         prob_cueing = (`Rare Distr.` - `Freq. Distr.`)) -> 
  int_col_tot

sint_col_tot <- int_col_tot %>% summarize(mint=mean(interference), mpc=mean(prob_cueing))

int_col_tot %>% ezANOVA(dv=interference, wid=sub, within=rcol, between=density)

int_col_tot %>% select(interference, rcol, density, sub) %>% 
  spread(rcol, interference) -> int_col_spread
write.csv(int_col_spread, './manuscript2/stats/int_col.csv')

int_col_tot %>% ezANOVA(dv=prob_cueing, wid=sub, within=rcol, between=density)

int_col_tot %>% select(prob_cueing, rcol, density, sub) %>% 
  spread(rcol, prob_cueing) -> pc_col_spread
write.csv(pc_col_spread, './manuscript2/stats/pc_col.csv')

# More detailed analysis of color rep. vs. swap effects

group_by(rawtot, blkNo) %>% mutate(pcol=lag(color),
                                   dist_present = if_else(dist_cond=="No Dist.", "Absent", "Present"), 
                                   pdp=lag(dist_present), 
                                   rcol=if_else(pcol==color, "Repeat", "Switch")) %>% 
  filter(!is.na(rcol), correct, colswap=="Swapping", !outlier, tno<=1440) %>% 
  group_by(density, rcol, dist_present, pdp,  sub) %>% 
  summarize(rt=mean(rt)*1000) %>% spread(rcol, rt) %>% mutate(diff=Switch-Repeat) -> col_tot

col_tot %>% ezANOVA(dv=diff, wid=sub, within=.(dist_present, pdp), between=density)

col_tot %>% select(density, dist_present, pdp, sub, diff) %>% unite(dp_pdp, dist_present, pdp) %>%
  spread(dp_pdp, diff) -> col_tot_spread
write.csv(col_tot_spread, "./manuscript2/stats/col_tot.csv")

group_by(rawtot, blkNo) %>% mutate(pcol=lag(color),
                                   dist_present = if_else(dist_cond=="No Dist.", "Absent", "Present"), 
                                   pdp=lag(dist_present), 
                                   rcol=if_else(pcol==color, "Repeat", "Switch")) %>% 
  filter(!is.na(rcol), colswap=="Swapping", tno<=1440) %>% 
  group_by(density, rcol, dist_present, pdp,  sub) %>% 
  summarize(err=1-mean(correct)) %>% spread(rcol, err) %>% mutate(diff=Switch-Repeat) -> col_err

fig_col <- col_tot %>% summarize(mdiff=mean(diff), se_diff=sd(diff)*1.96/sqrt(N-1)) %>% 
  ggplot(aes(x=dist_present, y=mdiff, color=pdp, shape=pdp, group=pdp)) + geom_point(size=3, position = pj) + 
  geom_line(position = pj) + geom_errorbar(aes(ymin=mdiff-se_diff, ymax=mdiff+se_diff), width=0.1, position = pj) + 
  labs(x="Distractor (trial n)", y = "Color repetition effect (ms)", color="Distractor (trial n-1)") + 
  theme(legend.position="bottom") + scale_shape_manual(name = "Distractor (trial n-1)", values=c('circle', 'triangle')) +
  scale_color_manual(name="Distractor (trial n-1)", values = c('grey','black')) +
  theme_classic() + theme(strip.background = element_blank()) + facet_wrap(~density)  + 
  theme(legend.position="none", text = element_text(size=15))

fig_col_err <- col_err %>% summarize(mdiff=mean(diff), se_diff=sd(diff)*1.96/sqrt(N-1)) %>% 
  ggplot(aes(x=dist_present, y=mdiff, color=pdp, shape=pdp, group=pdp)) + geom_point(size=3, position = pj) + 
  geom_line(position = pj) + geom_errorbar(aes(ymin=mdiff-se_diff, ymax=mdiff+se_diff), width=0.1, position = pj) + 
  labs(x="Distractor (trial n)", y = "Col. rep. error effect", color="Distractor (trial n-1)") + 
  theme(legend.position="bottom") + scale_shape_manual(name = "Distractor (trial n-1)", values=c('circle', 'triangle')) +
  scale_color_manual(name="Distractor (trial n-1)", values = c('grey','black')) +
  theme_classic() + theme(strip.background = element_blank()) + facet_wrap(~density)  + 
  theme(legend.position="bottom", legend.justification = "top", text = element_text(size=15))

fig_color = plot_grid(fig_col, fig_col_err, nrow=2, rel_heights = c(5,3))
fig_color

ggsave('manuscript2/figs/fig_col.png', fig_color, width=9.5, height=7)

group_by(col_tot, pdp, density, sub) %>% summarize(mdiff=mean(diff)) -> scol_tot
filter(col_tot, dist_present=="Present") -> col_tot_present
filter(col_tot, dist_present=="Absent") -> col_tot_absent

t.test(filter(col_tot_present, pdp=="Present", density=="Dense")$diff, filter(col_tot_present, pdp=="Absent", density=="Dense")$diff, paired=TRUE)
ttestBF(filter(col_tot_present, pdp=="Present", density=="Dense")$diff, filter(col_tot_present, pdp=="Absent", density=="Dense")$diff, paired=TRUE)

t.test(filter(col_tot_present, pdp=="Present", density=="Sparse")$diff, filter(col_tot_present, pdp=="Absent", density=="Sparse")$diff, paired=TRUE)
ttestBF(filter(col_tot_present, pdp=="Present", density=="Sparse")$diff, filter(col_tot_present, pdp=="Absent", density=="Sparse")$diff, paired=TRUE)

t.test(filter(col_tot_absent, pdp=="Present", density=="Dense")$diff, filter(col_tot_absent, pdp=="Absent", density=="Dense")$diff, paired=TRUE)
ttestBF(filter(col_tot_absent, pdp=="Present", density=="Dense")$diff, filter(col_tot_absent, pdp=="Absent", density=="Dense")$diff, paired=TRUE)

t.test(filter(col_tot_absent, pdp=="Present", density=="Sparse")$diff, filter(col_tot_absent, pdp=="Absent", density=="Sparse")$diff, paired=TRUE)
ttestBF(filter(col_tot_absent, pdp=="Present", density=="Sparse")$diff, filter(col_tot_absent, pdp=="Absent", density=="Sparse")$diff, paired=TRUE)

# ---- Baseline RT learning ----

base_RT_learn <- filter(rawtot, sub %in% old_subs, correct, !outlier, dist_cond=="No Dist.") %>%
  group_by(density, colswap, session, subsession, sub) %>% 
  summarize(rt=mean(rt)*1000) %>% mutate(N = length(unique(sub)))

base_RT_learn_summary <- base_RT_learn %>% summarize(N=mean(N), mRT=mean(rt),
                                                     seRT=sd(rt)/sqrt(N-1)) 

base_RT_learn_summary  %>%
  ggplot(aes(x=subsession, y=mRT, color=session)) + facet_grid(colswap~density) + geom_line() + 
  geom_point() + geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) +
  labs(x="Sub-session", y="Mean reponse time (ms)")

# ---- Distractor interference and prob. cueing learning ----

dist_learn <- filter(rawtot, sub %in% old_subs, correct, !outlier) %>%
  group_by(density, colswap, session, dist_cond, subsession, sub) %>% 
  summarize(rt=mean(rt)*1000) %>% spread(dist_cond, rt) %>% 
  mutate(distraction = (`Rare Distr.` + `Freq. Distr.`)/2 - `No Dist.`,
         prob_cueing = (`Rare Distr.` - `Freq. Distr.`)) %>% mutate(N = length(unique(sub)))

 dist_learn_summary <- dist_learn %>% summarize(N=mean(N), m_dist=mean(distraction), 
                                          se_dist=sd(distraction)/sqrt(N-1), 
                                          m_pc=mean(prob_cueing), 
                                          se_pc=sd(prob_cueing)/sqrt(N-1)) 
 
 dist_learn_summary %>%
  ggplot(aes(x=subsession, y=m_dist, color=session)) + facet_grid(colswap~density) + geom_line() + 
  geom_point() + geom_errorbar(aes(ymin=m_dist-se_dist, ymax=m_dist+se_dist), width=0.2) +
  labs(x="Sub-session", y="Distractor interference (ms)")

 dist_learn_summary %>%
   ggplot(aes(x=subsession, y=m_pc, color=session)) + facet_grid(colswap~density) + geom_line() + 
   geom_point() + geom_errorbar(aes(ymin=m_pc-se_pc, ymax=m_pc+se_pc), width=0.2) +
   labs(x="Sub-session", y="Probability cueing (ms)")
 
 # ---- Target position effect learning ----
 
 tar_learn <- group_by(rawtot, blkNo) %>% mutate(pos_inttrial=if_else(tar_pos==lag(dist_pos), 'Coincident', 'Non-coincident')) %>%
   filter(sub %in% old_subs, correct, !outlier, pos_inttrial=="Non-coincident", dist_cond=="No Dist.") %>%
   group_by(tar_cond, colswap, subsession, session, density, sub) %>% summarize(rt=mean(rt)*1000) %>% 
   spread(tar_cond, rt) %>% mutate(prob_cueing = Freq. - Rare) 
 
 tar_learn_summary <- tar_learn %>% summarize(N=mean(N), m_pc=mean(prob_cueing), 
                                                se_pc=sd(prob_cueing)/sqrt(N-1)) 
 
 tar_learn_summary %>%
   ggplot(aes(x=subsession, y=m_pc, color=session)) + facet_grid(colswap~density) + geom_line() + 
   geom_point() + geom_errorbar(aes(ymin=m_pc-se_pc, ymax=m_pc+se_pc), width=0.2) +
   labs(x="Sub-session", y="Probability cueing (ms)")
 
 