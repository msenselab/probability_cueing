library(tidyverse)
library(ez)
library(openxlsx)
# getMeans, getSummary, plotMeans
source('aux_functions.R')

# 1.=== read raw data ===
raw_e1 <- readRDS('data/E1_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 1' )
raw_e2 <- readRDS('data/E2_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 2' )
raw_e7 <- readRDS('data/E7_raw_data.rds') %>% mutate(outlier = (rt > 3 | rt < 0.15 | blkNo==1), exp = 'Exp. 3' )
# combine two experiments together
raw_e1_e2_e7 <- rbind(raw_e1, raw_e2, raw_e7) %>%
  mutate(tar_cond = fct_recode(tar_cond, "Rare" = "Low", "Freq." = "High"),
         dist_cond = fct_recode(dist_cond, "Rare Distr." = "Low", "Freq. Distr." = "High", "No Dist." = "No"), 
         session = fct_recode(session, "Session 1"  = "s1", "Session 2" = "s2"))
# above rename conditions
# number of subject
N <- length(unique(raw_e1$sub))

# 2. === Figure 2 Mean RTs and errors====
# obtain mean RTs and mean Errors
meanRTs = getSummary(raw_e1_e2_e7, grp.vars = quos(exp,session, tar_cond, dist_cond))

# export mean RT data
meanRTs %>% select(exp, session, tar_cond, dist_cond, m_rt) %>% 
  spread(session, m_rt) %>% arrange(exp, dist_cond)-> gmrt
#write.xlsx(gmrt, file = 'data/mrt.xlsx')

# mean RTs as target distance
meanRTs2 = getSummary(raw_e1_e2_e7, grp.vars = quos(exp,session, tar_distance, dist_cond))
meanRTs2 %>% select(exp, session, tar_distance, dist_cond, m_rt) %>% 
  spread(session, m_rt) %>% arrange(exp, dist_cond)-> gmrt2
#write.xlsx(gmrt2, file = 'data/mrt2.xlsx')

# plot RTs and errors
pj = position_dodge(width = 0.5) #width = 0.1)
# plot rt with distractor condition as x-axis, separate color for target position, 
# line type and point shape for the session.
fig2_RT = ggplot(meanRTs, aes(dist_cond, m_rt, fill = tar_cond)) + 
  #geom_point(size = 3, position=pj) + 
  geom_bar(aes(group = interaction(tar_cond, session)), color = 'black',stat = 'identity', position=pj, width = 0.5) + 
  geom_errorbar(aes(ymin = m_rt, ymax = m_rt + se_rt), width = 0.2, position=pj) + 
  scale_fill_manual(values = c('grey','black')) +
#  scale_fill_brewer(palette = 'Greys') +
  #scale_color_brewer(palette = 'Greys') +
  facet_grid(session ~ exp) +     theme_bw()+
  guides(linetype = FALSE, color = FALSE, shape = FALSE, fill = guide_legend(title='Target Position')) + # hide legends
  theme( strip.background = element_blank(), legend.position = 'bottom') + # make the subtitle no background
  xlab("") + # hide x-axis, since error figure has it
  ylab("Mean RTs (secs)") + 
  coord_cartesian(ylim = c(0.75,1.4)) + geom_text(mapping = aes(label = 1000*round(m_rt,3)), vjust = -3, position = pj)
fig2_RT #
# uncomment the following to save fig
ggsave('manuscript1/figs/fig2_RT.png',fig2_RT, width = 7, height = 5, dpi = 600)


# Re-plot RT figures for ECVP conference
meanRTs$m_rt <- 1000*round(meanRTs$m_rt,3)
meanRTs$se_rt <- 1000*round(meanRTs$se_rt,3)

# Re-plot for ECVP conference
fig1_exp3 = meanRTs %>% filter(exp=='Exp. 3')  %>% ggplot(aes(dist_cond, m_rt, fill = tar_cond)) + 
  #geom_point(size = 3, position=pj) + 
  geom_bar(aes(group = interaction(tar_cond, session)), color = 'black',stat = 'identity', position = position_dodge(0.7), width = 0.7) + 
  geom_errorbar(aes(ymin = m_rt, ymax = m_rt + se_rt), width = 0.2, position = position_dodge(0.7)) + 
    scale_color_manual(values = c("#20639B", "#3CAEA3"))+
    scale_fill_manual(values = c("#20639B", "#3CAEA3"))+ 
  #   scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  #   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))+ 
  facet_grid(~ session) +  theme_bw()+ theme(panel.grid =element_blank(), axis.text = element_text(face = "bold", size = 13), strip.text.x= element_text(face = "bold", size = 16))+ 
  guides(linetype = FALSE, color = FALSE, shape = FALSE, fill = guide_legend(title='Target Position')) + # hide legends
  theme(strip.background = element_blank(), legend.position = 'bottom', legend.title = element_text(face = "bold", size = 16), legend.text = element_text(face = "bold", size = 16)) + # make the subtitle no background
  xlab("") + # hide x-axis, since error figure has it 
  ylab("Response time (ms)") +  theme(axis.title.y = element_text(face = "bold", size = 16))+ 
  coord_cartesian(ylim = c(800,1400)) + theme(axis.text.y = element_text(face = "bold", size = 16)) + geom_text(mapping = aes(label = m_rt), colour = 'black', vjust =3,  size=2,position = position_dodge(0.7)) + 
  theme(panel.grid =element_blank())
fig1_exp3  
# uncomment the following to save fig
ggsave('manuscript1/figs/fig1_exp3.png',fig1_exp3, width = 8, height = 5, dpi = 600)


# plot error rates
fig2_ERR = ggplot(meanRTs, aes(dist_cond, m_err, color = tar_cond, 
                               linetype = session, shape = session)) + 
  geom_point(size = 3, position=pj) + 
  geom_line(aes(group = interaction(tar_cond, session)), position=pj) + 
  geom_errorbar(aes(ymin = m_err - se_err, ymax = m_err + se_err), width = 0.3, position=pj) + 
  facet_wrap(~exp) +     theme_classic() + 
  guides(color = guide_legend(title='Target Position'),
         shape = guide_legend(title = 'Session'), 
         linetype = guide_legend(title = 'Session')) +
  theme(legend.position = "bottom", strip.text = element_blank()) + 
  xlab("Distractor Condition") + 
  ylab("Error rates")
fig2_ERR
# uncomment to save fig
# ggsave('manuscript1/figs/fig2_ERR.pdf',fig2_ERR, width = 7, height = 3)

# combine rts and errors into one figure

#fig2 = plot_grid(fig2_RT, fig2_ERR, nrow = 2, rel_heights = c(5,3))
#fig2
#ggsave('manuscript1/figs/fig2.png',fig2, width = 7, height = 5)

#fig2b_RT <- dist_cond_plot(raw_e1_e2_e7, legend_pos="none") 
#fig2b_err <- dist_cond_plot(raw_e1_e2_e7, 1) 
#fig2b <- plot_grid(fig2b_RT, fig2b_err, nrow = 2, rel_heights = c(5,3))
#fig2b
#ggsave('manuscript1/figs/fig2b.pdf',fig2b, width = 7, height = 5)

fig3_RT <- plotDistDistance(raw_e1_e2_e7, xlabel="", leg.pos = "none", fs=13)
fig3_ERR <- plotDistDistance(raw_e1_e2_e7,1, leg.pos = "bottom", fs=13)
fig3 = plot_grid(fig3_RT, fig3_ERR, nrow = 2, rel_heights = c(5,3))
fig3
ggsave('manuscript1/figs/fig3.png',fig3, width = 7, height = 5)

fig4_RT <- tar_distance_plot(raw_e1_e2_e7, xlabel="", legend_pos="none", fs=13)
fig4_ERR <- tar_distance_plot(raw_e1_e2_e7, 1, fs=13)
fig4 = plot_grid(fig4_RT, fig4_ERR, nrow = 2, rel_heights = c(5,3))
fig4
ggsave('manuscript1/figs/fig4.png',fig4, width = 7, height = 5)

fig5_RT <- plotTarCond(filter(raw_e1_e2_e7, exp!="Exp. 3"), xlabel="", legend_pos="none", fs=14)
fig5_ERR <- plotTarCond(filter(raw_e1_e2_e7, exp!="Exp. 3"), 1, fs=14) + guides(linetype = FALSE)
fig5 = plot_grid(fig5_RT, fig5_ERR, nrow = 2, rel_heights = c(5,3))
fig5
ggsave('manuscript1/figs/fig5.png',fig5, width = 7, height = 5)

fig_s2_RT <- plotDistInttrial(raw_e1_e2_e7, xlabel="", legend_pos="none", fs=14)
fig_s2_ERR <- plotDistInttrial(raw_e1_e2_e7, 1, fs=14) + guides(linetype = FALSE, shape = FALSE)

fig_s2 = plot_grid(fig_s2_RT, fig_s2_ERR, nrow = 2, rel_heights = c(5,3))
fig_s2
ggsave('manuscript1/figs/figS2.png',fig_s2, width = 7, height = 5)



# ---- NOTE: 03.10.2018
# sdata_dpos_tpos not found. And it is not the color repetition effect in the manuscript
# harded coded for the first and second half, easily error prone
mutate(raw_e2, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e2_s1_split

mutate(raw_e2, subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>%
  filter(session=="s2", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e2_s2_split

sdata_tpos <- rbind(sdata_tpos_e2_s1_split, sdata_tpos_e2_s2_split)
sdata_tpos$type="Target"

mutate(raw_e2, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier) %>% group_by(dist_cond, subsession, sub) %>% 
  summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e2_s1_split

mutate(raw_e2,  subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>% filter(session=="s2", correct, !outlier) %>%
  group_by(dist_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e2_s2_split

sdata_dpos <- rbind(sdata_dpos_e2_s1_split, sdata_dpos_e2_s2_split)
sdata_dpos$type="Distractor"

sdata_dpos_tpos_e2 <- rbind(sdata_dpos, sdata_tpos)

mutate(raw_e1, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e1_s1_split

mutate(raw_e1, subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>%
  filter(session=="s2", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e1_s2_split

sdata_tpos_e1 <- rbind(sdata_tpos_e1_s1_split, sdata_tpos_e1_s2_split)
sdata_tpos_e1$type="Target"

mutate(raw_e1, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier) %>% group_by(dist_cond, subsession, sub) %>% 
  summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e1_s1_split

mutate(raw_e1,  subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>% filter(session=="s2", correct, !outlier) %>%
  group_by(dist_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e1_s2_split

sdata_dpos_e1 <- rbind(sdata_dpos_e1_s1_split, sdata_dpos_e1_s2_split)
sdata_dpos_e1$type="Distractor"

sdata_dpos_tpos_e1 <- rbind(sdata_dpos_e1, sdata_tpos_e1)


mutate(raw_e7, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e7_s1_split

mutate(raw_e7, subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>%
  filter(session=="s2", correct, !outlier, dist_pos==0) %>%
  group_by(tar_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(tar_cond, rt) %>% 
  mutate(pos_effect = (High - Low)*1000)-> sdata_tpos_e7_s2_split

sdata_tpos <- rbind(sdata_tpos_e7_s1_split, sdata_tpos_e7_s2_split)
sdata_tpos$type="Target"

mutate(raw_e7, subsession=ifelse(tno>max(tno)/2, "S1, 2nd half", "S1, 1st half")) %>% 
  filter(session=="s1", correct, !outlier) %>% group_by(dist_cond, subsession, sub) %>% 
  summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e7_s1_split

mutate(raw_e7,  subsession=ifelse(tno>max(tno)/2, "S2, 2nd half", "S2, 1st half")) %>% filter(session=="s2", correct, !outlier) %>%
  group_by(dist_cond, subsession, sub) %>% summarize(rt=mean(rt)) %>% spread(dist_cond, rt) %>% 
  mutate(pos_effect = (Low - High)*1000) -> sdata_dpos_e7_s2_split

sdata_dpos <- rbind(sdata_dpos_e7_s1_split, sdata_dpos_e7_s2_split)
sdata_dpos$type="Distractor"

sdata_dpos_tpos_e7 <- rbind(sdata_dpos, sdata_tpos)

sdata_dpos_tpos_e1$exp <- "Exp. 1"
sdata_dpos_tpos_e2$exp <- "Exp. 2"
sdata_dpos_tpos_e7$exp <- "Exp. 3"
sdata_dpos_tpos <- rbind(sdata_dpos_tpos_e1, sdata_dpos_tpos_e2)

fig6 <- group_by(sdata_dpos_tpos, type, subsession, exp) %>% 
  summarize(mpos_effect = mean(pos_effect), 
            se_pos_effect=sd(pos_effect)/sqrt(N-1)) %>% 
  ggplot(aes(x=subsession, y=mpos_effect, linetype=type, shape=type, group=type)) + geom_point(size=3) + geom_line() + 
  geom_errorbar(aes(ymin=mpos_effect-se_pos_effect, ymax=mpos_effect+se_pos_effect), width=0.1) + facet_wrap(~exp) +
  theme_classic() + theme(strip.background = element_blank(), 
                          text = element_text(size=14), 
                          legend.position = "bottom",
                          axis.text.x = element_text(angle = -30, hjust = 0),
                          plot.margin = unit(c(0,1,0,0), "cm")) + 
  labs(x="(Sub-)session", y='Location effect (ms)', linetype="Target/Distractor", shape="Target/Distractor") +
  coord_cartesian(y=c(-20,120)) 
fig6
#ggsave('manuscript1/figs/fig6.tiff',fig6, width = 7, height = 4)
ggsave('manuscript1/figs/fig6.png',fig6, width = 7, height = 4)


raw_comb <- raw_e1_e2_e7
raw_comb <- unite(raw_comb, sub, sub, exp)
raw_comb$exp <- "Exp 1 & 2"
fig7 <- plotColInttrial(raw_comb, fs=12)
ggsave('manuscript1/figs/fig7.png',fig7, width = 6, height = 5)


fig_s1 <- plotColInttrial(raw_e1_e2_e7, fs=11)
#ggsave('manuscript1/figs/figS1.tiff',fig_s1, width = 7, height = 4)
ggsave('manuscript1/figs/figS1.png',fig_s1, width = 7, height = 4)

# function for filter no distractor for the current and previous trial
np_dist <- function(data) {
  group_by(data, blkNo) %>% 
    mutate(pdist_pos = lag(dist_pos)) %>% filter(pdist_pos==0, dist_pos==0)
}

# function for filter no distractor trial, target position is not on previous distractor location
np_dist_pos <- function(data) {
  group_by(data, blkNo) %>% 
    mutate(pdist_pos = lag(dist_pos)) %>% filter(pdist_pos!=tar_pos, dist_pos==0)
}

# function for filter target at the previous distractor location, but current trial with no singleton dist.
p_dist_pos <- function(data) { 
  group_by(data, blkNo) %>% 
    mutate(pdist_pos = lag(dist_pos)) %>% filter(pdist_pos==tar_pos, dist_pos==0)
}


tar_cond_plot <- function(data) {
  N <- length(unique(data$sub))
  if('session' %in% colnames(data)) {
  filter(data,!outlier, correct, dist_pos==0) %>% group_by(tar_cond, session, sub) %>% 
    summarize(rt=mean(rt*1000)) %>% summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
    ggplot( aes(x=tar_cond, y=mRT, color=session, group=session)) + 
    geom_line() + geom_point(size=3) +  theme_bw() + theme(panel.grid = element_blank()) + 
    geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.1) + coord_cartesian(ylim=c(800,1300)) + 
    labs(x="Target condition", y="Response time (ms)")
  } else {
    filter(data,!outlier, correct, dist_pos==0) %>% group_by(tar_cond, sub) %>% 
      summarize(rt=mean(rt*1000)) %>% summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot( aes(x=tar_cond, y=mRT, group=1)) + 
      geom_line() + geom_point(size=3) +  theme_bw() + theme(panel.grid = element_blank()) + 
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.1) + coord_cartesian(ylim=c(800,1300)) + 
      labs(x="Target condition", y="Response time (ms)")
  }
}

np_dist_pos(raw_e1_e2_e7) %>% filter(correct, !outlier) %>% group_by(tar_cond, exp, session, sub) %>% 
  summarize(rt=mean(rt)) -> fdata_e1_e2

#np_dist_pos(raw_e4) %>% filter(correct, !outlier) %>% group_by(tar_cond, session, sub) %>% 
#  summarize(rt=mean(rt)) -> fdata_e4

cost_e1_e2 <- group_by(fdata_e1_e2, tar_cond, exp, session) %>% 
  spread(tar_cond, rt) %>% mutate(cost = `High Dist. Prob.` - `Low Dist. Prob.`)

s_cost_e1_e2 <- cost_e1_e2 %>% 
  summarize(mcost=mean(cost*1000), secost=sd(cost*1000)/sqrt(N-1))

#cost_e4 <- group_by(fdata_e4, tar_cond, session) %>% 
#  spread(tar_cond, rt) %>% mutate(cost=`High Dist. Prob.` - `Low Dist. Prob.`)

#s_cost_e4 <-  cost_e4 %>% 
#  summarize(mcost=mean(cost*1000), secost=sd(cost*1000)/sqrt(N_e4-1))

s_cost_e1_e2 %>% ggplot(aes(x=exp, y=mcost, fill=session)) + 
  geom_bar(stat="identity", position = position_dodge(0.6), width=0.5, color="black") +
  geom_errorbar(aes(ymin=mcost-secost, ymax=mcost+secost), position = position_dodge(0.6), width=0.1) + 
  labs(x="Experiment", y="Cost (ms)", fill="session") + theme_bw() + 
  scale_fill_brewer(palette = "Greys") + coord_cartesian(ylim=c(-10, 130))

#s_cost_e4 %>% ggplot(aes(x=session, y=mcost, fill=session)) + 
#  geom_bar(stat="identity", position = position_dodge(0.8), width=0.5, color="black") +
#  geom_errorbar(aes(ymin=mcost-secost, ymax=mcost+secost), position = position_dodge(0.6), width=0.1) + 
#  labs(x="Session", y="Cost (ms)", fill="session") + theme_bw() + 
#  scale_fill_brewer(palette = "Greys") + coord_cartesian(ylim=c(-10, 130))
