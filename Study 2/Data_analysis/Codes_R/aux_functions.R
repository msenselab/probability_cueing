library(tidyverse)
library(ez)
library(cowplot)

# some useful functions to summarize data and plotting

plotTarDistCond <- function(df, plotError=0, xlabel="Distractor position", leg.pos="bottom", fs=16) {
  N <- length(unique(df$sub))
  
  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  }
  
  if(plotError) {
    group_by(df, dist_cond, tar_cond, exp, session, sub) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_cond, y = m_err, color=tar_cond, shape=session, linetype=session, group=interaction(tar_cond, session))) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2) + 
      geom_point(size=3) + geom_line() + theme_bw() +
      theme(strip.background = element_blank(), strip.text=element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Error rate') + facet_wrap( ~ exp) + 
      scale_color_manual(name="Target condition", values = c('grey','black')) 
  } else {
    filter(df, correct, !outlier) %>% group_by(dist_cond, tar_cond, exp, session, sub) %>% summarize(rt = mean(rt)*1000) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_cond, y = mRT, color=tar_cond, shape=session, linetype=session, group=interaction(tar_cond, session))) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) + geom_point(size=3) +  geom_line() + 
      theme_bw() + theme(strip.background = element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Response time (ms)') + facet_wrap( ~ exp) + 
      scale_color_manual(name="Target condition", values = c('grey','black')) 
  }
}

plotTarDistDistance <- function(df, plotError=0, xlabel="Target-distractor distance", leg.pos="bottom", fs=16) {
  N <- length(unique(df$sub))
  
  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  }
  
  if(plotError) {
    group_by(df, tar_dist_distance, exp, session, sub) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_dist_distance, y = m_err, shape=session, linetype=session, group= session)) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2) + 
      geom_point(size=3) + geom_line() + theme_bw() +
      theme(strip.background = element_blank(), strip.text=element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Error rate') + facet_wrap( ~ exp) 
    } else {
    filter(df, correct, !outlier) %>% group_by(tar_dist_distance, exp, session, sub) %>% summarize(rt = mean(rt)*1000) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_dist_distance, y = mRT, shape=session, linetype=session, group=session)) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) + geom_point(size=3) +  geom_line() + 
      theme_bw() + theme(strip.background = element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Response time (ms)') + facet_wrap( ~ exp)
  }
}

plotDistDistance <- function(df, plotError=0, xlabel="Distractor distance from frequent location", leg.pos="right", fs=16) {
  N <- length(unique(df$sub))
  df$dist_distance <- factor(df$dist_distance, levels=-1:4, labels=c('No Dist.', '0', '1', '2', 
                                                                     '3', '4'))  
  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  }
  
  if(plotError) {
    group_by(df, dist_distance, exp, session, sub) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_distance, y = m_err, shape=session, linetype=session, group=session)) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2, color = 'black') + 
      geom_point(size=3, color = 'black') + geom_line(color = 'black') + theme_bw() +
       theme(strip.background = element_blank(), strip.text=element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Error rate') + facet_wrap( ~ exp)
  } else {
    filter(df, correct, !outlier) %>% group_by(dist_distance, exp, session, sub) %>% summarize(rt = mean(rt)*1000) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_distance, y = mRT, shape=session, linetype=session, group=session)) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2, color = 'black') + geom_point(size=3, color = 'black') + 
      geom_line(color = 'black') + 
      theme_bw() + theme(strip.background = element_blank(), legend.position = leg.pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Response time (ms)') + facet_wrap( ~ exp)
  }
}


plotTarDistance <- function(df, plotError=0) {
  N <- length(unique(df$sub))
  df$tar_distance <- factor(df$tar_distance, levels=0:4, labels=c('Tar-0', 'Tar-1', 'Tar-2', 
                                                                     'Tar-3', 'Tar-4'))  
  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  }
  
  if(plotError) {
    filter(df, dist_pos==0) %>% group_by(tar_distance, exp, session, sub) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_distance, y = m_err, color=session, group=session)) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2) + geom_point(size=3) + geom_line() + 
      theme_classic() + theme(strip.background = element_blank()) + 
      labs(x="Distractor location", y='Error rate') + facet_wrap( ~ exp)
  } else {
    filter(df, correct, !outlier, dist_pos==0) %>% group_by(tar_distance, exp, session, sub) %>% summarize(rt = mean(rt)*1000) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_distance, y = mRT, color=session, group=session)) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) + geom_point(size=3) + geom_line() + 
      theme_classic() + theme(strip.background = element_blank()) + 
      labs(x="Distractor location", y='Response time (ms)') + facet_wrap( ~ exp)
  }
}

np_dist_pos <- function(data) {
  group_by(data, blkNo) %>% 
    mutate(pdist_pos = lag(dist_pos)) %>% filter(pdist_pos!=tar_pos, dist_pos==0)
}

p_dist_pos <- function(data) { 
  group_by(data, blkNo) %>% 
    mutate(pdist_pos = lag(dist_pos)) %>% filter(pdist_pos==tar_pos, dist_pos==0)
}

plotTarCond <- function(df, plotError=0, legend_pos="bottom", xlabel="Target location", fs=16) {
  
  N <- length(unique(df$sub))

  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  } 
  
  pdp <- p_dist_pos(df)
  pdp$inttrial <- "Coincident"

  npdp <- np_dist_pos(df)
  npdp$inttrial <- "Non-coincident"
  
  df <- rbind(pdp, npdp)
  
  if(plotError) {
    filter(df, dist_pos==0) %>% group_by(tar_cond, inttrial, exp, sub, session) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(err=mean(err)) %>% summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_cond, y = m_err, color=inttrial, shape=inttrial, linetype=inttrial, 
                 group=inttrial)) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2) + geom_point(size=3) + geom_line() + theme_bw() +
       theme(strip.background = element_blank(), strip.text=element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Error rate', color="Target - Distractor coincidence", shape="session") + facet_wrap( ~ exp) + 
      scale_color_manual(name="Target-Distractor", values = c('grey','black')) + scale_shape_manual(name="Target-Distractor", values=c('circle', 'triangle'))
  } else {
    filter(df, correct, !outlier, dist_pos==0) %>% group_by(tar_cond, inttrial, exp, sub, session) %>% 
      summarize(rt = mean(rt)*1000) %>% summarize(rt = mean(rt)) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_cond, y = mRT, color=inttrial, shape=inttrial, linetype=inttrial, 
                 group=inttrial)) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) + geom_point(size=3) + geom_line() + 
      theme_bw() + theme(strip.background = element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Response time (ms)', color="Target - Distractor coincidence", shape="session") + facet_wrap( ~ exp) +
      scale_color_manual(name="Target-Distractor", values = c('grey','black')) + scale_shape_manual(name="Target-Distractor", values=c('circle', 'triangle'))
  }
}

plotDistInttrial <- function(df, plotError=0, legend_pos="bottom", xlabel="Distractor location", fs=16) {
  
  N <- length(unique(df$sub))
  
  if(!("exp" %in% colnames(df))) {
    df$exp <- 1
  } 
  
  pdp <- group_by(df, blkNo) %>% mutate(pdist_pos = lag(dist_pos)) %>% 
    filter(pdist_pos==dist_pos, dist_pos!=0)
  pdp$inttrial <- "Coincident"
  
  npdp <- group_by(df, blkNo) %>% mutate(pdist_pos = lag(dist_pos)) %>% 
    filter(pdist_pos!=dist_pos, dist_pos!=0)
  npdp$inttrial <- "Non-coincident"
  
  df <- rbind(pdp, npdp)

  if(plotError) {
    df %>% group_by(dist_cond, inttrial, exp, session, sub) %>% summarize(err = 1 - mean(correct)) %>% 
      summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_cond, y = m_err, color=inttrial, shape=session, linetype=session, 
                 group=interaction(inttrial, session))) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.2) + geom_point(size=3) + geom_line() + theme_bw() +
      theme(strip.background = element_blank(), strip.text=element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Error rate', color="Distractor - Distractor coincidence") + facet_wrap( ~ exp) + 
      scale_color_manual(values = c('grey','black'))
  } else {
    filter(df, correct, !outlier) %>% group_by(dist_cond, inttrial, exp, session, sub) %>% 
      summarize(rt = mean(rt)*1000) %>% 
      summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_cond, y = mRT, color=inttrial, shape=session, linetype=session, 
                 group=interaction(inttrial, session))) +
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.2) + geom_point(size=3) + geom_line() + 
      theme_bw() + theme(strip.background = element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y='Response time (ms)', color="Distractor - Distractor coincidence") + facet_wrap( ~ exp) +
      scale_color_manual(values = c('grey','black'))
  }
}

plotColInttrial <- function(df, fs=16) {
  
  N <- length(unique(df$sub))
  
  if(!("exp" %in% colnames(df))) {
    df$exp <- "Exp. 1"
  } 
  
  df <- group_by(df, blkNo, sub, session) %>% mutate(p_col=lag(color), p_dist_cond=lag(dist_cond), inttrial=ifelse(color==p_col, "Same", "Different"))  %>% 
    filter(!is.na(inttrial))
  
  filter(df, correct, !outlier) %>% group_by(dist_cond, p_dist_cond, inttrial, exp, sub) %>% 
    summarize(rt = mean(rt)*1000) %>% spread(inttrial, rt) %>% mutate(diff = Different - Same) %>%
    summarize(mdiff=mean(diff), sediff=sd(diff)/sqrt(N-1)) %>% 
    ggplot(aes(x = dist_cond, y = mdiff, col = p_dist_cond, shape=p_dist_cond, linetype=p_dist_cond, group=p_dist_cond)) +
    geom_errorbar(aes(ymin = mdiff - sediff, ymax = mdiff + sediff), width=0.1) + 
    geom_line() + geom_point(size=3) + facet_wrap( ~ exp) +
    theme_classic() + theme(strip.background = element_blank(), legend.position="bottom", text = element_text(size=fs)) + 
    labs(x="Distractor condition (n)", y="Color repetition effect (ms)",
         color="Distractor condition (n-1)", shape="Distractor condition (n-1)", linetype="Distractor condition (n-1)") +
    scale_color_manual(values = c('grey70','black','grey45'))
}

dist_cond_plot <- function(data, plotError=0, legend_pos="bottom") {
  N <- length(unique(data$sub))
  if(plotError) {
    sdata <-  group_by(data, dist_cond, session, exp, sub) %>% 
      summarize(err=1-mean(correct)) 
    sdata %>% summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>% 
      ggplot(aes(x=dist_cond, y=m_err, group=session, color=session)) + 
      geom_line() + geom_point(size=3) + 
      theme_classic() + theme(strip.background = element_blank(), legend.position=legend_pos) +
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.1) + 
      labs(x="Distractor condition", y="Error rate") + facet_wrap( ~ exp) 
  } else {
  sdata <-  filter(data,!outlier, correct) %>% group_by(dist_cond, session, exp, sub) %>% 
    summarize(rt=mean(rt*1000)) 
  sdata %>% summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
    ggplot(aes(x=dist_cond, y=mRT, group=session, color=session)) + 
    geom_line() + geom_point(size=3) + 
    theme_classic() + theme(strip.background = element_blank(), legend.position=legend_pos) +
    geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.1) + 
    labs(x="Distractor condition", y="Response time (ms)") + facet_wrap( ~ exp) 
  }
}


tar_cond_plot <- function(data, plotError=0, legend_pos="bottom", xlabel="Target location", fs=16) {
  N <- length(unique(data$sub))
  if(plotError) {
    sdata <- filter(data,dist_pos==0) %>% group_by(tar_cond, session, exp, sub) %>% 
      summarize(err=1-mean(correct)) 
    sdata %>% summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>%
      ggplot(aes(x=tar_cond, y=m_err, group=session, color=session)) + theme_classic() + 
      geom_line() + geom_point(size=3) + geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.1) +    
      theme(strip.background = element_blank(), strip.text=element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y="Error rate") + facet_wrap(~exp)
  } else {
    sdata <- filter(data, !outlier, correct, dist_pos==0) %>% group_by(tar_cond, session, exp, sub) %>% 
      summarize(rt=mean(rt*1000)) 
    sdata %>% summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_cond, y=mRT, group=session, color=session)) + 
      geom_line() + geom_point(size=3) + geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.1) + 
      theme_classic() + theme(strip.background = element_blank(), legend.position=legend_pos, text = element_text(size=fs)) +
      labs(x=xlabel, y="Response time (ms)") + facet_wrap(~exp)
  }
}

tar_distance_plot <- function(data, plotError=0, legend_pos="bottom", xlabel="Target distance from frequent distractor location", fs=16) {
  N <- length(unique(data$sub))
  data$tar_distance <- factor(data$tar_distance, levels=0:4, labels=c("0", "1", "2", 
                                                                      "3", "4"))
  if(plotError) {
    sdata <- filter(data, dist_pos==0) %>% group_by(tar_distance, session, exp, sub) %>% 
      summarize(err=1-mean(correct)) 
    sdata %>% summarize(m_err=mean(err), se_err=sd(err)/sqrt(N-1)) %>%
      ggplot(aes(x=tar_distance, y=m_err, group=session, shape=session, linetype=session)) + theme_bw() + 
      geom_line(color = 'black') + geom_point(size=3, color = 'black') + 
      geom_errorbar(aes(ymin=m_err-se_err, ymax=m_err+se_err), width=0.1, color = 'black') +    
      theme(strip.background = element_blank(), strip.text=element_blank(), legend.position=legend_pos, text = element_text(size=fs)) + 
      labs(x=xlabel, y="Error rate") + facet_wrap(~exp)
  } else {
    sdata <- filter(data, !outlier, correct, dist_pos==0) %>% group_by(tar_distance, session, exp, sub) %>% 
      summarize(rt=mean(rt*1000)) 
    sdata %>% summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) %>% 
      ggplot(aes(x=tar_distance, y=mRT, group=session, shape=session, linetype=session)) + 
      geom_line(color = 'black') + geom_point(size=3, color ='black') + 
      geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT), width=0.1, color = 'black') + 
      theme_bw() + theme(strip.background = element_blank(), legend.position=legend_pos, text = element_text(size=fs)) +
      labs(x=xlabel, y="Response time (ms)") + facet_wrap(~exp)
  }
}
