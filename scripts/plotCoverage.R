library(tidyverse)
require(cowplot) # imports save_plot
require(svglite)

source('settings.R')
file.R1  = paste0('../data/SWOW/output/centrality/cueStats.SWOW-RP.R1.',release,'.csv')
file.R123 = paste0('../data/SWOW/output/centrality/cueStats.SWOW-RP.R123.',release,'.csv')
file.svg = paste0('../figures/responseCoverage.',release,'.svg')
file.pdf = paste0('../figures/responseCoverage.',release,'.pdf')

cueStats.R1 = read.csv(file.R1)
cueStats.R123 = read.csv(file.R123)

mR1   = median(cueStats.R1$coverage)
mR123 = median(cueStats.R123$coverage)

p.R1 = ggplot(data=cueStats.R1,aes(coverage)) +
      geom_histogram(aes(y = ..count.. / sum(..count..)),
                     col="white",alpha=0.4,binwidth = 3) +
      geom_vline(data = cueStats.R1, aes(xintercept=round(median(coverage)),
                                         colour='black'),
                 linetype='solid',size=1,show.legend = FALSE) +
      labs(x="Response coverage R1 (%)", y="Proportion") +
      ylim(0,0.2) +
      scale_x_continuous(breaks=round(c(20,40,60,80,mR1,100))) +
      theme(legend.position="none") +
      theme_minimal()

p.R1

p.R123 = ggplot(data=cueStats.R123,aes(coverage)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),col="white",alpha=0.4,
                 binwidth = 3) +
  geom_vline(data = cueStats.R123, aes(xintercept=round(median(coverage)),
                                       colour='black'),linetype='solid',size=1,
             show.legend = FALSE) +
  labs(x="Response coverage R123 (%)", y="Proportion") +
  ylim(0,0.2) +
  scale_x_continuous(breaks=round(c(20,40,60,80,mR1,100))) +
  theme(legend.position="none") +
  theme_minimal()


p = plot_grid(p.R1, p.R123, labels = c("A", "B"))
save_plot(file.svg,p, base_height = 3, base_width = 8)
save_plot(file.pdf,p, base_height = 3, base_width = 8)


