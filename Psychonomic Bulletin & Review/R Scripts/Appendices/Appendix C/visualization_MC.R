library(ggplot2)
library(ggpattern)
library(ggpubr)

setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Appendices/Appendix C/combined_iter2e4_BF4.36.csv"

report <- read.csv(filepath)
report$delta_m <- report$m1 - report$m2


fig1 <- ggplot(report, aes(x=m1, color=package, fill=package)) +
  labs(x = "Posterior Mean of Level 1") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "white",
                       alpha = 0.4,
                       pattern_alpha = 0.4,
                       aes(pattern = package)) +
  scale_color_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  scale_fill_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(1,0.5,1,0.5, "cm"),
        text = element_text(size=23)) 

fig2 <- ggplot(report, aes(x=m2, color=package, fill=package)) +
  labs(x = "Posterior Mean of Level 2") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "white",
                       alpha = 0.4,
                       pattern_alpha = 0.4,
                       aes(pattern = package)) +
  scale_color_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  scale_fill_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(1,0.5,1,0.5, "cm"),
        text = element_text(size=23)) 

figd <- ggplot(report, aes(x=delta_m, color=package, fill=package)) +
  labs(x = "Posterior Mean Difference") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "white",
                       alpha = 0.4,
                       pattern_alpha = 0.4,
                       aes(pattern = package)) +
  scale_color_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  scale_fill_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(1,0.5,1,0.5, "cm"),
        text = element_text(size=23)) 

fig3 <- ggplot(report, aes(x=width, color=package, fill=package)) +
  labs(x = "HDI Width") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "white",
                       alpha = 0.4,
                       pattern_alpha = 0.4,
                       aes(pattern = package)) +
  scale_color_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  scale_fill_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(1,0.5,1,0.5, "cm"),
        text = element_text(size=23)) 

fig4 <- ggplot(report, aes(x=sep, color=package, fill=package)) +
  labs(x = expression("HDI"~italic("Sep%"))) +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "white",
                       alpha = 0.4,
                       pattern_alpha = 0.4,
                       aes(pattern = package)) +
  scale_color_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  scale_fill_manual(values=c("#5494cb", "#ff7411", "#a1a1a1")) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(1,0.5,1,0.5, "cm"),
        text = element_text(size=23)) 


pic.up <- ggarrange(figd,fig3,ncol=2,nrow=1,labels=c("A","B"),align = "h",common.legend = T, legend = "right",font.label = list(size = 31))
pic.down <- ggarrange(fig1, fig2, fig4, ncol = 3, nrow = 1, common.legend = T, legend = "none",
                 labels = c("C","D","E"),font.label = list(size = 31  ))
pic <- ggarrange(pic.up,pic.down,nrow=2,ncol=1)
pic.with.title <- annotate_figure(pic,text_grob(expression(paste(italic(BF)[10] == 4.36, ",  ", italic(hat(ρ)) == 0.90, ",  ", italic(n) == 48,",  iter = 20,000")), 
                                                size=25))
ggsave("~/Desktop/combined_iter2e4_BF4.36.png", width=6000, height=3200, units="px", device="png", dpi=300)

