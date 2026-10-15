library(ggplot2)
df <- data.frame("level"=factor(rep(c("0%","50%","100%","150%","63%"),2),levels=c("0%","50%","100%","150%","63%")),
                 "means"=c(75,75,75,75,75,75,100,125,150,100),
                 "Method"=factor(rep(c("a","b"),each=5)),
                 "upper"=c(100,100,100,100,100,100,125,150,175,112.5),
                 "lower"=c(50,50,50,50,50,50,75,100,125,87.5),
                 "width"=rep(c("Equal Widths","Unequal Widths"),c(4,1)))
ggplot(df,aes(level,means,color=Method))+
  geom_pointrange(aes(ymin=lower,ymax=upper),
                  position=position_dodge(.5),show.legend=F,size=3)+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.3,
                position=position_dodge(.5),show.legend=F,linewidth=3)+
  scale_color_manual(values=c("#5494cb", "#ff7411"))+
  labs(x="",y="")+
  geom_vline(xintercept=4.5, linetype="dotted", linewidth=2)+
  annotate(geom="text", x=2.5, y=175, label="Equal Widths", size=11)+
  annotate(geom="text", x=5.1, y=175, label="Unequal Widths", size=11)+
  theme(line = element_blank(),
        title = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=30,face="bold"))
ggsave("~/Desktop/Figure 1.png", width=4800, height=1800, units="px", device="png", dpi=300)

