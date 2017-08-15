## plot Funnel Plot

funnelplot <- function(number,p,df, anaes.name)
  {
  library(ggplot2)
  
fp <- ggplot(aes(x = number, y = p), data = df) +
  geom_point(shape = 1) +
  geom_point(aes(x = number[i], y = p[i]), shape = 16) + 
  geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ll999), linetype = "dashed", data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul999), linetype = "dashed", data = dfCI) +
  geom_hline(aes(yintercept = p.fem), data = dfCI) +
  scale_y_continuous(limits = c(0,0.15)) +
  ggtitle(paste("Funnel Plot of: ", anaes.names[i]," ",min(mot.data$Month_Yr)," - ", max(mot.data$Month_Yr))) +
  xlab("Number of cases") + ylab(paste("Proportion")) + theme_bw() 

} ## End of function