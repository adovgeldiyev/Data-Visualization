p <- ggplot(
  india_df,
  aes(x = date, y=New.Cases,colour = 'black')
)+scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b" )+
  geom_line(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +scale_y_continuous(labels =addUnits, limits = c(0, 100000))+
  #scale_y_log10() +
  labs(x = "Months in 2020", y = "New Cases")+ggtitle("Timeline of COVID-19 in India")
p
animate_plot<-p + transition_reveal(date)
animate(animate_plot, fps = 10, width = 750, height = 450)
anim_save("india_cases.gif")

