#Let's try this
fire_causes_colours <- c("#f72885", "#b754de", "#fabc20")


#Loading Data
califires <- read_csv("calfire_frap.csv")%>%
  mutate(cause2 = case_when(cause == 1 | cause == 17 ~ "Natural",
                            cause == 14 | is.na(cause) ~ "Unknown",
                            cause != 1 | cause != 14 | cause != 17 ~ "Human"),
         plot_date = as.Date(format(alarm_date, "2017-%m-%d")))

#Plot Template
plot_template <- ggplot(califires, aes(y=year_))+
  geom_hline(yintercept = seq(1950, 2017, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  scale_y_reverse(limits = c(2017,1950), breaks = c(2010, 1990, 1970, 1950)) + 
  xlab("") +
  ylab("") +
  theme_hc(bgcolor = "darkunica", base_size = 20, base_family = "ProximaNova-Semibold") +
  theme(axis.text = element_text(color = "#ffffff"))

plot_template +
  geom_point(aes(size=gis_acres, x=plot_date), color = "#57ebff", alpha = 0.7)

#Plot Template 2

cause_plot <- plot_template + 
  scale_color_manual(values = fire_causes_colours, guide = FALSE) + 
  geom_point(aes(size = gis_acres, x = plot_date, color = cause2, alpha = cause2))

#Natural fires
opacity <- c(0, 0.7, 0)
cause_plot +
  scale_alpha_manual(values = opacity, guide = FALSE) +
  ggtitle("Natural Causes") + theme(plot.title = element_text(color = "#d397fc", size = 16, hjust = 0.5))

#Human fires
opacity <- c(0.7, 0, 0)
cause_plot +
  scale_alpha_manual(values = opacity, guide = FALSE) + 
  ggtitle("Human-caused") + theme(plot.title = element_text(color = "#f72885", size = 16, hjust = 0.5))

#Unknown
opacity <- c(0,0,0.7)
cause_plot +
  scale_alpha_manual(values = opacity, guide = FALSE) +
  ggtitle("Unknown") + theme(plot.title = element_text(colour = "#fabc20", size = 16, hjust = 0.5))
