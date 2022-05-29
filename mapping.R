install.packages("ggplot2")
library(ggplot2)
trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))
(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)
# Plotting a map for each genus

tree.plots <-  
  trees.five  %>%      
  group_by(Genus) %>%  
  do(plots =          
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  )

tree.plots$plots

# Saving the plots to file

tree.plots %>%            
  do(ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

paste(getwd(), '/', 'map-', 'Genus', '.png', sep = '')
