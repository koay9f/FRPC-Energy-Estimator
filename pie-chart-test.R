fillcolor <- c("Fiber" = "#2960A8", "Intermediate"  = "#74A138", "Primary Matrix Material" = "#C46827",
          "Other Materials"= "#24A7C1", "Molding" = "#8D2A1E", "Curing" = "#E2B500", "Finishing" = "#8A9FCF")

## bar --> pie ----

ggplot(test, 
                   aes(x = factor(1), 
                       y = Energy,
                       fill = Process_Segment )) +
  geom_bar(width = 1, stat = "identity") +
  labs(fill= "Process_Segment") +
  facet_grid(facets=.~Part) + 
  coord_polar(theta = "y")+ 
  xlab("Part") +
  ylab("")+
     scale_fill_manual(values = fillcolor, name = "") +

     theme_bw() + theme(  legend.title = element_blank(), legend.text = element_text(size = 12), legend.background = element_rect(color = "black")
                      , axis.text = element_text(size = 14), axis.title.x = element_text(size = 18) )







## Percentage - USING ----

library(tidyverse)

 test %>%
  group_by(Part) %>%
  mutate(percentage = Energy / sum(Energy)) %>%
  ggplot(., aes(x = factor(1),
                y = percentage,
                fill = Process_Segment)) +
  geom_bar(width = 1, stat='identity') +
  # geom_text(aes(x=factor(1), y = percentage, label = paste{round(percentage*100,0)),"%"}, position = position_stack(vjust = 0.5), 
  #           check_overlap = TRUE)+
   #the vjust inside the position stack does not work now but should if can update to ggplot2 v. 2.2.1
 
   
  labs(fill= "Process_Segment") +
  facet_grid(facets=.~Part) + 
  coord_polar(theta = "y")+
  ylab("") +
  xlab("")+
   
  scale_fill_manual(values = fillcolor, name = "") +
  
  theme_bw() + theme(  legend.title = element_blank(), legend.text = element_text(size = 12), 
                       legend.background = element_rect(color = "black")
                       , axis.text = element_text(size = 14)
                       , axis.title.x = element_text(size = 18)
                       , axis.text.x = element_blank()
                       , axis.text.y = element_blank()
                       , strip.text.x = element_text(size = 14)
                       , panel.grid = element_blank()
                       , axis.ticks=element_blank()
                       
                       )
 
 
## Waffles! ----

install.packages('waffle')

test.alt <- test %>%
  filter(Part == 'Alternate') %>%
  transmute(names = Process_Segment,
            vals = as.integer(round(Energy,0)))l
test.conv <- test %>%
  filter(Part == 'Conventional')

waffle(as.data.frame(test.alt), rows = 8)

parts <- data.frame(
  names = LETTERS[1:4],
  vals = c(80, 30, 20, 10)
)

waffle(parts, rows=8)

parts <- c(80, 30, 20, 10)
waffle

iron(
  waffle(c("Fiber"=52,"Primary Matrix Material" = 43), rows = 8),
  waffle(c("Fiber"=54,"Primary Matrix Material" = 53), rows = 8)
)

waffle(c("Fiber"=54,"Primary Matrix Material" = 5), rows = 5)






# Using plotly ----

  



p1 <- plot_ly(test[test$Part== "Alternate",], labels = ~Process_Segment, values = ~Energy, type = 'pie',
             textposition = 'auto',
             textinfo = 'percent',
             insidetextfont = list(color = '#FFFFFF', family = "Raleway", size = 18),
             hoverinfo = 'text',
             text = ~paste(round(Energy, 0), ' MJ'),
             sort = FALSE,
             marker = list(colors = fillcolor,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(title = 'Alternate', font = list(family = "Raleway", size = 16),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 

p2 <- plot_ly(test[test$Part == "Conventional", ] , labels = ~Process_Segment, values = ~Energy, type = 'pie',
              textposition = 'auto',
              textinfo = 'percent',
              insidetextfont = list(color = '#FFFFFF', family = "Raleway", size = 18),
              hoverinfo = 'text',
              text = ~paste(round(Energy, 0), ' MJ'),
              sort = FALSE,
              marker = list(colors = fillcolor,
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = TRUE) %>%
  layout(title = 'Conventional', font = list(family = "Raleway", size = 16),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
         legend = list(bordercolor  = "#123254", borderwidth = 1, bgcolor = "rgba(255, 255, 255, 0.5)" )) 



m <- economics[which.max(economics$unemploy), ]
n <- economics[which.max(economics$uempmed), ]

# font style
f <- list(
  family = "Raleway, monospace",
  size = 18,
  color = "black")

# annotations
a <- list(
  text = "SUBPLOT TITLE A",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

b <- list(
  text = "SUBPLOT TITLE B",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>%
  add_lines(name = ~"unemploy") %>%
  layout(annotations = a)
p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>%
  add_lines(name = ~"uempmed") %>%
  layout(annotations = b)
p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE)
p

