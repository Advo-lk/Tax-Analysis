library(ggplot2)
library(scales)
rm(list = ls())

# Generate data
x <- seq(0, 1000000, by = 1000)
y_dotted <- x

#Income After Tax 

y_ranil <- ifelse(x <= 100000, x,
           ifelse(x <= 141666.67,(x-100000)* 0.94+100000,
           ifelse(x <= 183335.33,(41666.67*0.94)+(x - 100000 - 41666.67)* 0.88 + 100000,
           ifelse(x <= 225003, (41666.67*0.94)+((41666.67)*0.88)+(x-100000 - 2*41666.67)*0.82 +100000,
           ifelse(x <= 266670.67,(41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(x-100000-3*41666.67) * 0.76 +100000,
           ifelse(x <= 308333.33,(41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(41666.67*0.76)+(x-100000-4*41666.67) * 0.70 +100000,(41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(41666.67*0.76)+(41666.67*0.70)+(x - 100000 - 5*41666.67)*0.64 + 100000)))))) 

y_gota <- ifelse(x <= 250000,x,
          ifelse(x <= 500000, (x - 250000)*0.94 + 250000,
          ifelse(x <= 750000, (250000*0.94) + ((x - 250000 - 250000)*0.88 +250000)*0.92,(250000*0.94)+(250000*0.88)+(x - 250000 - 2*250000)*0.82 + 250000)))
                 
y_mangala <- ifelse(x <= 41666.67, x,
             ifelse(x <= 91666.67,(x-41666.67)* 0.96 + 41666.67,
             ifelse(x <= 141666.67,(50000*0.96)+(x - 41666.67 - 50000)* 0.92 + 41666.67,
             ifelse(x <= 191666.67,(50000*0.96)+(50000*0.92)+(x-41666.67 - 2*50000)*0.88 +41666.67,
             ifelse(x <= 241666.67,(50000*0.96)+(50000*0.92)+(41666.67*0.88)+(x-41666.67-3*50000) * 0.84 +41666.67,
             ifelse(x <= 291666.67,(50000*0.96)+(50000*0.92)+(50000*0.88)+(50000*0.84)+(x-41666.67-4*50000) * 0.80 +41666.67,(50000*0.96)+(50000*0.92)+(50000*0.88)+(50000*0.84)+(50000*0.80)+(x - 41667.66 - 5*50000)*0.76+41666.76)))))) 

df_list <-list("x" = x,"y_gota" = y_gota,"y_ranil" = y_ranil, "y_mangala" = y_mangala)
df <- as.data.frame(df_list)  

#Take home pay after EPF/ETF

ranil_z <- ifelse(x <= 100000, (x * 0.92),
           ifelse(x <= 141666.67,((x-100000)* 0.94+100000)*0.92,
           ifelse(x <= 183335.33,((41666.67*0.94)+(x - 100000 - 41666.67)* 0.88 + 100000)*0.92,
           ifelse(x <= 225003, ((41666.67*0.94)+((41666.67)*0.88)+(x-100000 - 2*41666.67)*0.82 +100000)*0.92,
           ifelse(x <= 266670.67, ((41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(x-100000-3*41666.67) * 0.76 +100000)*0.92,
           ifelse(x <= 308333.33, ((41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(41666.67*0.76)+(x-100000-4*41666.67) * 0.70 +100000)*0.92,((41666.67*0.94)+(41666.67*0.88)+(41666.67*0.82)+(41666.67*0.76)+(41666.67*0.70)+(x - 100000 - 5*41666.67)*0.64 + 100000)*0.92)))))) 

gota_z <- ifelse(x <= 250000, (x*0.92),
          ifelse(x <= 500000, ((x - 250000)*0.94+250000)*0.92,
          ifelse(x <= 750000, ((250000*0.94)+(x - 250000 - 250000)*0.88 +250000)*0.92,((250000*0.94)+(250000*0.88)+(x - 250000 - 2*250000)*0.82 + 250000)*0.92 )))

mangala_z <- ifelse(x <= 41666.67, (x*0.92),
             ifelse(x <= 91666.67,((x-41666.67)* 0.96+41666.67)*0.92,
             ifelse(x <= 141666.67,((50000*0.96)+(x - 41666.67 - 50000)* 0.92 + 41666.67)*0.92,
             ifelse(x <= 191666.67, ((50000*0.96)+(50000*0.92)+(x-41666.67 - 2*50000)*0.88 +41666.67)*0.92,
             ifelse(x <= 241666.67, ((50000*0.96)+(50000*0.92)+(41666.67*0.88)+(x-41666.67-3*50000) * 0.84 +41666.67)*0.92,
             ifelse(x <= 291666.67, ((50000*0.96)+(50000*0.92)+(50000*0.88)+(50000*0.84)+(x-41666.67-4*50000) * 0.80 +41666.67)*0.92,((50000*0.96)+(50000*0.92)+(50000*0.88)+(50000*0.84)+(50000*0.80)+(x - 41667.66 - 5*50000)*0.76+41666.76)*0.92)))))) 

df_list <-list("x" = x,"ranil_z" = ranil_z,"gota_z" = gota_z, "mangala_z" = mangala_z)
df <- as.data.frame(df_list) 
  
# Create the plot
ggplot() +
  geom_line(data = data.frame(x, y_dotted), aes(x, y_dotted), linetype = "dotted", color = "#666666", size=1) +
  geom_line(data = data.frame(x, y_ranil), aes(x, y_ranil), color = "#01B10C", size=1.25) +
  geom_line(data = data.frame(x, ranil_z), aes(x, ranil_z), color = "#01B10C", size=1.25,linetype = "dotted")+
  geom_line(data = data.frame(x, y_gota), aes(x, y_gota), color = "#8D153A", size=1.25) +
  geom_line(data = data.frame(x, gota_z), aes(x,gota_z), color = "#8D153A", size=1.25,linetype = "dotted")+
  geom_line(data = data.frame(x, y_mangala), aes(x,y_mangala), color = "yellow", size=1.25)+
  geom_line(data = data.frame(x,mangala_z), aes(x,mangala_z), color = "yellow", size=1.25,linetype = "dotted")+
  xlab("Total Income and Benefits") +
  ylab("Take-Home Pay") +
  scale_y_continuous(breaks = seq(0, 1000000, by = 100000), 
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(breaks = seq(0, 1000000, by = 100000), 
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  
  theme(panel.background = element_rect(fill = "#ffffff"), 
        plot.background = element_rect(fill = "white")) +
  theme(panel.grid.major.y = element_line(color = "#cccccc", linetype = "solid"),
        panel.grid.major.x = element_blank())

