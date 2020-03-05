# install.packages("ggthemes")
# install.packages("ggrepel")
# install.packages("ChannelAttribution")
# install.packages("markovchain")
# install.packages("purrr")
# install.packages("tidyverse")
# install.packages("visNetwork")
# install.packages("scales")

# library(dplyr)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(visNetwork)
library(expm)
library(stringr)
library(googleVis)

setwd('C:/Users/xqian/Documents/GitHub/mta-with-markov-chains')

df <- read.csv('path_to_conversion.csv', stringsAsFactors = FALSE)

level <- 'Tactic'

df_int <- df[,c('Conversion.ID','Interaction.Date.Time',level)]
df_conv <- df[,c('Conversion.ID','Activity.Date.Time','Activity')]
df_conv <- df_conv %>%
  group_by(Conversion.ID) %>%
  summarise(conversion_time = max(Activity.Date.Time))
df_conv$conversion_time <- as.POSIXct(df_conv$conversion_time, format="%m/%d/%Y %H:%M", tz=Sys.timezone())


colnames(df_int) <- c('client_id','date','channel')

df_int$date <- as.POSIXct(df_int$date, format="%m/%d/%Y %H:%M", tz=Sys.timezone())

# aggregating channels to the paths for each customer
df2 <- df_int %>%
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = 1,
            conv_null = 0) %>%
  ungroup()

# calculating the models (Markov and heuristics)
mod2 <- markov_model(df2,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

# heuristic_models()
h_mod2 <- heuristic_models(df2, var_path = 'path', var_conv = 'conv')

# merging all models
all_models <- merge(h_mod2, mod2$result, by = 'channel_name')
colnames(all_models)[c(5)] <- c('attrib_model_conversions')


############## visualizations ##############
# transition matrix heatmap
df_plot_trans <- mod2$transition_matrix

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")

# models comparison
all_mod_plot <- melt(all_models, id.vars = 'channel_name', variable.name = 'conv_type')
all_mod_plot$value <- round(all_mod_plot$value)

# slope chart
pal <- colorRampPalette(brewer.pal(10, "Set1"))
ggplot(all_mod_plot, aes(x = conv_type, y = value, group = channel_name)) +
  theme_solarized(base_size = 18, base_family = "", light = TRUE) +
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  geom_line(aes(color = channel_name), size = 2.5, alpha = 0.8) +
  geom_point(aes(color = channel_name), size = 5) +
  geom_label_repel(aes(label = paste0(channel_name, ': ', value), fill = factor(channel_name)),
                   alpha = 0.7,
                   fontface = 'bold', color = 'white', size = 5,
                   box.padding = unit(0.25, 'lines'), point.padding = unit(0.5, 'lines'),
                   max.iter = 100) +
  theme(legend.position = 'none',
        legend.title = element_text(size = 16, color = 'black'),
        legend.text = element_text(size = 16, vjust = 2, color = 'black'),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold", color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold", color = 'black'),
        strip.background = element_rect(fill = "#f0b35f")) +
  labs(x = 'Model', y = 'Conversions') +
  ggtitle('Models comparison') +
  guides(colour = guide_legend(override.aes = list(size = 4)))

###################################################
##### Customer journey duration #####

colnames(df_conv) <- c('client_id','date')
df_conv$channel <- 'Quote Start'
df_conv$conversion <- 1

df_int$conversion <- 0

df_multi_paths <- rbind(df_int, df_conv)

df_multi_paths_tl <- df_multi_paths %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            first_touch_date = min(date),
            last_touch_date = max(date),
            tot_time_lapse = round(as.numeric(last_touch_date - first_touch_date)),
            conversion = sum(conversion)) %>%
  ungroup()

# distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  geom_histogram(fill = '#4e79a7', binwidth = 1)


# cumulative distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  stat_ecdf(geom = 'step', color = '#4e79a7', size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0.95, color = '#e15759', size = 1.5) +
  geom_vline(xintercept = 31, color = '#e15759', size = 1.5, linetype = 2)

##### Sankey diagram #####

# DEDUPE CONSECUTIVE DUPLICATES
df_multi_paths_tl$path2 <- strsplit(df_multi_paths_tl$path," > ")
df_multi_paths_tl$path2 <- sapply(df_multi_paths_tl$path2,function(x) rle(x)$value)


orders <- df_multi_paths_tl$path2

orders.plot <- data.frame()

for (j in 1:length(orders)) {
ord.cache <- data.frame(matrix(ncol = 2, nrow = length(orders[[j]])-1))
colnames(ord.cache)[1] <-'from'
colnames(ord.cache)[2]<-'to'
  
for (i in 1:(length(orders[[j]])-1)) {
  
  ord.cache$from[i] <- orders[[j]][i]
  ord.cache$to[i] <- orders[[j]][i+1]

  # adding tags
  ord.cache$from[i] <- paste0(ord.cache$from[i], '(', i, ')', sep='')
  ord.cache$to[i] <- paste0(ord.cache$to[i], '(', i+1, ')', sep='')
  
}
orders.plot <- rbind(orders.plot, ord.cache)
}

orders.plot1 <- orders.plot %>%
  group_by(from,to) %>%
  summarise(n=n())


plot(gvisSankey(orders.plot1, from='from', to='to', weight='n',
                options=list(height=900, width=1800, cex = 2,sankey="{link:{color:{fill:'lightblue'}}}")))