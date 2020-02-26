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

setwd('C:/Users/xqian/Documents/GitHub/mta-with-markov-chains')


##### simple example #####
# creating a data sample
df1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'), conv = c(1, 0, 0), conv_null = c(0, 1, 1))

# calculating the model
mod1 <- markov_model(df1,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

# extracting the results of attribution
df_res1 <- mod1$result

# extracting a transition matrix
df_trans1 <- mod1$transition_matrix
df_trans1 <- dcast(df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

### plotting the Markov graph ###
df_trans <- mod1$transition_matrix

# adding dummies in order to plot the graph
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
df_trans <- rbind(df_trans, df_dummy)

# ordering channels
df_trans$channel_from <- factor(df_trans$channel_from,
                                levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans$channel_to <- factor(df_trans$channel_to,
                              levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans <- dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))
trans_matrix[is.na(trans_matrix)] <- 0
trans_matrix1 <- new("markovchain", transitionMatrix = trans_matrix)

# plotting the graph
plot(trans_matrix1, edge.arrow.size = 0.35)



##### simulating the "real" data####
set.seed(354)
df2 <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE),
                  date = sample(c(1:32), 5000, replace = TRUE),
                  channel = sample(c(0:9), 5000, replace = TRUE,
                                   prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))
df2$date <- as.Date(df2$date, origin = "2015-01-01")
df2$channel <- paste0('channel_', df2$channel)

# aggregating channels to the paths for each customer
df2 <- df2 %>%
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

# heuristic_models() function doesn't work for me, therefore I used the manual calculations
# instead of:
#h_mod <- heuristic_models(df2, var_path = 'path', var_conv = 'conv')

df_hm <- df2 %>%
  mutate(channel_name_ft = sub('>.*', '', path),
         channel_name_ft = sub(' ', '', channel_name_ft),
         channel_name_lt = sub('.*>', '', path),
         channel_name_lt = sub(' ', '', channel_name_lt))
# first-touch conversions
df_ft <- df_hm %>%
  group_by(channel_name_ft) %>%
  summarise(first_touch_conversions = sum(conv)) %>%
  ungroup()
# last-touch conversions
df_lt <- df_hm %>%
  group_by(channel_name_lt) %>%
  summarise(last_touch_conversions = sum(conv)) %>%
  ungroup()

h_mod2 <- merge(df_ft, df_lt, by.x = 'channel_name_ft', by.y = 'channel_name_lt')

# merging all models
all_models <- merge(h_mod2, mod2$result, by.x = 'channel_name_ft', by.y = 'channel_name')
colnames(all_models)[c(1, 4)] <- c('channel_name', 'attrib_model_conversions')


############## visualizations ##############
# transition matrix heatmap for "real" data
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



##### simulating the "real" data with >1 conversions per id #####
set.seed(454)
df_raw <- data.frame(customer_id = paste0('id', sample(c(1:20000), replace = TRUE)), date = as.Date(rbeta(80000, 0.7, 10) * 100, origin = "2016-01-01"), channel = paste0('channel_', sample(c(0:7), 80000, replace = TRUE, prob = c(0.2, 0.12, 0.03, 0.07, 0.15, 0.25, 0.1, 0.08))) ) %>%
  group_by(customer_id) %>%
  mutate(conversion = sample(c(0, 1), n(), prob = c(0.975, 0.025), replace = TRUE)) %>%
  ungroup() %>%
  # dmap_at(c(1, 3), as.character) %>%
  arrange(customer_id, date)

df_raw <- df_raw %>%
  mutate(channel = ifelse(channel == 'channel_2', NA, channel))


##### splitting paths #####
df_paths <- df_raw %>%
  group_by(customer_id) %>%
  mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()

# first purchases only
df_paths_1 <- df_paths %>%
  filter(path_no == 1) %>%
  select(-path_no)

##### replace some channels #####
df_path_1_clean <- df_paths_1 %>%
  # removing NAs
  filter(!is.na(channel)) %>%
  
  # adding order of channels in the path
  group_by(customer_id) %>%
  mutate(ord = c(1:n()),
         is_non_direct = ifelse(channel == 'channel_6', 0, 1),
         is_non_direct_cum = cumsum(is_non_direct)) %>%
  
  # removing Direct (channel_6) when it is the first in the path
  filter(is_non_direct_cum != 0) %>%
  
  # replacing Direct (channel_6) with the previous touch point
  mutate(channel = ifelse(channel == 'channel_6', channel[which(channel != 'channel_6')][is_non_direct_cum], channel)) %>%
  
  ungroup() %>%
  select(-ord, -is_non_direct, -is_non_direct_cum)


##### one- and multi-channel paths #####

df_path_1_clean <- df_path_1_clean %>%
  group_by(customer_id) %>%
  mutate(uniq_channel_tag = ifelse(length(unique(channel)) == 1, TRUE, FALSE)) %>%
  ungroup()

df_path_1_clean_uniq <- df_path_1_clean %>%
  filter(uniq_channel_tag == TRUE) %>%
  select(-uniq_channel_tag)

df_path_1_clean_multi <- df_path_1_clean %>%
  filter(uniq_channel_tag == FALSE) %>%
  select(-uniq_channel_tag)

### experiment ###
# attribution model for all paths
df_all_paths <- df_path_1_clean %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib <- markov_model(df_all_paths,
                           var_path = 'path',
                           var_conv = 'conversion',
                           out_more = TRUE)
mod_attrib$removal_effects
mod_attrib$result
d_all <- data.frame(mod_attrib$result)

# attribution model for splitted multi and unique channel paths
df_multi_paths <- df_path_1_clean_multi %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib_alt <- markov_model(df_multi_paths,
                               var_path = 'path',
                               var_conv = 'conversion',
                               out_more = TRUE)
mod_attrib_alt$removal_effects
mod_attrib_alt$result

# adding unique paths
df_uniq_paths <- df_path_1_clean_uniq %>%
  filter(conversion == 1) %>%
  group_by(channel) %>%
  summarise(conversions = sum(conversion)) %>%
  ungroup()

d_multi <- data.frame(mod_attrib_alt$result)
d_multi$channel_name <- as.numeric(d_multi$channel_name)

d_split <- full_join(d_multi, df_uniq_paths, by = c('channel_name' = 'channel')) %>%
  mutate(result = total_conversions + conversions)

# CHECK HERE!
sum(d_all$total_conversions)
sum(d_split$result)