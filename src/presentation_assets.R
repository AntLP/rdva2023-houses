library(tidyverse)
library(tidymodels)
library(GGally)
library(xaringanthemer)
library(ggridges)
library(vip)
library(gt)
library(grid)
library(gridExtra)
library(cld2)
library(wordcloud2)

PROMU_YELLOW <- "#fddb00"
PROMU_GREY = "#53565A"

theme_set(theme_bw() +
            theme(rect = element_rect(fill = "transparent")) +
            theme(
              panel.background = element_rect(fill = "transparent",
                                              colour = NA_character_),
              plot.background = element_rect(fill = "transparent",
                                             colour = NA_character_),
              legend.background = element_rect(fill = "transparent",
                                               colour = NA_character_),
              legend.box.background = element_rect(fill = "transparent",
                                                   colour = NA_character_),
              legend.key = element_rect(fill = "transparent")
            ))



# Data ----
model_data <- read_rds("./data/model_data.rds")
data_model_pres <- read_rds("./data/model_data_mini.rds")
model_fits <- read_rds("./data/all_fits.rds")


# Présentation du jeu de données ----

tbl_raw_data <- data_model_pres %>%
  slice(1:5) %>%
  select(price, region, building_style, year_built, lot_area, n_rooms, n_bathrooms,
         parking_total, fireplace_stove, pool, description) %>%
  mutate(lot_area = str_remove(lot_area, " sqft") %>% parse_number(),
         description = description %>%
           str_remove("Description\n\r\n\r\n\r ") %>%
           str_extract("^(?:[\\w-]+[^\\w-]+){2}[\\w-]+") %>%
           paste0("...")) %>%
  gt() %>%
  cols_label(price = "Prix",
             region = "Région",
             building_style = "Type d'habitation",
             year_built = "Année de construction",
             lot_area = "Taille du terrain",
             n_rooms = "Pièces",
             n_bathrooms = "Salles de bain",
             parking_total = "Stationnement",
             pool = "Piscine",
             fireplace_stove = "Foyer/Poêle",
             description = "Description") %>%
  fmt_currency(price, placement = "right", decimals = 0, sep_mark = " ") %>%
  fmt_integer(sep_mark = " ") %>%
  tab_style(
    style = list(
      cell_fill(color = "#AAAAAA", alpha = 0)
    ),
    locations = cells_body()
  )


write_rds(tbl_raw_data, "./src/assets/tbl_raw_data.rds")



# Analyse exploratoire des données ----
#
# parking_top <- model_data %>% count(parking_total) %>% filter(n > 100) %>% pull(parking_total)
# fireplace_top <- model_data %>% count(fireplace_stove) %>% filter(n > 100) %>% pull(fireplace_stove)
#
# model_data_expl <- model_data %>%
#   select(price, region, building_style, year_built, lot_area, n_rooms, n_bathrooms,
#          parking_total, fireplace_stove, pool) %>%
#   mutate(year_built = as.integer(year_built),
#          lot_area = parse_number(lot_area),
#          parking_total = ifelse(parking_total %in% parking_top, parking_total, "Other"),
#          fireplace_stove = ifelse(fireplace_stove %in% fireplace_top, fireplace_stove, "Other")
#   )
#
#
# ggpairs(model_data_expl %>%
#           select(price, region, lot_area, n_rooms, n_bathrooms))
#
#
#





## Price ----

plot_price_density <- model_data %>%
  ggplot(aes(x = pmin(price, 2000000))) +
  geom_density(fill = PROMU_YELLOW, alpha = 0.6, color = darken_color(PROMU_YELLOW, 0.1), linewidth = 0.75) +
  geom_vline(aes(xintercept = median(price), linetype = "Médiane")) +
  geom_vline(aes(xintercept = mean(price), linetype = "Moyenne")) +
  scale_x_continuous(labels = label_comma()) +
  scale_linetype_manual(values = c("Médiane" = 2, "Moyenne" = 1)) +
  labs(
    x = "Prix affiché",
    y = "Densité",
    linetype = ""
  ) +
  theme(legend.position = "top")

ggsave("./src/assets/plot_price_density.png", plot_price_density, width = 1920, height = 1080, units = "px", bg = "transparent")

plot_price_density



plot_price_region_boxplot <- model_data %>%
  ggplot(aes(x = region, y = pmin(price, 2000000), fill = region)) +
  geom_boxplot(alpha = 0.75) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_brewer(type = "qual") +
  labs(y = "Prix affiché",
       x = "") +
  coord_flip() +
  theme(legend.position = "none")

ggsave("./src/assets/plot_price_region_boxplot.png", plot_price_region_boxplot, width = 1920, height = 1080, units = "px", bg = "transparent")

plot_price_region_boxplot

# Premier modèle ----

## VIP ----
plot_vip_model1 <- model_fits$last_fit[[2]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = "midnightblue", color = "black"))

ggsave("./src/assets/plot_vip_model1.png", plot_vip_model1, width = 1920, height = 1080, units = "px", bg = "transparent")




## Performance ----


plot_AE_model1 <- model_fits$last_fit[[2]] %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred, y = price)) +
  geom_segment(aes(x = 0, y = 0, xend = 5000000, yend = 5000000), linetype = 2, linewidth = 0.4) +
  geom_point(alpha = 0.6, color = "grey20") +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  coord_cartesian(xlim = c(100000, 2000000),
                  ylim = c(100000, 2000000)) +
  labs(
    x = "Prédiction",
    y = "Prix réel"
  )

ggsave("./src/assets/plot_AE_model1.png", plot_AE_model1, width = 1920, height = 1080, units = "px", bg = "transparent")


quantiles_bounds <- quantile_test(model_fits$last_fit[[2]], 25) %>%
  group_by(quantile) %>%
  summarise(lbound = min(.pred))


plot_demo_lift <- plot_AE_model1 +
  geom_vline(aes(xintercept = lbound), data = quantiles_bounds, linewidth = 0.25)

ggsave("./src/assets/plot_demo_lift.png", plot_demo_lift, width = 1920, height = 1080, units = "px", bg = "transparent")


plot_lift_model1 <- lift_chart(model_fits$last_fit[[2]], 25, "")

ggsave("./src/assets/plot_lift_model1.png", plot_lift_model1 + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")



model_fits$last_fit[[2]] %>%
  collect_predictions() %>%
  mutate(
    error = price - .pred,
    error_oct = price / .pred - 1
  ) %>%
  ggplot(aes(y = error, x = 1)) +
  geom_violin(fill = PROMU_YELLOW, alpha = 0.5, color = PROMU_GREY) +
  scale_y_continuous(labels = label_comma())



lift_error(model_fits$last_fit[[2]], 15)





model_fits$last_fit[[2]] %>%
  lift_density(15) +
  geom_vline(aes(xintercept = -50000), linetype = 2) +
  geom_vline(aes(xintercept = 50000), linetype = 2) +
  coord_cartesian(xlim = c(-300000, 300000))

model_fits$last_fit[[2]] %>%
  collect_predictions() %>%
  yardstick::mae(truth = price, estimate = .pred)



# Embeddings ----

plot_exemple_embedding <- tribble(
  ~mot, ~x, ~y,
  "bricoleur", 0.5, 0.5,
  "potentiel", 0.55, 0.7,
  "abondance", -0.8, -0.2
) %>%
  ggplot(aes(xend = x, yend = y, label = mot)) +
  geom_segment(aes(x = 0, y = 0), arrow = arrow(length = unit(0.25, "cm"))) +
  geom_label(aes(x = x, y = y), nudge_x = 0.2, nudge_y = -0.05) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(
    x = "",
    y = ""
  )

ggsave("./src/assets/plot_exemple_embedding.png", plot_exemple_embedding, width = 2160, height = 2160, units = "px", bg = "transparent")






model_data %>%
  mutate(description = description %>%
           str_remove("Description\n\r\n\r\n\r ") %>%
           str_extract("^(?:[\\w-]+[^\\w-]+){2}[\\w-]+") %>%
           paste0("..."),
         temp1 = "⇒",
         temp2 = "⠀⠀⠀⠀...⠀⠀⠀⠀") %>%
  select(description, temp1, embedding_0001, embedding_0002, temp2, embedding_1535, embedding_1536) %>%
  slice(1:7) %>%
  gt() %>%
  cols_label(
    description = "Description",
    temp1 = "⇒",
    temp2 = "⠀⠀⠀⠀...⠀⠀⠀⠀"
  ) %>%
  write_rds("./src/tbl_embeddings.rds")

# Embed only ----


## VIP ----
plot_vip_model_embed <- model_fits$last_fit[[3]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = "midnightblue", color = "black"))

ggsave("./src/assets/plot_vip_model_embed.png", plot_vip_model_embed, width = 1920, height = 1080, units = "px", bg = "transparent")




## Performance ----



plot_lift_model_embed <- lift_chart(model_fits$last_fit[[3]], 25, "")

ggsave("./src/assets/plot_lift_model_embed.png", plot_lift_model_embed + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")

## Wordclouds ----

processed_description <- model_data %>%
  select(id, description) %>%
  mutate(
    description = str_remove_all(description, "Description\n\r\n\r\n\r "),
    lang = detect_language(description),
    lang = ifelse(is.na(lang), "fr", lang)
  ) %>%
  unnest_tokens(tokens, description, drop = FALSE) %>%
  select(-description) %>%
  nest_by(lang) %>%
  ungroup() %>%
  mutate(data = map2(lang, data, ~anti_join(.y, get_stopwords(language = .x), join_by(tokens == word)))) %>%
  unnest(data)


predictions_embed_only <- bind_cols(
  predict(model_fits$fit[[3]], new_data = model_data),
  model_data %>% select(id)
) %>%
  arrange(.pred) %>%
  mutate(quantile = 1:n() %/% (n() / 5 + 1))



tfidf_res <- predictions_embed_only %>%
  left_join(processed_description, by = "id") %>%
  count(quantile, tokens) %>%
  filter(!grepl("^[0-9]*$", tokens)) %>%
  filter(n > 75) %>%
  bind_tf_idf(tokens, quantile, n)


tfidf_res_fr <- predictions_embed_only %>%
  left_join(processed_description, by = "id") %>%
  filter(lang == "fr") %>%
  select(-quantile) %>%
  nest_by(id) %>%
  ungroup() %>%
  mutate(quantile = 1:n() %/% (n() / 5 + 1)) %>%
  unnest(data) %>%
  count(quantile, tokens) %>%
  filter(!grepl("^[0-9]*$", tokens)) %>%
  filter(n > 50) %>%
  bind_tf_idf(tokens, quantile, n)




palette <- c(
  sapply(seq(0, 1, by = 0.1), darken_color, color_hex = PROMU_YELLOW),
  sapply(seq(0, 0.5, by = 0.1), lighten_color, color_hex = PROMU_GREY)
) %>% rep(100)

set.seed(123)
tfidf_res_q0 <- tfidf_res %>% filter(quantile == 0)
wordcloud(words = tfidf_res_q0$tokens,
          freq = tfidf_res_q0$tf_idf,
          min.freq = 0,
          max.words = 2000,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

tfidf_res %>%
  filter(quantile == 0) %>%
  select(word = tokens, freq = tf_idf) %>%
  wordcloud2(shape = "pentagon",
             shuffle = F,
             color = "random-light", backgroundColor = "grey")


tfidf_res %>%
  filter(quantile == 4) %>%
  select(word = tokens, freq = tf_idf) %>%
  wordcloud2(shape = "pentagon",
             shuffle = F,
             color = "random-light", backgroundColor = "grey")



plot_lift_by_lang <- bind_cols(
  model_fits$last_fit[[3]] %>%
    collect_predictions(),
  model_data %>%
    slice(model_fits$last_fit[[3]] %>%
            collect_predictions() %>%
            select(.row) %>%
            pull()) %>%
    mutate(lang = detect_language(description)) %>%
    select(lang)
) %>%
  filter(!is.na(lang)) %>%
  mutate(lang = ifelse(lang == "fr", "Français", "Anglais")) %>%
  arrange(lang, .pred) %>%
  group_by(lang) %>%
  mutate(quantile = 1:n() %/% (n() / 10 + 1)) %>%
  group_by(lang, quantile) %>%
  summarise(
    `prédiction` = mean(.pred),
    `réel` = mean(price),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(`prédiction`, `réel`),
    names_to = "type",
    values_to = "valeur"
  ) %>%
  ggplot(aes(x = quantile, y = valeur, color = type)) +
  geom_point(alpha = 0.8) +
  geom_line() +
  facet_grid(rows = vars(lang), scales = "free_y") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  theme(legend.position = "top")

ggsave("./src/assets/plot_lift_by_lang.png", plot_lift_by_lang + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")


# Modèle final ----


model_fits$last_fit[[5]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = "midnightblue", color = "black"))

performance_stats(model_fits$last_fit[[5]], 25, "")
dbl_lift_chart(model_fits$last_fit[[2]], model_fits$last_fit[[5]], "sans description", "avec description", 25, "")

lift_density(model_fits$last_fit[[5]], 25) +
  geom_vline(aes(xintercept = -50000), linetype = 2) +
  geom_vline(aes(xintercept = 50000), linetype = 2) +
  coord_cartesian(xlim = c(-300000, 300000))

model_fits$last_fit[[5]] %>%
  lift_density(15) +
  geom_vline(aes(xintercept = -50000), linetype = 2) +
  geom_vline(aes(xintercept = 50000), linetype = 2)

model_fits$last_fit[[5]] %>%
  collect_predictions() %>%
  yardstick::mae(truth = price, estimate = .pred)






















