library(jsonlite)
library(tidyverse)
library(tidymodels)
library(GGally)
library(xaringanthemer)
library(ggrepel)
library(vip)
library(gt)
library(grid)
library(gridExtra)
library(cld2)
library(wordcloud2)
library(tidytext)

source("./src/functions.R")

PROMU_YELLOW <- "#fddb00"
PROMU_GREY = "#53565A"

update_gg_theme(c("./promu_theme.css", "./xaringan-themer.css"))

# Data ----
model_data <- read_rds("./data/data/model_data.rds")
data_model_pres <- read_rds("./data/data/model_data_mini.rds")
model_fits <- read_rds("./data/data/all_fits.rds")


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
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners())
  )


write_rds(tbl_raw_data, "./data/assets/tbl_raw_data.rds")




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

ggsave("./data/assets/plot_price_density.png", plot_price_density, width = 1920, height = 1080, units = "px", bg = "transparent")

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

ggsave("./data/assets/plot_price_region_boxplot.png", plot_price_region_boxplot, width = 1920, height = 1080, units = "px", bg = "transparent")

plot_price_region_boxplot

# Premier modèle ----

## VIP ----
plot_vip_model1 <- model_fits$last_fit[[2]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = PROMU_YELLOW, color = "black"))

ggsave("./data/assets/plot_vip_model1.png", plot_vip_model1, width = 1920, height = 1080, units = "px", bg = "transparent")




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

ggsave("./data/assets/plot_AE_model1.png", plot_AE_model1, width = 1920, height = 1080, units = "px", bg = "transparent")


quantiles_bounds <- quantile_test(model_fits$last_fit[[2]], 25) %>%
  group_by(quantile) %>%
  summarise(lbound = min(.pred))


plot_demo_lift <- plot_AE_model1 +
  geom_vline(aes(xintercept = lbound), data = quantiles_bounds, linewidth = 0.25)

ggsave("./data/assets/plot_demo_lift.png", plot_demo_lift, width = 1920, height = 1080, units = "px", bg = "transparent")



plot_lift_model1 <- lift_chart(model_fits$last_fit[[2]], 25)

ggsave("./data/assets/plot_lift_model1.png", plot_lift_model1 + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")



plot_lift_error_model1 <- lift_error(model_fits$last_fit[[2]], 25) +
  coord_cartesian(ylim = c(-300000, 50000)) +
  scale_y_continuous(breaks = seq(-225000, 50000, by = 25000),
                     labels = label_comma(big.mark = " "))

ggsave("./data/assets/plot_lift_error_model1.png", plot_lift_error_model1, width = 1920, height = 1080, units = "px", bg = "transparent")





# Embeddings ----

set.seed(234)
dat_exemple <- plot_exemple_embedding <- tribble(
  ~mot, ~x, ~y,
  "bricoleur", 0.35, -0.17,
  "potentiel", 0.35, -0.10,
  "démolir", 0.8, -0.8,
  "magnifique", 0.6, 0.5,
  "belle", -0.2, 0.2
)

dat_exemple %>%
  gt() %>%
  cols_label(
    mot = "Mot",
    x = "Magnitude",
    y = "Impact"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners())
  ) %>%
  write_rds("./data/assets/tbl_exemple_embedding.rds")

plot_exemple_embedding <- dat_exemple %>%
  ggplot(aes(xend = x, yend = y, label = mot, color = mot)) +
  geom_segment(aes(x = -1.1, xend = 1.1, y = 0, yend = 0), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.25, color = "grey90") +
  geom_segment(aes(x = 0, xend = 0, y = -1.1, yend = 1.1), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.25, color = "grey90") +
  geom_label_repel(aes(x = x, y = y), size = 9, force_pull = 0.1) +
  geom_segment(aes(x = 0, y = 0), arrow = arrow(length = unit(0.25, "cm"))) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(
    x = "Magnitude",
    y = "Impact"
  ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "transparent", fill = NA),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank())

ggsave("./data/assets/plot_exemple_embedding.png", plot_exemple_embedding, width = 2160, height = 2160, units = "px", bg = "transparent")






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
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners())
  ) %>%
  write_rds("./data/assets/tbl_embeddings.rds")



model_data %>%
  slice(1:7) %>%
  bake(extract_recipe(model_fits$fit[[3]]), new_data = .) %>%
  select(-description) %>%
  bind_cols(model_data %>% slice(1:7) %>% select(description, contains("embedding"))) %>%
  mutate(description = description %>%
           str_remove("Description\n\r\n\r\n\r ") %>%
           str_extract("^(?:[\\w-]+[^\\w-]+){2}[\\w-]+") %>%
           paste0("..."),
         temp1 = "⇒",
         temp2 = temp1,
         temp3 = "⠀⠀⠀⠀...⠀⠀⠀⠀",
         temp4 = temp3) %>%
  select(description, temp1, embedding_0001, temp3, embedding_1536, temp2, PC01, temp4, PC10) %>%
  gt() %>%
  cols_label(
    description = "Description",
    temp1 = "⇒",
    temp3 = "⠀⠀⠀⠀...⠀⠀⠀⠀",
    temp2 = "⇒",
    temp4 = "⠀⠀⠀⠀...⠀⠀⠀⠀"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners())
  ) %>%
  write_rds("./data/assets/tbl_embeddings_pc.rds")


# Embed only ----


## VIP ----
plot_vip_model_embed <- model_fits$last_fit[[3]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = PROMU_YELLOW, color = "black"))

ggsave("./data/assets/plot_vip_model_embed.png", plot_vip_model_embed, width = 1920, height = 1080, units = "px", bg = "transparent")




## Performance ----



plot_lift_model_embed <- lift_chart(model_fits$last_fit[[3]], 25)

ggsave("./data/assets/plot_lift_model_embed.png", plot_lift_model_embed + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")



plot_dbl_lift_model_embed <- dbl_lift_chart(model_fits$last_fit[[2]], model_fits$last_fit[[3]], "sans description", "description uniquement", 25)

ggsave("./data/assets/plot_dbl_lift_model_embed.png", plot_dbl_lift_model_embed + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")


## Analyse ----

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





tfidf_res %>%
  filter(quantile == 0) %>%
  arrange(desc(tf_idf)) %>%
  select(tokens, tf_idf) %>%
  head(10) %>%
  gt() %>%
  tab_header("Plus fréquent dans le premier quintile") %>%
  cols_label(
    tokens = "Mot",
    tf_idf = "TF-IDF"
  ) %>%
  fmt_percent(tf_idf) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title())
  ) %>%
  write_rds("./data/assets/tbl_top_words_low.rds")



tfidf_res %>%
  filter(quantile == 4) %>%
  arrange(desc(tf_idf)) %>%
  select(tokens, tf_idf) %>%
  head(10) %>%
  gt() %>%
  tab_header("Plus fréquent dans le dernier quintile") %>%
  cols_label(
    tokens = "Mot",
    tf_idf = "TF-IDF"
  ) %>%
  fmt_percent(tf_idf) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title())
  ) %>%
  write_rds("./data/assets/tbl_top_words_high.rds")






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

ggsave("./data/assets/plot_lift_by_lang.png", plot_lift_by_lang + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")


# Modèle final ----







## VIP ----
plot_vip_model3 <- model_fits$last_fit[[5]] %>%
  extract_fit_engine() %>%
  vip(aesthetics = list(alpha = 0.75, fill = PROMU_YELLOW, color = "black"))

ggsave("./data/assets/plot_vip_model3.png", plot_vip_model3, width = 1920, height = 1080, units = "px", bg = "transparent")




## Performance ----


plot_lift_model3 <- lift_chart(model_fits$last_fit[[5]], 25)

ggsave("./data/assets/plot_lift_model3.png", plot_lift_model3 + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")




plot_dbl_lift_model_final <- dbl_lift_chart(model_fits$last_fit[[2]], model_fits$last_fit[[5]], "sans description", "avec description", 25)

ggsave("./data/assets/plot_dbl_lift_model_final.png", plot_dbl_lift_model_final + theme(legend.position = "top"), width = 1920, height = 1080, units = "px", bg = "transparent")

dbl_lift_quantiles(model_fits$last_fit[[2]], model_fits$last_fit[[5]], 25) %>%
  mutate(
    diff = pred1 / pred2 - 1
  ) %>%
  left_join(model_data %>% mutate(.row =  row_number()) %>% select(.row, description)) %>%
  mutate(
    description = str_remove_all(description, "Description\n\r\n\r\n\r ")
  ) %>%
  filter(!grepl("^Centris", description)) %>%
  mutate(
    description = str_remove_all(description, regex("centris no. [0-9]*", ignore_case = T))
  ) %>%
  arrange(desc(diff)) %>%
  select(description, pred1, pred2, price) %>%
  filter(price < 800000) %>%
  slice(8, 14, 20, 26, 27) %>%
  mutate(
    description = description %>%
      str_replace_all('Zone inondable 0-20 ans', '<a style="color:red">Zone inondable 0-20 ans</a>') %>%
      str_replace_all(regex('Compagne|Campagne', ignore_case = T), '<a style="color:red">campagne</a>') %>%
      str_replace_all(regex('Village', ignore_case = T), '<a style="color:red">village</a>') %>%
      str_replace_all(regex("quelques rénovations ont été effectuées, d'autres restent à faire et à finaliser", ignore_case = T), '<a style="color:red">quelques rénovations ont été effectuées, d\'autres restent à faire et à finaliser</a>') %>%
      str_replace_all(regex('sans garantie légale de qualité', ignore_case = T), '<a style="color:red">sans garantie légale de qualité</a>') %>%
      str_replace_all(regex('potentiel de rénovation', ignore_case = T), '<a style="color:red">potentiel de rénovation</a>') %>%
      str_replace_all(regex('sous-sol partiellement aménagé', ignore_case = T), '<a style="color:red">sous-sol partiellement aménagé</a>') %>%
      str_replace_all(regex('besoin de plusieurs travaux', ignore_case = T), '<a style="color:red">besoin de plusieurs travaux</a>') %>%
      str_replace_all(regex('pas eu de chauffage intérieur depuis plus de 1 an', ignore_case = T), '<a style="color:red">pas eu de chauffage intérieur depuis plus de 1 an</a>')
  ) %>%
  gt() %>%
  fmt_markdown(description) %>%
  fmt_number(c(pred1, pred2, price), sep_mark = "&nbsp;", decimals = 0) %>%
  cols_label(
    description = "Description",
    pred1 = "sans description",
    pred2 = "avec description",
    price = "Réel"
  ) %>%
  tab_spanner(label = "Prédiction", columns = 2:3) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners())
  ) %>%
  cols_align("center", c(pred1, pred2, price)) %>%
  write_rds("./data/assets/tbl_avec_vs_sans_description.rds")








test_embeddings <- read_json("./data/data/test_descriptions_embeddings.json", )


test_data <- model_data %>%
  slice(6) %>%
  select(-description, -contains("embedding")) %>%
  bind_cols(
    tibble(
      description = sapply(test_embeddings, function(x) x$description[1]),
      embedding = lapply(test_embeddings, function(x) unlist(x$embedding))) %>%
      unnest(embedding) %>%
      group_by(description) %>%
      dplyr::mutate(i = stringr::str_pad(dplyr::row_number(), 4, "left", "0")) %>%
      ungroup() %>%
      pivot_wider(id_cols = description, names_from = i, values_from = embedding, names_prefix = "embedding_")
  )

test_predictions <- bind_cols(
  test_data %>% select(description),
  predict(model_fits$fit[[5]], new_data = test_data)
)




test_predictions %>%
  slice(c(1, 3, 2, 4)) %>%
  gt() %>%
  fmt_number(.pred, decimals = 0, sep_mark = " ") %>%
  cols_label(
    description = "Description",
    .pred = "Prédiction"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title())
  ) %>%
  write_rds("./data/assets/tbl_test_descriptions1.rds")



test_predictions %>%
  slice(5:6) %>%
  gt() %>%
  fmt_number(.pred, decimals = 0, sep_mark = " ") %>%
  cols_label(
    description = "Description",
    .pred = "Prédiction"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title())
  ) %>%
  write_rds("./data/assets/tbl_test_descriptions2.rds")





test_predictions %>%
  slice(7:8) %>%
  gt() %>%
  fmt_number(.pred, decimals = 0, sep_mark = " ") %>%
  cols_label(
    description = "Description",
    .pred = "Prédiction"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFFFFF00", alpha = 0.9)
    ),
    locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title())
  ) %>%
  write_rds("./data/assets/tbl_test_descriptions3.rds")




