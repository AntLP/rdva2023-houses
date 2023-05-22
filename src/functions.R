library(httr)
library(tidyverse)

quantile_test <- function(tuned_model, n_buckets){

  tuned_model %>%
    collect_predictions() %>%
    arrange(.pred) %>%
    mutate(quantile = 1:n() %/% (n() / n_buckets + 1))

}



lift_chart <- function(tuned_model, n_buckets, subtitle = ""){

  quantile_test(tuned_model, n_buckets) %>%
    group_by(quantile) %>%
    summarise(
      `prédiction` = mean(.pred),
      `réel`= mean(price)
    ) %>%
    pivot_longer(cols = c(`prédiction`, `réel`), names_to = "quantity", values_to = "amount") %>%
    ggplot(aes(x = quantile, y = amount, color = quantity)) +
    geom_line() +
    geom_point(alpha = 0.75, size = 0.75) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      y = "Prédiction",
      color = "",
      title = "",
      subtitle = subtitle
    ) +
    theme(legend.position = "top")

}




lift_error <- function(tuned_model, n_buckets, type = "abs"){


  quantile_test(tuned_model, n_buckets) %>%
    group_by(quantile) %>%
    summarise(
      prediction = mean(.pred),
      actual = mean(price),
      error = prediction - actual,
      error_pct = (prediction - actual) / actual
    ) %>%
    ggplot(aes(
      x = quantile,
      y = if(type == "abs") {error} else {error_pct},
      fill = factor(sign(error), levels = c(-1, 1),
                    labels = c("Négatif", "Positif")))) +
    geom_col(color = "black", alpha = 0.6) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_fill_manual(values = c("Négatif" = "#DC2A03", "Positif" = "#46D826")) +
    labs(
      y = "Erreur (Prédiction - Réel)",
      fill = ""
    ) +
    theme(legend.position = "none")

}


lift_density <- function(tuned_model, n_buckets, type = "abs"){


  quantile_test(tuned_model, n_buckets) %>%
    mutate(
      prediction = .pred,
      actual = price,
      error = prediction - actual,
      error_pct = (prediction - actual) / actual
    ) %>%
    ggplot(aes(
      x = if(type == "abs") {error} else {error_pct},
      y = as.factor(quantile),
      fill = as.factor(quantile))) +
    stat_binline(color = "black", alpha = 0.6, binwidth = 25000) +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      y = "Quantile",
      x = "Erreur",
      fill = ""
    ) +
    theme(legend.position = "none")

}



dbl_lift_chart <- function(tuned_model1, tuned_model2, model1_name = "Mod 1", model2_name = "Mod 2", n_buckets, subtitle = ""){

  bind_cols(  tuned_model1 %>%
                collect_predictions() %>%
                rename(pred1 = .pred),
              tuned_model2 %>%
                collect_predictions() %>%
                select(.pred) %>%
                rename(pred2 = .pred)) %>%
    arrange(pred1 / pred2) %>%
    mutate(quantile = 1:n() %/% (n() / n_buckets + 1)) %>%
    group_by(quantile) %>%
    summarise(
      pred1 = mean(pred1),
      pred2 = mean(pred2),
      actual = mean(price)
    ) %>%
    pivot_longer(cols = c(pred1, pred2, actual), names_to = "quantity", values_to = "amount") %>%
    ggplot(aes(x = quantile, y = amount, color = quantity)) +
    geom_line() +
    geom_point(alpha = 0.75) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_color_discrete(labels = c("Réel", paste0("Prédiction ", model1_name), paste0("Prédiction ", model2_name))) +
    labs(
      y = "Prédiction",
      color = "",
      title = "Performance sur données de test",
      subtitle = subtitle
    ) +
    theme(legend.position = "top")

}





tabulate_model_metrics <- function(model){

  model %>%
    collect_metrics() %>%
    select(.metric, .estimate) %>%
    mutate(mod = "") %>%
    pivot_wider(id_cols = mod, names_from = .metric, values_from = .estimate) %>%
    select(-mod) %>%
    mutate(
      rmse = label_comma(accuracy = 1)(rmse),
      rsq = label_percent(accuracy = 0.1)(rsq)
    ) %>%
    tableGrob(rows = "", theme = ttheme_default())

}


tabulate_model_metrics_2 <- function(model1, model2, model1_name, model2_name){

  bind_rows(
    model1 %>%
      collect_metrics() %>%
      select(.metric, .estimate) %>%
      mutate(mod = model1_name) %>%
      pivot_wider(id_cols = mod, names_from = .metric, values_from = .estimate),
    model2 %>%
      collect_metrics() %>%
      select(.metric, .estimate) %>%
      mutate(mod = model2_name) %>%
      pivot_wider(id_cols = mod, names_from = .metric, values_from = .estimate)
  ) %>%
    mutate(
      rmse = label_comma(accuracy = 1)(rmse),
      rsq = label_percent(accuracy = 0.1)(rsq)
    ) %>%
    select(-mod) %>%
    tableGrob(rows = c(model1_name, model2_name), theme = ttheme_default())

}


performance_stats <- function(model, n_buckets, subtitle){
  lift <- lift_chart(model, n_buckets = n_buckets, subtitle = subtitle)
  stats <- tabulate_model_metrics(model)


  grid.arrange(lift, stats, ncol = 1, heights = c(4, 1))
}

performance_stats_compare <- function(model1, model2, model1_name, model2_name, n_buckets){
  lift <- dbl_lift_chart(model1, model2, model1_name, model2_name, n_buckets = n_buckets, subtitle = paste(model1_name, model2_name, sep = " vs "))
  stats <- tabulate_model_metrics_2(model1, model2, model1_name, model2_name)


  grid.arrange(lift, stats, ncol = 1, heights = c(4, 1))
}




get_openai_embedding <- function(text){

  resp <- POST(
    "https://api.openai.com/v1/embeddings",
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_KEY"))),
    body = list(
      model = "text-embedding-ada-002",
      input = text
    ),
    encode = "json"
  )

  if(status_code(resp) == 200){

    content(resp, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      pluck("data", "embedding") %>%
      unlist() %>%
      tibble(embedding = .) %>%
      mutate(i = stringr::str_pad(dplyr::row_number(), 4, "left", "0")) %>%
      pivot_wider(values_from = "embedding", names_from = i, names_prefix = "embedding_")


  } else {
    stop("Error on request")
  }

}


