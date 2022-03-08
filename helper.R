
print_dim <- function(matrix) {
  cat('Dimension: (',dim(matrix), ')\n')
}

# ------------------- Ähnlichkeit von Nutzern und Filmen ---------------------------


calc_cos_similarity_twomtrx <- function(A, B) {
  zae <- A %*% t(B)
  nen1 <- A %*% t(A)
  nen2 <- B %*% t(B)
  nen <- sqrt(diag(nen1) %*% t(diag(nen2)))

  similarity <- zae / nen
  return(similarity)
}

plot_sim <- function(A, title)
{
  rownames(A) <- c()
  colnames(A) <- c()
  #if(min(A) < 0)
  #{
  #  levelplot(A, xlab="items", ylab="items", main="IBCF similarity visualization", col.regions=colorRampPalette(c("red", "white", "black")))
  #}
  #else
  #{
  levelplot(A, xlab="movies", ylab="users", main=title, col.regions=colorRampPalette(c("white", "black")))
  #}
}


plot_similarity <- function(df, sub_title) {
  ggplot(df) +
    geom_density(aes(x = value), fill = 'steelblue') +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(
      title = "Verteilung der Cosine-Similarity zwischen User und Filmen",
      subtitle = sub_title,
      x = "Cosine-Similarity", 
      y = "Dichte",
      fill = element_blank()
    ) +
    theme_classic() +
    theme(
      text = element_text(size = 12)
    )
}


# ------------------- cleveland dot plot ---------------------------


create_cleveland_plot <- function(rating_matrix, genres, n, our_user) {
  
  recommendation <- get_topn_recos(rating_matrix, n)
  recommendation <- recommendation %>% filter(user == our_user)
  
  genres_df <- cbind(item = rownames(genres),genres)
  
  # bring recommendations into the right form
  recommendation <- inner_join(recommendation, genres_df, by = 'item')
  recommendation <- pivot_longer(recommendation, cols = c('unknown': 'Western'), names_to = 'genre', values_to = 'genre_value')
  recommendation <- recommendation %>% group_by(user, genre) %>% summarise(recommendation = sum(genre_value))
  recommendation <- inner_join(recommendation, recommendation %>% group_by(user) %>% summarize(total = sum(recommendation)), by = 'user')
  recommendation <- recommendation %>% mutate(recommendation = (recommendation / total) * 100)
  
  # bring the actually seen movies into the right form
  movies <- as(MovieLense, "data.frame")
  movies <- movies %>% filter(user == our_user)
  movies <- inner_join(movies, genres_df, by = 'item')
  movies <- pivot_longer(movies, cols = c('unknown': 'Western'), names_to = 'genre', values_to = 'genre_value')
  movies <- movies %>% group_by(user, genre) %>% summarise(actual = sum(genre_value))
  movies <- inner_join(movies, movies %>% group_by(user) %>% summarize(total = sum(actual)), by = 'user')
  movies <- movies %>% mutate(actual = (actual / total) * 100)
  
  # join the two datasets
  reco_df <- inner_join(movies, recommendation, by = c('user', 'genre'))
  reco_df <- reco_df %>% mutate(act = actual)
  reco_df <- pivot_longer(reco_df, cols = c('actual', 'recommendation'), names_to = 'type', values_to = 'value')
  
  reco_df <- reco_df %>% filter(user == our_user)
  
  ggplot(reco_df, aes(reorder(genre, act), value), height = 500, width = 7) +
    scale_color_discrete(labels = c("bewertete Filme", "Top-20 Empfehlungen")) +
    coord_flip() +
    geom_line() +
    geom_point(aes(color = type)) +
    theme_minimal() +
    labs(
      title = glue('Top-{n} Empfehlungen für Nutzer {our_user}'),
      x = element_blank(),
      y = "Anteil in Prozent",
      color = element_blank()
    ) +
    theme(
      legend.position = 'bottom'
    )
}