
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


create_cleveland_plot <- function(df, subtitle) {
  ggplot(df, aes(genres, count), height = 500, width = 7) +
    scale_color_discrete(labels = c("bestbewertete Filme", "Top-N Empfehlungen")) +
    coord_flip() +
    geom_line() +
    geom_point(aes(color = list)) +
    theme_minimal() +
    labs(
      title = "Anteil Genres der bestbewerteten Filme im Vergleich zu \nden Top-N Empfehlungen der 20 Nutzer",
      subtitle = subtitle,
      x = element_blank(),
      y = "Anteil in Prozent",
      color = element_blank()
    ) +
    theme(
      text = element_text(size = 12),
      legend.position = 'bottom'
    )
}