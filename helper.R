






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
  levelplot(A, xlab="items", ylab="items", main=title, col.regions=colorRampPalette(c("white", "black")))
  #}
}

