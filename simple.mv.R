library(dplyr)

grid <- tidyr::expand_grid(iter = c(1:1000),
                          cor = seq(from = 0, to = 0.9, by = 0.1)
                           )
sim_cor <- purrr::map2(grid$iter,grid$cor, ~ {
  iter <- .x
  n <- 500
  r <- .y
  sd <- c(1, 1, 1, 1)
  mu <- rep(0, 4)

  cor.mat <- diag(1, 4)
  cor.mat[lower.tri(cor.mat)] <- r
  cor.mat[upper.tri(cor.mat)] <- r

  cov <- diag(sd) %*% cor.mat %*% diag(sd)

  all <- MASS::mvrnorm(n, mu, Sigma = cov) %>% as.data.frame() %>% dplyr::mutate(trt = rbinom(n, 1, 0.5))
  y1 <- all%>% dplyr::filter(trt == 0)
  y2 <- all %>% dplyr::filter(trt == 1)

  tsts <- purrr::map(1:4, ~{
   tst <- t.test(y1[,.], y2[,.])
   tibble::tibble(md = mean(y2[,.]) - mean(y1[,.]), p = tst$p.value, sig = p < 0.05,
                  outcome = glue:::glue("out_{.}"))


  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(npos = sum(md >= 0),
                  nsig = sum(sig)) %>%
    tidyr::pivot_wider(names_from = outcome, values_from = c(md, p,sig)) %>%
    dplyr::mutate(iter = iter,
                  cor = r,
                  prop_same = dplyr::case_when(npos == 2 ~ 0.5,
                                               npos < 2 ~ (4-npos)/4,
                                               npos > 2 ~ npos/4))


  lm.mv <- lm(cbind(V1, V2, V3, V4) ~ trt, data = all)
  tt <- summary(manova(lm.mv))


  tsts %>%
    dplyr::mutate(sig_manova = tt$stats[1,6] < 0.05)


  }) %>%
  dplyr::bind_rows()

usethis::use_data(sim_cor, overwrite = TRUE)




