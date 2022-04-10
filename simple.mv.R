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

usethis::use_data(sim_cor)



cond_sim <- sim_cor %>%
  dplyr::group_by(cor) %>%
  dplyr::filter(nsig > 0) %>%
                  dplyr::summarise(nig_ifany = mean(nsig))




data.frame(
y1 = rnorm(1000) >0,
y2 = rnorm(1000) >0,
y3 = rnorm(1000) >0,
y4 = rnorm(1000) >0) %>%
  dplyr::mutate(same = y1+y2+y3+y4) %>%
  dplyr::summarise(mean(same))

y1 %>%
  dplyr::mutate_all(~ .x > 0) %>%
  dplyr::mutate(same = V1+V2+V3+V4) %>%
  dplyr::summarise(mean(same))


y2 %>%
  dplyr::mutate_all(~ .x > 0) %>%
  dplyr::mutate(same = V1+V2+V3+V4) %>%
  dplyr::summarise(mean(same))



sim <- function() purrr::map_dbl(1:1000, ~ {



})



purrr::map_dbl(1:100, ~{
  all <- MASS::mvrnorm(500, rep(0, 4), Sigma = diag(1, nrow = 4)) %>%
    as.data.frame() %>%
    dplyr::mutate(trt = rbinom(500, 1, 0.5))

  t <- all %>%
    tidyr::pivot_longer(cols = -trt) %>%
    dplyr::group_by(trt, name) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::arrange(name,trt) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise_all(~ dplyr::last(.x) - dplyr::first(.x)) %>%
    dplyr::mutate(pos = mean >= 0) %>%
    dplyr::summarise(pos = sum(pos)) %>%
    dplyr::pull()

  t
}) %>% mean()



purrr::map_dbl(1:100, ~ {
  data.frame(y1 = mean(rnorm(500)) - mean(rnorm(500)),
             y2 = mean(rnorm(500)) - mean(rnorm(500)),
             y3 = mean(rnorm(500)) - mean(rnorm(500)),
             y4 = mean(rnorm(500)) - mean(rnorm(500))) %>%
    dplyr::mutate_all(~ . >= 0) %>%
    dplyr::mutate(sum = y1 + y2 + y3 + y4) %>%
    dplyr::pull()

}) %>% mean()

