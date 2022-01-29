library(data.table)
library(ggplot2)
library(hexSticker)

types <- c("event", "msg", "error")

n_rows <- 40; n_points <- 60; event_prob <- .75; success_prob <- .2;

seed <- runif(n=1, min=1, max =100000)
set.seed(48396.17)

get_rnd_type <- function() {
  rnd <- rnorm(n=1, mean = 0, sd = 1) / 2
  if(rnd < (-1+event_prob)) {
    "event"
  } else if(rnd>(1-success_prob)) {
    sample(c("success", "error"), size = 1, prob = c(.5, .5))
  } else {
    "msg"
  }
}

values <- matrix(rep(rep(NA, n_points), n_rows),
                 nrow = n_rows)

for (row in 1:nrow(values)) {

  idx <- 0; n_bars <- 0
  bar <- c()

  while (idx < n_points) {
    bar_width <- round(runif(n = 1, min = 3, max = 8))

    if (n_bars == 0) {
      bar_type <- "msg"
    } else {
      bar_type <- get_rnd_type()
    }

    bar <- c(bar, rep(bar_type, bar_width))
    n_bars <- n_bars + 1
    idx <- sum(idx, bar_width + 1)
  }

  spacer <- runif(n=1, min = 10, max = 50)
  bar[1:spacer] <- NA

  padding <- sample(which(!is.na(bar) & bar > 30), 3, replace = F)
  bar[padding] <- NA

  bar[(n_points-runif(n=1, min = 1, max = 10)):n_points] <- "msg"
  values[row,] <- c(rep(NA, times = n_points - length(bar)), bar)
}

values[which(is.na(values))] <- "none"

cnames <- paste0('Type', stringr::str_pad(1:ncol(values), width = 2, pad = 0))
rnames <- paste0('Row', stringr::str_pad(1:nrow(values), width = 2, pad = 0))
dimnames(values) <- list(rnames, cnames)
df_types <- data.table(values, keep.rownames = T)

types_long <- melt(df_types, id.vars = "rn")
types_long[, variable := as.factor(variable)]
types_long[, RowRank := frank(rn)][, ColRank := frank(variable)]

data.table::setorder(types_long, RowRank, -ColRank)

types_long[rn == "Row01",]

log_plot <- ggplot(types_long) +
  geom_bar(aes(x = reorder(rn, RowRank), y = variable, fill = factor(value)),
           stat = "identity") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("event" = "#3A6197", "msg" = "#113671",
                               "success" = "#149158", "none" = '#1D4B8A',
                               "error" = "#07275B"),
                    aesthetics = c("colour", "fill")) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = '#1D4B8A', colour = NA),
        plot.background = element_rect(fill = '#1D4B8A', colour = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null"),
        axis.ticks.length = unit(0, "null"))

#ggsave(here::here("dyn-log/log-plot.png"), plot = log_plot)
#?sticker

sticker(log_plot,
        package="dyn.log",
        p_size=16,
        p_family = "serif",
        p_y = 1.65,
        s_x=1.05,
        s_y=1,

        h_fill="#1D4B8A",
        h_color="#E6A93D",
        s_width=1.4,
        s_height=1.15,
        white_around_sticker = F,
        filename=here::here("dyn-log/sticker.png"))

