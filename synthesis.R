options(pillar.sigfig = 4)
options(dplyr.summarise.inform = FALSE)

library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Worlds and priors
worlds <- c("w0", "w1", "w2+")
P_wp <- function(w0 = 1, w1 = 1, `w2+` = 1) {
  total <- w0 + w1 + `w2+`
  norm_w0 <- w0 / total
  norm_w1 <- w1 / total
  `norm_w2+` <- `w2+` / total
  function(w) {
    case_when(
      w == "w0" ~ norm_w0,
      w == "w1" ~ norm_w1,
      w == "w2+" ~ `norm_w2+`
    )
  }
}

# QuDs and priors
QuDs <- c("Qex", "Qml", "Qfine")
Q_equiv <- function(Q, w) {
  case_when(
    Q == "Qex" & w == "w0" ~ list(c("w0")),
    Q == "Qex" & w == "w1" ~ list(c("w1", "w2+")),
    Q == "Qex" & w == "w2+" ~ list(c("w1", "w2+")),
    Q == "Qml" & w == "w0" ~ list(c("w0", "w1")),
    Q == "Qml" & w == "w1" ~ list(c("w0", "w1")),
    Q == "Qml" & w == "w2+" ~ list(c("w2+")),
    Q == "Qfine" & w == "w0" ~ list(c("w0")),
    Q == "Qfine" & w == "w1" ~ list(c("w1")),
    Q == "Qfine" & w == "w2+" ~ list(c("w2+"))
  )
}
P_Qp <- function(Qex = 1, Qml = 1, Qfine = 1) {
  total <- Qex + Qml + Qfine
  norm_Qex <- Qex / total
  norm_Qml <- Qml / total
  norm_Qfine <- Qfine / total
  name <- function(Q) {
    case_when(
      Q == "Qex" ~ norm_Qex,
      Q == "Qml" ~ norm_Qml,
      Q == "Qfine" ~ norm_Qfine
    )
  }
}

# Messages and costs
# messages <- c("NPsg", "NPpl", "nNPsg", "nNPpl", "!1", "n!1")
messages <- c(
  "NPsg", 
  "NPpl", 
  "nNPsg", 
  "nNPpl", 
  "!1", 
  "n!1",
  "+2",
  "n+2",
  "null"
)
costp <- function(
    NPpl = 0,
    NPsg = 0,
    nNPpl = 1.5,
    nNPsg = 1.5,
    `!1` = 2.5,
    `n!1` = 4,
    `+2` = 2.5,
    `n+2` = 4,
    `null` = -1) {
  function(u) {
    case_when(
      u == "NPpl" ~ NPpl,
      u == "NPsg" ~ NPsg,
      u == "nNPpl" ~ nNPpl,
      u == "nNPsg" ~ nNPsg,
      u == "!1" ~ `!1`,
      u == "n!1" ~ `n!1`,
      u == "+2" ~ `+2`,
      u == "n+2" ~ `n+2`,
      u == "null" ~ `null`
    )
  }
}


# Sub-interpretation function
# Worlds: w0, w1, w2+
iSG_Lit <- function(w) {
  case_when(
    w == "w0" ~ 0,
    TRUE ~ 1
  )
}

inSG_Lit <- function(w) {
  1 - iSG_Lit(w)
}


iSG_Exh <- function(w) {
  case_when(
    w == "w1" ~ 1,
    TRUE ~ 0
  )
}

inSG_Exh <- function(w) {
  1 - iSG_Exh(w)
}

# Note iSG_Lit and inSG_Lit are essentially the same as iPL_Lit and inPL_Lit
iPL_Lit <- function(w) {
  iSG_Lit(w)
}

inPL_Lit <- function(w) {
  1 - iPL_Lit(w)
}

iPL_Exh <- function(w) {
  case_when(
    w == "w2+" ~ 1,
    TRUE ~ 0
  )
}

inPL_Exh <- function(w) {
  1 - iPL_Exh(w)
}

i1 <- function(w) {
  iSG_Exh(w)
}

in1 <- function(w) {
  1 - i1(w)
}

i2 <- function(w) {
  iPL_Exh(w)
}

in2 <- function(w) {
  1 - i2(w)
}

inull <- function(w) {
  1
}

LitExh <- c("Lit", "Exh")
interprs <- setNames(
  list(
    setNames(c(iSG_Lit, iSG_Exh), LitExh),
    setNames(c(iPL_Lit, iPL_Exh), LitExh),
    setNames(c(inSG_Lit, inSG_Exh), LitExh),
    setNames(c(inPL_Lit, inPL_Exh), LitExh)
  ),
  messages[1:4]
)

# Interpretation function: [[u]]_i(w) -> {0, 1}

interpretations <- expand_grid(
  iNPsg = LitExh,
  iNPpl = LitExh,
  inNPsg = LitExh,
  inNPpl = LitExh,
) %>%
  rowwise() %>%
  mutate(
    name = paste0("i", iNPsg, iNPpl, inNPsg, inNPpl),
    inter = {
      inpsg <- iNPsg
      inppl <- iNPpl
      innpsg <- inNPsg
      innppl <- inNPpl
      list(function(u, w) {
        case_when(
          u == "NPsg" ~ interprs[["NPsg"]][[inpsg]](w),
          u == "NPpl" ~ interprs[["NPpl"]][[inppl]](w),
          u == "nNPsg" ~ interprs[["nNPsg"]][[innpsg]](w),
          u == "nNPpl" ~ interprs[["nNPpl"]][[innppl]](w),
          u == "!1" ~ i1(w),
          u == "n!1" ~ in1(w),
          u == "+2" ~ i2(w),
          u == "n+2" ~ in2(w),
          u == "null" ~ inull(w)
        )
      })
    }
  ) %$%
  setNames(inter, name)


inters <- names(interpretations)

interpret <- function(ms, ws, is) {
  pmap_vec(list(ms, ws, is), \(u, w, i) interpretations[[i]](u, w))
}

P_ip <- function(ExhExh = 1, ExhLit = 1, LitLit = 1) {
  total <- ExhExh * 4 + ExhLit * 8 + LitLit * 4
  norm_ExhExh <- ExhExh / total
  norm_ExhLit <- ExhLit / total
  norm_LitLit <- LitLit / total
  function(i) {
    # format: iExhExhExhExh, negative sentence spec is 7-13
    num_exh <- substr(i, 8, 13) %>% str_count("Exh")
    case_when(
      num_exh == 1 ~ norm_ExhLit,
      num_exh == 2 ~ norm_ExhExh,
      num_exh == 0 ~ norm_LitLit
    )
  }
}


# Model functions
L0 <- function() {
  expand_grid(
    world = worlds,
    QuD = QuDs,
    message = messages,
    inter = inters,
  ) %>%
    mutate(prob = P_w(world) * P_Q(QuD) * interpret(message, world, inter)) %>%
    group_by(message, inter) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(message, inter)
}

U1 <- function() {
  L0() %>%
    mutate(Qw = Q_equiv(QuD, world)) %>%
    group_by(inter, QuD, message, Qw) %>%
    mutate(sum_equiv = sum(prob)) %>%
    group_by(world, QuD, message) %>%
    summarise(util = sum(P_i(inter) * (log(sum_equiv) - cost(message)))) %>%
    ungroup()
}


Ln <- function(n) {
  if (n == 0) {
    L0()
  } else {
    Sn(n) %>%
      mutate(prob = P_w(world) * P_Q(QuD) * prob) %>%
      group_by(message) %>%
      mutate(prob = prob / sum(prob)) %>%
      ungroup() %>%
      arrange(message)
  }
}

Un <- function(n) {
  if (n == 1) {
    U1()
  } else {
    Ln(n - 1) %>%
      mutate(Qw = Q_equiv(QuD, world)) %>%
      group_by(QuD, message, Qw) %>%
      mutate(util = log(sum(prob)) - cost(message)) %>%
      ungroup() %>%
      arrange(world, QuD)
  }
}

Sn <- function(n) {
  Un(n) %>%
    transmute(world, QuD, message, prob = exp(lambda * util)) %>%
    group_by(world, QuD) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(world, QuD)
}


library(ggplot2)
custom_theme <- list(
  geom_col(alpha = .7),
  ylim(0, 1),
  theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    )
)

check_Ln <- function(n) {
  Ln(n) %>%
    mutate(`QuD-world` = paste(QuD, world, sep = "-")) %>%
    ggplot(aes(x = `QuD-world`, y = prob, fill = `QuD-world`)) +
    facet_wrap(~message) +
    custom_theme
}

check_Sn <- function(n) {
  Sn(n) %>%
    ggplot(aes(x = message, y = prob, fill = message)) +
    facet_grid(world ~ QuD) +
    custom_theme
}

# check_Un <- function(n) {
#   Un(n) %>% 
#     ggplot(aes(x = message, y = util, fill = message)) +
#     facet_grid(world ~ QuD) +
#     custom_theme
# }

check_Ln_w <- function(n) {
  Ln(n) %>%
    group_by(message, world) %>%
    summarise(Aggworld = sum(prob)) %>%
    group_by(message) %>%
    mutate(Aggworld = Aggworld / sum(Aggworld)) %>%
    ggplot(aes(x = world, y = Aggworld, fill = world)) +
    facet_wrap(~message) +
    custom_theme
}

check_Sn_w <- function(n) {
  Sn(n) %>%
    group_by(message, world) %>%
    summarise(Aggworld = sum(prob)) %>%
    group_by(world) %>%
    mutate(Aggworld = Aggworld / sum(Aggworld)) %>%
    ggplot(aes(x = message, y = Aggworld, fill = message)) +
    facet_grid(~world) +
    custom_theme
}

