# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_epidemio.R — Analyses épidémiologiques complètes                   ║
# ║  RR · OR brut/ajusté · PAF · IC exacts · Tendances · 2×2 complet          ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Tableau 2×2 complet ───────────────────────────────────────────────────────
analyse_2x2 <- function(exposed, outcome, conf_level = 0.95) {
  tbl <- table(exposed, outcome)
  if (ncol(tbl) < 2 || nrow(tbl) < 2) return(NULL)

  # Recodage : exposé=1 en ligne 1, cas=1 en colonne 1
  a <- tbl[2, 2]; b <- tbl[2, 1]
  c <- tbl[1, 2]; d <- tbl[1, 1]
  n <- a + b + c + d

  # Risque relatif (RR)
  rr_val  <- (a / (a + b)) / (c / (c + d))
  se_lnrr <- sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))
  z       <- qnorm(1 - (1 - conf_level) / 2)
  rr_lo   <- exp(log(rr_val) - z * se_lnrr)
  rr_hi   <- exp(log(rr_val) + z * se_lnrr)

  # Odds Ratio (OR) — méthode exacte de Woolf
  or_val  <- (a * d) / (b * c)
  se_lnor <- sqrt(1/a + 1/b + 1/c + 1/d)
  or_lo   <- exp(log(or_val) - z * se_lnor)
  or_hi   <- exp(log(or_val) + z * se_lnor)

  # Fraction Attribuable dans la Population (FAP / PAF)
  p_exp   <- (a + b) / n
  paf     <- p_exp * (rr_val - 1) / rr_val
  paf_lo  <- p_exp * (rr_lo - 1) / rr_lo
  paf_hi  <- p_exp * (rr_hi - 1) / rr_hi

  # Fraction Attribuable chez les Exposés (FAE)
  fae     <- (rr_val - 1) / rr_val

  # Risque absolu & Réduction du risque
  r_exp   <- a / (a + b)
  r_nexp  <- c / (c + d)
  rar     <- r_exp - r_nexp           # Risque Attribuable (différence)
  nnt     <- if (rar != 0) round(1 / abs(rar), 1) else Inf  # NNT

  # IC Wilson pour chaque prévalence
  ic_exp  <- binom.confint(a, a + b,  methods = "wilson")
  ic_nexp <- binom.confint(c, c + d, methods = "wilson")

  # Test Chi² et Fisher
  chi2  <- chisq.test(tbl, correct = FALSE)
  fish  <- fisher.test(tbl)

  list(
    tbl      = tbl,
    a=a, b=b, c=c, d=d, n=n,
    r_exp    = r_exp,   ic_exp_lo  = ic_exp$lower,  ic_exp_hi  = ic_exp$upper,
    r_nexp   = r_nexp,  ic_nexp_lo = ic_nexp$lower, ic_nexp_hi = ic_nexp$upper,
    rr = rr_val, rr_lo = rr_lo, rr_hi = rr_hi,
    or = or_val, or_lo = or_lo, or_hi = or_hi,
    rar = rar,   nnt = nnt,
    paf = paf,   paf_lo = paf_lo, paf_hi = paf_hi,
    fae = fae,
    chi2_p = chi2$p.value, chi2_stat = chi2$statistic,
    fish_p = fish$p.value,
    conf_level = conf_level
  )
}

# ── Rendu HTML du tableau 2×2 ────────────────────────────────────────────────
render_2x2_html <- function(res, exp_name = "Exposé", out_name = "Cas") {
  if (is.null(res)) return(tags$p("Tableau 2×2 non applicable"))
  sig_chi2 <- res$chi2_p < 0.05
  col_sig  <- if (sig_chi2) "#c9161d" else "#4dac26"

  tags$div(
    # Hypothèses
    tags$div(
      style = "background:#f0f8ff;border-left:4px solid #008DC9;padding:10px 14px;
               border-radius:0 8px 8px 0;margin-bottom:12px;",
      tags$b(style="color:#00426A;", "Hypothèses :"), tags$br(),
      tags$span(style="font-size:12px;",
        "H₀ : Pas d'association entre l'exposition et l'outcome (RR = 1, OR = 1)"), tags$br(),
      tags$span(style="font-size:12px;",
        "H₁ : Association significative (RR ≠ 1 ou OR ≠ 1)")
    ),

    # Tableau 2×2
    tags$div(
      style = "overflow-x:auto;margin-bottom:14px;",
      tags$table(
        style = "border-collapse:collapse;font-size:13px;width:100%;",
        tags$thead(tags$tr(
          tags$th(style="padding:8px;background:#008DC9;color:white;",""),
          tags$th(style="padding:8px;background:#008DC9;color:white;",paste(out_name,"= Oui")),
          tags$th(style="padding:8px;background:#008DC9;color:white;",paste(out_name,"= Non")),
          tags$th(style="padding:8px;background:#005A8B;color:white;","Total"),
          tags$th(style="padding:8px;background:#005A8B;color:white;",paste("Risque",out_name))
        )),
        tags$tbody(
          tags$tr(
            tags$td(style="padding:7px;font-weight:700;background:#e3f2fd;",paste(exp_name,"= Oui")),
            tags$td(style="padding:7px;text-align:center;font-weight:700;", res$a),
            tags$td(style="padding:7px;text-align:center;", res$b),
            tags$td(style="padding:7px;text-align:center;font-weight:600;", res$a + res$b),
            tags$td(style="padding:7px;text-align:center;color:#c9161d;font-weight:700;",
                    paste0(round(res$r_exp * 100, 1), "%"))
          ),
          tags$tr(style="background:#f5f5f5;",
            tags$td(style="padding:7px;font-weight:700;",paste(exp_name,"= Non")),
            tags$td(style="padding:7px;text-align:center;", res$c),
            tags$td(style="padding:7px;text-align:center;", res$d),
            tags$td(style="padding:7px;text-align:center;font-weight:600;", res$c + res$d),
            tags$td(style="padding:7px;text-align:center;color:#4dac26;font-weight:700;",
                    paste0(round(res$r_nexp * 100, 1), "%"))
          ),
          tags$tr(
            tags$td(style="padding:7px;font-weight:700;background:#e8f5e9;","Total"),
            tags$td(style="padding:7px;text-align:center;font-weight:700;",res$a + res$c),
            tags$td(style="padding:7px;text-align:center;font-weight:700;",res$b + res$d),
            tags$td(style="padding:7px;text-align:center;font-weight:700;background:#e8f5e9;",res$n),
            tags$td(style="padding:7px;")
          )
        )
      )
    ),

    # Mesures d'association
    fluidRow(
      column(4, tags$div(
        style="background:white;border:0.5px solid #e0e0e0;border-radius:8px;padding:12px;text-align:center;",
        tags$div(style="font-size:11px;font-weight:700;color:#888;text-transform:uppercase;","Risque Relatif (RR)"),
        tags$div(style=paste0("font-size:26px;font-weight:700;color:",if(res$rr>1)"#c9161d" else "#4dac26",";"),
                 round(res$rr, 2)),
        tags$div(style="font-size:11px;color:#555;",
                 paste0("IC", round(res$conf_level*100), "% [",
                        round(res$rr_lo,2)," – ", round(res$rr_hi,2),"]")),
        tags$div(style="font-size:11px;margin-top:4px;color:#777;",
                 if(res$rr > 1) paste0("Facteur de risque (RR=",round(res$rr,2),">1)")
                 else if(res$rr < 1) "Facteur protecteur (RR<1)"
                 else "Pas d'effet (RR=1)")
      )),
      column(4, tags$div(
        style="background:white;border:0.5px solid #e0e0e0;border-radius:8px;padding:12px;text-align:center;",
        tags$div(style="font-size:11px;font-weight:700;color:#888;text-transform:uppercase;","Odds Ratio (OR)"),
        tags$div(style=paste0("font-size:26px;font-weight:700;color:",if(res$or>1)"#c9161d" else "#4dac26",";"),
                 round(res$or, 2)),
        tags$div(style="font-size:11px;color:#555;",
                 paste0("IC", round(res$conf_level*100), "% [",
                        round(res$or_lo,2)," – ", round(res$or_hi,2),"]")),
        tags$div(style="font-size:11px;margin-top:4px;color:#777;",
                 if(abs(res$or-1) < 0.01) "OR ≈ 1 — Pas d'association"
                 else paste0("Différence OR-RR : ", round(abs(res$or - res$rr),3)))
      )),
      column(4, tags$div(
        style="background:white;border:0.5px solid #e0e0e0;border-radius:8px;padding:12px;text-align:center;",
        tags$div(style="font-size:11px;font-weight:700;color:#888;text-transform:uppercase;","Fract. Attrib. Pop. (PAF)"),
        tags$div(style="font-size:26px;font-weight:700;color:#7b2d8b;",
                 paste0(round(res$paf * 100, 1), "%")),
        tags$div(style="font-size:11px;color:#555;",
                 paste0("IC", round(res$conf_level*100), "% [",
                        round(res$paf_lo*100,1),"% – ", round(res$paf_hi*100,1),"%]")),
        tags$div(style="font-size:11px;margin-top:4px;color:#777;",
                 paste0("FAE = ", round(res$fae * 100, 1), "% | NNT = ", res$nnt))
      ))
    ),

    br(),

    # Mesures additionnelles
    fluidRow(
      column(6, tags$div(
        style="background:#fff3e0;border-left:3px solid #e88400;padding:10px 14px;border-radius:0 8px 8px 0;",
        tags$b("Risque Attribuable (RAR) :"), paste0(" ", round(res$rar*100,1), " points de % "),
        tags$span(style="color:#777;font-size:12px;",
                  paste0("(Différence absolue de risque entre exposés et non-exposés)"))
      )),
      column(6, tags$div(
        style=paste0("background:",if(sig_chi2)"#ffebee" else "#f1f8e9",
                     ";border-left:3px solid ",col_sig,";padding:10px 14px;border-radius:0 8px 8px 0;"),
        tags$b(style=paste0("color:",col_sig,";"),
               paste0("Chi² p=", round(res$chi2_p,4), " | Fisher p=", round(res$fish_p,4))),
        tags$br(),
        tags$span(style="font-size:12px;",
                  if(sig_chi2) "→ Association statistiquement significative (p<0.05)"
                  else "→ Pas d'association significative (p≥0.05)")
      ))
    )
  )
}

# ── Test de tendance de Cochran-Armitage ──────────────────────────────────────
cochran_armitage_test <- function(d, var_ord, outcome) {
  tryCatch({
    tbl   <- table(d[[var_ord]], d[[outcome]])
    scores <- seq_len(nrow(tbl))
    n_row  <- rowSums(tbl)
    p_row  <- tbl[, 2] / n_row
    N      <- sum(tbl)
    p_bar  <- sum(tbl[, 2]) / N
    S      <- sum(scores * n_row * p_row)
    E_S    <- p_bar * sum(scores * n_row)
    V_S    <- p_bar * (1 - p_bar) * (sum(scores^2 * n_row) - sum(scores * n_row)^2 / N)
    Z      <- (S - E_S) / sqrt(V_S)
    p_val  <- 2 * pnorm(-abs(Z))
    list(Z = round(Z, 4), p = round(p_val, 4),
         interpretation = if(p_val < 0.05)
           paste0("Tendance linéaire significative (Z=",round(Z,3)," p=",round(p_val,4),")")
         else paste0("Pas de tendance linéaire significative (p=",round(p_val,4),")"))
  }, error = function(e) list(Z=NA, p=NA, interpretation=paste("Erreur:", e$message)))
}

# ── Analyse multi-expositions ─────────────────────────────────────────────────
multi_exposition <- function(d, expositions, outcome, conf_level = 0.95) {
  map_dfr(expositions, function(exp_var) {
    tryCatch({
      res <- analyse_2x2(d[[exp_var]], d[[outcome]], conf_level)
      if (is.null(res)) return(NULL)
      tibble(
        Exposition    = exp_var,
        N             = res$n,
        `Cas exposés` = res$a,
        `% exposés`   = round(res$r_exp  * 100, 1),
        `% non exp.`  = round(res$r_nexp * 100, 1),
        RR            = round(res$rr, 3),
        `IC95 RR`     = paste0("[", round(res$rr_lo,3), "–", round(res$rr_hi,3), "]"),
        OR            = round(res$or, 3),
        `IC95 OR`     = paste0("[", round(res$or_lo,3), "–", round(res$or_hi,3), "]"),
        `PAF (%)`     = round(res$paf * 100, 1),
        `p Chi²`      = round(res$chi2_p, 4),
        Sig           = case_when(res$chi2_p < .001 ~ "***",
                                   res$chi2_p < .01  ~ "**",
                                   res$chi2_p < .05  ~ "*",
                                   res$chi2_p < .10  ~ ".",
                                   TRUE ~ "NS")
      )
    }, error = function(e) NULL)
  })
}

cat("module_epidemio.R chargé\n")
