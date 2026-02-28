#' Generate dummy ADPPK data (SAD/MAD)
#'
#' @param study_type Study type: "SAD" or "MAD".
#' @param n_subj Number of subjects.
#' @param seed Random seed.
#' @param inject_missing_dose Logical; inject subjects with PK but no dose records.
#' @param inject_non_poppk Logical; inject non-POPPK subjects/records.
#' @param inject_char_num_mismatch Logical; inject race/racen mismatch.
#'
#' @return A list with `adppk` and `addose` data frames.
#' @export
generate_dummy_adppk <- function(
    study_type = c("SAD", "MAD"),
    n_subj = 40,
    seed = 123,
    inject_missing_dose = FALSE,
    inject_non_poppk = FALSE,
    inject_char_num_mismatch = FALSE
) {
  study_type <- match.arg(study_type)
  set.seed(seed)

  sid <- sprintf("%03d", seq_len(n_subj))
  usubjid <- paste0("STDY01-", sid)
  arm <- ifelse(study_type == "SAD", sample(c("LOW", "HIGH"), n_subj, TRUE),
                sample(c("MAD_LOW", "MAD_HIGH"), n_subj, TRUE))
  sex <- sample(c("M", "F"), n_subj, TRUE)
  race <- sample(c("ASIAN", "WHITE", "BLACK"), n_subj, TRUE)
  racen <- c(ASIAN = 1, WHITE = 2, BLACK = 3)[race]
  poppkfl <- ifelse(runif(n_subj) > 0.1, "Y", "N")

  time_pts <- if (study_type == "SAD") c(0, 0.5, 1, 2, 4, 8, 12, 24) else c(0, 1, 2, 4, 8, 12)
  day_vec <- if (study_type == "SAD") 1 else rep(1:5, each = length(time_pts))
  atpt <- if (study_type == "SAD") sprintf("T%02g", time_pts) else paste0("D", day_vec, "_T", sprintf("%02g", rep(time_pts, if (study_type == "MAD") 5 else 1)))

  rec_list <- vector("list", n_subj)
  for (i in seq_len(n_subj)) {
    tm <- if (study_type == "SAD") time_pts else rep(time_pts, 5)
    ady <- if (study_type == "SAD") rep(1, length(tm)) else rep(1:5, each = length(time_pts))
    c0 <- ifelse(arm[i] %in% c("HIGH", "MAD_HIGH"), 120, 80)
    aval <- pmax(0, c0 * exp(-0.25 * tm) + rnorm(length(tm), 0, 3))

    rec_list[[i]] <- data.frame(
      STUDYID = "STDY01",
      USUBJID = usubjid[i],
      SUBJID = sid[i],
      ARM = arm[i],
      ACTARM = arm[i],
      TRT01P = arm[i],
      TRT01A = arm[i],
      VISIT = paste0("DAY ", ady),
      VISITNUM = ady,
      ADY = ady,
      ATPT = atpt[seq_along(tm)],
      ATPTN = seq_along(tm),
      PARAMCD = "CONC",
      PARAM = "Plasma Concentration",
      AVAL = round(aval, 3),
      AVALU = "ng/mL",
      LLOQ = 1,
      BLQFL = ifelse(aval < 1, "Y", "N"),
      EXTRT = "EXTRAVASCULAR",
      ROUTE = "ORAL",
      DOSE = ifelse(arm[i] %in% c("HIGH", "MAD_HIGH"), 100, 50),
      DOSEU = "mg",
      POPPKFL = poppkfl[i],
      SAFFL = "Y",
      SEX = sex[i],
      RACE = race[i],
      RACEN = as.integer(racen[i]),
      AGE = sample(18:70, 1),
      WT = round(rnorm(1, 70, 12), 1),
      HT = round(rnorm(1, 170, 10), 1),
      stringsAsFactors = FALSE
    )
  }

  adppk <- do.call(rbind, rec_list)

  addose <- unique(adppk[, c("STUDYID", "USUBJID", "SUBJID", "DOSE", "DOSEU", "ROUTE", "ADY")])
  names(addose)[names(addose) == "ADY"] <- "DOSEDY"

  if (inject_missing_dose) {
    miss_subj <- sample(unique(adppk$USUBJID), max(1, floor(n_subj * 0.05)))
    addose <- addose[!(addose$USUBJID %in% miss_subj), , drop = FALSE]
  }

  if (inject_non_poppk) {
    bad <- sample(unique(adppk$USUBJID), max(1, floor(n_subj * 0.1)))
    adppk$POPPKFL[adppk$USUBJID %in% bad] <- "N"
  }

  if (inject_char_num_mismatch) {
    ix <- sample(seq_len(nrow(adppk)), max(1, floor(0.02 * nrow(adppk))))
    adppk$RACEN[ix] <- adppk$RACEN[ix] + 9L
  }

  list(adppk = adppk, addose = addose)
}
