#' Generate dummy SDTM source domains and derived ADPPK
#'
#' This generator creates 3 source domains (DM, EX, PC) and derives ADPPK-like
#' analysis data from them.
#'
#' @param study_type Study type: "SAD" or "MAD".
#' @param n_subj Number of subjects.
#' @param period_n Number of periods (for multi-period studies).
#' @param seed Random seed.
#' @param inject_test_issues Logical; inject a small set of realistic issues for QC testing.
#' @param issue_level Issue injection level: "low", "medium", or "high".
#' @param inject_missing_dose Logical; inject subjects with PK but no dose records.
#' @param inject_non_poppk Logical; inject non-POPPK subjects/records.
#' @param inject_char_num_mismatch Logical; inject race/racen mismatch.
#'
#' @return A list with `dm`, `ex`, `pc`, `adppk`, and `addose` data frames.
#' @export
generate_dummy_pk_package <- function(
    study_type = c("SAD", "MAD"),
    n_subj = 40,
    period_n = 1,
    seed = 123,
    inject_test_issues = FALSE,
    issue_level = c("low", "medium", "high"),
    inject_missing_dose = FALSE,
    inject_non_poppk = FALSE,
    inject_char_num_mismatch = FALSE
) {
  study_type <- match.arg(study_type)
  issue_level <- match.arg(issue_level)
  set.seed(seed)

  # --- DM -----------------------------------------------------------------
  sid <- sprintf("%03d", seq_len(n_subj))
  usubjid <- paste0("STDY01-", sid)
  arm <- if (study_type == "SAD") sample(c("LOW", "HIGH"), n_subj, TRUE) else sample(c("MAD_LOW", "MAD_HIGH"), n_subj, TRUE)
  armcd <- ifelse(grepl("HIGH", arm), "H", "L")
  sex <- sample(c("M", "F"), n_subj, TRUE)
  race <- sample(c("ASIAN", "WHITE", "BLACK"), n_subj, TRUE)
  racen <- c(ASIAN = 1, WHITE = 2, BLACK = 3)[race]
  age <- sample(18:75, n_subj, TRUE)

  dm <- data.frame(
    STUDYID = "STDY01",
    USUBJID = usubjid,
    SUBJID = sid,
    ARMCD = armcd,
    ARM = arm,
    SEX = sex,
    RACE = race,
    RACEN = as.integer(racen),
    AGE = age,
    SAFFL = "Y",
    POPPKFL = "Y",
    stringsAsFactors = FALSE
  )

  # --- EX -----------------------------------------------------------------
  base_dt <- as.POSIXct("2026-01-01 08:00:00", tz = "UTC")
  n_days <- if (study_type == "SAD") 1 else 5
  period_n <- max(1, as.integer(period_n))
  ex_list <- vector("list", n_subj)
  for (i in seq_len(n_subj)) {
    dose_amt <- ifelse(grepl("HIGH", dm$ARM[i]), 100, 50)
    one_subj_ex <- list()
    exseq <- 1
    for (p in seq_len(period_n)) {
      days <- seq_len(n_days)
      period_offset <- (p - 1) * (n_days + 2) * 24 * 3600
      exdt <- base_dt + period_offset + (days - 1) * 24 * 3600
      one_subj_ex[[p]] <- data.frame(
        STUDYID = dm$STUDYID[i],
        USUBJID = dm$USUBJID[i],
        EXSEQ = seq(exseq, length.out = length(days)),
        APERIOD = p,
        EXTRT = "TEST DRUG",
        EXDOSE = dose_amt,
        EXDOSU = "mg",
        EXROUTE = "ORAL",
        EXSTDTC = format(exdt, "%Y-%m-%dT%H:%M:%SZ"),
        stringsAsFactors = FALSE
      )
      exseq <- exseq + length(days)
    }
    ex_list[[i]] <- do.call(rbind, one_subj_ex)
  }
  ex <- do.call(rbind, ex_list)

  # --- PC -----------------------------------------------------------------
  # Include predose and post-dose nominal sample times (hours)
  nominal_times <- c(-0.5, 0, 0.5, 1, 2, 4, 8, 12, 24)
  pc_list <- vector("list", n_subj)
  for (i in seq_len(n_subj)) {
    dose_rows <- ex[ex$USUBJID == dm$USUBJID[i], , drop = FALSE]
    dts <- as.POSIXct(dose_rows$EXSTDTC, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    one_subj <- list()
    seq_id <- 1

    for (d in seq_along(dts)) {
      tm <- nominal_times
      # actual clock times include realistic jitter around nominal
      jitter_min <- stats::rnorm(length(tm), mean = 0, sd = 6)
      abs_dt <- dts[d] + (tm * 60 + jitter_min) * 60

      c0 <- ifelse(grepl("HIGH", dm$ARM[i]), 180, 110)
      # Slight accumulation in MAD by day
      day_factor <- ifelse(study_type == "MAD", 1 + (d - 1) * 0.06, 1)
      conc <- pmax(0, (c0 * day_factor) * exp(-0.19 * pmax(tm, 0)) + stats::rnorm(length(tm), 0, 6))
      # predose values lower but not always zero
      conc[tm < 0] <- pmax(0, conc[tm < 0] * 0.2)

      lloq <- 2
      blq <- conc < lloq
      # inject some additional realistic BLQ at low tail only
      low_tail <- conc < stats::quantile(conc, probs = 0.2, na.rm = TRUE)
      blq <- blq | (low_tail & stats::runif(length(conc)) < 0.08)
      pcstat <- ifelse(blq, "BLQ", NA)

      one_subj[[d]] <- data.frame(
        STUDYID = dm$STUDYID[i],
        USUBJID = dm$USUBJID[i],
        PCSEQ = seq(seq_id, length.out = length(tm)),
        PCTPT = paste0("T", tm, "h"),
        PCTPTNUM = tm,
        PCDTC = format(abs_dt, "%Y-%m-%dT%H:%M:%SZ"),
        PCSTRESN = round(conc, 3),
        PCSTRESU = "ng/mL",
        PCSTAT = pcstat,
        PCDAY = d,
        stringsAsFactors = FALSE
      )
      seq_id <- seq_id + length(tm)
    }
    pc_list[[i]] <- do.call(rbind, one_subj)
  }
  pc <- do.call(rbind, pc_list)

  # --- Derive ADPPK -------------------------------------------------------
  # relative time from first dose (hours)
  first_dose <- stats::aggregate(EXSTDTC ~ USUBJID, ex, min)
  names(first_dose)[2] <- "FDTC"

  # Dosing records (EVID = 1)
  ad_dose <- merge(ex, first_dose, by = "USUBJID", all.x = TRUE)
  ad_dose$DTM <- as.POSIXct(ad_dose$EXSTDTC, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  ad_dose$FDTM <- as.POSIXct(ad_dose$FDTC, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  ad_dose$TIME <- as.numeric(difftime(ad_dose$DTM, ad_dose$FDTM, units = "hours"))
  ad_dose$NTIME <- ad_dose$TIME
  ad_dose$EVID <- 1
  ad_dose$MDV <- 1
  ad_dose$DV <- NA_real_
  ad_dose$AMT <- ad_dose$EXDOSE
  ad_dose$CMT <- 1
  ad_dose$PARAMCD <- "DOSE"
  ad_dose$PARAM <- "Dose Amount"
  ad_dose$AVAL <- ad_dose$AMT
  ad_dose$AVALU <- ad_dose$EXDOSU

  # PK records (EVID = 0)
  ad_pk <- merge(pc, first_dose, by = "USUBJID", all.x = TRUE)
  ad_pk$DTM <- as.POSIXct(ad_pk$PCDTC, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  ad_pk$FDTM <- as.POSIXct(ad_pk$FDTC, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  ad_pk$TIME <- as.numeric(difftime(ad_pk$DTM, ad_pk$FDTM, units = "hours"))
  ad_pk$NTIME <- ad_pk$PCTPTNUM
  ad_pk$EVID <- 0
  ad_pk$MDV <- ifelse(is.na(ad_pk$PCSTRESN), 1, 0)
  ad_pk$DV <- ad_pk$PCSTRESN
  ad_pk$AMT <- 0
  ad_pk$CMT <- 2
  ad_pk$PARAMCD <- "CONC"
  ad_pk$PARAM <- "Plasma Concentration"
  ad_pk$AVAL <- ad_pk$DV
  ad_pk$AVALU <- ad_pk$PCSTRESU

  # P2-9: look up ARM directly from DM rather than relying on merge-suffix logic
  arm_lookup <- stats::setNames(dm$ARM, dm$USUBJID)

  keep_dose <- data.frame(
    STUDYID = ad_dose$STUDYID,
    USUBJID = ad_dose$USUBJID,
    SUBJID = sub("STDY01-", "", ad_dose$USUBJID),
    ARM = arm_lookup[ad_dose$USUBJID], ACTARM = arm_lookup[ad_dose$USUBJID],
    TRT01P = arm_lookup[ad_dose$USUBJID], TRT01A = arm_lookup[ad_dose$USUBJID],
    VISIT = paste0("DAY ", floor(ad_dose$TIME / 24) + 1),
    VISITNUM = floor(ad_dose$TIME / 24) + 1,
    ADY = floor(ad_dose$TIME / 24) + 1,
    APERIOD = ad_dose$APERIOD,
    OCC = floor(ad_dose$TIME / 24) + 1,
    ATM = format(ad_dose$DTM, "%H:%M"),
    ATPT = "DOSE", ATPTN = NA_real_,
    DVID = "DOSE", DVIDN = 0,
    PARAMCD = ad_dose$PARAMCD, PARAM = ad_dose$PARAM,
    AVAL = ad_dose$AVAL, AVALU = ad_dose$AVALU,
    DV = ad_dose$DV, PCSTRESC = NA_character_,
    LLOQ = NA_real_, ALLOQ = NA_real_, AULOQ = NA_real_, BLQFL = NA_character_, BLQFN = NA_integer_, ALQFL = NA_character_, ALQFN = NA_integer_,
    EXTRT = ad_dose$EXTRT, ROUTE = ad_dose$EXROUTE, ROUTEN = 1,
    DOSE = ad_dose$EXDOSE, DOSEU = ad_dose$EXDOSU, DOSEA = ad_dose$EXDOSE, DOSEP = ad_dose$EXDOSE,
    EVID = ad_dose$EVID, MDV = ad_dose$MDV, AMT = ad_dose$AMT, CMT = ad_dose$CMT,
    TIME = ad_dose$TIME, NTIME = ad_dose$NTIME,
    II = ifelse(study_type == "MAD", 24, 0), ADDL = ifelse(study_type == "MAD", n_days - 1, 0), SS = 0,
    FORM = "TABLET", FORMN = 1,
    TRTP = ad_dose$EXTRT, TRTPN = 1,
    stringsAsFactors = FALSE
  )

  keep_pk <- data.frame(
    STUDYID = ad_pk$STUDYID,
    USUBJID = ad_pk$USUBJID,
    SUBJID = sub("STDY01-", "", ad_pk$USUBJID),
    ARM = arm_lookup[ad_pk$USUBJID], ACTARM = arm_lookup[ad_pk$USUBJID],
    TRT01P = arm_lookup[ad_pk$USUBJID], TRT01A = arm_lookup[ad_pk$USUBJID],
    VISIT = paste0("DAY ", floor(ad_pk$TIME / 24) + 1),
    VISITNUM = floor(ad_pk$TIME / 24) + 1,
    ADY = floor(ad_pk$TIME / 24) + 1,
    APERIOD = floor((ad_pk$TIME) / ((n_days + 2) * 24)) + 1,
    OCC = ad_pk$PCDAY,
    ATM = format(ad_pk$DTM, "%H:%M"),
    ATPT = ad_pk$PCTPT, ATPTN = ad_pk$PCTPTNUM,
    DVID = "DRUG (ng/mL)", DVIDN = 1,
    PARAMCD = ad_pk$PARAMCD, PARAM = ad_pk$PARAM,
    AVAL = ad_pk$AVAL, AVALU = ad_pk$AVALU,
    DV = ad_pk$DV, PCSTRESC = as.character(ad_pk$DV),
    LLOQ = 2, ALLOQ = 2, AULOQ = 10000,
    BLQFL = ifelse(toupper(as.character(ad_pk$PCSTAT)) == "BLQ" | ad_pk$AVAL < 2, "Y", "N"),
    BLQFN = ifelse(toupper(as.character(ad_pk$PCSTAT)) == "BLQ" | ad_pk$AVAL < 2, 1L, 0L),
    ALQFL = "N", ALQFN = 0L,
    EXTRT = "TEST DRUG", ROUTE = "ORAL", ROUTEN = 1,
    DOSE = NA_real_, DOSEU = "mg", DOSEA = NA_real_, DOSEP = NA_real_,
    EVID = ad_pk$EVID, MDV = ad_pk$MDV, AMT = ad_pk$AMT,
    TIME = ad_pk$TIME, NTIME = ad_pk$NTIME, CMT = ad_pk$CMT,
    II = ifelse(study_type == "MAD", 24, 0), ADDL = ifelse(study_type == "MAD", n_days - 1, 0), SS = 0,
    FORM = "TABLET", FORMN = 1,
    TRTP = "TEST DRUG", TRTPN = 1,
    stringsAsFactors = FALSE
  )

  adppk <- rbind(keep_dose, keep_pk)
  # P2-9: ARM is now set directly from DM lookup above; only merge the remaining
  # DM columns that are not yet in the combined frame.
  dm_extra <- dm[, c("USUBJID", "ARMCD", "SEX", "RACE", "RACEN", "AGE", "SAFFL", "POPPKFL")]
  adppk <- merge(adppk, dm_extra, by = "USUBJID", all.x = TRUE)

  # Inject requested anomalies
  if (inject_missing_dose) {
    miss_subj <- sample(unique(adppk$USUBJID), max(1, floor(n_subj * 0.05)))
    adppk <- adppk[!(adppk$USUBJID %in% miss_subj & adppk$EVID == 1), , drop = FALSE]
    ex <- ex[!(ex$USUBJID %in% miss_subj), , drop = FALSE]
  }

  if (inject_non_poppk) {
    bad <- sample(unique(adppk$USUBJID), max(1, floor(n_subj * 0.1)))
    adppk$POPPKFL[adppk$USUBJID %in% bad] <- "N"
  }

  if (inject_char_num_mismatch) {
    ix <- sample(which(!is.na(adppk$RACEN)), max(1, floor(0.02 * nrow(adppk))))
    adppk$RACEN[ix] <- adppk$RACEN[ix] + 9L
  }

  # Optional realistic QC issues for testing check logic
  if (isTRUE(inject_test_issues)) {
    lvl <- switch(issue_level, low = 0.01, medium = 0.03, high = 0.06)

    # 1) sampling deviations >10%
    ix_obs <- which(adppk$EVID == 0 & !is.na(adppk$NTIME))
    if (length(ix_obs) > 0) {
      pick <- sample(ix_obs, max(1, floor(lvl * length(ix_obs))))
      adppk$TIME[pick] <- adppk$NTIME[pick] * 1.2
    }

    # 2) duplicate rows
    ndup <- max(1, floor(lvl * nrow(adppk)))
    if (nrow(adppk) > 20) {
      dup_idx <- sample(seq_len(nrow(adppk)), min(ndup, nrow(adppk)))
      adppk <- rbind(adppk, adppk[dup_idx, , drop = FALSE])
    }

    # 3) MDV inconsistencies
    ix_mdv <- which(adppk$EVID == 0 & !is.na(adppk$DV))
    if (length(ix_mdv) > 0) {
      mdv_pick <- sample(ix_mdv, max(1, floor(lvl * length(ix_mdv))))
      adppk$MDV[mdv_pick] <- 1
    }
  }

  # Sort records: by subject, time, EVID desc (dose before obs at same time)
  ord <- order(adppk$USUBJID, adppk$TIME, -adppk$EVID)
  adppk <- adppk[ord, , drop = FALSE]
  rownames(adppk) <- NULL

  addose <- unique(adppk[adppk$EVID == 1, c("STUDYID", "USUBJID", "SUBJID", "AMT", "DOSEU", "ROUTE", "ADY")])
  names(addose)[names(addose) == "AMT"] <- "DOSE"
  names(addose)[names(addose) == "ADY"] <- "DOSEDY"

  list(dm = dm, ex = ex, pc = pc, adppk = adppk, addose = addose)
}

#' Backward-compatible wrapper to generate ADPPK and ADDOSE
#'
#' @inheritParams generate_dummy_pk_package
#' @return A list with `adppk` and `addose`.
#' @export
generate_dummy_adppk <- function(
    study_type = c("SAD", "MAD"),
    n_subj = 40,
    period_n = 1,
    seed = 123,
    inject_test_issues = FALSE,
    issue_level = c("low", "medium", "high"),
    inject_missing_dose = FALSE,
    inject_non_poppk = FALSE,
    inject_char_num_mismatch = FALSE
) {
  x <- generate_dummy_pk_package(
    study_type = study_type,
    n_subj = n_subj,
    period_n = period_n,
    seed = seed,
    inject_test_issues = inject_test_issues,
    issue_level = issue_level,
    inject_missing_dose = inject_missing_dose,
    inject_non_poppk = inject_non_poppk,
    inject_char_num_mismatch = inject_char_num_mismatch
  )
  list(adppk = x$adppk, addose = x$addose)
}
