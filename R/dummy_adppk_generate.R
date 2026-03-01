#' Generate dummy SDTM source domains and derived ADPPK
#'
#' This generator creates 3 source domains (DM, EX, PC) and derives ADPPK-like
#' analysis data from them.
#'
#' @param study_type Study type: "SAD" or "MAD".
#' @param n_subj Number of subjects.
#' @param seed Random seed.
#' @param inject_missing_dose Logical; inject subjects with PK but no dose records.
#' @param inject_non_poppk Logical; inject non-POPPK subjects/records.
#' @param inject_char_num_mismatch Logical; inject race/racen mismatch.
#'
#' @return A list with `dm`, `ex`, `pc`, `adppk`, and `addose` data frames.
#' @export
generate_dummy_pk_package <- function(
    study_type = c("SAD", "MAD"),
    n_subj = 40,
    seed = 123,
    inject_missing_dose = FALSE,
    inject_non_poppk = FALSE,
    inject_char_num_mismatch = FALSE
) {
  study_type <- match.arg(study_type)
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
  ex_list <- vector("list", n_subj)
  for (i in seq_len(n_subj)) {
    dose_amt <- ifelse(grepl("HIGH", dm$ARM[i]), 100, 50)
    days <- seq_len(n_days)
    exdt <- base_dt + (days - 1) * 24 * 3600
    ex_list[[i]] <- data.frame(
      STUDYID = dm$STUDYID[i],
      USUBJID = dm$USUBJID[i],
      EXSEQ = seq_along(days),
      EXTRT = "TEST DRUG",
      EXDOSE = dose_amt,
      EXDOSU = "mg",
      EXROUTE = "ORAL",
      EXSTDTC = format(exdt, "%Y-%m-%dT%H:%M:%SZ"),
      stringsAsFactors = FALSE
    )
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
      abs_dt <- dts[d] + tm * 3600
      c0 <- ifelse(grepl("HIGH", dm$ARM[i]), 120, 80)
      # Slight accumulation in MAD by day
      day_factor <- ifelse(study_type == "MAD", 1 + (d - 1) * 0.05, 1)
      conc <- pmax(0, (c0 * day_factor) * exp(-0.23 * pmax(tm, 0)) + stats::rnorm(length(tm), 0, 3))
      # predose values lower
      conc[tm < 0] <- pmax(0, conc[tm < 0] * 0.1)
      blq <- conc < 1
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

  keep_dose <- data.frame(
    STUDYID = ad_dose$STUDYID,
    USUBJID = ad_dose$USUBJID,
    SUBJID = sub("STDY01-", "", ad_dose$USUBJID),
    ARM = NA_character_, ACTARM = NA_character_, TRT01P = NA_character_, TRT01A = NA_character_,
    VISIT = paste0("DAY ", floor(ad_dose$TIME / 24) + 1),
    VISITNUM = floor(ad_dose$TIME / 24) + 1,
    ADY = floor(ad_dose$TIME / 24) + 1,
    ATPT = "DOSE", ATPTN = NA_real_,
    PARAMCD = ad_dose$PARAMCD, PARAM = ad_dose$PARAM,
    AVAL = ad_dose$AVAL, AVALU = ad_dose$AVALU,
    LLOQ = NA_real_, BLQFL = NA_character_,
    EXTRT = ad_dose$EXTRT, ROUTE = ad_dose$EXROUTE,
    DOSE = ad_dose$EXDOSE, DOSEU = ad_dose$EXDOSU,
    EVID = ad_dose$EVID, MDV = ad_dose$MDV, DV = ad_dose$DV, AMT = ad_dose$AMT,
    TIME = ad_dose$TIME, NTIME = ad_dose$NTIME, CMT = ad_dose$CMT,
    stringsAsFactors = FALSE
  )

  keep_pk <- data.frame(
    STUDYID = ad_pk$STUDYID,
    USUBJID = ad_pk$USUBJID,
    SUBJID = sub("STDY01-", "", ad_pk$USUBJID),
    ARM = NA_character_, ACTARM = NA_character_, TRT01P = NA_character_, TRT01A = NA_character_,
    VISIT = paste0("DAY ", floor(ad_pk$TIME / 24) + 1),
    VISITNUM = floor(ad_pk$TIME / 24) + 1,
    ADY = floor(ad_pk$TIME / 24) + 1,
    ATPT = ad_pk$PCTPT, ATPTN = ad_pk$PCTPTNUM,
    PARAMCD = ad_pk$PARAMCD, PARAM = ad_pk$PARAM,
    AVAL = ad_pk$AVAL, AVALU = ad_pk$AVALU,
    LLOQ = 1, BLQFL = ifelse(toupper(as.character(ad_pk$PCSTAT)) == "BLQ" | ad_pk$AVAL < 1, "Y", "N"),
    EXTRT = "TEST DRUG", ROUTE = "ORAL",
    DOSE = NA_real_, DOSEU = "mg",
    EVID = ad_pk$EVID, MDV = ad_pk$MDV, DV = ad_pk$DV, AMT = ad_pk$AMT,
    TIME = ad_pk$TIME, NTIME = ad_pk$NTIME, CMT = ad_pk$CMT,
    stringsAsFactors = FALSE
  )

  adppk <- rbind(keep_dose, keep_pk)
  adppk <- merge(adppk, dm[, c("USUBJID", "ARM", "ARMCD", "SEX", "RACE", "RACEN", "AGE", "SAFFL", "POPPKFL")], by = "USUBJID", all.x = TRUE, suffixes = c("", ".DM"))

  # fill arm values from DM if empty
  fill_from_dm <- function(x, y) ifelse(is.na(x) | x == "", y, x)
  adppk$ARM <- fill_from_dm(adppk$ARM, adppk$ARM.DM)
  adppk$ACTARM <- adppk$ARM
  adppk$TRT01P <- adppk$ARM
  adppk$TRT01A <- adppk$ARM
  adppk$ARM.DM <- NULL

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
    seed = 123,
    inject_missing_dose = FALSE,
    inject_non_poppk = FALSE,
    inject_char_num_mismatch = FALSE
) {
  x <- generate_dummy_pk_package(
    study_type = study_type,
    n_subj = n_subj,
    seed = seed,
    inject_missing_dose = inject_missing_dose,
    inject_non_poppk = inject_non_poppk,
    inject_char_num_mismatch = inject_char_num_mismatch
  )
  list(adppk = x$adppk, addose = x$addose)
}
