#' Convert MSDRGs into a "base DRG" and complication level
#'
#' @param drgs character vector of MSDRG descriptions, e.g. MSDRGDSC
#' @param remove_age logical; if TRUE will remove age descriptions
#'
#' @return a tibble with three columns: msdrg: the input vector, base_msdrg, and
#'   msdrg_complication
#' @export
#'
#' @details This function is not robust to different codings of complication in
#'   DRG descriptions. If you have a coding other than "W CC" / "W MCC" / "W
#'   CC/MCC" / "W/O CC" / "W/O MCC", please file an issue on Github and we'll
#'   try to add support for your coding.
#'
#' @examples
#' MSDRGs <- c("ACUTE LEUKEMIA W/O MAJOR O.R. PROCEDURE W CC",
#'             "ACUTE LEUKEMIA W/O MAJOR O.R. PROCEDURE W MCC",
#'             "ACUTE LEUKEMIA W/O MAJOR O.R. PROCEDURE W/O CC/MCC",
#'             "SIMPLE PNEUMONIA & PLEURISY",
#'             "SIMPLE PNEUMONIA & PLEURISY AGE 0-17")
#' separate_drgs(MSDRGs, remove_age = TRUE)
separate_drgs <- function(drgs, remove_age = FALSE) {
  cc_pos <- tibble::tibble(
    w_cc = stringr::str_locate(drgs, stringr::coll("W CC"))[, 1],
    w_mcc = stringr::str_locate(drgs, stringr::coll("W MCC"))[, 1],
    w_ccmcc = stringr::str_locate(drgs, stringr::coll("W CC/MCC"))[, 1],
    wo = stringr::str_locate(drgs, "(W/O CC)|(W/O MCC)")[, 1],
    drg = drgs
  ) %>%
    dplyr::mutate(w_cc = ifelse(!is.na(w_ccmcc), NA, w_cc))
  # Check only one (or zero) cc marker each:
  pos_tab <-
    cc_pos[, 1:4] %>%
    apply(1, function(x) sum(!is.na(x))) %>%
    table()
  if (!all(names(pos_tab) %in% 0:1))
    stop("Some DRGs appeared to have more than one complication level.")
  cc_charpos <-
    cc_pos[, 1:4] %>%
    apply(1, function(x) {
      pos <- unique(x[!is.na(x)])
      if (!length(pos)) -1L else pos - 2L
    })
  bases <- purrr::map2_chr(drgs, cc_charpos, ~ stringr::str_sub(.x, end = .y))
  complications <-
    apply(cc_pos[, 1:4], 1, function(x) {
      i <- which(!is.na(x))
      if (!length(i)) i <- 4
      return(i)
    })
  # No mention of CC currently look the same as missing, so replace complications with
  # missing where base is missing
  complications[is.na(bases)] <- NA
  complications <- c("complication", "major complication", "complication", "absent complication")[complications]
  if (remove_age) {
    age_loc <- stringr::str_locate(bases, stringr::coll(" AGE"))[, "start"] - 1L
    age_loc[is.na(age_loc)] <- -1L
    bases <- stringr::str_sub(bases, 0, age_loc)
  }
  tibble::tibble(msdrg = drgs,
                 base_msdrg = bases,
                 msdrg_complication = complications)
}
