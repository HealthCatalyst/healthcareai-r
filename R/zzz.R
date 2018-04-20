.onAttach <- function(...) {
  mes <- paste0("healthcareai version ", packageVersion("healthcareai"),
                "\nPlease visit https://docs.healthcare.ai for full documentation ",
                "and vignettes. Join the community at https://healthcare-ai.slack.com")
  packageStartupMessage(mes)
}
