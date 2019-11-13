#' Make freelance or company info suitable for HTML format
#'
#' @param freelance list of freelance from the YAML params
#'
#' @importFrom dplyr mutate_all mutate everything case_when as_tibble
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @importFrom tidyr pivot_longer unite drop_na
#' @importFrom fontawesome fa
#'
#' @export
#'
process_freelance <- function(freelance) {
  as_tibble(freelance) %>%
    mutate_all(~gsub("(^NA$|^$)", NA, .)) %>%
    unite(col = "address3", postal_code, city, sep = " ", na.rm = TRUE) %>%
    pivot_longer(everything(), names_to = "key", values_to = "value") %>%
    drop_na() %>%
    mutate(key = case_when(key == "name" ~ as.character(fa("id-card")),
                           key == "address1" ~ as.character(fa("home")),
                           grepl("address(2|3)", key) ~ "",
                           key == "mobile" ~ as.character(fa("mobile-alt")),
                           key == "e_mail" ~ as.character(fa("at")),
                           key == "web" ~ as.character(fa("globe")),
                           TRUE ~ up_first(key))) %>%
    kable(escape = FALSE) %>%
    column_spec(column = 1, width = "8mm", extra_css = "text-align:center;") %>%
    remove_header()
}


#' Internal function to process the boxheader part in Rmd template. Do not use outside
#'
#' @param info list of info from the YAML params
#' @param which either 1, 2 or 3 depending of the boxheader targeted
#'
#'
#' @export
#'
process_boxheader <- function(info, which) {
  if (which == 1) {
    tobox <- paste0("<p>Date</p><hr><p>", ifelse(info$date == "NA", format(Sys.Date(), "%d/%m/%Y"), info$date), "</p>")
  } else if (which == 2) {
    tobox <- paste0("<p>N\u00b0 " , info$doc, "</p><hr><p>", info$ndoc, "</p>")
  } else if (which == 3) {
    nclient <- unlist(strsplit(info$nclient, split = "\n"))
    nclient <- paste0("<p>", nclient, "</p>", collapse = "")
    tobox <- paste0("<p>N\u00b0 Client", "<hr>", nclient)
  }
  return(tobox)
}


#' Make client and billing parts in Rmd template
#'
#' @param address list of address info (client or billing)
#' @param col_name header name of address
#'
#' @importFrom dplyr mutate_all everything as_tibble select
#' @importFrom knitr kable
#' @importFrom tidyr pivot_longer unite drop_na
#'
#' @export
#'
process_address <- function(address, col_name = "Livraison") {
  address <- as_tibble(address) %>%
    mutate_all(~gsub("(^NA$|^$)", NA, .))
  if ("firstname" %in% colnames(address)) {
    address <- address %>%
      unite(col = "fullname", firstname, name, sep = " ", na.rm = TRUE)
  }
  address %>%
    unite(col = "address3", postal_code, city, sep = " ", na.rm = TRUE) %>%
    pivot_longer(everything(), names_to = "key", values_to = col_name) %>%
    drop_na() %>%
    select(-key) %>%
    kable(escape = FALSE)
}


#' Process descritpion of services table for HTML
#'
#' @param services services list from YAML params
#' @param tva yes/no
#'
#' @importFrom dplyr bind_rows mutate_at vars matches mutate tibble
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#'
#' @export
#'
process_services <- function(services, tva = "no") {
  data <- tibble('D\u00e9signation' = NA,#character(),
                 'Quantit\u00e9' = NA,#double(),
                 'Unit\u00e9' = NA,#character(),
                 'Prix unitaire' = NA,#double(),
                 'Total' = NA)#double())
  if (any(!(services$data %in% c("NA", ""))) & any(!is.na(services$data))) {
    temp <- services$data
    colnames(temp) <- colnames(data)
    data <- bind_rows(data, temp)
    data <- data[-1,]
  }
  data %>%
    mutate_at(vars(matches('Prix unitaire')), parse_amount) %>%
    mutate('Total' = parse_amount(Total)) %>%
    kable(format = "html", escape = FALSE) %>%
    column_spec(column = 1, width = "40%", extra_css = "text-align:justify;") %>%
    column_spec(column = 2, width = "13%", extra_css = "text-align:right;") %>%
    column_spec(column = 3, width = "11%", extra_css = "text-align:right;") %>%
    column_spec(column = 4, width = "20%", extra_css = "text-align:right;") %>%
    column_spec(column = 5, width = "16%", extra_css = "text-align:right;")
}
