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


#' Process services table
#'
#' @param services services list from YAML parameterss
#' @param tva tva parameter
#'
#' @importFrom dplyr as_tibble mutate_at vars mutate
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#'
#' @export
#'
process_services <- function(services, tva = "No") {
  if (tva == "No") {
    suffix <- NULL
  } else if (tva == "Yes") {
    suffix <- " HT"
  }
  lapply(services, function(x) if (is.list(x)) { unlist(x$value) } else {x}) %>%
    as_tibble() %>%
    mutate(total = quantity*unitprice) %>%
    mutate_at(vars(unitprice, total), parse_amount) %>%
    mutate_all(~gsub("(^NA$|^$)", NA, .)) %>%
    kable(format = "html", escape = FALSE, format.args = list(decimal.mark = ","),
          col.names = c("D\u00e9signation", "Quantit\u00e9", "Unit\u00e9",
                        paste0("Prix unit.", suffix), paste0("Total", suffix))) %>%
    column_spec(column = 1, width = "45%", extra_css = "text-align:justify;") %>%
    column_spec(column = 2, width = "10%", extra_css = "text-align:right;") %>%
    column_spec(column = 3, width = "10%", extra_css = "text-align:right;") %>%
    column_spec(column = 4, width = "20%", extra_css = "text-align:right;") %>%
    column_spec(column = 5, width = "15%", extra_css = "text-align:right;")
}


#' Process total table
#'
#' @param services services list from YAML params
#' @param tva "Yes"/"No"
#' @param discount discount rate (between 0 and 1)
#' @param deposit deposit amount. Only for bills
#'
#' @importFrom kableExtra column_spec footnote
#' @importFrom knitr kable
#' @importFrom dplyr as_tibble mutate summarize rename mutate_if everything recode pull
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
process_total <- function(services, tva = "No", discount = 0, deposit = 0) {
  if (tva == "No") {
    suffix <- NULL
  } else if (tva == "Yes") {
    suffix <- " HT"
  }
  tot <- lapply(services, function(x) if (is.list(x)) { unlist(x$value) } else {x}) %>%
    as_tibble() %>%
    mutate(total = quantity*unitprice) %>%
    summarize(total = sum(total))
  if (discount > 0) {
    if (discount > 1) {
      stop("Discount should not be larger than 1 if you don't want to give money to your client...")
    }
    tot <- tot %>%
      mutate(disc = paste0(100*discount, "%"),
             disc_tot = (1 - discount)*total)
  } else {
    if (discount < 0) {
      stop("A negative discount is not a discount...")
    }
    tot <- rename(tot, disc_tot = total)
  }

  if (tva == "Yes") {
    tot <- tot %>%
      mutate(tva = 0.2*disc_tot,
             ttc_tot = 1.2*disc_tot)
  }  else if (tva == "No") {
    tot <- rename(tot, ttc_tot = disc_tot)
  }

  if (deposit > 0) {
    if (deposit > pull(tot, ttc_tot)) {
      stop("Are you sure your client paid a deposit larger than the total amount ?")
    }
    tot <- tot %>%
      mutate(deposit = deposit,
             to_pay = ttc_tot - deposit)
  } else {
    if (deposit < 0) {
      stop("A negative deposit means you lend money to your client...")
    }
  }

  rname <- c(total = paste0("Total", suffix),
             disc = "Remise",
             disc_tot = paste0("Total remis\u00e9", suffix),
             tva = "TVA (20%)",
             ttc_tot = if (tva == "No") {"Net \u00e0 payer"} else {"Total TTC"},
             deposit = "Acompte",
             to_pay = if (tva == "No") {"Reste \u00e0 payer"} else {"Reste \u00e0 payer TTC"})

 ktab <- tot %>%
    mutate_if(is.numeric, parse_amount) %>%
    pivot_longer(everything(), names_to = "key", values_to = "values") %>%
    mutate(key = recode(key, !!!rname)) %>%
    kable(format = "html", escape = FALSE) %>%
    column_spec(column = 1, width = "57%", extra_css = "text-align:right; font-weight:bold;") %>%
    column_spec(column = 2, width = "43%", extra_css = "text-align:right;") %>%
    remove_header()
 if (tva == "No") {
   ktab %>%
     footnote(general = "TVA non applicable, article 293B du CGI",
              general_title = "")
 } else {
   ktab
 }
}



#' Process bank information for invoices
#'
#' @param bankinfo bank info list from YAML params
#'
#' @importFrom dplyr as_tibble mutate_all rename everything mutate
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom knitr kable
#'
#' @export
#'
process_bank <- function(bankinfo) {
  as_tibble(bankinfo) %>%
    mutate_all(~gsub("(^NA$|^$)", NA, .)) %>%
    rename(Titulaire = holder, Banque = bank, BIC = bic, IBAN = iban) %>%
    pivot_longer(everything(), names_to = "key", values_to = "value") %>%
    drop_na() %>%
    mutate(key = paste0(key, " :")) %>%
    kable("html", escape = FALSE) %>%
    remove_header()
}


#' Internal function to process annexes. Do not use outside
#'
#' @param file character. Path of the annexes file
#'
#' @importFrom rmarkdown render
#' @importFrom xml2 read_html xml_find_all xml_children xml_new_root xml_add_child xml_remove
#' @importFrom purrr walk
#' @importFrom htmltools HTML
#'
#' @export
#'
process_annexes <- function(file) {
  temphtml <- normalizePath(file.path(tempdir(), "annexes.html"), mustWork = FALSE, winslash = "/")
  render(file, output_format = "html_document", output_file = temphtml,
         envir = new.env(parent = globalenv()), quiet = TRUE)

  toadd <- read_html(temphtml) %>% xml_find_all(".//body") %>% xml_children()
  newroot <- xml_new_root("div", class = "newpage")
  walk(toadd, ~xml_add_child(.x = newroot, .value = .x))
  walk(xml_find_all(newroot, "script"), xml_remove)
  HTML(gsub("<\\?xml.+\\?>\n", "", as.character(newroot)))
}

