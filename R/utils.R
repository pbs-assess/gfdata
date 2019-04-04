#' Run SQL
#'
#' @param database The name of the database.
#' @param query The query to run.
#'
#' @details
#' If you need to use a user-password setup to access the databases, then you
#' will need to set the R options in your .Rprofile file: `pbs.uid`, `pbs.pwd`,
#' `pbs.ip`, `pbs.sqldriver`. E.g. `options(pbs.uid="MyUserName")` The default
#' SQL driver will be `"SQL Server"` if not specified in the options. This will
#' probably work for most people. You might try using
#' `usethis::edit_r_profile()` if you need help finding your R profile file.
#'
#' @export
run_sql <- function(database, query) {
  query <- paste(query, collapse = "\n")
  DBI::dbGetQuery(db_connection(database = database), query)
}

#' Database connection
#' @export
db_connection <- function(server = "DFBCV9TWVASP001",
                          database = "GFBioSQL") {

  pbs_uid <- getOption("pbs.uid")
  pbs_pwd <- getOption("pbs.pwd")
  pbs_ip <- getOption("pbs.ip")
  pbs_sqldriver <- getOption("pbs.sqldriver")
  if (!is.null(pbs_uid) && !is.null(pbs_pwd) && !is.null(pbs_ip)) {
    DBI::dbConnect(odbc::odbc(),
      driver = if (is.null(pbs_sqldriver)) "SQL Server" else pbs_sqldriver,
      server = pbs_ip, database = database,
      pwd = pbs_pwd, uid = pbs_uid
    )
  } else {
    DBI::dbConnect(odbc::odbc(),
      driver = "SQL Server",
      server = server, database = database
    )
  }
}

force_three_letter_species_code <- function(x) {
  if (is.numeric(x)) {
    sprintf(paste0("%0", 3L, "d"), x)
  } else {
    as.character(x)
  }
}

collapse_filters <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

#' gfplot utilities
#'
#' @param sql_precode SQL go to inject before a species list etc.
#' @param species A vector of species or similar objects.
#' @param sql_code The SQL go to operate on.
#' @param search_flag What to search for in terms of where to inject the code.
#' @param conversion_func A conversion function to apply to the species or
#'   related vector.
#' @export
inject_filter <- function(sql_precode, species, sql_code,
                          search_flag = "-- insert species here",
                          conversion_func = common2codes) {
  i <- grep(search_flag, sql_code)
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(conversion_func(species)), ")"
  )
  sql_code
}

first_cap <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

read_sql <- function(x) {
  if (file.exists(system.file("sql", x, package = "gfdata"))) {
    readLines(system.file("sql", x, package = "gfdata"))
  } else {
    stop("The sql file does not exist.")
  }
}

all_species_codes <- function(x) {
  all(grepl("[0-9]+", x))
}

common2codes <- function(common) {
  if (all_species_codes(common)) {
    return(force_three_letter_species_code(common))
  }

  species <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES"
  )
  common_df <- data.frame(
    SPECIES_COMMON_NAME = toupper(common),
    order_by = seq_along(common), stringsAsFactors = FALSE
  )
  .d <- filter(species, SPECIES_COMMON_NAME %in% toupper(common))
  # Remove erroneous species codes for basking shark and lingcod:
  .d <- filter(.d, !SPECIES_CODE %in% c("033", "465")) %>%
    left_join(common_df, by = "SPECIES_COMMON_NAME") %>%
    arrange(.data$order_by)
  .d$SPECIES_CODE
}

codes2common <- function(spp_code) {
  species <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES"
  )

  for (i in spp_code){
    if (i == 033) {
      message("Code 033 deprecated for Basking Shark. Use code 034.")
    }
    if (i == 465) {
      message("Code 465 deprecated for Lingcod. Use code 467.")
    }
  }

  spp_code <- force_three_letter_species_code(spp_code)

  code_df <- data.frame(
    SPECIES_CODE = as.character(spp_code),
    order_by = seq_along(spp_code), stringsAsFactors = FALSE
  )
  .d <- filter(species, SPECIES_CODE %in% spp_code)

  .d <- filter(.d, !SPECIES_CODE %in% c("033", "465")) %>%
    left_join(code_df, by = "SPECIES_CODE") %>%
    arrange(.data$order_by)
  .d$SPECIES_COMMON_NAME
}

