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
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' \dontrun{
#' run_sql("GFBioSQL", "EXEC sp_who2")
#' }
run_sql <- function(database, query) {
  query <- paste(query, collapse = "\n")
  con <- db_connection(database = database)
  on.exit(suppressWarnings(suppressMessages(DBI::dbDisconnect(con))))
  DBI::dbGetQuery(con, query)
}

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

#' gfdata utilities
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
  i <- suppressWarnings(grep(search_flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(conversion_func(species)), ")"
  )
  sql_code
}

first_cap <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
      s <- substring(s, 2)
      if (strict) tolower(s) else s
    },
    sep = "", collapse = " "
    )
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

  for (i in spp_code) {
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

#' Assign areas
#'
#' @param major_stat_area_description A vector of major statistical area
#'   descriptions.
#' @param area_regex A vector of regular expressions describing the areas group.
#' @export
#' @examples
#' x <- c("5D: NORTHERN HECATE STRAIT", "3C: S.W. VANCOUVER ISLAND",
#'   "3D: N.W. VANCOUVER ISLAND")
#' assign_areas(x)
assign_areas <- function(major_stat_area_description,
                         area_regex = c("3[CD]+", "5[AB]+", "5[CDE]+")) {
  out <- rep(NA, length(major_stat_area_description))
  for (i in seq_along(area_regex)) {
    out[grepl(area_regex[i], major_stat_area_description)] <-
      gsub("\\^|\\[|\\]|\\+", "", area_regex[i])
  }
  out
}


# Length type
#
# Returns the most commonly recorded length measurement (of fork length, total
# length, length to end of second dorsal fin, or standard length) for the
# given species.
get_spp_sample_length_type <- function(species) {
  .q <- read_sql("get-spp-sample-length-type.sql")
  .q <- inject_filter("WHERE SPECIES_CODE IN ", species, .q)
  .d <- run_sql("GFBioSQL", .q)
  .d <- .d %>%
    tidyr::gather("Fork_Length",
      "Standard_Length",
      "Total_Length",
      "Second_Dorsal_Length",
      key = "length_type",
      value = "count"
    )
  .d <- .d %>% dplyr::filter(count == max(count))
  if (nrow(.d) > 1L) .d <- .d[1L, ,drop = FALSE] # happens if all 0! pick any
  .d$length_type
}

add_version <- function(x) {
  attr(x, "version") <- utils::packageVersion("gfdata")
  attr(x, "date") <- Sys.time()
  x
}
