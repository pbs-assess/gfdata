# Run SQL

Run SQL

## Usage

``` r
run_sql(database, query)
```

## Arguments

- database:

  The name of the database.

- query:

  The query to run.

## Details

If you need to use a user-password setup to access the databases, then
you will need to set the R options in your .Rprofile file: `pbs.uid`,
`pbs.pwd`, `pbs.ip`, `pbs.sqldriver`. E.g.
`options(pbs.uid="MyUserName")` The default SQL driver will be
`"SQL Server"` if not specified in the options. This will probably work
for most people. You might try using
[`usethis::edit_r_profile()`](https://usethis.r-lib.org/reference/edit.html)
if you need help finding your R profile file.

## Examples

``` r
if (FALSE) { # \dontrun{
run_sql("GFBioSQL", "EXEC sp_who2")
} # }
```
