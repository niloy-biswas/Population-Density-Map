# download_kontur.R
# Download Kontur population hexagons and admin boundaries for countries by
# ISO 3166-1 alpha-2 code, straight from Kontur's public S3 bucket (the same
# files HDX links to). Files land in data/kontur/, which is gitignored: the
# larger ones (US, China) are over GitHub's 100 MB file limit.
#
# Already-downloaded files are skipped, so re-running is cheap.
#
# Usage (from repo root):
#   Rscript scripts/download_kontur.R GB DE FR

kontur_url <- paste0(
  "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/",
  "kontur_datasets/"
)
kontur_dir <- "data/kontur"
population_release <- "20231101"
boundaries_release <- "20230628"

# Boundary files reach ~350 MB gzipped (France); the default 60 s times out.
options(timeout = 3600)

kontur_files <- function(iso) {
  c(sprintf("kontur_population_%s_%s.gpkg", iso, population_release),
    sprintf("kontur_boundaries_%s_%s.gpkg", iso, boundaries_release))
}

download_kontur_file <- function(file, dir = kontur_dir) {
  dest <- file.path(dir, file)
  if (file.exists(dest)) {
    message("have ", dest)
    return(invisible(dest))
  }
  # Unpack under a temporary name, so an interrupted run never leaves a
  # truncated .gpkg that the next run would skip as already downloaded.
  gz <- paste0(dest, ".gz")
  part <- paste0(dest, ".part")
  status <- download.file(paste0(kontur_url, file, ".gz"), gz,
                          mode = "wb", quiet = TRUE)
  if (status != 0) stop("download failed: ", file)
  R.utils::gunzip(gz, destname = part, remove = TRUE, overwrite = TRUE)
  file.rename(part, dest)
  message(sprintf("got  %s (%.0f MB)", dest, file.size(dest) / 1e6))
  invisible(dest)
}

download_kontur <- function(iso, dir = kontur_dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  for (f in kontur_files(toupper(iso))) download_kontur_file(f, dir)
}

isos <- commandArgs(trailingOnly = TRUE)
if (length(isos) == 0) stop("Give one or more ISO codes, e.g. GB DE FR")
for (iso in isos) download_kontur(iso)
