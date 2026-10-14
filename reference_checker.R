# Prune a BibTeX file to references cited in a Quarto manuscript.
# Creates:
#   references_used.bib   — cited entries
#   references_unused.bib — uncited entries
#
# The original .bib file is never modified.

# ---- Settings ---------------------------------------------------------------

bib_file <- "references.bib"          # Your source bibliography
manuscript_dir <- "."                 # Folder containing .qmd files
used_file <- "references_used.bib"
unused_file <- "references_unused.bib"

# ---- Find manuscript files --------------------------------------------------

manuscript_files <- list.files(
  manuscript_dir,
  pattern = "\\.(qmd|Rmd|md)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(manuscript_files) == 0) {
  stop("No .qmd, .Rmd, or .md manuscript files were found.")
}

if (!file.exists(bib_file)) {
  stop("Could not find: ", bib_file)
}

manuscript_text <- paste(
  unlist(lapply(manuscript_files, readLines, warn = FALSE)),
  collapse = "\n"
)

# Protect against Quarto/Pandoc's nocite: '@*', which intentionally includes
# every reference in the bibliography.
if (grepl("nocite\\s*:\\s*['\"]?@\\*", manuscript_text, perl = TRUE)) {
  stop(
    "Found nocite: '@*'. This includes every BibTeX entry, so there are no ",
    "unused references to prune. Remove or change nocite first if desired."
  )
}

# Extract all citation keys used in text, including forms such as:
# @Smith2020, [@Smith2020], -@Smith2020, and nocite: '@Smith2020'
cited_keys <- regmatches(
  manuscript_text,
  gregexpr("@[A-Za-z0-9_.:/+\\-]+", manuscript_text, perl = TRUE)
)[[1]]

cited_keys <- sort(unique(sub("^@", "", cited_keys)))

# ---- Read and split the BibTeX file into entries ----------------------------

bib_lines <- readLines(bib_file, warn = FALSE)

# BibTeX entries must start with @type{key,
entry_start <- grep(
  "^\\s*@[A-Za-z]+\\s*\\{[^,]+,",
  bib_lines,
  perl = TRUE
)

if (length(entry_start) == 0) {
  stop("No standard BibTeX entries were found in: ", bib_file)
}

entry_end <- c(entry_start[-1] - 1, length(bib_lines))

entries <- Map(
  function(start, end) bib_lines[start:end],
  entry_start,
  entry_end
)

entry_keys <- vapply(entries, function(entry) {
  header <- paste(entry, collapse = "\n")
  match <- regexec(
    "^\\s*@[A-Za-z]+\\s*\\{\\s*([^,]+),",
    header,
    perl = TRUE
  )
  regmatches(header, match)[[1]][2]
}, character(1))

# ---- Compare and write new files --------------------------------------------

used_index <- entry_keys %in% cited_keys

writeLines(
  unlist(entries[used_index], use.names = FALSE),
  used_file
)

writeLines(
  unlist(entries[!used_index], use.names = FALSE),
  unused_file
)

# Report citations that are used in the manuscript but absent from the .bib file
missing_keys <- setdiff(cited_keys, entry_keys)

cat(
  "\nDone.\n",
  "Cited entries written to:  ", normalizePath(used_file), "\n",
  "Unused entries written to: ", normalizePath(unused_file), "\n",
  "Cited references: ", sum(used_index), "\n",
  "Unused references: ", sum(!used_index), "\n",
  sep = ""
)

if (length(missing_keys) > 0) {
  cat(
    "\n\nWARNING: These citation keys appear in the manuscript but are ",
    "missing from the BibTeX file:\n",
    paste0("  - ", missing_keys, collapse = "\n"),
    "\n",
    sep = ""
  )
}