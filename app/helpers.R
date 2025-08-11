# app/helpers.R

# Normalize coverage data for a group
normalize_coverage <- function(df) {
  original_names <- colnames(df)

  # convert to numeric matrix with decimal fix
  mat <- as.matrix(df)
  mode(mat) <- "character"
  mat <- matrix(as.numeric(gsub(",", ".", mat)), nrow = nrow(df))

  # normalize by column median
  col_medians <- apply(mat, 2, median, na.rm = TRUE)
  norm <- sweep(mat, 2, col_medians, "/")

  # center by row means
  row_means <- rowMeans(norm, na.rm = TRUE)
  centered <- sweep(norm, 1, row_means, "-")

  colnames(centered) <- original_names

  return(as.data.frame(centered))
}

# Load OMIM gene–phenotype reference table
load_omim_file <- function(path = file.path("..", "reference", "omim-phenptype-2024-upr-sl67.txt")) { # nolint
  if (!file.exists(path)) return(NULL)

  # read lines from file and split 
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  parsed <- strsplit(lines, "\t")

  # extract gene and phenotype(s), clean up empty values
  clean_df <- lapply(parsed, function(parts) {
    gene <- parts[1]
    phenotypes <- paste(Filter(function(x) x != "" && x != " ", parts[-1]), collapse = "; ")  # nolint
    return(data.frame(gene = gene, phenotyp = phenotypes, stringsAsFactors = FALSE)) # nolint
  })

  # combine all rows into one data frame and trim whitespace
  df <- do.call(rbind, clean_df)
  df[] <- lapply(df, trimws)
  return(df)
}

# Apply OMIM annotation to result
annotate_with_omim <- function(result_df, omim_df) {

  # If OMIM reference is missing or no 'name' column, assign "NA"
  if (is.null(omim_df) || !"name" %in% tolower(colnames(result_df))) {
    result_df$OMIM <- "NA"
    return(result_df)
  }
  # normalize gene names (trim whitespace, convert to uppercase for matching)
  query_genes <- toupper(trimws(result_df$name))
  ref_genes <- toupper(trimws(omim_df$gene))

  # match input genes to OMIM genes
  match_idx <- match(query_genes, ref_genes)

  # add matched phenotypes to OMIM column, use "NA" for unmatched genes
  result_df$OMIM <- ifelse(!is.na(match_idx), omim_df$phenotyp[match_idx], "NA")

  # Clean up phenotype values:
  #  - remove trailing semicolons
  result_df$OMIM <- gsub("(;\\s*)+$", "", result_df$OMIM)
  #  - replace repeated ;; with a single ;
  result_df$OMIM <- gsub("\\s*;\\s*;", ";", result_df$OMIM)

  return(result_df)
}
