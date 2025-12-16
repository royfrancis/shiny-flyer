# shiny-flyer
# functions

#' Create temp directory
#' @param session Shiny session object
#' @return Path to temporary directory
#' 
fn_dir <- function(session) {
  wd <- file.path(tempdir(check = TRUE), session$token)
  if (!dir.exists(wd)) dir.create(wd)
  cat(paste0("Working directory: ", wd, "\n"))
  return(wd)
}

#' Get function version
#' @return Function version string
#' 
fn_version <- function() {
  return("v1.0.1")
}

#' General input validation
#' @param input Input to validate
#' @param message1 Message for missing input
#' @param message2 Message for NA input
#' @param message3 Message for empty input
#' @return NULL or prints validation message
#' 
fn_validate <- function(input, message1, message2, message3) {
  if (missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing", message1))
  if (gcheck == 1) {
    m1 <- sub("Argument ", "", message1)
    m1 <- sub(" missing.", "", m1)
  }

  if (all(is.null(input))) {
    if (missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if (all(is.na(input))) {
      if (missing(message2)) {
        if (gcheck == 1) message2 <- paste0("Argument ", m1, " is NA.", sep = "")
        if (gcheck != 1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if (all(nchar(input) == 0)) {
      if (missing(message3)) {
        if (gcheck == 1) message3 <- paste0("Argument ", m1, " is empty.", sep = "")
        if (gcheck != 1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

#' Validate numeric input
#' @param input Input to validate
#' @return NULL or prints validation message
#' 
fn_validate_numeric <- function(input) {
  if (is.na(input) || !is.numeric(input)) print("Input is not a numeric.")
}

#' Validate image upload
#' @param x Uploaded image
#' @return NULL or prints validation message
#' 
fn_validate_im <- function(x) {
  if (!is.null(x)) {
    y <- tolower(sub("^.+[.]", "", basename(x$datapath)))
    if (!y %in% c("jpg", "png", "jpeg", "gif", "svg")) {
      return("Image must be one of JPG/JPEG, PNG, GIF or SVG formats.")
    }
    if ((x$size / 1024 / 1024) > 1) {
      return("Image must be less than 1MB in size.")
    }
  }
}

#' Handle image selection or upload and return metadata
#' @param option Either "Select" or "Upload".
#' @param selected_value Value from the picker when option == "Select".
#' @param upload Shiny fileInput object when option == "Upload".
#' @param workdir Session working directory where files should be stored.
#' @param kind Short label used for filenames and messages (e.g. "background").
#' @param validate_image Whether to run fn_validate_im on the upload.
#' @return A list(path = <relative path>) or NULL.
#'
handle_image_asset <- function(
  option,
  selected_value,
  upload,
  workdir,
  kind = "image",
  validate_image = TRUE
) {
  if (identical(option, "Select")) {
    if (is.null(selected_value) || identical(selected_value, "")) {
      return(NULL)
    }
    return(list(path = selected_value))
  }

  if (identical(option, "Upload")) {
    if (is.null(upload)) {
      shiny::validate(shiny::need(FALSE, paste("Please upload a", kind, "image.")))
    }

    if (validate_image) {
      shiny::validate(fn_validate_im(upload))
    }

    source_path <- upload$datapath
    dest_rel <- file.path("upload", paste0(kind, "-", basename(source_path)))
    dest_abs <- file.path(workdir, dest_rel)
    dest_dir <- dirname(dest_abs)
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(source_path, dest_abs, overwrite = TRUE)

    return(list(path = dest_rel))
  }

  NULL
}

#' Copy directories and files to output path
#' @param path Output directory path
#' @return NULL
#' 
copy_dirs <- function(path) {
  dirs_to_copy <- c("_extensions", "fonts", "www", "assets")

  # ensure the directory exists and copy the contents
  copy_directory <- function(dir_name) {
    dir_to_create <- file.path(path, dir_name)
    if (!dir.exists(dir_to_create)) {
      dir.create(dir_to_create, recursive = TRUE)
    }
    file.copy(
      from = list.files(dir_name, full.names = TRUE),
      to = dir_to_create,
      recursive = TRUE
    )
  }

  for (dir_name in dirs_to_copy) {
    copy_directory(dir_name)
  }

  # copy files with extensions *.r and *.qmd to output directory
  files_to_copy_r <- list.files(pattern = "\\.r$", full.names = TRUE)
  files_to_copy_qmd <- list.files(pattern = "\\.qmd$", full.names = TRUE)

  if (length(files_to_copy_r) > 0) {
    file.copy(from = files_to_copy_r, to = path)
  }

  if (length(files_to_copy_qmd) > 0) {
    file.copy(from = files_to_copy_qmd, to = path)
  }
}

# set defaults -----------------------------------------------------------------

#' Get asset choices from a directory
#' @param directory The directory to search for assets.
#' @param label_none The label to use for the "none" option.
#' @param pattern The pattern to match files.
#' @return A named vector of asset choices.
#' 
get_asset_choices <- function(directory, label_none = "none", pattern = "*") {
  if (!dir.exists(directory)) {
    out <- ""
    names(out) <- label_none
    return(out)
  }
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  values <- c("", files)
  labels <- c(label_none, tools::file_path_sans_ext(basename(files)))
  names(values) <- labels
  values
}

#' Build picker metadata for asset selection
#' @param values A named vector of asset values.
#' @return A data frame with metadata for the picker input.
#' 
build_picker_metadata <- function(values) {
  df <- data.frame(
    value = values,
    label = names(values),
    path = sub("^www/", "", values),
    stringsAsFactors = FALSE
  )
  df$img <- ifelse(
    df$path == "",
    sprintf("<div class='picker-inner'>%s</div>", df$label),
    sprintf("<img class='picker-outer' src='%s'><div class='picker-inner'>%s</div></img>", df$path, df$label)
  )
  df
}

#' Sanitize text input
#' @param value Text input to sanitize
#' @return Sanitized text or NULL
#' 
sanitize_text <- function(value) {
  if (is.null(value)) return(NULL)
  trimmed <- trimws(value)
  if (identical(trimmed, "")) return(NULL)
  trimmed
}

#' Format unit string
#' @param value Numeric value
#' @param unit Unit string
#' @param default Default value if input is NULL or NA
#' @return Formatted unit string
#' 
format_unit <- function(value, unit, default) {
  if (is.null(value) || is.na(value)) value <- default
  paste0(value, unit)
}

#' Compact a list by removing NULL elements
#' @param x List to compact
#' @return Compacted list
#' 
compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

description_text <- "
Join us for an in-depth workshop on data analysis techniques and tools. Over several sessions, we will combine short lectures with hands-on exercises. By the end of the workshop, you will have practical experience working with real-world datasets.
"

content_text <- "
**This course** combines lectures, demonstrations, and guided practicals that follow a structured syllabus. We will start with core *data handling* and visualization, then move on to exploratory data analysis, statistical modelling, and reporting. Each module includes clearly defined learning objectives, recommended readings, and example datasets to support self-study.

Participants are selected based on motivation, relevance to their current projects, and basic familiarity with data analysis tools. For more information about NBIS training and upcoming courses, visit <https://nbis.se/training/>.
"

dummy_text <- "
Here is some text content with formatting. You can have **bold**, *italic* or ***bold italic*** text. Here is an inline code `code()` and a direct link <https://bla.com> and a named [link](https://bla.com). Here is an example of using subscript (H~2~0) and superscript (x^2^).

Here is some math: $\\int_0^1 x^2 dx$. You can mix inline math $a^2 + b^2 = c^2$ and Greek letters like $\\alpha$, $\\beta$, and $\\gamma$."
