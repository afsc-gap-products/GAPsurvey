#' Merge SCS XML Files
#'
#' Reads multiple SCS XML data files, sorts them 
#' chronologically by start time, and merges their triggers, diagnostics, metadata, 
#' and output nodes into a single master XML file. Optionally crops data points 
#' outside a specified local time window.
#'
#' @param file_paths Character vector. File paths of the XML files to merge.
#' @param output_path Character. File path where the merged XML document will be saved. 
#'   Defaults to `"Merged_SCS.xml"`.
#' @param start_time Character. Optional local timestamp string (e.g., `"2026-06-22 12:34:00"`) 
#'   to filter out records occurring before this time.
#' @param end_time Character. Optional local timestamp string (e.g., `"2026-06-22 13:46:00"`) 
#'   to filter out records occurring after this time.
#' @param tz Character. Timezone for `start_time` and `end_time`. Defaults to 
#'   `"America/Anchorage"` (automatically handles AKDT/AKST transitions).
#'
#' @return Invisibly returns the merged `xml_document` object.
#'
#' @export
#'
#' @importFrom xml2 read_xml write_xml xml_attr xml_attr<- xml_find_first xml_find_all xml_add_child xml_remove xml_attrs xml_text xml_name
#'
#' @examples
#' \dontrun{
#' scs_files <- c("file1.xml", "file2.xml", "file3.xml")
#' 
#' merge_scs_files(
#'   file_paths = scs_files,
#'   output_path = "Haul0073.xml",
#'   start_time  = "2026-06-22 12:34:00", # Alaska Daylight Time
#'   end_time    = "2026-06-22 13:46:00"  # Alaska Daylight Time
#' )
#' }
merge_scs_files <- function(file_paths, 
                            output_path = "Merged_SCS.xml", 
                            start_time = NULL, 
                            end_time = NULL,
                            tz = "America/Anchorage") {
  if (length(file_paths) == 0) {
    stop("No file paths provided.")
  }
  
  # Helper to parse UTC timestamps from XML tags
  parse_xml_utc <- function(time_str) {
    if (is.null(time_str) || is.na(time_str) || time_str == "") return(NULL)
    clean_str <- sub("Z$", "", time_str)
    return(as.POSIXct(clean_str, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  
  # Helper to parse local user input (e.g. Alaska time) and convert it to UTC POSIXct
  parse_local_input <- function(time_str, local_tz) {
    if (is.null(time_str) || is.na(time_str) || time_str == "") return(NULL)
    # Parse in specified timezone, then convert to UTC
    local_ct <- as.POSIXct(time_str, tz = local_tz)
    utc_ct <- format(local_ct, tz = "UTC", usetz = FALSE)
    return(as.POSIXct(utc_ct, tz = "UTC"))
  }
  
  # Helper to format POSIXct UTC object into ISO 8601 string for XML attribute output
  format_utc_iso <- function(posix_utc) {
    if (is.null(posix_utc)) return(NULL)
    return(paste0(format(posix_utc, "%Y-%m-%dT%H:%M:%OS3"), "Z"))
  }
  
  start_ct <- parse_local_input(start_time, tz)
  end_ct   <- parse_local_input(end_time, tz)
  
  # 1. Sort files chronologically based on their event-start-time attribute
  get_start_time <- function(path) {
    doc <- xml2::read_xml(path)
    time_str <- xml2::xml_attr(doc, "event-start-time")
    t <- parse_xml_utc(time_str)
    if (is.null(t)) return(as.POSIXct(0, origin = "1970-01-01", tz = "UTC"))
    return(t)
  }
  
  file_times <- sapply(file_paths, get_start_time)
  file_paths <- file_paths[order(file_times)]
  
  # 2. Read the earliest file as base template
  base_xml <- xml2::read_xml(file_paths[1])
  
  base_triggers    <- xml2::xml_find_first(base_xml, "/EventResult/EventData/Triggers")
  base_diagnostics <- xml2::xml_find_first(base_xml, "/EventResult/EventData/DiagnosticsLog")
  base_metaitems   <- xml2::xml_find_first(base_xml, "/EventResult/EventData/MetaItems")
  base_outputs     <- xml2::xml_find_first(base_xml, "/EventResult/EventData/Outputs")
  
  overall_start <- xml2::xml_attr(base_xml, "event-start-time")
  overall_end   <- xml2::xml_attr(base_xml, "event-end-time")
  
  # 3. Append nodes from remaining files
  if (length(file_paths) > 1) {
    for (i in 2:length(file_paths)) {
      current_xml <- xml2::read_xml(file_paths[i])
      
      curr_end <- xml2::xml_attr(current_xml, "event-end-time")
      if (!is.na(curr_end)) overall_end <- curr_end
      
      triggers <- xml2::xml_find_all(current_xml, "/EventResult/EventData/Triggers/Trigger")
      for (node in triggers) xml2::xml_add_child(base_triggers, node)
      
      diagnostics <- xml2::xml_find_all(current_xml, "/EventResult/EventData/DiagnosticsLog/DiagnosticsLogEntry")
      for (node in diagnostics) xml2::xml_add_child(base_diagnostics, node)
      
      metaitems <- xml2::xml_find_all(current_xml, "/EventResult/EventData/MetaItems/MetaItem")
      for (node in metaitems) xml2::xml_add_child(base_metaitems, node)
      
      outputs <- xml2::xml_find_all(current_xml, "/EventResult/EventData/Outputs/*")
      for (node in outputs) xml2::xml_add_child(base_outputs, node)
    }
  }
  
  # 4. Trim entries outside [start_time, end_time]
  if (!is.null(start_ct) || !is.null(end_ct)) {
    timestamped_nodes <- xml2::xml_find_all(base_xml, "/EventResult/EventData//*[@timestamp]")
    trimmed_count <- 0
    
    for (node in timestamped_nodes) {
      ts_str <- xml2::xml_attr(node, "timestamp")
      node_ct <- parse_xml_utc(ts_str)
      
      if (!is.null(node_ct)) {
        if (!is.null(start_ct) && node_ct < start_ct) {
          xml2::xml_remove(node)
          trimmed_count <- trimmed_count + 1
        } else if (!is.null(end_ct) && node_ct > end_ct) {
          xml2::xml_remove(node)
          trimmed_count <- trimmed_count + 1
        }
      }
    }
  }
  
  # 5. Deduplicate records across EventData
  dedupe_container <- function(container_xpath, child_xpath) {
    container <- xml2::xml_find_first(base_xml, container_xpath)
    if (length(container) == 0) return(0)
    
    children <- xml2::xml_find_all(container, child_xpath)
    if (length(children) <= 1) return(0)
    
    seen_keys <- character(0)
    removed_count <- 0
    
    for (child in children) {
      attrs <- xml2::xml_attrs(child)
      text_content <- xml2::xml_text(child)
      tag_name <- xml2::xml_name(child)
      
      attr_string <- paste(names(attrs), attrs, collapse = "|", sep = "=")
      key <- paste(tag_name, attr_string, text_content, sep = "::")
      
      if (key %in% seen_keys) {
        xml2::xml_remove(child)
        removed_count <- removed_count + 1
      } else {
        seen_keys <- c(seen_keys, key)
      }
    }
    return(removed_count)
  }
  
  dupes_triggers    <- dedupe_container("/EventResult/EventData/Triggers", "./Trigger")
  dupes_diagnostics <- dedupe_container("/EventResult/EventData/DiagnosticsLog", "./DiagnosticsLogEntry")
  dupes_metaitems   <- dedupe_container("/EventResult/EventData/MetaItems", "./MetaItem")
  dupes_outputs     <- dedupe_container("/EventResult/EventData/Outputs", "./*")

  
  # 6. Set root timestamps (converting local input back to ISO UTC string if provided)
  final_start <- if (!is.null(start_ct)) format_utc_iso(start_ct) else overall_start
  final_end   <- if (!is.null(end_ct)) format_utc_iso(end_ct) else overall_end
  
  if (!is.null(final_start)) xml2::xml_attr(base_xml, "event-start-time") <- final_start
  if (!is.null(final_end))   xml2::xml_attr(base_xml, "event-end-time")   <- final_end
  
  # 7. Save output
  xml2::write_xml(base_xml, output_path)
  message(sprintf("Successfully merged %d files into '%s'.", length(file_paths), output_path))
  
  return(invisible(base_xml))
}
