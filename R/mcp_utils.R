# Small shared helpers for the Certara MCP host engine.

# ISO-8601 local timestamp, used in memory records and other audit fields.
.mcp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Absolute path to the Rscript binary of the running R, baked into the launch
# command written by write_mcp_config().
.mcp_rscript_bin <- function() {
  bin <- file.path(R.home("bin"),
                   if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  normalizePath(bin, mustWork = FALSE)
}
