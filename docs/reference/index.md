# Package index

## Ecosystem

Attach the Certara pharmacometrics R suite and inspect its membership

- [`certara_attach()`](https://github.com/certara/R-Certara/reference/certara_attach.md)
  : Attach the Certara pharmacometrics ecosystem

- [`certara_packages()`](https://github.com/certara/R-Certara/reference/certara_packages.md)
  : List all packages developed by Certara

- [`certara_core_packages()`](https://github.com/certara/R-Certara/reference/certara_core_packages.md)
  :

  Core ecosystem packages attached by
  [`library(Certara.R)`](https://github.com/certara/R-Certara)

## MCP setup and diagnostics

Configure, run, and troubleshoot the Certara MCP server

- [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  : Write MCP client configuration for the Certara server
- [`remove_mcp_config()`](https://github.com/certara/R-Certara/reference/remove_mcp_config.md)
  : Remove the Certara MCP server from client configuration
- [`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
  : Launch the Certara MCP server
- [`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
  : Report the Certara MCP session and execution context
- [`list_certara_mcp_servers()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_servers.md)
  : List running Certara MCP server processes
- [`list_certara_mcp_configs()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_configs.md)
  : Inventory Certara MCP entries across client configuration files

## Memory controls

Manage the local, opt-in memory used by Certara MCP tools

- [`enable_memory()`](https://github.com/certara/R-Certara/reference/enable_memory.md)
  : Enable per-user memory
- [`disable_memory()`](https://github.com/certara/R-Certara/reference/disable_memory.md)
  : Disable per-user memory (does not delete existing records)
- [`list_memory_records()`](https://github.com/certara/R-Certara/reference/list_memory_records.md)
  : List all per-user memory records
- [`export_memory()`](https://github.com/certara/R-Certara/reference/export_memory.md)
  : Export all per-user memory records to a JSON file
- [`delete_memory_record()`](https://github.com/certara/R-Certara/reference/delete_memory_record.md)
  : Delete a single per-user memory record by id
- [`clear_memory()`](https://github.com/certara/R-Certara/reference/clear_memory.md)
  : Clear all per-user memory records

## Reports and outputs

Choose the project output location and render the accumulated report

- [`mcp_session_project_dir()`](https://github.com/certara/R-Certara/reference/mcp_session_project_dir.md)
  : Session project root for MCP deliverables
- [`render_certara_report()`](https://github.com/certara/R-Certara/reference/render_certara_report.md)
  : Render the accumulated report Rmd
