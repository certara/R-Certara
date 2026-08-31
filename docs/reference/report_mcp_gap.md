# Report an MCP capability gap encountered in-session

Records a structured gap the agent hit when a tool worked but could not
express what the user asked for. Stored inactive-by-default under the
per-user memory gaps store and surfaced by
[`list_memory_records()`](https://github.com/certara/R-Certara/reference/list_memory_records.md).
Does not require
[`enable_memory()`](https://github.com/certara/R-Certara/reference/enable_memory.md) -
gap reporting is always on so maintainers get a signal even when the
analyst has not opted into lessons/preferences.

## Usage

``` r
report_mcp_gap(
  tool,
  task,
  missing_capability,
  attempted_args = NULL,
  workaround = NULL,
  session_id = NULL
)
```

## Arguments

- tool:

  Tool name that was insufficient (or `"none"`).

- task:

  What the user asked for.

- missing_capability:

  Short description of the missing capability.

- attempted_args:

  Optional JSON text / list of args that were tried.

- workaround:

  Optional description of the fallback used (or `"none"`).

- session_id:

  Optional session identifier.

## Value

A list with the stored `id` and the record.
