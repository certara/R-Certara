# Certara.R: Certara Pharmacometrics R Ecosystem and MCP Server

Meta-package for the Certara pharmacometrics ecosystem. Attaching
`Certara.R` loads the suite of Certara modeling, simulation, and
reporting packages (see
[`certara_attach()`](https://github.com/certara/R-Certara/reference/certara_attach.md)),
and the package hosts the federated Certara Model Context Protocol (MCP)
server (see
[`launch_certara_mcp()`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)).

## Details

`Certara.RsNLME` is declared in `Depends` so
[`library(Certara.R)`](https://github.com/certara/R-Certara) attaches it
for interactive use, matching the package's stated purpose; this package
itself calls provider packages generically by name (installed-package
discovery, [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html))
rather than any specific `Certara.RsNLME` function, so the `@import`
below exists only to record that intentional `Depends` relationship for
`R CMD check`.

## See also

Useful links:

- <https://github.com/certara/R-Certara>

- Report bugs at <https://github.com/certara/R-Certara/issues>

## Author

**Maintainer**: James Craig <james.craig@certara.com>

Authors:

- James Craig <james.craig@certara.com>

- Michael Tomashevskiy <michael.tomashevskiy@certara.com>

Other contributors:

- Certara USA, Inc. \[copyright holder, funder\]
