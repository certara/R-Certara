# Certara MCP Server: Setup and Usage

## For AI agents: setup checklist

> **For humans.** Point your AI assistant at this page and say: *“Set up
> the certara-r MCP
> <https://certara.github.io/R-Certara/articles/mcp_setup.md>”*. The
> agent works through the checklist below and pauses whenever it needs
> you (to approve a change, install the NLME Engine, sign in for a
> license, or restart the client).

**Agent instructions.** This document is the source of truth. Work
through the steps in order. Follow these ground rules throughout:

- **Check before changing.** Inspect what is already in place and skip
  any step that is already satisfied.
- **Ask before acting.** Tell the user what you plan to do and wait for
  their OK before installing software or editing any config file.
- **Back up and merge.** Before editing a config file, make a
  timestamped copy
  (e.g. `claude_desktop_config.json.bak-20261125-0630`). Merge the
  `certara-r` entry in; never overwrite other servers or settings.
- **Pause for manual steps.** When a step needs the user (a download, a
  browser sign-in, a restart), stop, explain exactly what to do, and
  resume once they confirm.
- **Never echo secrets.** Do not print `.Renviron` contents, API keys,
  or license tokens.
- **Report errors in full.** Do not hide or paraphrase failing output.
- **Do not change system settings** (PATH, system environment variables)
  or delete or move R libraries yourself. Report and recommend instead.

### Step 1: Identify the client and platform

Ask which client is being set up: **Cursor**, **Claude Code** (the
`claude` CLI), **Claude Desktop** (its **Code** tab), or **Codex**.
Several can be set up in one pass; repeat steps 7-10 for each.

### Step 2: Claude Desktop only – anchor the session in a folder

In the Claude Desktop **Code** tab, the chat session must be anchored to
a folder on this machine (select a folder when starting the session).
Without one, NLME Engine runs fail with a PowerShell error saying the
script cannot be executed or is not authorized. Tell the user this now;
if the current session is not anchored, ask them to start a new one in a
folder (for example their project folder) before any fits are run.

### Step 3: Check R

1.  Run `Rscript --version`. If `Rscript` is not on `PATH`, look under
    `C:\Program Files\R\` and tell the user the exact folder to add to
    `PATH` (e.g. `C:\Program Files\R\R-4.6.0\bin`). Record the full path
    to `Rscript.exe` for later steps.

2.  R must be **4.0 or later**. Compare with the current release on
    [CRAN](https://cran.r-project.org/). If R is missing or below 4.0,
    tell the user where to download it and pause.

3.  Check for non-standard library locations:

    ``` r

    .libPaths()
    Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"))
    ```

    Also check the user and system environment variables for `R_LIBS`,
    `R_LIBS_USER`, and `R_LIBS_SITE`, and look for `.Renviron` or
    `Rprofile.site` files that set library paths.

4.  If a custom library folder is configured, check whether it holds
    packages built under different R versions (look at the `Built:`
    field in the `DESCRIPTION` of a few packages). A library shared
    across R versions can load packages built for the wrong version or
    hide the Certara packages; recommend a separate library per R
    version, or the default per-version library. Report only.

5.  If the user keeps a custom library set through an OS environment
    variable, note the variable and its value: Claude Desktop needs it
    in the `env` block (step 8).

### Step 4: Install the R packages

Show the user the repositories before installing: the Certara JFrog CRAN
repository plus CRAN.

``` r

install.packages(
  "Certara.R",
  dependencies = TRUE,
  repos = c(
    "https://certara.jfrog.io/artifactory/certara-cran-release-public/",
    "https://cloud.r-project.org"
  )
)
```

Then verify:

``` r

library(Certara.R)
packageVersion("Certara.R")
stopifnot(
  packageVersion("btw") >= "1.2.0",
  packageVersion("mcptools") >= "0.2.0",
  packageVersion("ellmer") >= "0.4.0"
)
```

If the helper packages are missing, install them with
`install.packages(c("btw", "mcptools", "ellmer"))`. See [Prerequisites
and installation](#prerequisites-and-installation) for details.

### Step 5: Check the NLME Engine (version 26.9.1)

``` r

installdir <- Sys.getenv("INSTALLDIR")
if (!nzchar(installdir)) installdir <- "C:/Program Files/Certara/NLME_Engine"
Certara.NLME8::checkInstallDir(installdir)
system2(file.path(installdir, "TDL5.exe"), "-v", stdout = TRUE, stderr = TRUE)
# Expect a line like: "TDL5 version: 26.9.1.1"
```

- **Engine missing or not 26.9.1:** pause. Tell the user to open
  [Software
  Downloads](https://help.certara.net/en/articles/7981602-software-downloads)
  in the Certara Help Center, go to the **RsNLME** product section,
  download **NLME Engine 26.9.1 for Windows** (or **for Linux**), and
  run the installer. If their IT department manages installs, the engine
  may be available in Company Portal instead. Resume when they confirm,
  then re-run the check.
- **GCC compiler.** Check `Sys.getenv("NLMEGCCDir64")` and that the
  folder exists. The standalone engine installer uses
  `C:\Program Files\Certara\mingw64\`; a Phoenix installation uses
  `C:\PHSTMinGW64`. Either is valid.
- **MS-MPI runtime.** Check `Sys.getenv("PhoenixMSMPIDir")` and that the
  folder exists. The standalone installer uses
  `C:\Program Files\Certara\MPI\`; a Phoenix installation uses
  `C:\Program Files (x86)\Certara\MPI\`. Either is valid. Without MPI,
  runs still work but execute serially.

Report anything missing.

### Step 6: Check the NLME license (Certara Admin)

``` r

system2(file.path(installdir, "TDL5.exe"), "/license_check",
        stdout = TRUE, stderr = TRUE)
# Look for "Status: OK" and the "License expires:" line.
```

If the status is not OK, pause and explain the next step:
[`Certara.RsNLME::obtain_NLMELicense()`](https://certara.github.io/R-RsNLME/reference/obtain_NLMELicense.html)
opens a browser window where the user signs in to Certara Admin (CAD).
Either run it for them or ask them to run it in RStudio, wait for them
to finish the browser sign-in, then re-run the license check.

``` r

Certara.RsNLME::obtain_NLMELicense(InstallDir = installdir, verbose = TRUE)
```

Users without a license can request a trial from
`certara.licensing@certara.com`.

### Step 7: Write the client configuration

Use
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
at user scope (the server is then available in every project). Use the
call for the client from step 1:

``` r

Certara.R::write_mcp_config(client = "cursor",         scope = "user", run = TRUE)
Certara.R::write_mcp_config(client = "claude-code",    scope = "user", run = TRUE)
Certara.R::write_mcp_config(client = "claude-desktop", scope = "user", run = TRUE)
Certara.R::write_mcp_config(client = "codex",          scope = "user", run = TRUE)
```

**Claude Desktop: confirm the config file first.** The file Claude
Desktop reads depends on how it was installed:

- Installed by the user from Anthropic’s website:
  `%APPDATA%\Claude\claude_desktop_config.json`
  (`C:\Users\<you>\AppData\Roaming\Claude\claude_desktop_config.json`).
- Installed from the Microsoft Store or by IT: a packaged folder whose
  name has random characters after `Claude`, such as
  `%LOCALAPPDATA%\Packages\Claude_<suffix>\LocalCache\Roaming\Claude\claude_desktop_config.json`.

Ask the user to open **File \> Settings \> Desktop app**, click
**Manage** next to **Extensions**, then click **Edit config** (under
*Local MCP servers*), and tell you the path of the file that opens.
Compare it with the path
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
reports. If they differ, the file **Edit config** opens is the one
Claude Desktop reads: back it up and merge the same `certara-r` entry
(`command`, `args`, `type`) into it.

### Step 8: Claude Desktop on Windows only – add the `env` block

Claude Desktop starts MCP servers with a stripped-down environment, and
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
does **not** write an `env` block. Without these variables MCP-launched
fits fail with `Compile/Link Failed` (see [Claude Desktop on Windows:
required environment
variables](#claude-desktop-on-windows-required-environment-variables)
for why each variable is needed).

Run this in R on the user’s machine to print the real values. Fill in
`extra_vars` with any custom library variable kept in step 3:

``` r

extra_vars <- character()  # e.g. c("R_LIBS") if kept in step 3

env_or <- function(var, default) {
  value <- Sys.getenv(var)
  if (nzchar(value)) value else default
}
first_dir <- function(...) {
  dirs <- sub("[\\\\/]+$", "", c(...))
  dirs <- dirs[nzchar(dirs) & dir.exists(dirs)]
  if (length(dirs)) paste0(normalizePath(dirs[1], winslash = "\\"), "\\") else ""
}

env <- list(
  ComSpec = env_or("ComSpec", file.path(env_or("SystemRoot", "C:\\WINDOWS"),
                                        "system32", "cmd.exe", fsep = "\\")),
  INSTALLDIR = sub("\\\\$", "", first_dir(
    Sys.getenv("INSTALLDIR"), "C:/Program Files/Certara/NLME_Engine"
  )),
  NLMEGCCDir64 = first_dir(
    Sys.getenv("NLMEGCCDir64"), "C:/Program Files/Certara/mingw64",
    "C:/PHSTMinGW64"
  ),
  PhoenixMSMPIDir = first_dir(
    Sys.getenv("PhoenixMSMPIDir"), "C:/Program Files/Certara/MPI",
    "C:/Program Files (x86)/Certara/MPI"
  ),
  TMP = env_or("TMP", env_or("TEMP", tempdir())),
  NUMBER_OF_PROCESSORS = env_or("NUMBER_OF_PROCESSORS",
                                as.character(parallel::detectCores())),
  PATHEXT = env_or("PATHEXT",
                   ".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC")
)
for (v in extra_vars) env[[v]] <- Sys.getenv(v)
env <- env[nzchar(unlist(env))]

cat(jsonlite::toJSON(list(env = env), auto_unbox = TRUE, pretty = TRUE))
```

Any expected variable missing from the output means the corresponding
component was not found (step 5); resolve that first. Then, after
backing up the config file confirmed in step 7, merge the block into the
`certara-r` entry and check the result:

``` r

cfg_path <- "<config file confirmed in step 7>"
file.copy(cfg_path, paste0(cfg_path, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S")))

cfg <- jsonlite::read_json(cfg_path, simplifyVector = FALSE)
stopifnot(!is.null(cfg$mcpServers[["certara-r"]]))
cfg$mcpServers[["certara-r"]]$env <- env
jsonlite::write_json(cfg, cfg_path, auto_unbox = TRUE, pretty = TRUE, null = "null")

str(jsonlite::read_json(cfg_path)$mcpServers[["certara-r"]]$env)
```

The finished entry looks like the example in [Claude Desktop on Windows:
required environment
variables](#claude-desktop-on-windows-required-environment-variables).

> **Warning.** Re-running `write_mcp_config(client = "claude-desktop")`
> replaces the whole `certara-r` entry and **deletes the `env` block**.
> Repeat this step after any re-run.

Other clients normally inherit the full environment from the terminal or
IDE and do not need an `env` block. Add one only if a custom library
variable from step 3 is not visible to the MCP server, or if fits fail
as described above.

### Step 9: Restart the client

- **Claude Desktop:** fully quit (right-click the system-tray icon and
  choose **Quit**; closing the window is not enough), reopen it, and
  start a new **Code** session anchored in a folder (step 2).
- **Cursor:** open **Settings \> Tools & MCPs** and enable or refresh
  `certara-r`.
- **Claude Code / Codex:** start a new session.

The user has to do this, and for Claude Desktop, Claude Code, and Codex
the current chat session ends. Before the restart, summarize progress so
far and tell the user what to say when they are back (for example,
*“continue the certara-r MCP setup at step 10”*).

### Step 10: Verify the connection

1.  Call `certara_mcp_capabilities` and `certara_session_status`, and
    list a few Certara tools (`mcp__certara-r__*` in Claude,
    `user-certara-r` in Cursor).
2.  Optionally run the `env-check` job from the [environment variables
    section](#claude-desktop-on-windows-required-environment-variables)
    to confirm `ComSpec`, `INSTALLDIR`, `NLMEGCCDir64`, and
    `PhoenixMSMPIDir` reach the fit processes.
3.  If the server does not connect:
    - run
      [`Certara.R::list_certara_mcp_configs()`](https://github.com/certara/R-Certara/reference/list_certara_mcp_configs.md)
      to see which config files hold a `certara-r` entry;
    - read the client’s MCP logs (for Claude Desktop, the `logs` folder
      next to the config file, e.g. `%APPDATA%\Claude\logs`);
    - check the usual causes: a missing `env` variable, the entry
      written to a file the client does not read (step 7), or the MCP
      server’s R seeing different
      [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) than the
      user’s interactive R (step 3).

### Step 11: Final report

Give the user a short checklist of what passed, what you changed
(including the paths of any backup files), and anything they still need
to do by hand.

## Quick start

Already have `Certara.R` installed? Configure your assistant with a
single call, then reload the client. These use `scope = "user"` (enabled
in every current and future project) and `run = TRUE` (register via the
client CLI when applicable). Pick your client below. New to the details?
Read on past the quick start.

- Cursor
- Claude Code
- Claude Desktop
- Codex

``` r

Certara.R::write_mcp_config(client = "cursor", scope = "user", run = TRUE)
```

``` r

Certara.R::write_mcp_config(client = "claude-code", scope = "user", run = TRUE)
```

``` r

Certara.R::write_mcp_config(client = "claude-desktop", scope = "user", run = TRUE)
```

``` r

Certara.R::write_mcp_config(client = "codex", scope = "user", run = TRUE)
```

## What the Certara MCP server is

`Certara.R` hosts an optional, federated [Model Context Protocol
(MCP)](https://modelcontextprotocol.io/) server that exposes the Certara
ecosystem’s knowledge base and tool providers – NLME modeling via
`Certara.RsNLME`, machine-learning model search via `Certara.RDarwin`,
VPC via `tidyvpc`, and more – to AI coding assistants.

Supported clients:

- **Cursor**
- **Claude Code** – the `claude` command-line agent (terminal / CLI)
- **Claude Desktop** – the desktop app, including its **Code** tab
- **Codex**

The MCP server is **optional**. Building, fitting, and reporting models
with `Certara.RsNLME` and the rest of the suite does not require it; it
is a convenience layer for AI-assisted workflows. When enabled, agents
can author and validate PML, launch and watch fits asynchronously, run
diagnostics, and emit audit-ready R scripts, all grounded in the same
knowledge base the packages ship.

## Prerequisites and installation

The MCP stack ships as dependencies of `Certara.R`. Installing the
ecosystem meta-package pulls everything the server needs:

``` r

install.packages(
  "Certara.R",
  dependencies = TRUE,
  repos = c(
    "https://certara.jfrog.io/artifactory/certara-cran-release-public/",
    "https://cloud.r-project.org"
  )
)
```

RsNLME users who install the full suite via the `Certara_Packages`
vector in the [RsNLME Installation
Guide](https://certara.github.io/R-RsNLME/articles/installation.html)
already have `Certara.R`.

The MCP layer has three required helpers with version floors:

- `btw (>= 1.2.0)`
- `mcptools (>= 0.2.0)`
- `ellmer (>= 0.4.0)`

A normal release install pulls these automatically. A developer install
with `repos = NULL` (installing a source tarball directly) skips
`Suggests` and can leave them missing; install them explicitly in that
case:

``` r

install.packages(c("btw", "mcptools", "ellmer"))
```

The provider packages that require Certara’s NLME Engine (for example
`Certara.RsNLME`) still need the engine installed and `INSTALLDIR` set;
see the [RsNLME Installation
Guide](https://certara.github.io/R-RsNLME/articles/installation.html).

## Configure a client with `write_mcp_config()`

[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
writes (or merges) the MCP server entry into the client’s own
configuration file and, where supported, pre-authorizes the Certara
tools. You run it once per client from an R console; the client then
launches the server automatically.

``` r

# Cursor, project scope -- writes/merges .cursor/mcp.json
Certara.R::write_mcp_config(client = "cursor", scope = "project")

# Claude Code, project scope -- writes/merges .mcp.json
Certara.R::write_mcp_config(client = "claude-code", scope = "project")

# Claude Code, user scope -- registered via the claude CLI when on PATH
Certara.R::write_mcp_config(client = "claude-code", scope = "user", run = TRUE)

# Claude Desktop (user scope) -- merges claude_desktop_config.json
Certara.R::write_mcp_config(client = "claude-desktop")

# Codex -- writes the managed block to ~/.codex/config.toml directly
Certara.R::write_mcp_config(client = "codex")
```

What each client writes:

- **Cursor** – the server entry in `.cursor/mcp.json` (project) or
  `~/.cursor/mcp.json` (user), plus a
  `.cursor/rules/certara-mcp-usage.mdc` routing rule at project scope.
- **Claude Code** – `.mcp.json` at project scope; user/local scopes are
  managed through the `claude` CLI. A `certara-mcp-usage.md` guidance
  doc is imported via `CLAUDE.md`.
- **Claude Desktop** – merged into `claude_desktop_config.json` (user
  scope only). On Windows the file’s location depends on how Desktop was
  installed: `%APPDATA%\Claude\` for a self-install, or a
  `Claude_<suffix>` package folder under `%LOCALAPPDATA%\Packages\` for
  Microsoft Store/IT installs. Confirm the real path with **File \>
  Settings \> Desktop app \> Extensions (Manage) \> Edit config**. Fully
  quit and relaunch Desktop after a config change.
  [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  does not write an `env` block, and re-running it replaces the entry,
  deleting any `env` block you added. On Windows, also see [Claude
  Desktop on Windows: required environment
  variables](#claude-desktop-on-windows-required-environment-variables)
  – MCP-launched fits need a few variables added to the `env` block.
- **Codex** – a managed block in `~/.codex/config.toml` (the only way to
  set Codex per-tool approvals); the equivalent `codex mcp add` command
  is printed for reference.

Key arguments:

- `scope` – where the configuration is written, which controls where the
  server is available:
  - `"project"` (default) – writes into the current project folder (e.g.
    `.cursor/mcp.json`), so the server is enabled **only when you open
    that folder**. Choose this for per-analysis setup that can be
    version-controlled with the project.
  - `"user"` – writes into your home-directory config (e.g.
    `~/.cursor/mcp.json`), so the server is enabled **globally – in
    every current and future project** on your machine.
  - `"local"` (Claude Code only) – a private, per-project entry in
    `~/.claude.json` that is not shared with collaborators through the
    project’s `.mcp.json`.
- `tool_allowlist` – when `TRUE` (default), pre-authorize the Certara
  tools so they run without per-tool approval prompts, using each
  client’s native mechanism (Cursor `permissions.json`, Claude Code
  `settings.json`, Codex approval settings). Claude Desktop has no
  config-file allowlist, so approve tools in the UI per session.
- `run` – controls whether the CLI-managed clients are configured *for*
  you. By default (`FALSE`)
  [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  only **prints** the client CLI command so you can inspect and run it
  yourself. Setting `run = TRUE` executes that command when the client
  CLI is on your `PATH` (currently the Claude Code user/local scopes,
  via `claude mcp add`), falling back to printing if the CLI is not
  found. File-based targets (Cursor, Claude Code project scope, Codex,
  Claude Desktop) are always written directly, so `run` does not affect
  them.

Use
[`remove_mcp_config()`](https://github.com/certara/R-Certara/reference/remove_mcp_config.md)
to undo a configuration:

``` r

# Remove the Cursor project-scope server written above.
Certara.R::remove_mcp_config(client = "cursor", scope = "project")
```

## Claude Desktop on Windows: required environment variables

> **Quick tip.** Short on time? Point your Claude session at this
> article (share the page, or paste this section) and ask it to
> *“diagnose and fill in the environment variables that Claude Desktop
> sanitizes out of my `claude_desktop_config.json`.”* With filesystem
> access the agent can run the env-check job below, see which variables
> are empty, and add the `env` block to your config for you – you then
> just **fully quit and restart Claude Desktop** for it to take effect.

**This applies to Claude Desktop on Windows when you launch NLME fits
through the MCP job tools** (`start_nlme_fit_spec`, `start_nlme_job`,
`start_nlme_fitmodel`, the SCM scripts, etc.).

**Symptom.** MCP-launched fits fail at the compile stage with
`Compile/Link Failed`, even though the *same model fits fine* from
RStudio or a plain `Rscript` run in a terminal.

**Cause.** Claude Desktop launches MCP servers with a **sanitized,
minimal environment** – it does not pass your full user/system
environment to the server process. On Windows a child process inherits a
**frozen snapshot** of its parent’s environment (it never re-reads the
registry), and the job launcher copies the server’s environment into
every spawned fit. So whatever Claude Desktop omits is missing in the
server *and* in each fit child:

``` text
Claude Desktop  ->  Rscript.exe (MCP server)  ->  job child (Rscript -> PowerShell -> TDL5/gcc)
```

The critical omission is **`ComSpec`**: the engine’s PowerShell wrapper
shells out with `& $env:comspec /c ...`, and with `ComSpec` empty every
shell-out fails, which surfaces as `ERROR in generating Model.cpp` -\>
`Compile/Link Failed`. `NLMEGCCDir64` and `INSTALLDIR` are also
typically dropped, so the compile either cannot find `TDL5.exe`/engine
libraries or falls back to an incompatible gcc (e.g. RTools) instead of
the Certara-shipped GCC 8.4.0.

**Fix.** Declare the variables explicitly in the server’s `env` block in
`claude_desktop_config.json`, then fully restart Claude Desktop:

``` json
"mcpServers": {
  "certara-r": {
    "command": "C:\\Program Files\\R\\R-4.6.0\\bin\\x64\\Rscript.exe",
    "args": [
      "-e",
      "Certara.R::launch_certara_mcp(btw_groups = c('docs'), session_tools = FALSE, server_name = 'certara-r', job_watch_wait_seconds = 600, tool_profile = 'full')"
    ],
    "type": "stdio",
    "env": {
      "ComSpec": "C:\\WINDOWS\\system32\\cmd.exe",
      "INSTALLDIR": "C:\\Program Files\\Certara\\NLME_Engine",
      "NLMEGCCDir64": "C:\\Program Files\\Certara\\mingw64\\",
      "TMP": "C:\\Users\\<you>\\AppData\\Local\\Temp",
      "NUMBER_OF_PROCESSORS": "<your core count>",
      "PATHEXT": ".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC",
      "PhoenixMSMPIDir": "C:\\Program Files\\Certara\\MPI\\"
    }
  }
}
```

Notes:

- The `command` and `args` are the ones
  `write_mcp_config("claude-desktop")` already wrote – you are only
  **adding** the `env` block. Edit the file Claude Desktop actually
  reads: open **File \> Settings \> Desktop app \> Extensions (Manage)
  \> Edit config** to see its path (Microsoft Store/IT installs use a
  `Claude_<suffix>` package folder rather than `%APPDATA%\Claude`). If
  it differs from the path
  [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  reported, use the **Edit config** file.
- [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  does not write this block, and re-running it replaces the whole
  `certara-r` entry, so the `env` block must be added again afterwards.
  [Step 8 of the agent
  checklist](#step-8-claude-desktop-on-windows-only-add-the-env-block)
  has an R snippet that prints this machine’s values and merges them in.
- Replace `<you>` and `<your core count>` with your values.
- `ComSpec` is the one that actually unblocks `Compile/Link Failed`; the
  gcc compatibility warning is a red herring, but set `NLMEGCCDir64` too
  so the Certara GCC 8.4.0 is used. `INSTALLDIR` must point at your
  NLME-Engine install.
- `TMP`, `NUMBER_OF_PROCESSORS`, and `PATHEXT` are commonly dropped as
  well (`TEMP`/`USERPROFILE`/`SystemRoot` usually survive); include them
  to be safe. The full default allowlist Claude Desktop passes on
  Windows is `APPDATA`, `HOMEDRIVE`, `HOMEPATH`, `LOCALAPPDATA`, `PATH`,
  `PROCESSOR_ARCHITECTURE`, `SYSTEMDRIVE`, `SYSTEMROOT`, `TEMP`,
  `USERNAME`, and `USERPROFILE` – anything else must be declared
  explicitly.
- **`PhoenixMSMPIDir` (optional – for MPI parallelization).** Locates
  the MS-MPI runtime that RsNLME execution hosts use when
  `hostParams(parallelMethod = "LOCAL_MPI")` (the directory holding
  `mpiexec.exe` / `msmpi.dll`). Unlike the variables above, **its
  absence is silent, not an error**: the run does not fail – the job log
  prints
  `MPI not found on the system. Using localhost without parallelization.`
  and proceeds **serially**. A single small fit barely notices, but SCM
  stepwise/shotgun covariate searches and bootstraps lose all
  scenario-level parallelism, so MCP-launched runs come out much slower
  than the identical run from RStudio/terminal. Add it (value
  `C:\Program Files\Certara\MPI\`, trailing backslash) to restore
  parallel execution; a correctly-wired run instead logs
  `Using MPI host with N cores` and `[MPI plan] ... method=local_mpi`.
- **Restart is required.** Claude Desktop reads MCP config only at
  startup – fully quit it (system tray -\> Quit, not just closing the
  window) and relaunch.

**Diagnose it yourself.** Run a throwaway MCP job that prints what the
child actually sees, then read `stdout.log` in the returned `run_dir`
(empty values mean the variables are not reaching the fit children):

``` r

start_nlme_job(
  expr = "cat('ComSpec=', Sys.getenv('ComSpec'), '\n');
          cat('INSTALLDIR=', Sys.getenv('INSTALLDIR'), '\n');
          cat('NLMEGCCDir64=', Sys.getenv('NLMEGCCDir64'), '\n');
          cat('PhoenixMSMPIDir=', Sys.getenv('PhoenixMSMPIDir'), '\n')",
  label = "env-check", project_dir = "<your project>"
)
```

`<run_dir>/artifacts/GEN*/Shared/compilelog.txt` shows the actual
compile-stage error rather than the generic `Compile/Link Failed`. For
parallelization, the job’s stderr distinguishes the two states directly:
`MPI not found on the system. Using localhost without parallelization.`
(serial) versus `Using MPI host with N cores` /
`[MPI plan] ... method=local_mpi` (parallel).

**Claude Code (CLI) generally does not need this.** A terminal-launched
MCP server inherits the shell’s full environment, so `ComSpec` and the
Certara toolchain variables are already present. Caveats: a terminal
only snapshots the environment when it opens (open a new one after
changing system variables), and IDE-embedded terminals inherit from the
IDE process (restart the IDE). If a launcher ever sanitizes the
environment, the same `env`-block fix applies to `.mcp.json` /
`claude mcp add`.

## Tool profiles

The launch command bakes in a curated tool subset so the agent’s tool
list stays focused. Pass `tool_profile` to
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md):

- **`full`** (default) – every tool every provider offers.
- **`core`** – read/author/validate and data inspection only; no job
  launches.
- **`authoring`** – `core` plus model comparison; still no job launches.
- **`execution`** – `core` plus fit/search job launches and their
  results/qualification tools.
- **`diagnostics`** – `execution` plus comparison and interpretation.

[`find_certara_tools()`](https://github.com/certara/R-Certara/reference/find_certara_tools.md),
[`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md),
and
[`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
are present in every profile.

`full` is the default and is a fine choice most of the time – there is
no correctness penalty to exposing every tool. Narrow the profile when
you want to:

- **reduce tool overload** – a shorter, task-shaped tool list is easier
  for the agent to reason over and lowers the chance it reaches for a
  wrong or heavier tool;
- **gate risky actions** – `core` and `authoring` expose no job-launch
  tools (`start_*`), so the agent can read, author, and validate models
  but cannot kick off fits or searches on its own; and
- **trim the startup/latency footprint** on constrained clients.

Pick the narrowest profile that still contains the tools your task
needs, and keep `full` when you want the complete surface available.

## `btw_groups` and live-session tools

`btw_groups` controls which general-purpose `btw` R tool groups are
exposed alongside the Certara tools. The default `"docs"` provides
read-only R documentation lookup and is the leanest, lowest-latency
option. Other useful groups include `"pkg"` (package-development
actions), and – for a live R session bridged with
[`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
– `"env"` (inspect that session’s objects) and `"run"` (execute R code
there via `btw_tool_run_r`).

``` r

# Expose docs + package-dev tools, plus a bridge to a live RStudio session.
Certara.R::write_mcp_config(
  client        = "cursor",
  btw_groups    = c("docs", "pkg", "env", "run"),
  session_tools = TRUE
)
# Then, in the RStudio session you want the agent to reach:
btw::btw_mcp_session()
```

`session_tools = TRUE` exposes the live-session bridge but does not by
itself connect a session – after writing the config, reload the MCP
server, run
[`btw::btw_mcp_session()`](https://posit-dev.github.io/btw/reference/mcp.html)
in the target session, and have the agent attach to it.

Three R contexts are in play and are easy to confuse:

- the **server process** that answers control-plane tool calls,
- the **bridged live session** (`env` inspection and `run` execution),
  and
- the **per-job child process** launched by the `start_nlme_*` tools.

[`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
reports which of these are active and how to attach a session.

**When to enable it.** Keep `session_tools = FALSE` (the default) for
the common case: the server runs in its own clean R process, starts
faster, and cannot disturb your interactive workspace. Set
`session_tools = TRUE` when you want the agent to work against *your*
live RStudio session – to read objects you have already loaded (a
prepared dataset, a fitted model) or to run R code in that session –
instead of re-loading everything in the isolated server process. Pair it
with `btw_groups` `"env"` (inspect objects) and/or `"run"` (execute
code).

## Your first MCP session

1.  **Reload the client** so it picks up the new configuration (in
    Cursor: Settings -\> Tools & MCPs, then enable the Certara R
    server).
2.  **Call
    [`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md)
    first.** It reports the concurrency policy, the Certara-first rule,
    the discovered tool/KB providers, the active tool profile, and the
    merged workflow phases (the canonical routing map).
3.  **Mind Cursor’s routing key.** The key in `.cursor/mcp.json` is
    `certara-r`, but Cursor’s tool-call API addresses it as
    `user-certara-r`. The project-scope
    `.cursor/rules/certara-mcp-usage.mdc` records this, and
    [`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md)
    reports the mapping under `client_routing`.
4.  **Look up knowledge guidance-first.** For “what should I do” /
    regulatory / fit-for-purpose questions, call
    `guide_pharmacometrics(task, intended_use)` before reaching for
    reference lookups
    ([`search_certara_kb()`](https://github.com/certara/R-Certara/reference/search_certara_kb.md),
    [`lookup_pml_symbol()`](https://github.com/certara/R-Certara/reference/lookup_pml_symbol.md))
    or `btw` documentation tools.

## Approving tool calls

By default, coding assistants prompt you to approve **every** tool call.
Depending on your client and settings, the agent may ask you to
“Approve” the same Certara tools over and over – which, for a server you
trust and use constantly, quickly becomes a full-time job of clicking
*Approve*. The durable fix is to **allowlist the whole `certara-r`
server** so its tools run without prompts.

`write_mcp_config(tool_allowlist = TRUE)` (the default) already sets
this up for the clients that support a config-file allowlist:

- **Cursor** – merges `certara-r:*` into `permissions.json`; takes
  effect once you enable Cursor Run Mode (Allowlist or Auto-review).
- **Claude Code** – merges `mcp__certara-r` into `permissions$allow` of
  the scope-appropriate `settings.json` (`~/.claude/settings.json` for
  user, `<project>/.claude/settings.json` for project,
  `<project>/.claude/settings.local.json` for local).
- **Codex** – writes per-tool approvals into `~/.codex/config.toml`
  (most tools approved; privileged cleanup and arbitrary-R job tools
  stay prompt-gated).

To do it by hand for Claude Code, add the server-wide rule to
`settings.json`:

``` json
{
  "permissions": {
    "allow": ["mcp__certara-r"]
  }
}
```

`mcp__certara-r` authorizes **every** tool from the server. To approve a
single tool instead, use the fully-qualified form
`mcp__certara-r__<tool>` (e.g.
`mcp__certara-r__certara_mcp_capabilities`). Note that Claude Code does
**not** support `*` wildcards for MCP tools, so use the bare server rule
`mcp__certara-r` for “all tools” rather than `mcp__certara-r__*`. Reload
the client after editing.

**Claude Desktop is the exception.** It has no working config-file
allowlist – “Always allow” / “Allow for all tasks” does not persist
across sessions (upstream
[claude-code#24433](https://github.com/anthropics/claude-code/issues/24433),
[\#56954](https://github.com/anthropics/claude-code/issues/56954)),
which is why `write_mcp_config("claude-desktop")` only prints a note
instead of writing one. If the Desktop **Code** tab honors
`~/.claude/settings.json`, the `mcp__certara-r` rule above silences the
prompts; if it uses the desktop app’s own agent path instead, you are
limited to clicking “Always allow” per session until Anthropic ships a
persistent setting.

## Inventory, status, and removal

Two read-only console helpers distinguish what is *configured* from what
is *running*:

``` r

# What Certara MCP entries are written to client config files right now?
Certara.R::list_certara_mcp_configs(client = "cursor", project_dir = tempdir())
#>   client   scope
#> 1 cursor project
#> 2 cursor    user
#> 3 cursor   local
#>                                                                   path exists
#> 1 C:\\Users\\jcraig\\AppData\\Local\\Temp\\Rtmp4avxQz/.cursor/mcp.json  FALSE
#> 2                                   C:\\Users\\jcraig/.cursor/mcp.json   TRUE
#> 3                                                                 <NA>     NA
#>   configured server_key         status
#> 1      FALSE       <NA> not_configured
#> 2       TRUE  certara-r     configured
#> 3      FALSE       <NA>    unsupported
#>                                                command server_name btw_groups
#> 1                                                 <NA>        <NA>       <NA>
#> 2 C:\\Program Files\\R\\R-4.6.0\\bin\\x64\\Rscript.exe   certara-r       docs
#> 3                                                 <NA>        <NA>       <NA>
#>   session_tools job_watch_wait_seconds tool_profile
#> 1            NA                     NA         <NA>
#> 2         FALSE                     45         full
#> 3            NA                     NA         <NA>
#>                           note
#> 1                         <NA>
#> 2                         <NA>
#> 3 Cursor has no 'local' scope.
```

``` r

# What Certara MCP server processes has this machine actually started?
Certara.R::list_certara_mcp_servers()
```

These are ordinary R functions you call from the R console (or RStudio),
not MCP tools. A server that is configured but never listed as running
usually means the client has not been reloaded since
[`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md).
[`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
is likewise callable from the console, but its fields read `NA` unless
it runs *inside* a live server (the launch options it reports are
recorded only at server startup) – in practice the agent calls it as a
tool to report the live wiring (server process, bridged session, active
profile).

## Troubleshooting

MCP setup friction is mostly an *environment* problem, not something
another tool can fix. Start with
[`certara_mcp_capabilities()`](https://github.com/certara/R-Certara/reference/certara_mcp_capabilities.md)
and
[`certara_session_status()`](https://github.com/certara/R-Certara/reference/certara_session_status.md)
to see what is actually wired, then work through the common cases:

- **MCP tools missing / server not listed** –
  [`write_mcp_config()`](https://github.com/certara/R-Certara/reference/write_mcp_config.md)
  was not run, or the client was not reloaded. Re-run it, reload the
  client, and enable the server in the client’s tool settings.
- **`btw` / `mcptools` / `ellmer` not found** – a `repos = NULL` source
  install skipped `Suggests`. Install from the release repo, or
  `install.packages(c("btw", "mcptools", "ellmer"))` (floors:
  `btw >= 1.2.0`, `mcptools >= 0.2.0`, `ellmer >= 0.4.0`).
- **Engine / compile failures** – `INSTALLDIR` / NLME not resolvable.
  Confirm the NLME Engine install and that `INSTALLDIR` is set (see the
  RsNLME installation guide).
- **Claude Desktop on Windows: `Compile/Link Failed`** even though the
  same model fits from RStudio – Desktop sanitizes the server
  environment. Add `ComSpec`, `INSTALLDIR`, `NLMEGCCDir64`, `TMP`,
  `NUMBER_OF_PROCESSORS`, and `PATHEXT` to the `env` block; see [Claude
  Desktop on Windows: required environment
  variables](#claude-desktop-on-windows-required-environment-variables).
  The same case is summarized in the [RsNLME troubleshooting
  guide](https://certara.github.io/R-RsNLME/articles/troubleshooting.html#claude-desktop-on-windows-extra-environment-variables).
- **Claude Desktop: PowerShell “cannot be executed” / “not authorized”
  errors** when a fit starts – the **Code** session is not anchored to a
  folder. Start a new Code session with a folder on your machine
  selected (for example your project folder) and run the fit again.
- **Claude Desktop: config edits have no effect** – the entry was
  written to a file Desktop does not read. Check the path shown by
  **File \> Settings \> Desktop app \> Extensions (Manage) \> Edit
  config** (Microsoft Store/IT installs use a `Claude_<suffix>` folder)
  and merge the `certara-r` entry there.
- **Agent behavior feels “old”** – the installed package predates the
  current MCP knowledge base / rules. Install the current release and
  re-run `write_mcp_config(client = "cursor", scope = "project")` so the
  Cursor rule is refreshed too.
- **Too many tools** – normal when `btw` groups are enabled. Reason by
  task using `certara_mcp_capabilities()$workflows`; for the leanest
  server use `btw_groups = "docs"` or a narrower `tool_profile`.

There is no automated “doctor” tool – this checklist plus the two status
tools is the supported path.

## Memory (optional)

The Certara MCP tools can use a local, **opt-in** memory of your
preferences, lessons, and past runs, stored on your machine. The memory
is *consumed by the agent* (its `get_user_preferences` / `get_lessons` /
`record_lesson` tools read and write it so behavior carries across
sessions), but the functions below are **for you, the human**, to
control whether that happens and to audit what is kept. They are
ordinary R console functions; none are required to use the MCP server.

Run them under these conditions:

- [`enable_memory()`](https://github.com/certara/R-Certara/reference/enable_memory.md)
  – once, if you *want* the agent to remember preferences and lessons
  between sessions. Pass `redact =` a vector of regex patterns to scrub
  sensitive identifiers (compound or project names) from stored free
  text. Memory stays off until you call this.
- [`disable_memory()`](https://github.com/certara/R-Certara/reference/disable_memory.md)
  – to turn it back off (existing records are kept but not used).
- [`list_memory_records()`](https://github.com/certara/R-Certara/reference/list_memory_records.md)
  – to see what is stored (`preferences`, `lessons`, `run_memory`,
  `gaps`, and whether memory is `enabled`).
- [`export_memory()`](https://github.com/certara/R-Certara/reference/export_memory.md)
  – to copy the records out for review or backup.
- [`delete_memory_record()`](https://github.com/certara/R-Certara/reference/delete_memory_record.md)
  /
  [`clear_memory()`](https://github.com/certara/R-Certara/reference/clear_memory.md)
  – to prune a single record or wipe everything.

If you never call
[`enable_memory()`](https://github.com/certara/R-Certara/reference/enable_memory.md),
the agent simply operates without memory – a perfectly normal way to
run.

## See also

- [`?Certara.R::write_mcp_config`](https://github.com/certara/R-Certara/reference/write_mcp_config.md),
  [`?Certara.R::launch_certara_mcp`](https://github.com/certara/R-Certara/reference/launch_certara_mcp.md)
- [`?Certara.R::list_certara_mcp_configs`](https://github.com/certara/R-Certara/reference/list_certara_mcp_configs.md),
  [`?Certara.R::remove_mcp_config`](https://github.com/certara/R-Certara/reference/remove_mcp_config.md)
- [RsNLME Installation
  Guide](https://certara.github.io/R-RsNLME/articles/installation.html)
