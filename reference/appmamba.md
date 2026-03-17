# Manage Environment with `micromamba`

Manage Environment with `micromamba`

## Usage

``` r
appmamba(...)

install_appmamba(force = FALSE)

uninstall_appmamba()

appmamba_rc(edit = FALSE)
```

## Arguments

- ...:

  \<[dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to `micromamba`. Run `appmamba()` for more
  details.

- force:

  A logical value indicating whether to reinstall `appmamba` if it is
  already installed.

- edit:

  A logical value indicating whether to open the config file for
  editing.

## Functions

- `appmamba()`: `blit` utilizes `micromamba` to manage environments.
  This function simply executes the specified `micromamba` command.

- `install_appmamba()`: Install appmamba (`micromamba`).

- `uninstall_appmamba()`: Remove appmamba (`micromamba`).

- `appmamba_rc()`: Get the `run commands` config file of the
  `micromamba`.

## Examples

``` r
# \donttest{
install_appmamba()
#> Installing appmamba
#> Downloading from https://micro.mamba.pm/api/micromamba/linux-64/latest
#> Install appmamba successfully!
appmamba()
#> Running command (2026-03-17 16:14:11):
#> /home/runner/.local/share/R/blit/apps/appmamba/bin/micromamba --root-prefix
#> /home/runner/.local/share/R/blit/appmamba --yes --help
#> 
appmamba("env", "list")
#> Running command (2026-03-17 16:14:11):
#> /home/runner/.local/share/R/blit/apps/appmamba/bin/micromamba --root-prefix
#> /home/runner/.local/share/R/blit/appmamba --yes env list
#> 
# uninstall_appmamba() # Uninstall the `micromamba`
# }
```
