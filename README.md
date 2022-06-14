# hasknoaa 

Hasknoaa gets weather data from [NOAA's weather API for
you](https://api.weather.gov).

## Usage

Weather data is centered around points which can be obtained with latitude and
longitude. To add a point to your local database run `hasknoaa add <latitude>
<longitude>`. For negative values you will need to use parenthesis to keep it
from being interpreted as an option. If you run `hasknoaa list` you will be
able to see your saved points. Now set your default point with the set command
using the id of the point you desired like so: `hasknoaa set <pointId>`. Now
you can use the `hourly` and `multiday` subcommands for the respective
forecasts.

## Development

### Bulid or Run

This project can be built with `cabal-install` using the Nix style build
commands introduced in version 2. To build use `cabal build`. And to run
use `cabal run -- hasknoaa`.

### Formatting

Formatting is done with Ormolu. The Haskell Language Server supports Ormolu and
you can use the editor's LSP reformatting command to trigger it on edits.
