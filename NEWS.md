# modeltime.gluonts (development version)


### New Features

* __Custom Python Environments:__ Provide an option for setting a Custom Python Environment by supplying a `GLUONTS_PYTHON` environment variable. Before running `library(modeltime.gluonts)` use `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` to set the path of your python executable in a Conda or Virtual Environment that has 'gluonts', 'mxnet', 'numpy', 'pandas' and 'pathlib' available as dependencies.

### Fixes & Improvements

* __GluonTS 0.6.3 Upgrade:__ `install_gluonts()` now uses `gluonts==0.6.3`. This upgrade improves forecast accuracy.
* CRAN Comment - Add `SystemRequirements`: GluonTS.
* CRAN Comment - Fix `.onLoad` message to provide options for configuring the Python Environment.

# modeltime.gluonts 0.1.0

* Initial Release incorporates 2 GluonTS Algorithms:

    - `deepar()`: Integrates GluonTS DeepAREstimator
    - `nbeats()`: Integrates N-BEATS & N-BEATS Ensemble Estimators
