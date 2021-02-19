# modeltime.gluonts 0.2.0.9000

Improvements made to connect with the GluonTS Python Environment on Startup. 

# modeltime.gluonts 0.2.0

### Documentation

- __New Vignette:__ [Managing GluonTS Environments](https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html)

- __New Vignette:__ [Using GPUs with GluonTS](https://business-science.github.io/modeltime.gluonts/articles/using-gpus.html)

### New Features

- __Internal Scaling by Group:__ After significant testing it appears that some data sets return better results when the data is scaled by time series "id" (group). To help facilitate this, a new option is available scale by id: `scale = TRUE`.

- __Custom Python Environments:__ Provide an option for setting a Custom Python Environment by supplying a `GLUONTS_PYTHON` environment variable. Before running `library(modeltime.gluonts)` use `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` to set the path of your python executable in a Conda or Virtual Environment that has 'gluonts', 'mxnet', 'numpy', 'pandas' and 'pathlib' available as dependencies.

### Fixes & Improvements

* __GluonTS 0.6.3 Upgrade:__ `install_gluonts()` now uses `gluonts==0.6.3`. This upgrade improves forecast accuracy.
* CRAN Comment - Add `SystemRequirements`: GluonTS.
* CRAN Comment - Fix `.onLoad` message to provide options for configuring the Python Environment.

# modeltime.gluonts 0.1.0

* __Models:__ Initial Release incorporates 2 GluonTS Algorithms:

    - `deepar()`: Integrates GluonTS DeepAREstimator
    - `nbeats()`: Integrates N-BEATS & N-BEATS Ensemble Estimators
    
* __New Vignette:__ [Getting Started](https://business-science.github.io/modeltime.gluonts/articles/getting-started.html)

* __Website:__ [Modeltime GluonTS](https://business-science.github.io/modeltime.gluonts/)
