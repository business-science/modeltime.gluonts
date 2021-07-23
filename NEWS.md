
# modeltime.gluont 0.3.0.9000 (Development Version)

## Installation Support

### Windows Conflicting Dependencies

Improved support for conflicting package dependencies on Windows Operating Systems. Solution is to separate the installation process into two stages, which happens inside of `install_gluonts()`.

```
pytorch-lightning 1.3.8 depends on numpy>=1.17.2
mxnet 1.7.0.post1 depends on numpy<1.17.0 and >=1.8.2
```

### New Uninstall Function

Users can now `uninstall_gluonts()`.



# modeltime.gluonts 0.3.0

## Support for GluonTS 0.8.0 and Pytorch Backend:

Modeltime GluonTS now support `gluonts 0.8.0`. Simply run `install_gluonts()` to upgrade. The upgraded support makes `modeltime.gluonts` incompatible with earlier versions of GluonTS (e.g. `gluonts 0.6.3`). The solution is to upgrade to `gluonts 0.8.0`, which requires:

- `gluonts==0.8.0`

- `mxnet~=1.7`

Additionally, GluonTS 0.8.0 now supports __pytorch as a backend.__ Use `install_gluonts(include_pytorch = TRUE)` to simplify installation of the PyTorch backend. Pytorch backend requirements:

- `torch~=1.6.0`
            
- `pytorch-lightning~=1.1`

## New Algorithms

### Pytorch  DeepAR 

A new engine has been added to `deep_ar()` that enables the Pytorch backend using `set_engine("torch")`. This requires the Python packages `pytorch` and `pytorch-lightning`. Use `install_gluonts(include_pytorch = TRUE)` to simplify installation.

### GP Forecaster Algorithm

A new function, `gp_forecaster()`, integrates the Gaussian Process Estimator from GluonTS. 

### Deep State Algorithm

A new function, `deep_state()`, integrates the Deep State Estimator from GluonTS. 

## Tutorials

- We've updated the [Installation Guide](https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html). This includes revised requirements for installation, upgrading to `modeltime.gluonts` >= 0.3.0, troubleshooting installation, python environment requirements, and custom python environments. 

- We've updated the [Getting Started Guide](https://business-science.github.io/modeltime.gluonts/articles/getting-started.html) to go through a DeepAR example.

- We've update the [GPU Setup Instructions](https://business-science.github.io/modeltime.gluonts/articles/using-gpus.html) to cover Modeltime >=0.3.0.

## Improvements

- `install_gluonts()`: Gains two new parameters to help with upgrading:
    1. `fresh_install`: If TRUE, will remove prior installations of the r-glounts conda environment to setup for a fresh installation. This can be useful if errors appear during upgrades. Default: FALSE.
    2. `include_pytorch`: If TRUE, will install torch. Needed for Torch implementation of deep_ar(). Default: FALSE.

## Breaking Changes

- GluonTS <= 0.8.0. The `modeltime.gluonts` package version >= 0.2.2.9000 is not compatible with `gluonts` < 0.8.0. To fix, simply upgrade to `gluonts` 0.8.0. 

# modeltime.gluonts 0.2.2

### Dials Params

- NBEATS Models: Adding Dials helpers #14

# modeltime.gluonts 0.2.1

Improvements made to connect with the GluonTS Python Environment on Startup. 

# modeltime.gluonts 0.2.0

### New Vignettes

- [Managing GluonTS Environments](https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html)

- [Using GPUs with GluonTS](https://business-science.github.io/modeltime.gluonts/articles/using-gpus.html)

### New Features

- __Internal Scaling by Group:__ After significant testing it appears that some data sets return better results when the data is scaled by time series "id" (group). To help facilitate this, a new option is available scale by id: `scale = TRUE`.

- __Custom Python Environments:__ Provide an option for setting a Custom Python Environment by supplying a `GLUONTS_PYTHON` environment variable. Before running `library(modeltime.gluonts)` use `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` to set the path of your python executable in a Conda or Virtual Environment that has 'gluonts', 'mxnet', 'numpy', 'pandas' and 'pathlib' available as dependencies.

### Fixes & Improvements

* __GluonTS 0.6.3 Upgrade:__ `install_gluonts()` now uses `gluonts==0.6.3`. This upgrade improves forecast accuracy.
* CRAN Comment - Add `SystemRequirements`: GluonTS.
* CRAN Comment - Fix `.onLoad` message to provide options for configuring the Python Environment.

# modeltime.gluonts 0.1.0

* __Models:__ Initial Release incorporates 2 GluonTS Algorithms:

    - `deep_ar()`: Integrates GluonTS DeepAREstimator
    - `nbeats()`: Integrates N-BEATS & N-BEATS Ensemble Estimators
    
* __New Vignette:__ [Getting Started](https://business-science.github.io/modeltime.gluonts/articles/getting-started.html)

* __Website:__ [Modeltime GluonTS](https://business-science.github.io/modeltime.gluonts/)
