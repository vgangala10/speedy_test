This is forked from the original F90 SPEEDY model with deliberately breaking changes. I removed custom types to allow easy f2py generation and removed the file input-output to allow compilation without NetCDF libraries. 

## Installation

The f2py setup seems to be quite sensitive to numpy / python versions, but this setup seemed to work on MacOS:

$ conda create -n speedy python=3.11 gfortran numpy

Build the `speedy` library with:

$ f2py -c params.f90 physical_constants.f90 geometry.f90 auxiliaries.f90 input_output.f90 fftpack.f90 fourier.f90 legendre.f90 spectral.f90 boundaries.f90 mod_radcon.f90 convection.f90 humidity.f90 large_scale_condensation.f90 longwave_radiation.f90 land_model.f90 shortwave_radiation.f90 surface_fluxes.f90 sea_model.f90 sppt.f90 vertical_diffusion.f90 physics.f90 -m speedy

## Usage

The python interface is quite simple and designed to allow testing individual subroutines, here is an example:
```python
import numpy as np
from speedy import convection

import numpy as np

psa= np.ones((96, 48))
se = np.ones((96, 48, 8))
qa = np.ones((96, 48, 8))
qsat = np.ones((96, 48, 8))

itop, qdif = convection.diagnose_convection(psa, se, qa, qsat)

print(itop)
print(qdif)
```