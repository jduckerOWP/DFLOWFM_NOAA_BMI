#!/bin/bash

    # At present, this runscript will only work after having executed the following command in a command-box, at the top folder of the source tree:
    # build.sh all
    # See README.md there for more information

../../build_delft3d4/install/bin/run_dflow2d3d_fluidmud.sh -wconfig config_d_hydro_sed.xml -mconfig config_d_hydro_mud.xml

