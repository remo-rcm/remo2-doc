The REMO Software Architecture
==============================

The remo2 version implements a new software architecture, that is based on the Flash Code (:cite:`FLASH2013`)
by the Flash_ Center for Computational Science. The main directory of remo2 looks like:


::

    bin/                    # bin directory for python source code
    ChangeLog              
    doc/                  
    jobs/                   # job directory with test scripts
    lib/                    # directory that manages external dependencies
    README.md   
    setup                   # main setup script that controls source code configuration
    sites/                  # Makefile configurations for different machines
    source/                 # Fortran source code

The main difference to earlier model versions is that there is no main directory for the source code (like `CODE`) any more. 
Instead, the source code is scattered into a number of subdirectories in the `source` directory. A script called `setup` 
is provided that manages different source code configurations. The main task of the `setup` script is to scan the `source`
directory and link all source files into a subdirectory depending on the configuration the user requests. The idea
is to have more control over the source code and how it is combined into an application that serves the needs of the
user's experiment. The main model configurations can be found in the `source/Simulations/SimulationsMain` directory
which can be used during the model setup step, e.g.,

::

    ./setup -auto Remo2015 -objdir=Remo2015

This command will create a subdirectory called `Remo2015` that contains only links to source code that is 
required by the configuration defined in `source/Simulations/SimulationsMain/Remo2015` 


.. _Flash: http://flash.uchicago.edu
