Postprocessing
==============

In this chapter, we describe how some of the REMO postprocessing tools
work.

Pressure Interpolation
----------------------

It is often necessary to interpolate fields from the hybrid model
levels, which follow the orographic patterns in the troposphere, to
quasi-horizontal pressure levels. The pressure interpolator is a suite
of FORTRAN routines similar to the preprocessor. It gets the (mostly)
6-hourly t-files which, in the vertical dimension, are defined on model
levels, and interpolates the basic atmospheric quantities to pressure
levels. The standard REMO pressure interpolator is called ``druint``
(druint: Druckinterpolation = pressure interpolation). You can also find
the pressure interpolator in the GERICS gitlab:

::

    http://git.gerics.de/REMO/druintzr

The main directory only contains three subfolders:

::

      /bin      # executable 
      /build    # makefile 
      /namelist # namelist template
      /source   # source code

Compilation
~~~~~~~~~~~

To compile the pressure interpolator, just change to the ``build``
directory. Here, you will find a number of Makefile headers for
different systems which contain system dependent compiler options and
flags. The ``Makefile`` will include the ``Makfile.h`` which you can
adapt to your system. You can also replace ``Makefile.h`` by one the
other headers if your system fits the compiler options. E.g.,
``Makefile.mistral`` mainly configures the intel compiler and
``Makefile.gfortran`` might be suitable for any Linux system. After
configuring the Makefile, just type

.. code:: bash

    make

and the executable ``druint`` should be found in the ``bin`` directory.

Running the Pressure Interpolator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| The pressure interpolator should be able to create pressure levels
  from REMO input (a-file) and output data (t-file). These files usually
  contain several REMO variables at a certain timestep per file and the
  pressure interpolator creates make a timeseries out of these. In
  addition, the program will create one file per variable and pressure
  level containing all timesteps of the period chosen in the ``INPUT``
  file.
| A template namelist file can be found in the ``namelist`` subdirectory
  of ``druint`` (see Table [tab:namelist\_druint]). The timeperiod is
  defined by the start and stop hour in the namelist ``PINNLST``. In
  addition, the template files shows the most common pressure levels
  (``PINFL=100,200,500,850,950``) and variables
  (``YVARPIN=’T’,’FI’,’U’,’V’,’QD’,’QW’,’REL_HUM’``) that are usually
  interpolated during the postprocessing of standard ``REMO`` run.
| The file containing the namelist has to be named ``INPUT`` and has to
  be in the same directory from where ``druint`` is called. However,
  usually the pressure interpolator is not run manually but should be
  used and configured automatically by a REMO run script that manages
  the monthly postprocessing of output data, see also Section
  [sec:job\_control].

+------------+-------------+-----------------------------------------------------------------------------------------------+
| Namelist   | Parameter   | Description                                                                                   |
+============+=============+===============================================================================================+
| PINNLST    | YUSER       | User Number of the Input file                                                                 |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | YEMEXPN     | Experiment Number of the Input file                                                           |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | HANF        | Start Hour                                                                                    |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | HENDE       | Stop Hour                                                                                     |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | HDERG       | Output Interval in hours                                                                      |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | ANA         | Switch for data type: .FALSE. REMO output (t-files), .TRUE. REMO input (a-files)              |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | LPHYEM      | Switch for model physics: .FALSE. ECHAM-physics, .TRUE. DWD-physics                           |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | LMOMON      | Switch for calender type: .FALSE. real calender, .TRUE. model calendar (360 days)             |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | LXT         | Switch for also calculating liquid water (it is not present in some a-files)                  |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | YEMCAT      | Path to input, should be 80 characters or less                                                |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | YPICAT      | Path to interpolated output data                                                              |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | YVARPIN     | List of internval variables names that should be interpolated (Appendix [cha:code\_list])     |
+------------+-------------+-----------------------------------------------------------------------------------------------+
|            | PINFL       | Pressure levels that should be interpolated (from the top of the atmosphere to the surface)   |
+------------+-------------+-----------------------------------------------------------------------------------------------+

Table: Namelist Parameters for the Pressure Interpolator

**NOTE:** The pressure interpolator now reads in the resolution directly
from the file and allocates data internally. This will require some more
memory on the stack! If you run into trouble with segmentation faults,
try to set the stack size to unlimited by ,e.g.,

::

    ulimit -s unlimited

Climate Data Operators
----------------------

The CDOs are a very useful suite of command line operators to manipulate
and analyse climate model data. The program package as well as a
detailed manual can be downloaded from
http://www.mpimet.mpg.de/fileadmin/software/cdo. The CDOs can handle
both the ieg and the srv format, as well as other formats (e.g., NetCDF,
GRIB). This allows them for instance also to convert ieg or srv to
NetCDF (using the copy function). It is recommended to locally install
the CDOs and to use them (maybe in addition to shell scripts and
dedicated FORTRAN programs) for postprocessing your REMO. Some often
used CDO commands are for instance

-  copy (including the -f type option): converts the file format, for
   instance from ieg to NetCDF (-f nc) or srv (-f srv)

-  info and sinfo: gives some short information on the contents of the
   data file (data fields, grid, date, etc.)

-  gradsdes: creates a GrADS control file (.ctl) which can be directly
   opened by the visualisation package GrADS (for example for a quick
   look at the model results)

-  selcode and delcode: selects / deletes a specific code number from a
   data file

-  seldate: selects all entries within a given date range

-  selindexbox: selects a rectangular sub-domain (or even one single
   grid cell)

-  timmean: computes the temporal average for each code in the data file

-  hourmean, daymean, monmean, seasmean and yearmean: computes hourly /
   daily / monthly / seasonal / annual means for each code in the data
   file

-  ydaymean, ymonmean, yseasmean: computes climatological daily /
   monthly / seasonal means for each code in the data file

-  remapcon: conservative remapping to another grid (e.g., a regular
   lat/lon grid)

-  rotuvb: backward rotation of wind components (to geographic North /
   East) If the CDOs are applied on srv-files the rotation of the grid
   of the original file is not known (since this information is not
   contained in the srv-header). In this case, some of the
   above-mentioned functions (e.g. remapcon) require a grid
   specification file (see CDO manual). For example, in case of the
   can03 domain (see above) this file would look like this:

PyRemo and PyPlot Tools
-----------------------

A python library that can handel some REMO data manipulation is
available here:

::

    http://git.gerics.de/PyREMO/PyREMO.git

You can download it and add it to your ``PYTHONPATH`` environment
variable to import it to your own scripts for using it. On MISTRAL, it
is available from the GERICS project:

::

    export PYTHONPATH=/work/ch0636/software/python/PyRemo-1.0.0:\${PYTHONPATH}

The PyPlot library contains tools for plotting REMO data and is
available here:

::

    http://git.gerics.de/PyREMO/PyPlotTools.git

Once downloaded, the tools should be added to you ``PATH`` variable to
be available on the command line for use in further scripts. Again, on
MISTRAL, it is available by adding this line to your .profile or
.bashrrc file:

::

    export PATH=/work/ch0636/sw/python/PyPlotTools-1.2.0:\$PATH

Clidas
------

| The Clidas tool (Climate Data Analysing System) is another set of
  shell scripts that utilizes the CDO, PyRemo and PyPlot tools to create
  a number of standard plots of REMO results. The main script provides
  an interactive command interface that allows to create plots for a
  standard REMO experiment. The tool creates scripts from a number of
  templates and is usually configured to work with the same path and
  tape archive conventions as the run scripts. Therefore, it works
  togehther smooothly if the run script stored the REMO output data in
  the same directory structure.
| However, the plot scripts that Clidas creates from a number of
  templates can also easily be adapted to work on any other
  infracstructure.
| There is a documentation available from the developer Torsten Weber .
