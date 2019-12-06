.. _cha_preprocessing:

Preprocessing
=============

This chapter deals with a more detailed introduction into the general
simulation workflow, in the context of REMO often called ’experiment’.
It requires a proper planning since a useful REMO run needs some well
prepared input data. In this chapter, we describe how to plan and setup
your own REMO experiment. This includes choosing a domain and resolution
([sec:preproc:plan]), creating your own surface data
([sec:preproc:bodlib]) and interpolating the forcing data from a global
dataset ([sec:preproc:forcing]).

Experiment Plan
---------------

When you obtained the REMO model and you have successfully run the
tutorial run from Chapter [cha:quickstart], you might already have an
idea for your own REMO experiment and scientific task. You then should
consider the following points, before diving into your own preprocessing
quest.

Which model domain do you want to look at?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all, you have to be aware of what domain you want to look at.
That means you need to know the location and extent of your domain of
interest. The location is usually given in the Geopgraphic coordinate
system (e.g, lontitude, latitude) and the unit is usually in degrees.
Since the geopraphich coordinate system has two singularities at the
north and the south pole, depending on the model domain, a grid rotation
of your model domain to the equator might be necessary (see Fig.
[fig:rot\_grid]). The problem is that near the poles, the grid get very
much distorted and grid boxes are not of equals size. Near the equator,
the grid boxes cover more or less the same area. Therefore, the grid
should be rotated so that the equator goes straight through the center
of the domain of interest to cover the domain with a symmetric
undistored spherical grid. It is recommended to use the same rotated
grid from model domains, where REMO was running in past experiments,
when you like to compare the results. Information about the existing
experiments is available on the REMO homepage. In the Appendix
[cha:model\_domains]

Which resolution do you need? Does it require more than one nesting?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

More than one nesting is necessary to avoid big (factor 10) scale jumps
between the driving data and REMO. Both questions are linked as high
resolution runs (10km) require usually a double nesting. For example,
one might produce a REMO experiment with a horizontal resolution of 50km
forced by GCM data. The output data of the REMO experiment is used as
boundary data for a second domain nesting with REMO afterwards. The
second REMO run resolves a smaller area inside of the previous REMO
domain,e.g. with a 10km horizontal grid. An overview of transforming
horizontal resolution from km into degree (as needed in scripts) can be
viewed under UsefulInformations. The vertical resolution is usually 27
levels nad can be switched to up to 49 levels. The level choice is
independent from the vertical resolution of the driving data.

What kind of forcing data do you want to use?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In principle, control, scenario and validation runs are possible.
Control- and scenario runs are driven by start and boundary data from
global climate models, e.g. ECHAM. The data from the global model is
preprocessed for REMO. Observed greenhouse gas concentrations are used
as boundary condition for control runs. Scenario runs use greenhouse gas
concentrations based on different socio-economic assumptions. Standard
scenarios that are produced by REMO are A1B, B1 and A2. In contrast,
validation runs are driven by reanalysis data, e.g. ERA-40 and
ERA-interm, and operational analysis. The kind of run determines which
forcing data you need and how to configure the preprocessor (Section
[sec:preproc:forcing]).

Which kind of supercomputer will you use?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Different supercomputers use varied compilers. You can either use a
massive parallel or a vector machine to run REMO. REMO versions are
available for both supercomputer types. The latest Version REMO 2015
is suiteable for massive parallel machines (Intel machines). How much
computation time is available and how do you spend it reasonably? You
can quite easily waste of lot of cpu hours if you choose to many
processors or if your resolution if unnecessarily large. Refer to
Section [sec:] for more details on the parallelization of REMO.
Consider that different supercomputers produce slightly different
results due to technical reasons when you compare output data from
different machines.

Introduction to the Preprocessing Workflow
------------------------------------------

The general representation of the worlflow is presented on the Fig.
[fig:workflow]. The first step into your own REMO experiment is to setup
your model domain and create some data to your model. This *driving*
(often also called *forcing*) data is essential since it provides time
dependent boundary condtions for your model domain. There are three
possible places your driving data can come from:

-  Global atmospheric model (like MPI-OM or Nor-ESM)

-  Atmospheric reanalysis (like ERA Iterim or NCEP reanalysis)

-  Run of the regional model with larger domain (usually another REMO
   run)

.. figure:: ./fig/REMO_workflow.png
   :alt: Overview of the general worfklow  
   :width: 80.0%

   Overview of the general workflow for a user customated REMO
   experiment.


However, the actual files containing results from a global model or a
reanalysis run can neither be used as an input file for REMO nor the
preprocessor directly. Instead, the data from the files has to be
extracted and stored in a file that fullfils certain meta data
conventions (independent from the acutal file format, e.g., IEG or
NETCDF). Such a file is simply called a ’g-file’ and its main purpose
is to store global data using naming conventions and a code table that
can be understood by REMO and its preprocessor while conserving the
original model grid and resolution. However, for the actual REMO run,
this global data has to be interpolated to the domain of interest.
This mainly means that the data from the g-file has to be interpolated
to the new (rotated) grid and resolution. The file that contains only
this *regional* model data and can be read by REMO is called an
’a-file’ (from the German word ’Antrieb’). This is the file that
contains the actual forcing data for the experiment. It also contains
specific fields for the surface boundary conditions which are created
with the *bodlib* toolkit which is a set of scripts that creates the
so-called domain specific *surface library*. Although during the model
run the forcing data is only used to determine the time dependent
boundary conditions, the a-file always contains all forcing fields for
the whole model domain (and not only the boundary region). This means
storing a lot of redundant data, however, this is necessary since
usually REMO should also be able to read *inital conditions* from
these files and it is also helpful to compare the original global data
with the REMO results. The process of interpolating global data from a
g-file to an a-file and combining it with the surface library data is
handled by a seperate FORTRAN program, usually called the
*preprocessor* (also the preprocessor only handles the interpolation
of g-files to a-files, it is convenient to call *preprocessing* the
whole process of creating g-files, a-files and surface data).
In addition to the forcing data stored in g-files and a-files, REMO
requires additional files that contain surface data. Since this data
is also specific for the domain of interest, is has to be created for
each model domain separately. This surface data usually contains time
dependent fields for the surface albedo, vegatation ratio and the leaf
area index. The creation of these surface files is also handled by the
bodlib toolkit (German ’Boden Library’) which, in addition to the
surface library, creates three files for the surface fields on the
spatial model grid and each in a montly resolution (e.g., 12 time
steps in each file).

Choosing Grid Parameters
------------------------

For the setup of a REMO simulation, the second step (after deciding on
a name for the simulation) is to define the model domain and the
horizontal grid spacing. A distinct name should be assigned to each
model domain and it might also be good to include the horizontal
resolution in the name, e.g. europa044 for a 0.44domain over Europe.
For the horizontal discretisation, REMO uses a **rotated spherical
grid** (Figure [fig:rot\_grid]). The grid should be placed in a way
that the rotated equator approximately crosses the center of the model
domain. This would ensure a negligible meridional convergence and
almost equal real grid box sizes. For model domains within or close to
the tropics (close to the real-world equator) a rotation of the grid
is usually not necessary.
A specific model domain has to be defined by setting the following
parameters:

-  real-world latitude (lat) and longitude (lon) of the rotated North
   Pole (or South Pole)

-  horizontal resolution in the rotated system in degrees (valid for
   both the x- and the y-direction)

-  total number of grid boxes in x- and y-direction

-  lat/lon location of the lower left grid box center in the rotated
   system

Typical horizontal resolutions for REMO are 0.5(approx. 55 km),
0.44(approx. 50 km), 1/6(approx. 18 km) and 0.11(approx. 12 km). Not
all numbers of grid boxes in x- and y-direction are allowed! The
number of boxes in each direction (nx and ny) have to follow these
rules (see Appendix [app:domainsize]):

nx - 1 = 2 lx \* 3 mx \* 5 nx with lx not equal to 1
ny - 1 = 2 ly \* 3 my \* 5 ny with ly not equal to 1

.. figure:: ./fig/rot_grid.eps
   :alt: Defining a REMO domain. Advantages of the rotated grid: The
   grid spacings in both x and y direction are approximately equal, all
   grid boxes have approximately the same area. The rotation of the grid
   is defined by the location of the new North Pole (or South Pole
   respectively). The figure indicates a rotated grid for the Europe
   domain with the rotated North Pole at a longitude of -162and latitude
   of 39.25.
   :width: 80.0%

   Defining a REMO domain. Advantages of the rotated grid: The grid
   spacings in both x and y direction are approximately equal, all grid
   boxes have approximately the same area. The rotation of the grid is
   defined by the location of the new North Pole (or South Pole
   respectively). The figure indicates a rotated grid for the Europe
   domain with the rotated North Pole at a longitude of -162and latitude
   of 39.25.

The Surface Library
-------------------

After defining a model domain and a corresponding horizontal resolution,
a surface library (sometimes referred to as soil library) and monthly
vegetation fields have to be created. The surface library provides
information about land sea mask, orography, and other surface parameters
like roughness length, albedo, vegetation cycle etc.. These informations
are neccessary as input data for the REMO model run as well as the REMO
preprocessors which creates the forcing data for the boundary conditions
(see Section [sec:preproc:forcing]). The surface library contains
information on those surface characteristics which are constant in time
(e.g. orography, surface roughness lenght, land-sea-mask, field
capacity). The monthly vegetation fields contain information about the
climatological annual cycle in monthly resolution of the surface
background albedo, vegetation fraction and leaf area index (LAI), i.e.,
12 fields for each of these three parameters. In essence the surface
library is the file with your surface boundary condition interpolated on
to the model grid.

Content of the Surface Library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The surface library itself usually consists of a single srv-file (which
is a special binary format) and contains the fields list in Table
[tab:surflib].

+--------+-----------+----------+--------------------------------------------+
| Code   | Name      | Unit     | Description                                |
+========+===========+==========+============================================+
| 129    | FIB       | m        | surface geopotential (orography)           |
+--------+-----------+----------+--------------------------------------------+
| 172    | BLA       | fract.   | land sea mask                              |
+--------+-----------+----------+--------------------------------------------+
| 173    | AZ0       | m        | surface roughness length                   |
+--------+-----------+----------+--------------------------------------------+
| 174    | ALB       | fract.   | surface background albedo                  |
+--------+-----------+----------+--------------------------------------------+
| 198    | VGRAT     |          | vegetation ratio                           |
+--------+-----------+----------+--------------------------------------------+
| 199    | VAROR     |          | orographic variance (for surface runoff)   |
+--------+-----------+----------+--------------------------------------------+
| 200    | VLT       |          | leaf area index                            |
+--------+-----------+----------+--------------------------------------------+
| 212    | FOREST    |          | vegetation type                            |
+--------+-----------+----------+--------------------------------------------+
| 226    | FAO       |          | FAO data set (soil data flags)             |
+--------+-----------+----------+--------------------------------------------+
| 229    | WSMX      |          | field capacity of soil                     |
+--------+-----------+----------+--------------------------------------------+
| 272    | BETA      |          | shape parameter for Arno Scheme            |
+--------+-----------+----------+--------------------------------------------+
| 273    | WMINLOK   |          | minimum subgrid wcap (field capacity)      |
+--------+-----------+----------+--------------------------------------------+
| 274    | WMAXLOK   |          | maximum subgrid wcap (field capacity)      |
+--------+-----------+----------+--------------------------------------------+

Table: Fields in the Surface Library

The surface library file itself is usually named something like `lib_[NAME_OF_DOMAN]_frac`
where `[NAME_OF_DOMAIN]` indicates the name of the domain of the
surface library. The fields in this file are already interpolated to
the correct model domain and resolution and they are required by REMO
as surface boundary conditions. However, this file is not used
directly during the model run but rather indirectly. The surface
library is used by the preprocessing of forcing data (Section
[sec:preproc:forcing]) and is included in the forcing files.

Data for the Mean Annual Cycle
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are another three srv-files that contain the time-dependent mean
annual cycles for the albedo (ALB), leaf area index (VLT) and vegetation
ratio (VGRAT) (Table [tab:annual])

+--------+---------+----------+-----------------------------+
| Code   | Name    | Unit     | Description                 |
+========+=========+==========+=============================+
| 174    | ALB     | fract.   | surface background albedo   |
+--------+---------+----------+-----------------------------+
| 198    | VGRAT   |          | vegetation ratio            |
+--------+---------+----------+-----------------------------+
| 200    | VLT     |          | leaf area index             |
+--------+---------+----------+-----------------------------+

Table: Fields for the Mean Annual Cycle

There is a file for each field, usally named somthing like this:

-  ``albyear_[NAME_OF_DOMAIN].srv``

-  ``vgryear_[NAME_OF_DOMAIN].srv``

-  ``vltyear_[NAME_OF_DOMAIN].srv``

These files are used directly for the REMO model run and have to
specified in the Input Namelist (see Appendix [cha:remo\_namelist]). The
surface library and the files for the annual cycles are domain specific
and have to be prepared for each model domain individually There is a
number of surface libraries available from GERICS on MISTRAL at this
location:

.. code:: bash

    /work/ch0636/surflibs/

and the associated files for the mean annual cycle can be found here:

.. code:: bash

    /pool/data/remo/

or ask for support from GERICS. A list of available model domains, for
which surface data (and maybe even forcing data) is available, can be
found in Appendix [cha:grid\_list]. However, in the following sections,
we describe how to create these files for a new domain.

Creating a new Surface Library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The surface library is created by interpolating data from the GTOPO30
global digital elevation model (DEM) to the REMO model grid. The
interpolation is done by using

-  shell-scripts, Fortan- and C-Programs (available from the REMO
   gitlab)

-  a global surface library and FAO-datasets (located at
   /hpss/arch/ch0636/bodlib/input\_data/)

-  global GTOPO orography (located at /hpss/arch/ch0636/bodlib/gtopo/)

This collcetion of scripts and data is referred to as the *bodlibkit*.
You can download it from the REMO gitlab at GERICS, e.g.,

.. code:: bash

    git clone http://git.gerics.de/REMO/BodLibKit.git

Among others, the main directory contains these directories and files:

.. code:: bash

      /bin      # shell scripts
      /config   # configuration files 
      /data     # several binary data files (srv,dat,bin) 
      /gtopo    # GTOPO30 global digital elevation model 
      /GOOD     # fortran codes to read binary elevation data 
      /src      # C source code for goodrot program
      setup.sh  # shell script to setup a new surface library from a config file 

Obtaining additional Datasets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The surface library is created by interpolating data from the GTOPO30
global digital elevation model (DEM) 3 and additional FAO-datasets to
the REMO model grid. The interpolation is done by using a number of
shell-scripts, Fortan- and C-Programs. These programs are partly created
and compiled on the fly by the shell scripts.

To create a new Surface Library, you first have to obtain the GTOPO30
and FAO datasets. At GERICS, these can be found in the HPSS archive at
DKRZ at a global surface library and FAO-datasets (located at
``/hpss/arch/ch0636/bodlib/input_data``, and global GTOPO orography
(located at ``/hpss/arch/ch0636/bodlib/gtopo``)

.. code:: bash

    /hpss/arch/ch0636/bodlib/input_data/gtopo.tar.gz            # GTOPO30 global elevation
    /hpss/arch/ch0636/bodlib/gtopo/bodlib_input_data.tar.gz     # additional FAO datasets

The GTOPO30 data is available in with different endianess (e.g., big and
little endian) since these are binary files. However, the scripts will
check for endianess and convert them if neccessary. Download these files
from the HPSS archive (using ‘get‘) and untar them by using, e.g.

.. code:: bash

    tar -xzf bodlib_input_data.tar.gz 

This data has to go into the ‘/data‘ subdirectory of the ‘BodLibKiT‘
root directory. So it is advisable to untar this file directly in the
‘data‘ subdirectory. So in addition to the data from the gitlab, the
‘/data‘ subdirectory should contain these additional files:

.. code:: bash

       MSA0100_1996_DHR30.med 
       MSA0100_1996_LAT.bin 
       MSA0100_1996_LON.bin 
       goge2_0g.img 
       f_lim.dat

The GTOPO30 data hast to be untarred directly in the ‘BodLibKit‘ root
directory since these files will automaticall go into the gtopo
subdirectory.

.. code:: bash

    tar -xzf gtopo.tar.gz 

In the end, the gtopo subdirectory should contain about 33 elvevation
data files, e.g.

.. code:: bash

    E020N40.DEM  E020S10.DEM  E060N90.DEM  E060S60.DEM ... 

Configuration
^^^^^^^^^^^^^

To setup a configuration for a new REMO domain, you can create a new
config file in the ``config`` subdirectory. You can either copy one of
the available configurations to a new file or use the
``template.config`` file. However, your config file should get a short
name by which it can easily be identified and it should be equal to the
``EXP`` variable in the config file. In the end, you have to fill in the
following parameters in the config file:

.. code:: bash

       EXP=                     # Name of the area
       IWATER=1                 # 1=fractional surface library, 0=nonfractional
       
       RESOLUTION_X=            # Number of Longitudes
       RESOLUTION_Y=            # Number of Latitudes
       RESOLUTION=              # Resolution of destination grid
       LON_LL=                  # Longitude of origin - dest. in rotated system
       LAT_LL=                  # Latitude of origin - dest. in rotated system
       #
       # lon/lat coordinates of South Pole! If you have the North Pole available,
       # you can compute the South Pole like this:
       #
       # LON_SP = LON_NP + 180 (even if it gets larger than 180 degrees)
       # LAT_SP = -1 * LAT_NP 
       #
       LON_SP=                  # Longitude of South Pole - destination grid
       LAT_SP=                  # Latitude of South Pole - destination grid
       POL=0                    # Pole inside the domain
                                # 0 = No, 1 = North Pole, 2 = South Pole
       IGLAC=0                  # 0 = default
                                # 1 = REMOglacier version: surface parameters (except
                                #     WAVA and ICAP) will computed only based on non-
                                #     glaciated land fraction

.. note:: The surface library covers the REMO model domain PLUS one row of
 gridboxes outside the model domain along each side. You need an
 additional band of one boundary box around your future REMO domain in
 the surface library. Consequently, we have to add 2 grid boxes for each
 dimension resulting in ``RESOLUTION_X`` and ``RESOLUTION_Y`` being
 larger by 2 gridboxes than the actual grid of the REMO run. It is also
 crucial to shift the coordinates for the lower left grid box center
 (``LON_LL``, ``LAT_LL``) by one grid cell to the left and bottom
 respectively. Note that in REMO and the BodLibKit, grid related
 coordinates always denote the center of a grid box, e.g., (``LON_LL``,
 ``LAT_LL``) denotes the coordinates of the center of the lower left grid
 box and NOT the lower left corner of the model domain (which would be
 shifted by another half of a grid box). If you are unsure, you can have
 a look at the REMO user guide for a detailled example of how to create a
 Surface Library with the BodLibKit.

The parameter POL has to be set to 0 if none of the rotated poles lies
within the model domain (usually this is the case). It has to be set to
1 (2) if the rotated North (South) Pole lies within the domain
boundaries. Please be aware, that ``LON_SP`` and ``LAT_SP`` denote the
coordinates of the South pole (NOT the North Pole). Usually the rotated
REMO grid is defined by the North Pole and you have to convert these to
the South Pole coordinates as indicated in the config file. From version
5.1 on, REMO uses a fractional surface coverage (each grid box can
contain a land, a water and a sea ice fraction). Therefore, when
carrying out simulations with a newer REMO version, the parameter IWATER
has to be set to 1. For the fractional version, there are also modified
scripts ending on `_frac.sh` that have to be executed (see below).

Running the setup.sh script
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The setup.sh script is supposed to setup a new directory named after
``EXP`` (name of the are) in the BodLibKit root directory and to create
a ``work`` and ``results`` directory. In addition, the script also
creates links to all neccessary data in the ``EXP/work`` directory and
copies the main shell scripts from the ``/bin`` directory to the ``EXP``
directory. All you have to provide to the setups.sh script is your
domain configuration file in the ``config`` directory, e.g,

.. code:: bash

    ./setup.sh <EXP.config>

The script should also check if all neccessary data has been obtained
from the HPSS archive and whether it is in the right place. After
running the setup script successfully, the ``EXP`` directory should be
available in the BodLibKit root directory.

Creating the Surface Library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After creating the setup directory, you can change to your ``EXP``
directory and execute the scripts in the indicated order. Please be
aware, that these scripts create and compile C and Fortran Code on the
fly so you need to configure a compiler for these. The compiler is
chosen in the ``system_setting.sh`` configuration script in the work
subdirectory. There are some default configurations, e.g., for the
Mistral the intel compiler is chosen. Make sure, you have a compiler
available and set in the ``system_setting.sh``.

Troubleshooting
^^^^^^^^^^^^^^^

As mentioned, the scripts of the surface library create C and Fortran
Codes on the fly and compile it. These programs allocate their memory on
the stack memory since they define their required memory at compilation
time (automatic array). This might create trouble if the stack size on
your system is limited. So if a script, especially the first.sh or
second.sh, exists with, typically, a segmentation fault, you can try to
unlimit the stack size. To remove the stack size limit on a non-Windows
system (i.e. Linux, Unix or OSX), use one of the following commands.

If you your shell is sh, bash or ksh use

.. code:: bash

    ulimit -s unlimited

If your shell is csh, tcsh or zsh use:

.. code:: bash

    limit stacksize unlimited

Step by Step Example
~~~~~~~~~~~~~~~~~~~~

In the following, we give a detailled example of how to configure the
scripts of the BodLibKit and how to create a new surface library and the
files for mean annual cycle. The first thing to do, is to configure the
BodLibKit with your desired grid parameters (see Section
[sec:grid\_params]) This is done in the header of the first script
``first.sh``. In the header of the file, the following parameters can be
configured:

::

    EXP=europa011            # Name of the area
    IWATER=1                 # 1=fractional surface library, 0=nonfractional

    RESOLUTION_X=435         # Number of Longitudes
    RESOLUTION_Y=435         # Number of Latitudes
    RESOLUTION=0.11          # Resolution of destination grid
    LAT_LL=-24.035           # Latitude of origin - dest. in rotated system
    LON_LL=-29.035           # Longitude of origin - dest. in rotated system
    LAT_SP=-39.25            # Latitude of South Pole - destination grid
    LON_SP=18.0              # Longitude of South Pole - destination grid
    POL=0                    # Pol innerhalb des Gebietes
                             # 0 = No, 1 = North Pole, 2 = South Pole
    IGLAC=0                  # 0 = default
                             # 1 = REMOglacier version: surface parameters (except
                             #     WAVA and ICAP) will computed only based on non-
                             #     glaciated land fraction

| By default parameters in the ``first.sh`` are defined for Baltex model
  domain. For this example, we put in the parameters for a grid that has
  a horizontal resolution of 0.11and that extends roughly over the
  Eurocordex domain with an appropriate grid rotation (Fig.
  [fig:rot\_grid]). The grid should be placed in a way that the rotated
  equator approximately crosses the center of the model domain. This
  would ensure a negligible meridional convergence and almost equal real
  grid box sizes. For model domains within or close to the tropics
  (close to the real-world-equator) a rotation of the grid is usually
  not necessary.
| **NOTE:** The surface library covers the REMO model domain **PLUS**
  one row of gridboxes outside the model domain along each side. You
  need an additional band of one boundary box around your future REMO
  domain in the surface library. In this example, we plan to create the
  surface library for a REMO model domain with a resolution of 433x433
  grid boxes (see the Appendix [app:domainsize]) and we have to add 2
  grid boxes for each dimension resulting in ``RESOLUTION_X=435`` and
  ``RESOLUTION_Y=435``. It is also crucial to shift the coordinates for
  the lower left grid box center ``(LON_LL, LAT_LL)`` by one grid cell,
  e.g. 0.11, to the left and bottom respectively so that the resulting
  REMO model domain lies within the slightly larger domain of the
  surface library. Note that in REMO, grid related coordinates always
  denote the center of a grid box, e.g., ``(LON_LL, LAT_LL)`` denotes
  the coordinates of the center of the lower left grid box and **NOT**
  the lower left corner of the model domain (which would be shifted by
  another half of a grid box).

    **Example**: If you are applying a 1/2 degree resolution and your
    lower left corner of a grid box should be at 20 degree west/40
    degree south you have to define it as 20.25 west/40.25 south in the
    ’first.sh’ - script. (It’s actually one gridbox more than one would
    usually expect, because you need one boundary row at each side) (the
    coordinates define the center of the gridbox. 20.25 west/40.25 south
    means that the lower left corner of the gridbox in the lower left
    corner of the model domain is at 20.5 west/40.5 south.)

The parameter POL has to be set to 0 if none of the rotated poles lies
within the model domain (usually this is the case). It has to be set to
1 (2) if the rotated North (South) Pole lies within the domain
boundaries. From version 5.1 on, REMO uses a fractional surface coverage
(each grid box can contain a land, a water and a sea ice fraction).
Therefore, when carrying out simulations with a newer REMO version, the
parameter IWATER has to be set to 1. For the fractional version, there
are also modified scripts ending on ``..._frac.sh`` that have to be
executed (see below).

Step 1
^^^^^^

After putting in the right grid parameters, the first script can be
executed by running:

::

    ./first.sh

Note that this script will also check for the endianess of the machine
you are working on and, if neccessary, convert some binary files.
Afterwards, it will compile and execute the ``good.exe`` program and you
will see some output similar to this:

::

    Distance at ilnord: 0.013161 Distance at ilsued: 0.008993 
    fline:    74.3025 iline=900 ityp=12 JB=435 JL=292 Weight= 0.000000 reg: lat=  74.303 lon= 179.991 rot: lat=  53.936 lon=   8.169 
    fline:    73.0167 iline=1000 ityp=12 JB=435 JL=340 Weight= 0.000000 reg: lat=  73.017 lon= 179.995 rot: lat=  55.108 lon=   9.081 
    fline:    71.7632 iline=1100 ityp=12 JB=435 JL=368 Weight= 31.000000 reg: lat=  71.763 lon= 179.986 rot: lat=  56.242 lon=  10.030 
    fline:    70.5384 iline=1200 ityp=12 JB=435 JL=391 Weight= 0.000000 reg: lat=  70.538 lon= 179.995 rot: lat=  57.344 lon=  11.003 
    ...

.. warning:: Running the first script might take some time. If the script
 fails due to some segmentation faul error, the reason might lie in an
 insufficient stack size. Setting the stack size to unlimited might often
 resolve this problem using, e.g,
 ::

    ulimit -s unlimited

| If the script still fails to execute, it might be worth trying to use
  a different C compiler in the ``system_settings.sh`` scripts, e.g.,
  the GNU Fortran compiler ``gfortran``.
| If the first script runs successfully, it should finish with some
  output similiar to:

::

    ...
    Erstelle neue Land-See-Maske
    Erstelle Grads-Dateien
    Ende des ersten Teil

Step 2
^^^^^^

For the next step, the script ``rotcoord.sh`` has to be executed:

::

    ./rotcoord.sh

The script creates corners and intervals of latitude and longitude in
geographic coordinates. Among other things, this script will give
information (standard output) on the four corners of the chosen model
domain in real-world lat/lon coordinates.

::

    links unten  (norm) - lower left corner
    rechts unten (norm) - lower right corner
    links oben   (norm) - Upper left corner
    rechts oben  (norm) - upper right corne

With this information one can check whether the model domain has been
correctly placed or not. For the Europe 0.11example, the output should
be something like this

::

    ...
             435           435
               1           435
     Max / Min      (WO):    69.04558        -48.28784    
     Max / Min      (NS):    74.45499         21.14123    
     links unten  (norm):   -10.37563         21.14123    
     rechts unten (norm):    36.75190         24.34295    
     links oben   (norm):   -48.28784         60.96326    
     rechts oben  (norm):    69.04558         67.81503    
     links unten   (rot):   -29.03500        -24.03500    
     rechts oben   (rot):    18.70500         23.70500
    ...

.. warning:: that running the script ``rotcoord.sh`` is not optional because it
 provides files that are used by other scripts! If you are happy with the
 result, you can run ``second.sh`` straight away. If you want the script
 to run faster have a look at ``gtopo30_tiles.gif``, choose tiles that
 are necessary for your domain and remove unnecessary .DEM files from
 ``gtopo`` subdirectory.

::

    ./second.sh &

This program will compile and execute another C program that interpolate
orography from the GTOPO30 elevation data. The output should look
similar to

::

    !!!! 
    !!!! The gtopo files exist and are correct build for this little Endian machine
    !!!! 
    gpolphi = 39.250000, gpollam = -162.000000
    1. part for file: gtopo/W180N90.DEM, gj: 0 
    1. part for file: gtopo/W180N90.DEM, gj: 1000 
    1. part for file: gtopo/W180N90.DEM, gj: 2000 
    1. part for file: gtopo/W180N90.DEM, gj: 3000 
    ...
    ...
    2. part for file: gtopo/E120S60.DEM, gj: 2000 
    2. part for file: gtopo/E120S60.DEM, gj: 3000 
    Erstelle Grads-Dateien
    Ende des zweiten Teils

If the ``second.sh`` script finished successfully, one can view the
result for the min, mean and max values of the orography with cdo:

::

    cdo info europa011_oro.srv

and the output should be similar to this

::

    Level Gridsize    Miss :     Minimum        Mean     Maximum : Parameter ID                        
        0   189225       0 :     -395.99      205.24      3587.7 : 129

A good way of checking the interpolated and rotated orography data is by
converting the srv file to netcdf and have a quick look with ``ncview``.
The srv file can be converted using the REMO table of cdo with the
following command:

::

    cdo -t remo -f nc copy europa011_oro.srv europa011_oro.nc

The resulting orography data (FIB, code=129) can be plotted using

::

    ncview europa011_oro.nc

and the result should look similar to Figure [fig:europa011]

.. figure:: ./fig/europa011.eps
   :alt: Orography Data (FIB, code=129) for the Eurocordex Domain with a
   resolution of 0.11and how it should look like after the execution of
   the ``second.sh`` script.
   :width: 50.0%

   Orography Data (FIB, code=129) for the Eurocordex Domain with a
   resolution of 0.11and how it should look like after the execution of
   the ``second.sh`` script.

Step 3
^^^^^^

The third step is to create the FAO-soiltype-dataset for your domain.
Start the scripts in the order:

::

    ./third.sh  
    ./fourth.sh 

If one of the poles is located inside the model domain, the scripts
``third_b.sh`` and ``fourth_b.sh`` have to be used instead. Since the
REMO5.7, the following scripts have to be used in their fractional
version and the next script is executed using

::

    ./fifth_frac.sh

Before REMO5.7, which had no fractional land, water and sea ice, use

::

    ./fifth.sh

instead. Note that these scripts actually create, compile and execute
Fortran code.

Step 4
^^^^^^

In the fourth step, the script

::

    ./sixth_frac.sh

or

::

    ./sixth.sh

has to be executed respectively. This script finally creates the surface
library ``lib_europa011_frac`` in IEG format. The content can be checked
using cdo:

::

    cdo -t remo sinfo lib_europa011_frac

which should give output similiar to

::

       File format : IEG  LITTLEENDIAN
        -1 : Institut Source   Ttype    Levels Num    Points Num Dtype : Parameter ID
         1 : MPIMET   REMO     instant       1   1    189225   1  F32  : 129.128       
         2 : MPIMET   REMO     instant       1   1    189225   1  F32  : 172.128       
         3 : MPIMET   REMO     instant       1   1    189225   1  F32  : 173.128       
         4 : MPIMET   REMO     instant       1   1    189225   1  F32  : 174.128       
         5 : MPIMET   REMO     instant       1   1    189225   1  F32  : 198.128       
         6 : MPIMET   REMO     instant       1   1    189225   1  F32  : 199.128       
         7 : MPIMET   REMO     instant       1   1    189225   1  F32  : 200.128       
         8 : MPIMET   REMO     instant       1   1    189225   1  F32  : 212.128       
         9 : MPIMET   REMO     instant       1   1    189225   1  F32  : 226.128       
        10 : MPIMET   REMO     instant       1   1    189225   1  F32  : 229.128       
        11 : MPIMET   REMO     instant       1   1    189225   1  F32  : 272.128       
        12 : MPIMET   REMO     instant       1   1    189225   1  F32  : 273.128       
        13 : MPIMET   REMO     instant       1   1    189225   1  F32  : 274.128       
       Grid coordinates :
         1 : lonlat                   : points=189225 (435x435)
                                 rlon : -29.035 to 18.705 by 0.11 degrees
                                 rlat : -24.035 to 23.705 by 0.11 degrees
                            northpole : lon=-162  lat=39.25
       Vertical coordinates :
         1 : surface                  : levels=1
       Time coordinate :  1 step
      YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss
      0017-02-28 12:00:00

Note that the Grid coordinates should match the parameters that were set
in the header of the ``first.sh`` script and that the codes (Parameter
ID) should match the list in Table [tab:surflib].

Step 5
^^^^^^

In the last step, the files for the mean annual cycle of the leaf area
index, vegetation ratio and albedo are created by running

::

    ./vegcycle_1.sh
    ./vegcycle_2.sh
    ./albcycle.sh

in this order. These scripts create the files ``vltyear_europa011.srv``,
``vgryear_europa011.srv`` and ``albyear_europa011.srv``.

Summary
^^^^^^^

If everything worked, the five steps should have created the following
files for the Eurocordex domain with a resolution of 0.11:

-  ``lib_europa011_frac``

-  ``vltyear_europa011.srv``

-  ``vgryear_europa011.srv``

-  ``albyear_europa011.srv``

Note that the surface library itself contains 435x435 cells while the
mean annual cycle files contain 433x433 cells (which can be checked with
``cdo sinfo``). The surface library file ``lib_europa011_frac`` is used
in the following step to create forcing data from global data sets while
the mean annual cycle files will be used directly for the REMO model run
and, therefore, already have the right grid specifications. If nothing
went wrong, the files that were created in this example should match the
files that can be found on MISTRAL in ``/pool/data/remo`` and
``/work/ch0636/surflibs`` respectively. This can be checked by using
``cdo diff``.

Driving Data and Boundary Conditions
------------------------------------

To run the REMO model, apart from the appropriate soil data created with
the bodlib tool, you will also need boundary forcing data, which is the
so called ’a-files’ (a stands for ’Antrieb’, which means forcing in
German). The purpose of a-files is to provide initial and boundary
conditions for the planned REMO run, that is why there is typically an
a-file for every six hours of simulation time. Since the timestep of the
REMO run is usually much shorter (e.g., 60 s), REMO will interpolate
forcing data in between two a-files. E.g., in the beginning of a
simulated month, REMO will read in the first two boundary files (e.g., 0
hours and 6 hours) and start the simulation of the 6 hours in between on
a timestep of, e.g, 60 s by interpolating the forcing data between the
a-file at 0 hours and 6 hours. However, in the process of preparing your
input data for a REMO run, you have to create these a-files for your
domain of interest. However, there are some ’preprocessors’ for this
which you can adapt to your domain. The input to these preprocessors can
come from any global model that provides data for your domain, e.g., any
GCM model, reanalysis data or even another REMO run on a coarser grid.
However this global data has to be prepared for the preprocessor in a
certain file format, called g-files (global file). These files have, in
general, the same format as the a-files but they provide global data
from which the preprocessor can create a-files for any domain of
interest by interpolation to your resolution. The g-files are archived
and there is a good chance that they already exist for the forcing data
you want to use (e.g., ERA-INTERIM). However, the preprocessing of these
g-files into a-files still has to be done by the user.

Creating Global Files (g-files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The preparation of global files stronlgy depends on the available global
data. If the data comes from a GCM, one has to consult carefully the
meta data information of the model to determine which fields are
available and how they correspond to the REMO code convention. Once the
global file exist in a format the REMO preprocessor can work with, the
user can create forcing files from the global files for any domain and
resolution of interest by using the REMO preprocessor. Creating the
global files is not straight-forward since the different GCMs create
their own model output data usually using different kinds of meta
information or file formats.

RemapToREMO - Creating Forcing Files (a-files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Forcing files (a-files) are created from g-files using the
preprocesser. The preprocessor is a Fortran program that can read a
g-file and does the interpolation to the model domain of interest
including the transformation to the rotated grid of REMO. Each a-file
contains forcing data for a certain date and time and there is usually
a forcing file for each 6 hours (e.g., for each day at 00 hours, 06
hours, 12 hours, 18 hours). The a-files usually contain about 35 2D
and 3D variables (depending on some switches in the REMO preprocessor
configuration) that initialize and force the model run. They are
listed in table [tab:afile\_content]. The first 9 fields are the
minimum that is required to be in each forcing file to update the
boundary conditions dynamically with a 6-hour interval, the fields for
the sea ice depth (``SICED``) and liquid water content (``QW``) are
optional. The rest of the fields are only needed at the start of the
model run to define initial conditions. Consequently, these fields are
required to be, at least, in the a-file of the initilisation date but
not neccessarily in all of them (since these fields are constant in
time anyway). However, most often it is a good idea to include all 35
fields in all a-files so that the model can be initalized at any time.
The additional fields are also useful for comparision of input and
output data.

+-----------+--------+-------+--------------------------------------------+
| Name      | Dim    | Code  | Description                                |
+===========+========+=======+============================================+
| U         | MOKE   | 131   | U-velocity                                 |
+-----------+--------+-------+--------------------------------------------+
| V         | MOKE   | 132   | V-velocity                                 |
+-----------+--------+-------+--------------------------------------------+
| T,        | MOKE   | 130   | Air temperature                            |
+-----------+--------+-------+--------------------------------------------+
| QD        | MOKE   | 133   | specific humidity                          |
+-----------+--------+-------+--------------------------------------------+
| PS        | 1      | 134   | surface pressure                           |
+-----------+--------+-------+--------------------------------------------+
| QDBL      | 1      | 84    | specific humidity surface (land)           |
+-----------+--------+-------+--------------------------------------------+
| TSWECH    | 1      | 55    | surface temperature (water)                |
+-----------+--------+-------+--------------------------------------------+
| TSIECH    | 1      | 56    | surface temperature (ice)                  |
+-----------+--------+-------+--------------------------------------------+
| SEAICE    | 1      | 210   | sea ice cover                              |
+-----------+--------+-------+--------------------------------------------+
| SICED     | 1      | 211   | sea ice depth                              |
+-----------+--------+-------+--------------------------------------------+
| QW        | MOKE   | 153   | liquid water content                       |
+-----------+--------+-------+--------------------------------------------+
| TSLECH    | 1      | 54    | surface temperature (land)                 |
+-----------+--------+-------+--------------------------------------------+
| TSN       | 1      | 206   | snow temperature                           |
+-----------+--------+-------+--------------------------------------------+
| TD3       | 1      | 207   | soil temperature                           |
+-----------+--------+-------+--------------------------------------------+
| TD4       | 1      | 208   | soil temperature                           |
+-----------+--------+-------+--------------------------------------------+
| TD5       | 1      | 209   | soil temperature                           |
+-----------+--------+-------+--------------------------------------------+
| TD        | 1      | 170   | deep soil temperature                      |
+-----------+--------+-------+--------------------------------------------+
| TDCL      | 1      | 183   | soil temperature                           |
+-----------+--------+-------+--------------------------------------------+
| WSECH     | 1      | 140   | soil wetness                               |
+-----------+--------+-------+--------------------------------------------+
| SN        | 1      | 141   | snow depth                                 |
+-----------+--------+-------+--------------------------------------------+
| WL        | 1      | 194   | skin reservoir content                     |
+-----------+--------+-------+--------------------------------------------+
| FIB       | 1      | 129   | surface geopotential (orography)           |
+-----------+--------+-------+--------------------------------------------+
| BLA       | 1      | 172   | land sea mask                              |
+-----------+--------+-------+--------------------------------------------+
| GLAC      | 1      | 232   | glacier mask                               |
+-----------+--------+-------+--------------------------------------------+
| AZ0       | 1      | 173   | surface roughness length                   |
+-----------+--------+-------+--------------------------------------------+
| VGRAT     | 1      | 198   | vegetation ratio                           |
+-----------+--------+-------+--------------------------------------------+
| FOREST    | 1      | 212   | vegetation type                            |
+-----------+--------+-------+--------------------------------------------+
| ALBECH    | 1      | 174   | surface background albedo                  |
+-----------+--------+-------+--------------------------------------------+
| WSMX      | 1      | 229   | field capacity of soil                     |
+-----------+--------+-------+--------------------------------------------+
| VLT       | 1      | 200   | leaf area index                            |
+-----------+--------+-------+--------------------------------------------+
| FAO       | 1      | 226   | FAO data set (soil data flags)             |
+-----------+--------+-------+--------------------------------------------+
| VAROR     | 1      | 199   | orographic variance (for surface runoff)   |
+-----------+--------+-------+--------------------------------------------+
| BETA      | 1      | 272   | shape parameter for Arno Scheme            |
+-----------+--------+-------+--------------------------------------------+
| WMINLOK   | 1      | 273   | minimum subgrid wcap (field capacity)      |
+-----------+--------+-------+--------------------------------------------+
| WMAXLOK   | 1      | 274   | maximum subgrid wcap (field capacity)      |
+-----------+--------+-------+--------------------------------------------+

Table: Forcing Fields

RemapToREMO
^^^^^^^^^^^

The task of the REMO preprocessor (``RemapToREMO``) is to interpolate
the coarse resolution input forcing data to the resolution and (rotated)
grid of the regional domain. The input forcing data are either from:

-  global data from reanalysis datasets;

-  model output from global climate simulations (GCMs); and

-  model output from regional climate data simulations.

The interpolated forcing data are combined with the surface data
created with the bodlib toolkit (Section [sec:preproc:bodlib]) and are
stored as **a**-files.
The new preprocessor is now called ``RemapToREMO``. It contains all
neccessary source code to compile any preprocessor configuration
including reanalysis with ERA-INTERIM, global model data that fullfils
nc-conventions as well as a configuration for preparing remo output
data for double nesting. The different configurations originate from
several legacy codes that have been combined to avoid further code
duplication. This program will build the preprocessor executable that
then can be used to convert g files generated from the global model
output or t files generated by the REMO in to a files, that are used
to force REMO.

Requirements
^^^^^^^^^^^^

In present works only with intel compiler, so you have to have ifort in
your ``$PATH`` variable. On DKRZ computers you can usually load intel
compiler by

module load intel

Setup and compilation
^^^^^^^^^^^^^^^^^^^^^

The directory structure of the remapping program contains the following

::

    setup        # setup script to manage configurations
    setups       # directory containgin preprocessor configrations
    source       # main source code
    vc_tables    # tables with additional vertical hybrid sigma coordinates

In the setups directory one can find several possible preconfigured
options for the precprocessor. For example:

::

    era_interim  # configuration for ECMWF ERA-INTERIM reanalysis
    hadgem2      # configuration for HadGEM2 
    gcm_cf       # global model data with cf-conventions, e.g., from ESGF
    remo         # preprocessor for double nesting
    ...

The configruations in the setup directory typically contain some special
implementations of specific subroutines from the source directory. These
special implementations are combined with the rest of the generic source
code into an object directory where the configuration can be compiled.

To create an executable for a specific configuration, the setup script
will handle the source code management. For example, to setup the
preprocessor for ERA-INTERIM data, the configuration is chosen by typing

::

    ./setup era_interim -auto -objdir=era_interim

in the root directory.

The ``era_interim`` directory will be created in the root directory of
the program and contains source files of the preprocessor and the
Makefile.

To compile the configuration, simply change to the newly created
directory and run:

::

    make

The resulting executable is usually called ``preproc``

| \| l \| l \| p9cm \| l \| [tab:preproc\_input] Namelist & Parameter &
  Description & Datatype
| & & &

PrepareAFiles - Running the preprocessor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| Once the preprocessor has been compiled successfully, it requires a
  ``Fortran`` Input file containing a number of namelists to configure
  it, see table [tab:preproc\_input]. All g-files have to be located in
  a certain directory (``YGMCAT``) and the remapped data will be written
  to the output directory (``YEMCAT``). However, similar to a model run
  (see Chapter [cha:model\_run]), it is usually recommended to prepare
  forcing data on a monthly basis while the start hour (``HANF``) and
  end hour (``HENDE``) are adapted with respect to the start date. The
  time frequency is usually 6 hours (``DH``) if global model data is
  remapped, however, this also depends on the temporal resolution of the
  driving model’s global data and that of the resulting g-files.
  Consequently, the time frequency of the double nesting approach
  depends on the time frequency in which data is written by the parent
  model run with ``REMO``.
| However, a set of python scripts for controlling the preprocessor is
  availabe in the ``REMO`` tools suite and is called ``PrepareAFiles``.
  In particular the script do the following:

-  [optional] download files with original external forcing (g-files in
   case of global model or t-files in case of remo) from DKRZ archive

-  convert g- (or t- in case of REMO nesting) files to a-files with use
   of executable created by ``RemapToREMO``

-  [optional] interpolate variables from a-files to pressure levels with
   the ``druintzr`` executable (p-files)

-  [optional] create monthly means with ``mitzrpe`` executable (l-files)

-  tar resulted a- and optionally p- and l-files

-  upload tar-files back to the DKRZ archive

Requirements
^^^^^^^^^^^^

-  PyRemo

-  Python

-  surface library for the target REMO setup (created by the
   ``BodLibKit``)

-  Executable created by ``RemapToREMO``

-  [optional] Executable created by ``druintzr`` if creation of p-files
   is required

-  [optional] Executable created by ``mitzrpe`` if creation of l-files
   (monthly means) is required

The user hase to setup no-password acces to DKRZ archive. To do so, add
the following to your ``$HOME/.netrc`` file:

::

    machine tape
       login mylogin
       password mypassword

The forcing files (‘g‘ or ‘t‘) are stored in ‘tar‘ files on monthly
basis with 6h interval.

Usage
^^^^^

The main script is called ``create_input.py``, that hase to be called
with a configuration file as an argument:

::

    python create_input.py config/config.ini

An alternative is to use the ``submit.py`` script if the config file
contains a ``[batch]`` section.

::

    python submit.py config/config.ini

The ``submit.py`` script will create a series of batch scripts each
handling a certain chunk of the entire timeframe for which forcing data
is requested. E.g., the user can choose ‘chunks=month‘ and the
‘submit.py‘ script will create a series of job scripts in the
subdirectory ‘scripts‘ and a configuration file for on month. The
jobscript itself calls the ``create_input.py`` script and use the
corresponding configuration file.

The main configuration file should reside in the ‘config‘ subdirectory
and might be exchanged between users. The configuration for input data
is extracted into a ‘global‘ config file (for the input of g-files) and
a ‘grid‘ configuration. These configuration files can be found in the
corresponding subdirectories of the ‘config‘ directory and should be
specified in the main ‘config‘ file in the ‘global‘ and ``grid_out``
sections respectively. This should prevent code duplication and the user
can easily choose between different predefined domains and global model
input.
