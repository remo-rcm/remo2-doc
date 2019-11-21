Preprocessing
=============

This chapter deals with a more detailled introduction into the general
simulation workflow, in the context of REMO often called ’experiment’.
It requires some proper planning since a useful REMO run needs some well
prepared input data. In this chapter, we describe how to plan and setup
your own REMO experiment. This includes choosing a domain and resoultion
([sec:preproc:plan]), creataing your own surface data
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
experiments is available on the REMO homepage.

Which resolution do you need? Does it require more than one nesting?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

More than one nesting is necessary to avoid big (factor 10) scale jumps
between the driving data and REMO. Both questions are linked as high
resolution runs (<10km) require usually a double nesting. For example,
one might produce a REMO experiment with a horizontal resolution of 50km
forced by GCM data. The output data of the REMO experiment is used as
boundary data for a second domain nesting with REMO afterwards. The
second REMO run resolves a smaller area inside of the previous REMO
domain,e.g. with a 10km horizontal grid. The vertical resolution is
usually 27 levels nad can be switched to up to 49 levels. The level
choice is independent from the vertical resolution of the driving data.

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
ERA-INTERIM, and operational analysis. The kind of run determines which
forcing data you need and how to configure the preprocessor (Section
[sec:preproc:forcing]).

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

.. figure:: ./fig/REMO_workflow.eps
   :alt: Overview of the general workflow for a user customated REMO
   experiment.
   :width: 80.0%

   Overview of the general workflow for a user customated REMO
   experiment.

| However, the actual files containing results from a global model or a
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
| In addition to the forcing data stored in g-files and a-files, REMO
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

| For the setup of a REMO simulation, the second step (after deciding on
  a name for the simulation) is to define the model domain and the
  horizontal grid spacing. A distinct name should be assigned to each
  model domain and it might also be good to include the horizontal
  resolution in the name, e.g., EUR-44 for a 0.44domain over Europe. For
  the horizontal discretisation, REMO uses a **rotated spherical grid**
  (Figure [fig:rot\_grid]). The grid should be placed in a way that the
  rotated equator approximately crosses the center of the model domain.
  This would ensure a negligible meridional convergence and almost equal
  real grid box sizes. For model domains within or close to the tropics
  (close to the real-world equator) a rotation of the grid is usually
  not necessary.
| A specific model domain has to be defined by setting the following
  parameters:

-  real-world latitude (lat) and longitude (lon) of the rotated North
   Pole (or South Pole)

-  horizontal resolution in the rotated system in degrees (valid for
   both the x- and the y-direction)

-  total number of grid boxes in x- and y-direction

-  lat/lon location of the lower left grid box center in the rotated
   system

| Typical horizontal resolutions for REMO are 0.5(approx. 55 km),
  0.44(approx. 50 km), 1/6(approx. 18 km) and 0.11(approx. 12 km). Not
  all numbers of grid boxes in x- and y-direction are allowed! The
  number of boxes in each direction (nx and ny) have to follow these
  rules (see Appendix [app:domainsize]):
| ``nx - 1 = 2 lx * 3 mx * 5 nx with lx not equal to 1``
| ``ny - 1 = 2 ly * 3 my * 5 ny with ly not equal to 1``

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

The surface library file itself is usually named something like

-  ``lib_[NAME_OF_DOMAN]_frac``

| where ``[NAME_OF_DOMAIN]`` indicates the name of the domain of the
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
specified in the Input Namelist (see Appendix [app:namelist]). The
surface library and the files for the annual cycles are domain specific
and have to be prepared for each model domain individually There is a
number of surface libraries and files for the mean annual cycle
available from GERICS on MISTRAL at this location:

.. code:: bash

    /work/ch0636/surflibs/

or ask for support from GERICS. However, in the following sections, we
describe how to create these files for a new domain.

Creating a new Surface Library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The surface library is created by interpolating data from the GTOPO30
global digital elevation model (DEM) to the REMO model grid. The
interpolation is done by using

-  shell-scripts, Fortan- and C-Programs (available from the REMO
   gitlab)

-  a global surface library and FAO-datasets (located at
   ``/hpss/arch/ch0636/bodlib/input_data/``)

-  global GTOPO orography (located at
   ``/hpss/arch/ch0636/bodlib/gtopo/``)

This collcetion of scripts and data is referred to as the *BodLibKit*.
You can find it either in the tutorial data in ``/BodLibKit`` or your
you can download it from the REMO gitlab at GERICS, e.g.,

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

The surface library is created by interpolating data from the GTOPO30
global digital elevation model (DEM) 3 and additional FAO-datasets to
the REMO model grid. The interpolation is done by using a number of
shell-scripts, Fortan- and C-Programs. These programs are partly created
and compiled on the fly by the shell scripts.

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

NOTE: The surface library covers the REMO model domain PLUS one row of
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
has to be set to 1. For the fractional version, there are also modiﬁed
scripts ending on ...\_frac.sh that have to be executed (see below).

Running the setup.sh script
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The setup.sh script is supposed to setup a new directory named after
``EXP`` (the domain name) in the BodLibKit root directory and to create
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

Step by Step Example for the Eurocordex Domain
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following, we give a detailled example of how to configure the
scripts of the BodLibKit for Eurocordex Domain with a resolution of 0.44
and how to create a new surface library and the files for mean annual
cycle. The first thing to do, is to setup a working directory configured
with your desired grid parameters (see Section [sec:grid\_params]). For
this example, we will use the ``/config/EUR-44.config`` which can be
found in the ``BodLibKit`` root directory.

::

    EXP=EUR-44               # Name of the area
    IWATER=1                 # 1=fractional surface library, 0=nonfractional

    RESOLUTION_X=131         # Number of Longitudes
    RESOLUTION_Y=123         # Number of Latitudes
    RESOLUTION=0.44          # Resolution of destination grid
    LON_LL=-32.17            # Longitude of origin - dest. in rotated system
    LAT_LL=-27.17            # Latitude of origin - dest. in rotated system
    #
    # lon/lat coordinates of South Pole! If you have the North Pole available,
    # you can compute the South Pole like this:
    #
    # LON_SP = LON_NP + 180 (even if it gets larger than 180 degrees)
    # LAT_SP = -1 * LAT_NP 
    #
    LON_SP=18.0              # Longitude of South Pole - destination grid
    LAT_SP=-39.25            # Latitude of South Pole - destination grid
    POL=0                    # Pole inside the domain
                             # 0 = No, 1 = North Pole, 2 = South Pole
    IGLAC=0                  # 0 = default
                             # 1 = REMOglacier version: surface parameters (except
                             #     WAVA and ICAP) will computed only based on non-
                             #     glaciated land fraction

| For this example, we put in the parameters for a grid that has a
  horizontal resolution of 0.44and that extends roughly over the
  Eurocordex domain with an appropriate grid rotation (Fig.
  [fig:rot\_grid]). The grid should be placed in a way that the rotated
  equator approximately crosses the center of the model domain. This
  would ensure a negligible meridional convergence and almost equal real
  grid box sizes. For model domains within or close to the tropics
  (close to the real-world-equator) a rotation of the grid is usually
  not necessary.
| To use this configuration file, just setup the working directory
  using:

.. code:: bash

    ./setup.sh EUR-44.config

The setup.sh script setups a new directory named after ``EUR-44`` in the
BodLibKit root directory and creates a ``work`` and ``results``
directory. In addition, the script also creates links to all neccessary
data in the ``EXP/work`` directory and copies the main shell scripts
from the ``/bin`` directory to the ``EXP`` directory.

Step 1
^^^^^^

You can now change to your working directory ``EUR-44`` and the first
script can be executed by running:

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

**NOTE:** Running the first script might take some time. If the script
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
             131           123
               1           123
     Max / Min      (WO):    84.15322        -56.33586    
     Max / Min      (NS):    77.25995         17.11300    
     links unten  (norm):   -11.71098         17.11300    
     rechts unten (norm):    41.54940         19.59249    
     links oben   (norm):   -56.33586         60.34155    
     rechts oben  (norm):    84.15322         65.54705    
     links unten   (rot):   -32.17000        -27.17000    
     rechts oben   (rot):    25.03000         26.51000
    ...

Note that running the script ``rotcoord.sh`` is not optional because it
provides files that are used by other scripts! If you are happy with the
result, you can run ``second.sh`` straight away.

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
result for the min, mean and max values of the orography with cdo
(intermediate working files will be created in the ``work`` subdirectory
of your ``EUR-44`` directory):

::

    cdo info work/EUR-44_oro.srv

and the output should be similar to this

::

    Level Gridsize    Miss :     Minimum        Mean     Maximum : Parameter ID                        
        0   189225       0 :     -395.99      205.24      3587.7 : 129

A good way of checking the interpolated and rotated orography data is by
converting the srv file to netcdf and have a quick look with ``ncview``.
The srv file can be converted using the REMO table of cdo with the
following command:

::

    cdo -t remo -f nc copy work/EUR-44_oro.srv EUR-44_oro.nc

The resulting orography data (FIB, code=129) can be plotted using

::

    ncview EUR-44_oro.nc

and the result should look similar to Figure [fig:europa044]

.. figure:: ./fig/europa044.eps
   :alt: Orography Data (FIB, code=129) for the Eurocordex Domain with a
   resolution of 0.44and how it should look like after the execution of
   the ``second.sh`` script.
   :width: 50.0%

   Orography Data (FIB, code=129) for the Eurocordex Domain with a
   resolution of 0.44and how it should look like after the execution of
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
library ``lib_EUR-11_frac`` in the results subdirectory in IEG format.
The content can be checked using cdo:

::

    cdo -t remo sinfo results/lib_EUR-44_frac

which should give output similiar to

::

      File format : IEG  LITTLEENDIAN
        -1 : Institut Source   Ttype    Levels Num    Points Num Dtype : Parameter ID
         1 : MPIMET   REMO     instant       1   1     16113   1  F32  : 129.128       
         2 : MPIMET   REMO     instant       1   1     16113   1  F32  : 172.128       
         3 : MPIMET   REMO     instant       1   1     16113   1  F32  : 173.128       
         4 : MPIMET   REMO     instant       1   1     16113   1  F32  : 174.128       
         5 : MPIMET   REMO     instant       1   1     16113   1  F32  : 198.128       
         6 : MPIMET   REMO     instant       1   1     16113   1  F32  : 199.128       
         7 : MPIMET   REMO     instant       1   1     16113   1  F32  : 200.128       
         8 : MPIMET   REMO     instant       1   1     16113   1  F32  : 212.128       
         9 : MPIMET   REMO     instant       1   1     16113   1  F32  : 226.128       
        10 : MPIMET   REMO     instant       1   1     16113   1  F32  : 229.128       
        11 : MPIMET   REMO     instant       1   1     16113   1  F32  : 272.128       
        12 : MPIMET   REMO     instant       1   1     16113   1  F32  : 273.128       
        13 : MPIMET   REMO     instant       1   1     16113   1  F32  : 274.128       
       Grid coordinates :
         1 : lonlat                   : points=16113 (131x123)
                                 rlon : -32.17 to 25.03 by 0.44 degrees
                                 rlat : -27.17 to 26.51 by 0.44 degrees
                            northpole : lon=-162  lat=39.25
       Vertical coordinates :
         1 : surface                  : levels=1
       Time coordinate :  1 step
      YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss
      0018-02-16 12:00:00

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

| in this order. These scripts create the files ``vltyear_EUR-44.srv``,
  ``vgryear_EUR-44.srv`` and ``albyear_EUR-44.srv`` in the results
  directory.
| **NOTE:** Please be aware that the MISTRAL is a Little Endian machine
  but we usually use Big Endian files for in and output of REMO data.
  This has something to do of how the machine reads binary data and it
  refers to the sequential order in which bytes are arranged. By
  default, all programs that run on MISTRAL create Little Endian files
  and you can check this using ``cdo sinfo``. However, if we want to use
  the newly created Surface Library for the ongoing tutorial and create
  forcing data, we need to convert the files to Big Endian format. This
  can be achieved using the ``cdo -b B copy`` command or simply use the
  available script for this, e.g., by executing

::

    ./convert_big_endian.sh

You can then check the endianes of your surface files, e.g., type

::

    cdo sinfo results/lib_EUR-44_frac

which you can find in you ``EUR-44`` configuration directory.

Summary
^^^^^^^

If everything worked, the five steps should have created the following
files for the Eurocordex domain with a resolution of 0.44:

-  ``lib_EUR-44_frac``

-  ``vltyear_EUR-44.srv``

-  ``vgryear_EUR-44.srv``

-  ``albyear_EUR-44.srv``

Note that the surface library itself contains 131x123 cells while the
mean annual cycle files contain 129x121 cells (which can be checked with
``cdo sinfo``). The surface library file ``lib_EUR-44_frac`` is used in
the following step to create forcing data from global data sets while
the mean annual cycle files will be used directly for the REMO model run
and, therefore, already have the right grid specifications. If nothing
went wrong, the files that were created in this example should match the
files that can be found on MISTRAL in ``/work/ch0636/surflibs/cordex``.
This can be checked by using ``cdo diff``.

Outlook: Driving Data and Boundary Conditions
---------------------------------------------

To run the REMO model, you do not only need the appropriate soil data
created with the bodlib tool, you will also need boundary forcing data,
so called ’a-files’ (a stands for ’Antrieb’, forcing in German). The
purpose of a-files is to provide boundary conditions for the entire REMO
run, that is why there is typically an a-file for every six hours of
simulation time. Since the timestep of the REMO run is usually much
shorter (e.g., 60 s), REMO will interpolate forcing data in between two
a-files. E.g., in the beginning of a simulated month, REMO will read in
the first two boundary files (e.g., 0 hours and 6 hours) and start the
simulation of the 6 hours in between on a timestep of, e.g, 60 s by
interpolating the forcing data between the a-file at 0 hours and 6
hours. However, in the process of preparing your input data for a REMO
run, you have to create these a-files for your domain of interest.
However, there are some ’preprocessors’ for this which you can adapt to
your domain. The input to these preprocessors can come from any global
model that provides data for your domain, e.g., any GCM model,
reanalysis data or even another REMO run on a coarser grid. However this
global data has to be prepared for the preprocessor in a certain file
format, called g-files (global file). These files have, in general, the
same format as the a-files but they provide global data from which the
preprocessor can create a-files for any domain of interest by
interpolation to your resolution. The g-files are archived and theres is
a good chance that they already exist for the forcing data you want to
use (e.g., ERA-INTERIM). However, the preprocessing of these g-files
into a-files still has to be done by the user.

Creating Global Files (g-files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The preparation of global files stronlgy depends on the available global
data. If the data comes from a GCM, one has to consult carefully the
meta data information of the model to determine which fields are
available and how they correspond to the REMO code convention. Once the
global file exist in a format the REMO preprocessor can work with, the
user can create forcing files from the global files for any domain and
resolution of interest by using the REMO preprocessor. Creating the
global files is not straigh-forward since the different GCMs create
their own model output data usually using different kinds of meta
information or file formats.

Creating Forcing Files (a-files)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| Forcing files (a-files) are created from g-files using the
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
  boundary conditions dynamically with a 6 hour interval, the fields for
  the sea ice depth (``SICED``) and liquid water content (``QW``) are
  optional. The rest of the fields are only needed at the start of the
  model run to define initial conditions. Consequently, these fields are
  required to be, at least, in the a-file of the initilisation date but
  not neccessarily in all of them (since these fields are constant in
  time anyway). However, most often it is a good idea to include all 35
  fields in all a-files so that the model can be initalized at any time.
  The additional fields are also useful for comparision of input and
  output data.

|  \| l \| l \| l \| p7cm \| Name & Layers & Code & Description
| U & MOKE& 131&U-velocity
| V & MOKE& 132&V-velocity
| T, & MOKE& 130&Air temperature
| QD & MOKE& 133&specific humidity
| PS & 1& 134&surface pressure
| QDBL & 1& 84&specific humidity surface (land)
| TSWECH & 1& 55&surface temperature (water)
| TSIECH & 1& 56&surface temperature (ice)
| SEAICE & 1& 210&sea ice cover
| SICED & 1& 211&sea ice depth
| QW & MOKE& 153&liquid water content
| TSLECH & 1& 54&surface temperature (land)
| TSN & 1& 206&snow temperature
| TD3 & 1& 207&soil temperature
| TD4 & 1& 208&soil temperature
| TD5 & 1& 209&soil temperature
| TD & 1& 170&deep soil temperature
| TDCL & 1& 183&soil temperature
| WSECH & 1& 140&soil wetness
| SN & 1& 141&snow depth
| WL & 1& 194&skin reservoir content
| FIB & 1& 129&surface geopotential (orography)
| BLA & 1& 172&land sea mask
| GLAC & 1& 232&glacier mask
| AZ0 & 1& 173&surface roughness length
| VGRAT & 1& 198&vegetation ratio
| FOREST & 1& 212&vegetation type
| ALBECH & 1& 174&surface background albedo
| WSMX & 1& 229&field capacity of soil
| VLT & 1& 200&leaf area index
| FAO & 1& 226&FAO data set (soil data flags)
| VAROR & 1& 199&orographic variance (for surface runoff)
| BETA & 1& 272&shape parameter for Arno Scheme
| WMINLOK & 1& 273&minimum subgrid wcap (field capacity)
| WMAXLOK & 1& 274&maximum subgrid wcap (field capacity)

Forcing Data for the 0.44 Eurocordex Domain
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the last step, we will now use the Fortran preprocessor to create
Forcing data (a-files) for one month of simulation data (January of
1979) using our newly created Soil Library. The preprocessor is prepared
in the ``/PrepareForcing`` directory of the tutorial data. It contains
the following files and subdirectories

::

      /data        # data directory for output
      /INPUT       # Namelist file for the Fortran preprocessor
      /RemapToRemo # The Fortran preprocessor source code

Let’s have a look at the Namelist INPUT file. Open it with your editor
of choice (or use the ``less`` command). The INPUT file should like
something like this:

::

     &CONTRL
      YANDAT='1979010100', HANF=0, HENDE=744, DH=6 /
     &EMGRID
      IEEM=129, JEEM=121, KEEM=27,
      DLAMEM=0.44, DPHIEM=0.44,
      POLLAM=-162.0, POLPHI=39.25, LAMLUEM=-31.73, PHILUEM=-26.73  /
     &GMGRID
      IEGM=512, JEGM=256, KEGM=60 /
     &DATEN
      YBDPDN='lib_EUR-44_frac',
      YBDCAT='../BodLibKit/EUR-44/results/',
      YGMCAT='/work/ch0636/remo_tutorial/global_data/era-interim/197901/',
      YEMCAT='./data/xa',
      YGMEXPN='000',
      YEMEXPN='000',
      YGMUSER='001',
      YEMUSER='051' /
     &PRICTR
      PRGPK=.FALSE.,
      PRDPDT=.FALSE.,
      PRPS=.FALSE.,
      PRT=.FALSE.,
      PRUD=.FALSE.,
      PRVD=.FALSE.,
      PRGRID=.FALSE. /

| It is standard Fortran namelist syntax and seems to be a little
  cryptic due to the history of the legacy code. However, you can see
  that the path to the newly created soil library is already prepared
  (``YBDCAT``) and there is also a path to the global input data from
  the ERA-INTERIM reanalysis model (``YGMCAT``). The global data usually
  takes quite a lot storage capacity, so we put in a central directory
  outside the tutorial directory. The output files will be written to
  the ``data`` directory (``YEMCAT``). There is also a namelist that
  controls the time range for which we will produce forcing data
  (``CONTROL``) and a namelist for controling the resolution of in and
  ouput data (``EMGRID`` and ``GMGRID``). You can see that the global
  data has a numerical resolution of 512x256 grid boxes which
  corresponds to a resolution of about 80km for the ERA-INTERIM model.
  The namelist is configured for preprocessing this data to our
  Eurocordex Domain on 0.44resolution which corresponds to a spatial
  resolution of about 50km.
| Before we can start the preprocessing, you need to compile the Fortran
  source code which is located in the ``RemapToRemo`` directory. All you
  have to do is to change to the directory and execute the Makefile,
  e.g.,

::

    cd RemapToRemo
    make

If something goes wrong, open up the ``Makefile.h`` header file and
check out the compiler settings. Remember that we usually use the Intel
Fortran compiler and that you have a suitable module loaded to use it.
If the compilation went through, it creates an executable called
``preproc``. Now you have to change back to the ``PrepareForcing``
directory where the INPUT file is located. The preprocessor will
automatically read in the INPUT file, so we have to executed it where
the file is located. You can execute the preprocessor after successful
compilation by typing, e.g.,

::

    ./RemapToRemo/preproc

If everything worked out, the preprocessor should start by reading in
the ``lib_EUR-44_frac`` soil library and the first g-file and the output
should something like this:

::

    OEFFNE: ../BodLibKit/EUR-44/results/lib_EUR-44_frac                                                                                                                                                                                                             
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010100                                                                                                                            
    SAVE:   ./data/xa/a051000a1979010100                                                                                            
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010106                                                                                                                            
    SAVE:   ./data/xa/a051000a1979010106                                                                                            
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010112                                                                                                                            
    SAVE:   ./data/xa/a051000a1979010112                                                                                            
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010118                                                                                                                            
    SAVE:   ./data/xa/a051000a1979010118                                                                                            
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010200                                                                                                                            
    SAVE:   ./data/xa/a051000a1979010200                                                                                            
    OEFFNE: /work/ch0636/remo_tutorial/global_data/era-interim/197901/g001000a1979010206

| The preprocessor creates new a-files in our output directory with a
  temporal resolution of 6 hours. If you have a look back at the INPUT
  file, you can see that the preprocessing will last for 744 hours
  (``HENDE``) which corresponds to a full month of 31 days.
| After the preprocessing has finished, you can check out the newly
  created a-files in the ``data/xa`` directory using, e.g.,
  ``cdo sinfo``. You can also convert them again to NetCDF format using
  the REMO metadata table and use the ``ncdump -h`` command to get some
  more detail about the content of these files. Do the files contain all
  the data that is listed in Table [tab:afile\_content]? As a final
  task, you can go back to the ``ModelRun`` directory and try to start
  another model run using your newly created forcing data and the
  monthly files that we created with the Surface Library. You could also
  try to run the Eurocordex Domain in a higher resolution by using the
  ``EUR-11`` configuration. Good Luck!
