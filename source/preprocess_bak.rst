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

