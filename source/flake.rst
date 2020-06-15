.. _sec_flake:

FLake lake model
================

Background
----------

The default configuration of REMO includes lakes, but they are treated
with a very simple approach: the surface temperature and icing
conditions of lakes are derived from the forcing data by using the
nearest sea-point as a reference. The fraction of lakes is included in
the seawater or sea-ice tile. This approach has many deficiencies, for
example, in Northern Europe the lake surface temperatures are too high
during autumn and the ice season is too short. These causes artificial
biases to temperature and precipitation (open lake is a source for
moisture).


FLake lake model
----------------

In cooperation with the Finnish Meteorological Institute the FLake
lake model :cite:`Mironov2008` was implemented into REMO
:cite:`Pietikainen2018`. FLake is a thermodynamic freshwater lake
model, which predicts the mixing conditions and vertical temperature
structure of lakes on time scales from a few hours to several years
:cite:`Mironov2008,Mironov2010`. FLake's water module calculates the
heat and kinetic energy budget for the upper mixed-layer and the basin
bottom. The model can be used for various basin depths and even though
it has been intended for use in NWP and climate models, it can be used
as a standalone model as well. FLake uses a bulk-approach and is based
on a self-similarity (assumed-shape) representation of the temperature
profile in the mixed-layer and thermocline. In addition, it calculates
the mixed-layer depth as well as the temperature and thickness of both
ice and snow on ice. Optionally, FLake can calculate the flux between
the bottom sediment and the lake bottom. In this case, thickness and
temperature of the thermally active upper sediment layer are
calculated. A more detailed description of FLake can be found in
:cite:`Mironov2010`, where the authors give a detailed list of FLake
parameters and more information about the numerical core. Details of
the REMO-FLake implementation can be found from
:cite:`Pietikainen2018`.

For further information, please visit:
http://www.flake.igb-berlin.de/

FLake preprocessor
------------------

The preprocessor of FLake is available here:

http://git.gerics.de/REMO/remo-flake-preprocessor.git

It readme is available, but we will still go through the main points.

Use lakes.sh in BodLib creation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first thing to do is to run lakes.sh (in folder for_bodlib) in the
same directory where the original bodlib was created. It is highly
recommended to run lakes.sh even in cases when there is no intent to
use FLake. For domains covering the Black Sea it is recommended to use
a fixed ``goge2_0ll`` file, in which the Black Sea is treated as a sea
(in the original it is a lake). Instruction to get the file in the
next section.

Prepare global lake file
~~~~~~~~~~~~~~~~~~~~~~~~

The next part needs to be done only once: it creates a global lake
depth file based on the data by :cite:`Chouldga2014`. The original
lake depth information is given in a binary-file format. The
``flake_GLCC_data_dat2netcdf.f90`` file in ``Prepare_global_data``
folder can be used to create a netcdf file out of the binary data
file. The original binary file can be found from tape archieve:
``/hpss/arch/ch0636/g300096/FLake_input`` from the file
``GlobalLakeDepth_v3release.tar.gz``. In the same tape archieve folder
one also finds the glcc file: ``goge2_0ll_fixed.tar.gz`` This file
does not consider Black Sea to be a lake and should be used to prepare
BodLib and in FLake data preparation.

The program ``flake_GLCC_data_dat2netcdf.f90`` will use also the GLCC
information. It checks if the lake input file has information about
the lake depth when GLCC has a lake. If not, a value of -999 is
given. This value will be later on replaced with domain specific
default depth. If the lake input data has a lake, but GLCC not, the
lake depth is set to zero. Otherwise the fractional cover used in REMO
would have inconsistencies.

When the global netcdf is ready, it can be used to create any REMO
domain. Thus, there is no need to redo this step and it can be
skipped.


Prepare the domain specific input file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The final step is to run the script
``prepare_flake_glcc_depth_data_to_domain.py`` which is in the
``Preprocessor_for_global_data`` folder. This script will create a
FLake input file that contains lake fraction and depth. In this file
one has to set the default depth for lakes in the target domain
(please check :cite:`Chouldga2014`). After running
``prepare_flake_glcc_depth_data_to_domain.py`` one should have a FLake
input file which contains the lake fraction (FLFRA, code=493) and lake
depth (FLADE, code=494). One should always check that the sum on FLFRA
and land-sea mask BLA of any forcing file is either 1 or 0, otherwise
contact Joni-Pekka Pietikäinen @GERICS (if the forcing files are
created without using lakes.sh, it is possible that due to compiler
changes etc. the sum of FLFRA and BLA will not be exactly 1 or 0 when
applying lakes.sh in a later phase; in these cases a correction must
be done).


Running REMO-FLake
------------------

Running REMO-FLake does not differ much from a normal REMO
simulation. Three biggest changes are the compilation procedure, a new
namelist, and new output variables.


Compiling the model
~~~~~~~~~~~~~~~~~~~

The first thing to do is to compile the model with FLake. This can be
done in the following way:
::

    ./setup -auto Remo2015FLake -objdir=Remo2015FLake

This will link the needed files and the model can be normally
compiled.

Setting the namelist
~~~~~~~~~~~~~~~~~~~~

FLake needs to have it's own namelist. It can be used, for example,
after the physics namelist PHYCTL. In the following an example of the
FLake namelist:

::

   &FLKCTL
    LFLBSMO=.TRUE.,
    LFLWARM=.FALSE.,
    LFLSNMO=.TRUE.,
    TBOMISS=271.37,
    FLKDEPM=50.0,
    FLKSEDM=5.0,
    ISNHECO=1,
    IFLREL=0,
    YFLAKED='/work/ch0636/g300096/Flake/remo-flake-preprocessor/FLakeLibrary',
    YFLAKEF='flake_v3_glcc_defD10.0m_frac_cordex044.nc',
    YFLAWAD=' ',
    YFLAWAF=' '
   /

The following table will explain the different variables:

+------------+-------------+---------------+-------------------------------------------------------------------------------+
| Namelist   | Parameter   | Default value | Description                                                                   |
+============+=============+===============+===============================================================================+
| FLKCTL     | LFLBSMO     | FALSE         | Is the bottom sediment module on/off                                          |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | LFLWARM     | FALSE         | Is this a warm-start                                                          |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | LFLSNMO     | FALSE         | Is the snow-on-ice module on/off                                              |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | TBOMISS     | 271.13        | What is the missing value of forcing data for water temperatures              |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | FLKDEPM     | 50.0          | Set an artificial maximum lake depth                                          |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | FLKSEDM     | 5.0           | Maximum depth for bottom sediment module to be active                         |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | ISNHECO     | 1             | Heat conductivity approach: 0 = FLake default; 1 = Semmler et.al.             |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | IFLREL      | 0             | Relaxation time scale method: 0 = FLake default; 1 = Layden et.al             |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YFLAKED     | ' '           | Path to FLake input file                                                      |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YFLAKEF     | ' '           | Name of FLake input file (NetCDF file)                                        |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YFLAWAD     | ' '           | Path to FLake warmstart file                                                  |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YFLAWAF     | ' '           | Name of FLake warmstart file (NetCDF file)                                    |
+------------+-------------+---------------+-------------------------------------------------------------------------------+

Table: Namelist Parameters for the FLake lake model

Here is some extra information about the namelist switches (some
copied from FLake webpages):

LFLBSMO and FLKSEDM: Thermal interaction between the water column and
the bottom sediments is an issue for shallow lakes only. A seasonal
cycle in shallow lakes may be noticeably affected by the accumulation
of heat in the bottom sediments during spring and summer and the
release of heat from the sediments during autumn and, in particular,
winter. Apart from shallow lakes, the bottom heat flux can be set to
zero. Experience suggests that for lakes deeper than about 5 m the
heat flux through the bottom can safely be neglected.

LFLWARM: FLake will initialize lake related variables from the forcing
data. This is not the optimal approach, because usually the quality of
the forcing data for lakes is not very good (temperatures can be
derived from the nearest sea-point). Also, many internal FLake
variables will not be in equivalent state as, for example, the surface
temperature that is read in. Thus, it is possible to run FLake using
existing data and do the so called warmstart. Warmstart file can be
derived from output file or preferably from a restart file. This means
that the model has to be used for the domain in focus until lake
related variables are presumably in equilibrium and then the restart
is used to actually start a longer simulation for the same domain
(analogical to soil warmstart). Warmstart file needs to be in NetCDF
format (can be derived from IEG-restart file) and it has to have the
following (default FLake) variables:
::
   FLTMWAT = mean temperature of the water column  [K]
   FLTMIX = mixed-layer temperature [K]
   FLTWBSED = temperature at the water-bottom sediment interface [K]
   FLTULSED = temperature at the bottom of the upper layer of the sediments [K]
   FLSHF = shape factor (thermocline)
   FLHSN = snow thickness [m]
   FLHICE = ice thickness [m]
   FLHMIX = thickness of the mixed-layer [m]
   FLHULBS = thickness of the upper layer of bottom sediments [m]
   FLTSN = temperature at the air-snow interface [K]
   FLTICE = temperature at the snow-ice or air-ice interface [K]
   TSFLECH = surface temperature (lake)

LFLSNMO: The usage of the snow module was tested in
:cite:`Pietikainen2018` and it is recommended to use it.

TBOMISS: REMO's preprocessor uses a default temperature value for
areas where there are no water. This should be indicated in the
namelist.

FLKDEPM: Strictly speaking, FLake is not suitable for deep lakes,
where a two-layer representation of the temperature-depth curve with
the lake thermocline extending from the outer edge of the upper mixed
layer down to the lake bottom becomes inapplicable. For deep lakes, it
is suggested to run FLake with a "false bottom".  That is, an
artificial lake bottom is set at a depth of about 50 m, and the
bottom-sediment module is switched off. The reasoning behind this
trick is pretty simple. The deep abyssal zone of a temperate deep lake
is usually filled with the water at the temperature close the
temperature of maximum density, FLake variable tpl_T_r, and seasonal
variations of abyssal temperature are usually small. Setting bottom
heat flux to zero and using tpl_T_r as the initial condition for the
water temperature at the bottom, one can expect FLake to reproduce
this behaviour. Notice, however, that equatorial lakes may have their
abyssal temperature well above tpl_T_r and they may never mix down to
the bottom. In such a case, it is better to initiate FLake with the
"climatological mean" temperature profile. Such profile can be
obtained with FLake by computing a perpetual year solution.

ISNHECO: The method how the ice-snow-atmosphere heat conductivity is
calculated. Recommended setting is =1.

IFLREL: The method how to calculate the relaxation time scale for the
temperature profile in the thermocline. No big difference between 0
or 1.

New output variables
~~~~~~~~~~~~~~~~~~~~

For the new output variables, please see ``mo_memory_flake.f90`` in
remo2:

::
   
   source/physics/land/Lake/LakeMain/FLake/kernel/mo_memory_flake.f90


