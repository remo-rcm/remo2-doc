Remo-IMOVE
==========


There are two documents available concerning the IMOVE documentation:

- :download:`Scientific Documentation <pdf/REMO_iMOVE_documentation_and_sensistudy.pdf>`
- :download:`Technical Documentation <pdf/REMO_iMOVE_technical_docu_and_preprocessor.pdf>`

Implementation to remo2
=======================

IMove was re-implemented to REMO in 2020. The new implementation
follows the original implementation structure, but has some
changes. The first change is that IMove is now a passive module, which
means that it will be compiled and be ready for use if the IMove
configuration is used:
::

   ./setup -auto Remo2015IMove -objdir=Remo2015IMove

Using the REMO2015IMove configuration the IMove related files will be
linked and compiled.

The initialization of IMove is now done in
::

   source/physics/land/Vegetation/VegetationMain/IMove/Vegetation_init.f90

This routine also calls the IMove input I/O routines, which are
located in the main IMove module
::

   source/physics/land/Vegetation/VegetationMain/IMove/kernel/mo_imove.f90

The I/O routines only support NetCDF input files. There are changes
also to the other IMove I/O files. The IMove I/O is now handled by the
memory stream approach and all old IMove I/O files are removed. The
available variable list for I/O can be found from
::

   source/physics/land/Vegetation/VegetationMain/IMove/kernel/mo_memory_imove.f90

In the original implementation, the IMove main part was called before
vertical diffusion (old:``phyorg``;
new:``Driver_organize_physics``). The thing here is that vertical
diffusion sets the canopy resistance and IMove changes this value,
thus it is called before vertical diffusion. However, there is a
problem with radiation, which is called after IMove call. IMove uses
the incoming short-wave fluxes and thus in the first time step they
have been zero and otherwise always behind the calculation by one time
step. The reason for this is that IMove also updates the land albedo
and albedo is an input variable for radiation part.

In the new implementation a different approach has been done. First of
all, the modification of the canopy resistance is now in its separate
subroutine
::

   source/physics/land/Vegetation/VegetationMain/IMove/Vegetation_can_res.f90

IMove is called only after radiation just before the main ``Surface``
subroutine is called. Thus, in vertical diffusion
``Vegetation_can_res.f90`` is only called after the first time step
(it uses the default values for the first time step).

Next bigger modification is that earlier the albedo changes were
calculated in the main part of IMove and in the new approach they are
in a separate subroutine that is called from the main albedo
routine ``update_albedo`` which is in
::

   source/physics/land/Surface/SurfaceMain/EC4/kernel/mo_albedo.f90

``update_albedo`` call the IMove albedo routine
::

   source/physics/land/Vegetation/VegetationMain/IMove/update_vegetation_surface.f90

Here one should notice that if IMove is switched off, the (empty)
subroutine
::

   source/physics/land/Vegetation/update_vegetation_surface.f90

The same applies to many other IMove routines and we don't explicitly
go them through. Nevertheless, this approach updates the albedo with
IMove changes before radiation and the new fluxes from radiation will
be used as an input for IMove. Moreover, with this approach there is
no discrepancy in the time step values.

The main IMove part is called before surface and the new interface can
be found from
::

   source/physics/land/Vegetation/VegetationMain/IMove/Vegetation.f90

There are some minor changes throughout the IMove code. The final
biggest change is in the namelist of IMove; it has now it's own
namelist called ``IMOVECTL``. Here is an example of the namelist:
::

    &IMOVECTL
    LIMOVELUC=.FALSE.,
    YNAMPFT='GLC_2_PFT_translate_Holdrige_16_classes_correct_b',
    YNAMZ0='173_EUR-044_b.nc',
    YNAMSALB='REMO_iMOVE_backgrd_albedo_correct_b.nc',
    YNAMROOT='ROOT_DEPTH_FOR_PFTS_16_classes_correct_b.nc',
    YIMOVEP='/work/ch0636/g300096/IMove/044'
    /

+------------+-------------+---------------+-------------------------------------------------------------------------------+
| Namelist   | Parameter   | Default value | Description                                                                   |
+============+=============+===============+===============================================================================+
| IMOVECTL   | LIMOVELUC   | FALSE         | Are land use changes                                                          |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YNAMPFT     | ' '           | Name of PFT file (NetCDF file)                                                |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YNAMZ0      | ' '           | Name of roughness length file (NetCDF file)                                   |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YNAMSALB    | ' '           | Name of albedo file (NetCDF file)                                             |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YNAMROOT    | ' '           | Name of root depth file (NetCDF file)                                         |
+------------+-------------+---------------+-------------------------------------------------------------------------------+
|            | YIMOVEP     | ' '           | Path to IMove input files                                                     |
+------------+-------------+---------------+-------------------------------------------------------------------------------+

Table: Namelist Parameters for the IMove vegetation model
