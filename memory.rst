.. _cha_memory:

Memory Management
=================

The new REMO version contains a new concept for managing fields and
allocating data based on ECHAM6. The concept is based on memory streams
which are lists of derived types that contain meta data and a pointer
that holds the actual data in the memory. Such a derived type is called
``memory_info`` and it is defined in ``mo_linked_list.f90``. Here is a
short excerpt from the meta data

::

    TYPE memory_info                      ! meta data type
        
        !
        SEQUENCE
        !
        ! Content of the field
        !
        CHARACTER(len= 64) :: name          ! variable name
        CHARACTER(len= 64) :: units         ! units
        CHARACTER(len=128) :: longname      ! long name
        !
        ! Memory buffer information
        !
        INTEGER            :: ldim(4)       ! local dimensions (lon,lat)
        INTEGER            :: gdim(4)       ! global dimensions of variable
        INTEGER            :: dima(4)       ! allocated dimensions (nproma,ngpblks)
        LOGICAL            :: lreg          ! true for dim==dima
        INTEGER            :: ndim          ! nr physical dimensions
        INTEGER            :: rank          ! rank of data 
        INTEGER            :: klev          ! number of vertical levels
        INTEGER            :: iklev         ! vertical level index
        INTEGER            :: ltype         ! ltype 
        INTEGER            :: kake(2)       ! first and last level index 
        
        .....

      END TYPE memory_info

The derived type ``memory_type`` contains a variable ``info`` of type
``memory_info`` and a 4D pointer which can be allocated to hold data in
the memory. It looks like this:

::

      !
      ! Type 'memory_type' holds the pointer to the field as well as the meta
      ! associated information.
      !
      TYPE memory_type                         ! linked list entry type
        !
        SEQUENCE
        ! 
        REAL(dp), POINTER   :: ptr (:,:,:,:)  ! pointer to 3D-field
        TYPE (memory_info)  :: info           ! meta data for this entry
      END TYPE memory_type
      !

The derived type ``list_element`` contains a variable ``field`` of type
``memory_type`` and a reference to the next list element called
``next_list_element`` which is also of type ``list_element``. It looks
like this:

::

      !
      ! Type 'list_element' provides the entry to the actual information 
      ! and a reference to the next element in the list.
      !
      TYPE list_element
        !
        SEQUENCE
        !
        TYPE (memory_type)          :: field
        TYPE(list_element), POINTER :: next_list_element
      END TYPE list_element
      !

| All routines that manage field data and list elements are implemented
  in ``mo_memory_base.f90``. However, the user usually does not have to
  care about how streams and stream elements are handled. The user can
  simply work with a variable that points to the address where memory is
  allocated for this variable.
| For example, fields that are part of the main REMO table are declared
  in ``mo_memory_main.f90``. A variable for the temperature is declared
  in the data section of the Fortran module like this and a stream for
  the main REMO table which is of type ``t_stream``

::

       TYPE(t_stream)          , POINTER ::  REMO_TABLE
       REAL, DIMENSION(:,:,:,:), POINTER ::  T

The module also contains a subroutine called ``construct_remo_table``
which is called in the ``init_memory`` subroutine in
``mo_memory_streams.f90`` during the model initialization. This
subroutine constructs the main REMO table:

::

       !
       !  create main stream called REMO_TABLE
       !
       CALL new_stream(REMO_TABLE, name='REMO_TABLE')
       !

and after adding this new stream, all variables are added to the stream
including, e.g., the temperature:

::

       CALL add(REMO_TABLE, 'T       ', T       , code=130, adims=(/IE,JE,KE ,3/), leveltype=110, gbtri=10, kake=(/1  ,KE /), ntime=3)

The subroutine ``add(_stream_element)`` in ``mo_memory_base.f90``) will
add a new list element to the stream called ``REMO_TABLE`` and will
allocate memory based on the argument ``adims``. For example, the
temperature is allocated as a 3D field with 3 time dimensions since it
is a dynamic variable on all model levels. The subroutine
``add_stream_elements`` allows for a lot more arguments where most is
them is set by default. However, the way REMO used to handle the number
of model and time levels has not always been consistent. For example,
the index for the model level in the Fortran variable is not
neccessarily consistent with the model level itself. A simple example
would be a 2D variable where the index for the third dimension always is
1 although the model level would be, e.g., 27 for a 27 layer model. On
the other hand, variables like ``FTKVM`` and ``FTKVH`` are allocated
with KE levels but physically only reach from level 2 to KE and which
requires some shifting during the output. It is therefore usually a good
idea to explicitly specifiy the model level range with the argument
``kake`` as seen in the example for the temperature. The argument
``ntime`` will make sure that the fourth dimension is handled as a time
variable during output. In general, it is a good idea to look at the
module file ``mo_memory_main.f90`` for examples of how to add variable
of different dimensions and time levels. Note that although the pointers
that are delcared and are used to work with in the model, internally,
the pointers in the streams are all 4D pointers for consistent output
handling.

In and Output Streams
---------------------

| All fields that need input data or fields that should be written into
  an output file require meta information for the IO process. In earlier
  versions of REMO, this was handled using tables of names, codes, etc.
  which where connected to the Fortran variable manually in a “put”
  routine, e.g., ``putec4`` or ``puteca``. The netcdf IO make use of the
  derived types which connect meta information with a data pointer so
  that the IO prcoess can be handled more easily and dynamically. This
  also has the advantage that fields are not statically allocated any
  more but are allocated dynamically which makes memory management much
  easier if the model should be applied at different resolutions.
| The most important stream that holds output data is called
  ``REMO_TABLE`` and is organized in ``mo_memory_main.f90``. This stream
  is mainly created to hold all variables in an organized manner.
  However, the data is not put into a file directly from the
  ``REMO_TABLE`` but rather from an output stream that holds references
  to this table. These output streams are organized in
  ``mo_memory_output.f90``. The output streams are created at the
  beginning of the model run from the lists of variables that are
  defined in the namelist ``DATEN``. The output streams then will hold a
  fixed list of references to the data that should go in a certain file.
  This has the advantage that, at output time, data in an output streams
  can be accessed easily and quickly without having to walk through the
  whole ``REMO_TABLE`` again and again.
| The same is true for reading in data from a forcing file during the
  run. The main table for boundary data is called ``BOUNDARY_TABLE`` and
  it is organized in ``mo_memory_boundary.f90``. This table holds all
  variables that will hold boundary data during the run. They have two
  timelevels since they will always hold the forcing data of two
  consecutive a-files (e.g., with a 6 hour resolution). Although this
  table is not very large, we still create an extra reference input
  stream for this (the ``a-stream`` in ``mo_memory_input.f90``) so that
  data can be read in depending on the actual content of the a-file.
  Similarly, restart data is read in by creating an f-stream that is
  referencing memory in the ``REMO_TABLE`` stream to fill in data from
  restart files.
| Note, that in ``mo_memory_output.f90`` also streams for monthly and
  daily means as well as a table for standard deviations is created.
  These tables allocate extra memory to hold and accumulate output data
  for monthly and daily min/max and mean values.
