Quick Start Guide
=================

| This chapter describes how to set up and run a simple test experiment
  with REMO. Usually, a resaonable model run with REMO requires a lot of
  careful preparation of boundary forcing data, a soil library, run and
  batch scripts, etc. But to use REMO, it is most important to get a
  feeling for how a model run works in general in order to get
  understand what input data is required and what data REMO produces.
  For this purpose, a test setup is available which can be used without
  much preparation and should lead to quick results. This chapter
  explains in detail how to run this test setup. For a more detailled
  explanation of the REMO model workflow, experiment design and
  preprocessing, please have a look at chapters.
| It is usally recommeded to execute REMO in parallel although you
  should also be albe to run this test setup on a laptop. In this guide,
  we will show how to run REMO in parallel on the supercomputer MISTRAL
  at DKRZ which means you will need to have a valid user account and be
  associated to a project ID so that the DKRZ can account properly for
  your computing time. However, if you don’t have access to the MISTRAL,
  you can still run this setup on your laptop. In this case, this guide
  should also lead you to success.

Compilation of the Model Code
-----------------------------

REMO is a regional climate model written in Fortran and when you have
obtained the code, the directory structure of the model should more or
less look like this:

::

      /build  # makefiles 
      /CBS    # module source code
      /CODE   # source code
      /jobs   # job scripts 
      /OFS    # object directory for compiling 

You will have to compile the source code yourself on the machine you
will execute the run. Go to the /build directory in which you will find
a Makefile to create a machine specific makefile. There is also another
subdirectory you don’t have to care about right now. In the /build
directory try to type the following:

.. code:: bash

    make

which should result in a similar output to this:

.. code:: bash

    usage: <make clean> <make clean_all> <make mistral> <make blizzard> <make tornado> <make ubuntu_laptop> <make meteo> <make bull> <make juropa>

The only purpose of the makefile here is to create a machine specific
makefile from some files in the subdirectory /makefiles. There are some
predefined makefile configurations for different machines but, in
general, they only differ in which fortran compiler or mpi
implementation is used. So if you are working on an ubuntu system, try
typing:

.. code:: bash

    make ubuntu_laptop

which means nothing else than using the gnu fortran compiler *gfortran*
with the open mpi implementation which both are usually available on the
ubuntu distribution. However, if this fails, you can adapt the makefile
template for a linux machine to your configuration by editing the file
Makefile\_ubuntu\_laptop.txt in /makefiles/templates. If you are lucky
to have a valid account on the MISTRAL supercomputer of the DKRZ in
Hamburg, you can simply type:

.. code:: bash

    make mistral

which should work if your environment matches the DKRZ recommendations.
Again, this only means that remo is compiled on an intel machine using
the intel fortran compiler (ifort) with a bullxmpi implementation of
mpif90. This means that you should have loaded the following modules,
e.g, by adding this line your shell or bash profile:

.. code:: bash

    # Use the default versions of Intel compiler and Bullx MPI with Mellanox MXM and FCA tools
    module load intel mxm fca bullxmpi_mlx

We highly recommend that you have a look at the MISTRAL documentation on
the homepage of DKRZ.

Setup of the Run Directory
--------------------------

It is recommended not to execute the run in the model code directory
itself but to create an additional run directory since REMO will create
a lot of output data. On the MISTRAL (like on any other supercomputer),
this should be on the WORK partition which you find at /work on MISTRAL.
To execute the run on MISTRAL, you need to be associated to a project
account so that the DKRZ can account for your computing time. If you
have an account on MISTRAL, you usually should be associated to at least
one project. You can then find you personal WORK directory at:

.. code:: bash

    /work/YOUR_PROJECT_ID/YOUR_USER_ID

| where you can create a run directory, e.g., remo\_test. It is not
  necessary to copy the model code to the run directory, instead, you
  should host the model code in you home directory and feed the output
  data to the run directory since you might want to do several runs with
  the same model code. If you run the test setup on your laptop, you
  might want to create a run directory at any aritrary place where you
  have a couple of gigabytes to catch the remo output data.

Input Data
^^^^^^^^^^

However, the first thing you need is the input data for REMO. If you are
on MISTRAL, you will find a zipped archive with the data here:

.. code:: bash

    /work/ch0636/remo_test/test_data.tar.gz 

Copy this data to you run directory and untar it, e.g., by typing this:

.. code:: bash

    cp /work/ch0636/remo_test/test_data.tar.gz .
    tar -xvzf test_data.tar.gz

This will create a directory called ’data’ in your run directory and it
has the following structure:

.. code:: bash

    data/libs: # soil data
      albyear_cordex044.srv  GHG_rcp45_1850-2101.txt  test  vgryear_cordex044.srv  vltyear_cordex044.srv
    data/xa    # forcing data
      a051000a1990010100 a051000a1990010106 a051000a1990010112 ...

| This is the main input data REMO requires to start and run a
  simulation. The /libs directory contains data of the so-called ’soil
  library’. This includes monthly data of the soil albedo, vegetation,
  etc. The second directory /xa contains the forcing data in a 6-hourly
  resolution. Each file contains all data that is required by REMO to
  setup the *boundary conditions* for a certain timestep during the
  simulation. This is necessary, since REMO is a *regional* model. It
  does not model the entire globe and consequently, it will need some
  boundary conditions derived from a global data set. This is exactly
  what the /xa directory contains. By the way, the timestep of the REMO
  run can, of course, be much shorter than the 6-hourly resolution of
  the forcing data. For the timesteps in between two forcing files, REMO
  will interpolate the boundary data from two consecutive forcing files.
| The forcing files (also often called ’a-files’) satisfy a certain
  naming convention which is in this case ’a051000aYYYYMMDDHH’ and where
  YYYYMMDDHH denotes the date in time of the forcing data in UTC format.
  Consquently, the forcing data for the test setup reaches from the 1st
  of January 1990 at 00:00 UTC to the 31st at 24:00 UTC (which is, in
  fact, the 1st of February at 00:00 UTC). The first part of the forcing
  file name contains some information on how the data was created, see,
  e.g., [appendix]. Although the forcing files are only needed to setup
  boundary conditions during the run, they actually contain data for the
  whole computational domain. The reason for this is that REMO also and
  only reads the *initial conditions* from the forcing file when the
  simulation starts.
| Again, don’t worry about the details right now since the data is
  already setup for you and will work for the test setup.

Run Script
^^^^^^^^^^

Now that we have created the run directory and prepared the boundary
data, you can now modify the example run script. It is called
test\_remo.ksh and you find it in the model directory at:

::

    remo_mpi/jobs/test_remo.ksh 

The example run script is a kshell script and its purpose is simply to

#. setup a directory structure for the REMO output data

#. create an INPUT file containing a namelist for REMO

#. execute REMO

The run script is actually a batch script containing a header for the
Slurm Workload Manager that is used on MISTRAL. The header contains all
necessary information to submit the run script as a batch job to the
queuing system on MISTRAL and it looks like this:

| What you can read from this information is, e.g., that we will run the
  test setup on two nodes on MISTRAL where each node can handle 24 MPI
  tasks. This means that we will have 48 processors in total to run the
  setup in parallel. The Slurm header is required when you submit the
  run script to the MISTRAL, if you want to run the test locally on your
  laptop, you can ignore it.
| However, when you run REMO in parallel you have to tell it how to
  decompose the computational domain for parallel computation. This is
  defined by the Parameters PROCX and PROCY which describe the number of
  subdomains in the x-direction and y-directions. Make sure that these
  parameters are set in the run script:

| The important point here is that the number of subdomains matches the
  total number of MPI processes you plan to run the setup with, e.g., 48
  processes in this example, one MPI process for each subdomain. If you
  want to know more about the domain decomposition, please refer to the
  REMO Documentation.
| Now, you have to set a parameter concerning the system on which you
  run the test.

Choosing the right system will only take care of which command will be
used to execute remo (e.g., mpirun or srun).

Next, you have to make sure that you set the right directory path for
the model and run directory:

| Here, we use the environment variables $HOME and $WORK since these
  should be set correctly on the MISTRAL by default. The BASEDIR denotes
  the run directory (e.g, where the /data subdirectory is located) and
  MODELDIR is the root directory of the REMO Code.
| Finally, you can choose the length of the run.

| Let us do a short run for the beginning. This means that the run will
  only last for one hour of simulation time.
| There are many more parameters in the run script you don’t have to
  care about right now since they should be ready to run on MISTRAL or
  your linux laptop. If you are a little familiar with shell scripting,
  you will recognize that the script parameters are used to create a
  file called ’INPUT’ which contains a Fortran NAMELIST which, amongst
  other things, tells REMO about where the boundary data is located,
  where it should write output data, the resolution of the domain, the
  timestep, start and end point of the simulation, etc... (see also the
  section of the namelist, [sec:namelist]). At the same time, the run
  script creates the appropriate file structure for REMO to write its
  output data to. The rest of the run script deals executing REMO.
| The idea of the run script is to make things as easy and flexible as
  possible. E.g., if you want to run a climate simulation over sevaral
  decades, you will need thousands or even millions of cpu hours to
  complete the run. You will never be able to complete this simulation
  in one go since all supercomputing centers forbid to submit jobs with
  more than a couple of hours of computing time (or ’wallclock time’).
  What is usually done is to compute one month of simulation time per
  batch job and then, after this job was finished, *restart* the
  simulation with the next month. Using a job script will allow to
  dynamically setup a REMO and even submit a batch job for the following
  month, when one month of computation was finished. However, when you
  want to design your own experiment in the future, you will probably
  create your own run script in your favourite scripting language which
  deals with setting up the REMO simulation. Anyway, our test script
  might be a good starting point for this.

Execution of the Run Script
---------------------------

You should now be able to execute the batch script by either submitting
it to the job queue on MISTRAL:

.. code:: bash

    sbatch test_remo.ksh

or by executing it directly on your laptop by typing:

.. code:: bash

    ksh test_remo.ksh

If the script is executed successfully it should create a subdirectory
called ’test’ in your data folder of your run directory. The structure
of this output directory should look similar to this:

.. code:: bash

      /data/test : 
      /xe    # results with hourly resolution
      /xf    # restart files
      /xt    # results with 6-hourly resolution

You will also find a batch log file in your run directory which will
contain the output of the the run script and the REMO log. If REMO was
launched successfully, you can see that REMO starts by creating output
files in the /data/test/ directory. If this is finished, REMO starts
reading input from the /data directory e.g.:

.. code:: bash

    OPEN:   remo_test/data/xa/a051000a1990010100
    OPEN:   remo_test/data/xa/a051000a1990010106
    OPEN:   remo_test/data/libs/vltyear_cordex044.srv
    OPEN:   remo_test/data/libs/albyear_cordex044.srv
    OPEN:   remo_test/data/libs/GHG_rcp45_1850-2101.txt

You can see, that REMO, in fact, reads in the soil library data and also
the first two boundary forcing files. REMO will then fill all necessary
initial conditions and will be able to compute boundary conditions for
the first six hours of simulation time. Since we chose the short run
with only one hour of simulation time, look for some a line that gives
information on the simulation progress, e.g.,

.. code:: bash

    FORECASTTIME  = 00000001 HOURS

| which will let you know that REMO started successfully has calculated
  the first hour of simulation time. The run should then have stopped.
| If everything worked fine, you can have revisit the results directory
  in /data/test and have a look at the results:

.. code:: bash

    /data/test : 
      /xe   # results with hourly resolution 
         e031001e_c001_199001  e031001e_c002_199001  e031001e_c003_199001  e031001e_c004_199001 ...
      /xf   # restart files 
      /xt   # results with 6-hourly resolution

| The subdirectory /xe contains the main results.
| The naming convention here is ’e031001e\_cCCC\_YYYYMM’ (“e” =
  “Ergebnis” = result). Each file contains a single 2D field, usually in
  hourly resolution (hourly means) and organised by a code (3-digit code
  number CCC, see Appendix [app:codelist]). The files are stored in the
  binary IEG Format (see also [appendix:ieg]) and can be read using the
  Climate Data Operators (CDO). If you are working on MISTRAL (or have
  cdo installed yourself), go to the results directory /data/test/xe and
  try, e.g.:

.. code:: bash

    cdo sinfo e031001e_c167_199001

and you should get something like this:

.. code:: bash

     File format : IEG  BIGENDIAN
      -1 : Institut Source   Ttype    Levels Num    Points Num Dtype : Parameter ID
       1 : unknown  unknown  instant       1   1     15609   1  F32  : 167.128       
     Grid coordinates :
       1 : lonlat                   : points=15609 (129x121)
                               rlon : -31.73 to 24.59 by 0.44 degrees
                               rlat : -26.73 to 26.07 by 0.44 degrees
                          northpole : lon=-162  lat=39.25
     Vertical coordinates :
       1 : height                   : levels=1
                             height : 2 m
     Time coordinate :  1 step
    YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss  YYYY-MM-DD hh:mm:ss
    1990-01-01 01:00:00

The header of the file contains some important information on:

-  the file format (e.g., IEG)

-  field information and dimension

-  the grid

-  the time axis

| You can see that this file contains only one field (code 167) of
  resolution 129x121 with one level at a height of 2m (it’s actually the
  2m air temperature). Notice that this file only contains one timestep
  at the 1st of January 1990 at 1 hour after midnight (the format of
  time here is hours:minutes:seconds). In fact, it is not the 2m
  temperature exactly at this point in time but it is the mean value
  over the last hour since these ’e-files’ usually have an hourly
  resolution. Notice that there are more files with the naming
  convention ’e031001n\_cCCC\_YYYYMM’ (so called ’n-files’) which are
  supposed to contain daily means of a 2D field. These are empty right
  now since we only computed one hour and consquently didn’t produce
  enough data for at least one day. Furthermore, the other result
  directories /xf and /xt are empty as well since these will contain
  REMO data in 6-hourly resolution.
| Now let’s start this run again from the beginning but this time, we
  will compute a whole month. Set the parameter for the simulation
  lenght in the run script like this:

.. code:: bash

    TEST=2      # Number of the test
                # 1: Short
                # 2: Long
                # 3: Restart

| If you look carefully at the run script you will see that this will
  set a parameter ’KSE=744’ which is then fed into the NAMELIST. This
  denotes the last hour of simulation time at which the run will stop,
  consequently, we will now run the simulation for the whole January of
  1990 (31 days x 24 hours/day = 744 hours).
| Now restart the simulation with:

.. code:: bash

    sbatch test_remo.ksh

or

.. code:: bash

    ksh test_remo.ksh

respectively. This will take some more time (about 10 minutes on Mistral
with 48 processors) and if you have a look at the log file, you can
follow the progress in hourly steps,e.g.

.. code:: bash

    FORECASTTIME  = 00000001 HOURS
    FORECASTTIME  = 00000002 HOURS
    FORECASTTIME  = 00000003 HOURS
    FORECASTTIME  = 00000004 HOURS
    FORECASTTIME  = 00000005 HOURS
    SAVE:   remo_test/data/test20/xt/e031001t1990010106
    FORECASTTIME  = 00000006 HOURS
    OPEN:   remo_test/data/xa/a051000a1990010112
    FORECASTTIME  = 00000007 HOURS
    FORECASTTIME  = 00000008 HOURS
    FORECASTTIME  = 00000009 HOURS
    FORECASTTIME  = 00000010 HOURS
    FORECASTTIME  = 00000011 HOURS
    SAVE:   remo_test/data/test20/xt/e031001t1990010112
    FORECASTTIME  = 00000012 HOURS
    OPEN:   remo_test/data/xa/a051000a1990010118
    FORECASTTIME  = 00000013 HOURS
    FORECASTTIME  = 00000014 HOURS

| You can see here that at 6 hours (at the end of the 5th hour), a file
  with the naming conenvtion ’e031001tYYYYMMDDHH’ is created (the so
  called ’t-files’). These files contains results for a number of 2D
  surface fields and for the 3D atmospheric fields and are organised by
  time, one file every 6 hours. Among other purposes, the t-files are
  used for computing the forcing for finer-resolution REMO experiments.
| Now let’s have a look in the log file after the run has fininshed at
  744 hours:

.. code:: bash

    FORECASTTIME  = 00000740 HOURS
    FORECASTTIME  = 00000741 HOURS
    FORECASTTIME  = 00000742 HOURS
    FORECASTTIME  = 00000743 HOURS
    SAVE:   remo_test/data/test20/xe/e031001m199001
    SAVE:   remo_test/data/test20/xe/e031001s199001
    SAVE:   remo_test/data/test20/xt/e031001t1990020100
    SAVE:   remo_test/data/test20/xf/e031001f1990020100
    SAVE:   remo_test/data/test20/xf/e031001g1990020100
    FORECASTTIME  = 00000744 HOURS

You can see that at the end of the month, several new files are created:

-  e031001mYYYYMM: monthly means (m-file)

-  e031001sYYYYMM: monthly standard deviations (s-file)

-  e031001fYYYYMMDDHH: contains restart data (f-file) at timestep n-2

-  e031001gYYYYMMDDHH: contains restart data (g-file) at timestep n-1

| The m- and s-files contains mean values and standard deviations
  respectively of the most important 2D fields. That why they are saved
  only at the end of the month. The f- and f-files contain REMOs restart
  data, i.e. the prognostic fields at the last two time steps of the
  month. They are written at the end of each month’s calculation and can
  be use for initialisation to restart the model at the beginning of the
  next month.
| You can go on and explore all these result data using cdo. You can
  also use cdo to convert an IEG file to the NETCDF format and then
  explore it using ncdump, e.g., like this:

.. code:: bash

    cdo -t remo -f nc copy e031001e_c167_199001 e031001e_c167_199001.nc
    ncdump -h e031001e_c167_199001.nc 

which will give you file information in the cdl format:

.. code:: bash

    netcdf e031001e_c167_199001 {
    dimensions:
            rlon = 129 ;
            rlat = 121 ;
            height = 1 ;
            time = UNLIMITED ; // (744 currently)
    variables:
            ..
            double height(height) ;
                    height:standard_name = "height" ;
                    height:long_name = "height" ;
                    height:units = "m" ;
                    height:positive = "up" ;
                    height:axis = "Z" ;
            double time(time) ;
                    time:standard_name = "time" ;
                    time:units = "day as %Y%m%d.%f" ;
                    time:calendar = "proleptic_gregorian" ;
            float TEMP2(time, height, rlat, rlon) ;
                    TEMP2:long_name = "2m temperature" ;
                    TEMP2:units = "K" ;
                    TEMP2:code = 167 ;
                    TEMP2:table = 128 ;
                    TEMP2:grid_mapping = "rotated_pole" ;
            ..

| During the conversion to NETCDF, the file information was connected to
  a REMO code table which will give some more intuitive information on
  the file content. Notice that the e-file now contains 744 timesteps,
  one for each hour of the month. You can now go on and have a closer
  look at the other result files and maybe use ’ncview’ (use the command
  ’module load ncview’ on MISTRAL) to create a quick plot and get a
  feeling for how the output data is organized.
| The following chapters of this User Guide will deal with more or less
  the same things we just did during the example setup but, of course,
  in a much more detailled way.
