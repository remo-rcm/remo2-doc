Model Run
=========

| In this chapter, we describe the basics steps of compiling and running
  the REMO model after all input data has been created successfully.
  Usually, a model run should be organized on a monthly basis. This
  means that the entire period of the experiment is not computed during
  one model run but rather successively by consecutive model runs that
  span a period of one month. The model is configured to write a restart
  file at the end of each month and is restarted at beginning of the
  next month. By this, the entire timeframe of the experiment can be
  divided into relatively small computational tasks (typically of a few
  minutes to hours of wallclock time) that can be handled by most job
  scheduling system. Trying to run the model in one go would most
  probably exceed the allowed wallclock limit of most supercomputing
  systems. Furthermore, if the model run fails, the user does not loose
  to much data but can simply restart the model at the current month.
  Most REMO runscripts (Section [sec:runscript]) follow this idea and
  are based on the idea of creating REMO namelists on a monthly basis.
  Furthermore, the runscript should also take care of creating an
  appropriate directory structure for the model run and handle output
  data.
| Furthermore, since REMO most often is used for downscaling
  experiments, soil temperatures and some other model variables might
  not be in an equilibrium with the model atmosphere because of the
  higher resolution in comparison to the original driving data.
  Therefore, an initial *spin up* of some soil variables and a following
  restart of the model might be neccessary (Section [sec:spinup])

Compilation
-----------

| REMO is written in Fortran and when you have obtained the code, the
  directory structure of the model should more or less look like this:

::

      /build  # makefiles 
      /CBS    # module source code
      /CODE   # source code
      /jobs   # job scripts 
      /OFS    # object directory for compiling 

You will have to compile the source code yourself on the machine you
will execute the run. Go to the /build directory in which you will find
a Makefile to create a machine specific makefile. There is also another
subdirectory you don’t have to care about right now. Go to the /build
directory and get the available compilation options by typing:

.. code:: bash

    cd build
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
the homepage of DKRZ. If compilation was successful, you can find the
machine specific makefile in the ``makefiles`` subdirectory in the
``build`` directory. If you have to change any compiler options, do it
in this file, e.g., ``Makefile_mistral`` and not in the template itself
which is located in a further subdirectory called
``makefile/templates``.

Compilation with NETCDF support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The compilation of REMO with NETCDF support requires to link the netcdf
library. The netcdf library needs to be installed on the system which on
an HPC system usually is the case. Configuration details for the netcdf
libary can be accessed using the ``nc-config`` tool. However, for most
Linux desktop computers and for the MISTRAL, the Makefiles templates
contain the required pathes and libraries for compiling REMO with netcdf
support. Managing the call of in and ouput routine in REMO is done using
the a preprocessor macro defined by ``NCMODE``. To use the NETCDF
support, you have to change the Makefile in the makefiles subdirectory
in the build directory, e.g., the Makefile\_mistral. Open the Makefile
and uncomment the lines that define the NETCDF variable (for compiling)
and the NCLIBS variable (for linking). For the Mistral, these lines
should be something like this:

::

    NCMODE    =  NETCDF_IO
    NCFLAGS   = -D{NCMODE} -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include
    NCLIBS    = -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/lib -lnetcdff \
                -Wl,-rpath,/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/lib \
                -L/sw/rhel6-x64/netcdf/netcdf_c-4.3.2-gcc48/lib \
                -Wl,-rpath,/sw/rhel6-x64/netcdf/netcdf_c-4.3.2-gcc48/lib \
                -L/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib \
                -Wl,-rpath,/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib \
                -L/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib \
                -Wl,-rpath,/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib \
                -lnetcdf -lhdf5_hl -lhdf5 -lsz -lcurl -lz

The ``NCMODE`` variable will define a flag for a macro called that is
interpreted by the preprocessor. The ``NCMODE`` has three options:

::

    NCMODE    =  NETCDF_IO    for Netcd Input and Output
    NCMODE    =  NETCDF_IN    for Netcdf Input and IEG Output
    NCMODE    =  NETCDF_OUT   for IEG Input and Netcdf Output

Note, that in the case you choose IEG input and Netcdf Output, the
restart file will be written still in IEG format so that the model can
be restarted with IEG input. Make sure, that you have activated the
preprocessor, e.g, the ``-fpp`` flag for the Intel compiler or the
``-xf77-cpp-input`` for the GNU compiler (it’s actually c preprocessor
syntax). But this should be already prepared in the appropriate Makefile
templates. Furthermore, you have to uncomment the lines, that include
the new dependencies of the REMO netcdf module source code (in the CODE
directory). These are further down in the Makefile, e.g.:

::

    include ../CODE/makefile.netcdf

We have separated the netcdf dependencies and used preprocessor macros
because this is the most flexible way to handle the different
implementations of the IO module. This is also useful because REMO can
still be compiled without relying on the external fortran netcdf
library.

Now, you can compile REMO the usual way by typing, e.g.:

::

    make mistral

Remeber, you have to be in the build directory for this to work. If you
did already compile REMO before without NETCDF support, you have to
recompile. To make sure, first clean up and compile then, e.g.:

::

    make clean_all
    make mistral

That sould compile REMO with NETCDF support.

The INPUT Namelist File
-----------------------

The REMO model requires a Namelist File called ``INPUT`` at runtime.
Some of the most important Namelists and Parameters are listed in
Table [tab:namelist\_overview]. A full description of all parameters
can be found in Appendix [cha:remo\_namelist]. However, most
parameters are set by default during the REMO run but the parameters
in Table [tab:namelist\_overview] need special attention since they
are domain specific and dependet on the setup of the experiment during
preprocessing. The most important settings are the following:

.. topic:: ``PARCTL``

  Here you have to choose how the REMO domain is
  decomposed for parallel computation. ``NPROCXM`` and ``NPROCYM`` define the
  number of subdivions in x- and y-direction of the domain. Usually, an
  evenly distribution of cells per processor is most efficient but
  should not be less than 400 cells (e.g., 20x20 cells) because
  otherwise the communication overhead might dominate the model run.

.. topic:: ``EMGRID``

  Here you have to adjust the parameters of your
  model domain (as in the preprocessor namelist, see above), i.e., lat
  and lon of the lower left corner in the rotated system (``PHILU``,``RLALU``),
  lat and lon of the rotated north pole (``POLPHI``, ``POLLAM``) and the
  resolution in x- and y-direction in degrees (``DLAM``, ``DPHI``).

.. topic:: ``RUNCTL``

  Adjust the time of model initialisation (very
  first time step, ``YADAT``), the time step in seconds (``DT``) and the
  temporal resolution of the driving fields (``NHDR``) which is in almost
  all cases 6 hours. Furthermore, the switches ``LMOMON`` (FALSE if real
  month are to be computed, ``TRUE`` if the forcing only has 30 days per
  month), ``LQWR`` (``TRUE`` if forcing contains liquid water, ``FALSE`` if not) and
  ``LSCEN`` (``TRUE`` if time-dependent greenhouse gas concentrations are to be
  used, FALSE for constant greenhouse gas concentrations) have to be
  set.

.. topic:: ``PHYCTL``

  Adjust the switches ``LVEG`` (``TRUE`` if monthly varying
  vegetation fields are to be used, ``FALSE`` if not), ``LSICED`` (``TRUE`` if the
  forcing contains sea ice concentrations, FALSE if not and if the sea
  ice concentration should be diagnosed from ``SST``) and ``LAEROZ`` (``TRUE`` if
  time dependent aerosol and ozone background concentrations are to be
  used, ``FALSE`` if constant background concentrations are used).

.. topic:: ``DATEN``
  
  Adjust the names of the three vegetation files
  (annual cycle of three parameters, see abobe; ``YBDNAM``), the name of the
  file containing the time-dependent greenhouse gas concentrations
  (``YGDNAM``) and, if applicable, the filenames for the time-dependent
  ozone, aerosol and sulfate concentrations (``YO3DNAM``, ``YSADNAM``, ``YSNDNAM``).

+------------+-------------+-----------------------------------------------------------------------------------+
| Namelist   | Parameter   | Description                                                                       |
+============+=============+===================================================================================+
| PARCTL     | NPROCXM     | Number of preocessors in x-direction (longitude)                                  |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | NPROCYM     | Number of preocessors in y-direction (latitude)                                   |
+------------+-------------+-----------------------------------------------------------------------------------+
| EMGRID     | MOIE        | Number of grid boxes in x-direction (longitude)                                   |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | MOJE        | Number of grid boxes in y-direction (latitude)                                    |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | MOKE        | Number of layers                                                                  |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | PHILU       | Latitude of lower left grid box center                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | RLALU       | Longitude of lower left grid box center                                           |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | POLPHI      | Latitude of the rotated North Pole                                                |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | POLLAM      | Longitude of the rotated North Pole                                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | DLAM        | Meshsize in x-direction (degree)                                                  |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | DPHI        | Meshsize in y-direction (degree)                                                  |
+------------+-------------+-----------------------------------------------------------------------------------+
| RUNCTL     | NHANF       | Starting hour relative to start date                                              |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | NHENDE      | Ending hour relative to start date                                                |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YADAT       | Start date                                                                        |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | DT          | time step in seconds                                                              |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | NHDR        | Temporal resolution of drving data (usually 6 hours)                              |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LQWR        | TRUE if forcinf files contains liquid water content                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LMOMON      | TRUE if forcing files have 30 days per month                                      |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LSCEN       | Switch for time-dependent greenhouse gas concentration                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LMOMIT      | TRUE if monthly means, max and mins should be written                             |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | DLAND       | Cloud height over land that has to be reached at least before a cloud may rain    |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | DNOPRC      | Cloud height over ocean that has to be reached at least before a cloud may rain   |
+------------+-------------+-----------------------------------------------------------------------------------+
| PHYCTL     | LPHY        | TRUE if REMO physics should computed                                              |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | HDRAD       | Time intervall for radiation calculations                                         |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LVEG        | TRUE if monthly varying vegetation fields are used                                |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LSICED      | TRUE if forcing file contains sea ice concentration                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | LAEROZ      | TRUE if time dependent aerosol and ozone concentrations are used                  |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | L5LAY       | TRUE if 5 soil layer model should be used                                         |
+------------+-------------+-----------------------------------------------------------------------------------+
| DATEN      | YADEN       | Experiment number of the input data                                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YRDEN       | Experiment number of the input data (YADEN=YRDEN)                                 |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YEDEN       | Experiment number of the ’xe’ files                                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YFDEN       | Experiment number of the ’xf’ files                                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YTDEN       | Experiment number of the ’xt’ files                                               |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YUSERA      | User number of the input data                                                     |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YUSERE      | User number of the output files                                                   |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YADCAT      | Path to the input files (a-files)                                                 |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YRDCAT      | Path to the input files                                                           |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YEDCAT      | Path to the ’xe’ files                                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YFDCAT      | Path to the ’xf’ files                                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YMDCAT      | Path to the ’xm’ files                                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YTDCAT      | Path to the ’xt’ files                                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YNDCAT      | Path to the ’xn’ files                                                            |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YMVARN      | Name of fields that go into timeseries files (e-file,m-file)                      |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YNVARN      | Name of fields that go daily mean,min,max files (n-file)                          |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YTVARN      | Name of fields that go into a t-file                                              |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YBDCAT      | Path to the files below                                                           |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YBDNAM      | List of files                                                                     |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YGDCAT      | Path to GHG-file                                                                  |
+------------+-------------+-----------------------------------------------------------------------------------+
|            | YGDNAM      | GHG-filename                                                                      |
+------------+-------------+-----------------------------------------------------------------------------------+

Table: Important Namelist Parameters

You might want to have another look at the run script for the test setup
that was describe in Chapter [cha:quickstart] for a better understanding
of how the Namelist looks like and what kind of numbers go into it.

| If the REMO experiment should be handled on a monthly basis, some of
  the parameters in the Namelists will change from month to month (e.g.
  start and end hour ot the simulation) and some will stay the same
  (e.g. data pathes, parallelization, etc.). Therefore, it is a good
  idea to use a job control script (e.g., the one from Chapter
  [cha:quickstart]) that will handle the creation of the NAMELIST
  dynamically and automatically submits a job script for each month
  successively.
| You could either use the jobscript from Chapter [cha:quickstart] as a
  basis and create your own job control script from that. But depending
  on the environment you work in, it might be a good idea to adapt
  existing job scripts that are already working in the existing
  environment and, e.g., fullfill certain data directory conventions.

The Job Control Script
----------------------

The job control script for a model production run, that spans a
timeframe of several years and decades and produces a considerable
amount of output data, should take care of these tasks:

-  prepare an appropriate directory structure for the model run

-  copy and extract input data to the model run directory

-  create an INPUT file and set NAMELIST parameters

-  execute the model run

-  postprocess and backup the output model data

All of this taks should be handled on a monthly basis by the jobscript.
The script should basically contain a loop over all month that should be
computed, and for each month, it submits a job script to the scheduling
system. Fortunaltey, a number of job control scripts already exist that
can be, in principle, adapted to any HPC system.

The GERICS Bash Job Control Script Suite
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| A suite of jobscripts that has been used for most of the REMO model
  runs so far is available in the ``./jobs`` subdirectory of the model.
  These scripts are especially useful for REMO runs that are conducted
  at the DKRZ on MISTRAL because they make use of the tape archive (HPSS
  filesystem) from where it gets forcing data and where it stores output
  data of the model run. It actually consists of a bash script called
  ``run_remo.ksh`` that requires some python scripts and templates to
  run. The job script ``run_remo.ksh`` itself is supposed to be a batch
  script that can be submitted to the job scheduling system of the
  MISTRAL. The script itself checks if an appropriate directory
  structure for the output data existst on the ``WORK`` partition (where
  the output data is copied after finishing one month of model run) and
  if a directory for the model run itself is available on the
  ``SCRATCH`` partition (where data is written during the model run
  itself). This is usually the best approach of handling output data
  from REMO: Writing the model output data to a ``SCRATCH`` parition
  where the user usually has a lot of disk space (which is cleaned
  regularly) and afterwards, copying the results to the ``WORK``
  parition where it can be stored until the end of the experiment (the
  last month of simulation time) and the be archived.
| The directory structure of files in the job control script more or
  less should look like this:

::

      config_druint.txt         # config file for pressure interpolation        
      log/                      # directory for logfiles                        
      NEXP                      # experiment number                             
      NMON                      # number of current month in current year       
      NSA                       # starting hour of current run                  
      NYEAR                     # current year                                  
      pressure_interpolation.py # script for running the pressure interpolation 
      putscript.py              # script for tarring and archiving output data  
      run_remo.ksh              # the main job script (batch script)            
      scripts/                  # directory for job scripts                     
      templates/                # directory for job script templates            

Configuration
^^^^^^^^^^^^^

The main job script that is submitted to the job scheduling system is
``run_remo.ksh``. In the header of the script, a number of variables are
defined which control pathes and parameters for the REMO run. The first
part deals with the job scheduling system and, for the SLURM job
scheduler on MISTRAL, it looks like this:

::

    #SBATCH --job-name=056112           # Specify job name
    #SBATCH --partition=compute         # Specify partition name for job execution
    #SBATCH --nodes=3                   # Specify number of nodes
    #SBATCH --ntasks-per-node=24        # Specify max. number of tasks per node
    #SBATCH --time=01:00:00             # Set a limit on the total run time
    #SBATCH --account=ch0636            # Charge resources on this project account
    #SBATCH --output ./log/056112_%j.o  # output file
    #SBATCH --error ./log/056112_%j.o   # error file

| The entries here are examples for an MPI parallelized job. On the
  homepage of the DKRZ, you can find many more examples for job scripts.
  However, if you run REMO on a different machine, you would have to
  adapt the header to the job scheduling system available. You can also
  ignore the job scheduling header and execute the job script directly
  if you want to run it on a desktop computer without job scheduling.
| Afterwards, you can define some parameters concerning your experiment,
  e.g.,

::

    PFADJOB=                    # working directory where the job control script is
    EXP=`cat ${PFADJOB}/NEXP`   # experiment number of current experiment
    M=  `cat ${PFADJOB}/NMON`   # current month
    Y=  `cat ${PFADJOB}/NYEAR`  # current year
    KSA=`cat ${PFADJOB}/NSA`    # current start hour
    USER=056                    # user number of current experiment
    BUSER=056                   # user number of boundary data
    BEXP=112                    # experiment number of boundary data
    YSTART=1948                 # first year
    YSTOP=2101                  # first year  that is not calculated
    MSTOP=01                    # first month that is not calculated
    PROJID=ch0636               # project number (for the pathes)
    PROJIDRUN=ch0636            # account number for the MISTRAL 
    DT=240                      # time step of the simulation 

| The experiment number, current month, current year and current start
  hour (``NEXP``, ``NMON``, ``NYEAR``, ``NSA``) are read from the
  corresponding plain ASCII files in the main directory of the job
  script suite (called ``PFADJOB``). The reason for this is, that these
  small files will be updated by the jobscript itself when the run for
  one month is finished. In the end, the jobscript will actually submit
  itself to start the model run for the next month and read in the
  updated files for the next month. This has the advantage that no loop
  over month is neccessary and no control script has to run in a bash
  shell for the duration of the complete experiment (which could easily
  be several weeks of wallclock time).
| You also have to put in your user number ``USER`` which is maily
  needed to setup the directory structure and because it is part of file
  naming conventions when the script postprocesses the model output. The
  same holds for the user number ``BUSER`` and experiment number
  ``BEXP`` of boundary data. This information is needed for the script
  to know where to look for boundary data eithner on the ``SCRATCH``
  partition or in the tape archive.
| Afterwards, you can define the first and last year of the simulation
  as well as the last month. The project number ``PROJID`` is als used
  for setting up the directory structure and for searching of boundary
  data in the archive if neccessary. ``PROJIDRUN`` is the account number
  that is used for the scheduling system. Finally, you can define a time
  step for the simulation ``DT``.
| These parameters are used to define some path variables:

::

    #
    # Dateipfade festlegen:
    #
    WRKSHR=${SCRATCH}                  # Scratch directory
    PFL=${PFADJOB}/remo2009_mpi/libs   # Remo executable
    PFADCTRL=/pool/data/remo           # input files, such as albcycle.f
    PFADFRC=${WRKSHR}/bound_${BUSER}${BEXP} # where forcing is copied/locally stored
    DIRARC=/hpss/arch/${PROJID}/${USERID}/exp${BUSER}${BEXP}  # Boundary-Data
    PFADRES=/work/${PROJID}/g300046/remo_results_${USER}${EXP}  #  Results
    DIR=${WRKSHR}/tmp_${USER}${EXP}    # Temporary working directory
    DIRWS=$DIR                         # where namelist and restartfiles are written

where most of the variables are defined automaticall using a certain
convention for names and pathes within GERICS at DKRZ. For example,
files concerning the files for the mean annucal cycle is located on the
MISTRAL in ``/pool/data/remo``. Further down in the ``run_remo.ksh``
script you can see how the ``INPUT`` file is created:

::

    cat > INPUT << EOF
    &PARCTL
     NPROCXM=9,
     NPROCYM=8
    /
    &EMGRID
     MOIE=129,
     MOJE=121,
    ...
     YBDNAM='vgryear_cordex044.srv','vltyear_cordex044.srv','albyear_cordex044.srv',
     YGDCAT='\${PFADCTRL}',
     YGDNAM='GHG_rcp26_1850-2101.txt'
    /
    EOF

The script creates the namelists in a file called ``INPUT`` using the
``cat`` command. You can see that some parameters are set directly, e.g,
in the ``EMGRID`` namelist while others are derived from variables in
the bash script, e.g., in the ``DATEN`` namelist. These parameters are
partly defined by the bash script variables that where defined above or
depend on the current month of the model run, e.g., ``NHANF`` and
``NHENDE``. The ``INPUT`` file is then created automatically in the
temporary working directory (the script variable ``DIR``) and updated
for each month. If you want to change some of the parameters that are
set directly, you can do it here. However, you should not change the
parameters that are set dynamically and which are used for data in and
output since these depend on the parameters that where set above.

Execution of the REMO model
^^^^^^^^^^^^^^^^^^^^^^^^^^^

After the preparation of input data and the creation of the ``INPUT``
file, the REMO model can finally be executed by using the appropriate
implementation of ``mpirun`` or any other tool that manages parallel
execution of an MPI program for the job scheduler at hand. For ``SLRUM``
on MISTRAL, we use ``srun`` which can is executed in the run script
similar to this:

::

    srun -l --cpu_bind=verbose,cores ${PFL}/rremo < INPUT

Postprocessing
^^^^^^^^^^^^^^

Further down in the ``run_remo.ksh`` file, you can see how a
postprocessing script is created in the ``./scripts`` subdirectory,
e.g.,

::

    #
    # ******************  Write and call postprocessing script ******************
    #
    sed -e 's%^#BQSC%%' > ./scripts/fpost_${USER}${EXP}_${Y}${M} <<EOF
    #!/bin/ksh
    ###############################################################################
    #
    #SBATCH --job-name=fpost_${USER}${EXP}_${Y}${M}   # Specify job name
    #SBATCH --partition=shared         # Specify partition name for job execution
    #SBATCH --nodes=1                  # Specify number of nodes
    #SBATCH --ntasks-per-node=1        # Specify max. number of tasks
    #SBATCH --time=01:00:00            # Set a limit on the total run time
    #SBATCH --account=ch0636           # Charge resources on this project account
    #SBATCH --output=./log/fpost_${USER}${EXP}_${Y}${M}.log
    #SBATCH --error=./log/fpost_${USER}${EXP}_${Y}${M}.log
    ###############################################################################
    #

    ...

    #
    # **** Job fuer Folgemonat submitten:
    #
    cd \${PFADJOB}
    sbatch run_remo_mistral.sh
    #

    ....

    print " |- Post processing completed    |\n |-------------------------------|\n"
    set -x
    date
    #-----------------------------------------------------------------------------
    exit 0
    #-----------------------------------------------------------------------------
    EOF
    #
    # ******  Launch postprocessing script
    #
    sbatch ./scripts/fpost_${USER}${EXP}_${Y}\${M}
    #
    echo "Remo postprocessing was sent to the queue."

| Here are two important things to notice. First, the ``run_script.ksh``
  script creates the postprocessing script ``fpost_$USER$EXP_$Y$M`` for
  the current month separately in the ``./scripts`` subdirectory and
  submits it to the job scheduler afterwards (using the ``sbatch``)
  command. That is why the postprocessing script also contains another
  header for the job scheduler but using a different partition (e.g.,
  the *shared* partition for serial jobs). After the ``run_script.ksh``
  script has submitted the postprocessing script, it finishes and the
  the postprocessing script is executed by the job scheduler. The main
  job of the postprocessing script is then to tar all output data into a
  small number of files and copy them to the ``WORK`` partiton.
  Afterwards, the postprocessing script also submits the
  ``run_remo.ksh`` which then is ready to start again for the following
  month by reading in the updated configuration files ``NMON`` (current
  month), ``NYEAR`` (current year) and ``NSA`` (current starting hour).
| In summary, the approach of the shell script suite is to submit the
  jobscript ``run_remo.ksh`` which reads in the configuration for the
  current month, executes the model run, and submits a postprocessing
  script. After the postprocessing has been finished, the postprocessing
  scripts resubmits the jobscript ``run_remo.ksh`` with the
  configuration for the next month and the whole workflow starts again.
| When the configuration has been done successfully, the job script can
  be submitted using

::

    sbatch run_remo.ksh

The Python MRUN Control Script Suite
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Autosubmit
~~~~~~~~~~

Autosubmit is a python based job control infrastructure maintained by
the Barcelona Supercomputing Center (BSC). It allows to manage
dependencies in a hierarchy of job scripts across several computers.
Especially the management of large ensemble simulations can be easily
automated using autosubmit, but it also provides convenience when
performing single transient runs. To learn the basics of autosubmit
please refer to the official autosubmit documentation
(https://autosubmit.readthedocs.io/en/latest/). At GERICS we set up an
internal server with a central installation of autosubmit called SUBMIT
(IP: 136.172.63.15) which is set up to perform simulations on MISTRAL.
If you plan to use autosubmit elsewhere and run on MISTRAL, some
adjustments have to be made due to insufficient support of the SLURM
batch system used on MISTRAL in the official release of autsubmit.
**TODO: Add patch to autosubmit folder?** In the following, the
procedure to use autosubmit from our SUBMIT server to run jobs on
MISTRAL is explained.

Prerequisites for autosubmit on MISTRAL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to make autosubmit run properly, some settings have to be
done/adjusted. In short the following steps have to taken:

#. set up ``ssh`` autologin to MISTRAL from SUBMIT

#. install PyRemo on MISTRAL

#. add PyRemo path to ``$PYTHONPATH`` environment variable

#. load the ``module`` environment for non-interactive shells

#. load ``cdo`` and ``pftp`` modules for non-interactive shells

The first step is straightforward and is equivalent to the set up on
other machines with ``ssh`` password-less login. A simple google search
will provide a number of guides on how to do that. The SUBMIT server has
``keychain`` installed for easy passphrase protected ``ssh`` autologin.
The second and third step is installing PyRemo on MISTRAl and adding it
to the python environment. If you have not done it yet, please refer to
the PyRemo documentation on how to do it. The third and fourth steps are
essential to make python scripts running that are part of the autosubmit
set up coming with REMO. Depending on your shell, the following needs be
included in your ``.profile`` or ``.cshrc`` respectively:

::

    # in bash or ksh script
    source /sw/rhel6-x64/etc/profile.mistral

    # in tcsh or csh script
    source /sw/rhel6-x64/etc/csh.mistral

to make the ``module`` command available in all by autosubmit submitted
jobs. Currently two modules have to be loaded in same script which are
``pftp`` and ``cdo``. The complete section in a ``.profile`` script for
``bash`` would look like this:

::

    source /sw/rhel6-x64/etc/profile.mistral

    module load pftp
    module load cdo

All these settings could also be added to each individual autosubmit
script, but would make executing python scripts impossible.

Running a REMO experiment with autosubmit
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In general, autosubmit gives the freedom to define all sorts of
dependencies between jobs running on various computer platforms. The
following example describes an autosubmit workflow that allows to
downscale one or multiple members of a global model experiment with the
same amount of REMO simulations. In this example many scripts described
above and below from model compilation and NAMELIST creation to data
archiving are implemented in the autosubmit workflow. Depending on the
parent script more or less adjustments where undertaken to make best use
of the autosubmit capabilities (e.g., build-in calendar).

First of all, after setting up autosubmit an autosubmit experiment needs
to be defined. This can be achieved by typing, e.g.,

::

     autosubmit expid --HPC DKRZ -d "REMO test"

on the command line of the SUBMIT server. The option ``–HPC DKRZ`` will
tell autosubmit that the DKRZ platform will be used as default for our
experiment. After ``-d`` you can put a short description of the
experiment. Running the command will create a new directory (e.g.,
``a001``) in your autosubmit root folder, which will contain several
sub-directories. Change to the sub-directory with the configuration
files (e.g., ``a001/conf``). It will contain at least four files:

::

     autosubmit_a001.conf  expdef_a001.conf  jobs_a001.conf  platforms_a001.conf

The ``platforms_a001.conf`` holds the configuration for the computer you
are running your experiment on. For MISTRAL it will look like the
following:

::

    # Example platform with all options specified

    ## Platform name
    [DKRZ]
    ## Queue type. Options: PBS, SGE, PS, LSF, ecaccess, SLURM
    TYPE = SLURM
    ## Version of queue manager to use. Needed only in PBS (options: 10, 11, 12) and ecaccess (options: pbs, loadleveler)
    # VERSION =
    ## Hostname of the HPC
    HOST = mistral.dkrz.de
    ## Project for the machine scheduler
    PROJECT = ch0636
    ## Budget account for the machine scheduler. If omitted, takes the value defined in PROJECT
    BUDGET = ch0636
    ## Option to add project name to host. This is required for some HPCs
    # ADD_PROJECT_TO_HOST = False
    ## User for the machine scheduler
    USER = m212075
    ## Path to the scratch directory for the machine
    SCRATCH_DIR = /scratch/m/m212075
    ## If true, autosubmit test command can use this queue as a main queue. Defaults to false
    # TEST_SUITE = False
    ## If given, autosubmit will add jobs to the given queue
    QUEUE = compute
    ## If specified, autosubmit will run jobs with only one processor in the specified platform.
    # SERIAL_PLATFORM = SERIAL_PLATFORM_NAME
    ## If specified, autosubmit will run jobs with only one processor in the specified queue.
    ## Autosubmit will ignore this configuration if SERIAL_PLATFORM is provided
    SERIAL_QUEUE = shared
    ## Default number of processors per node to be used in jobs
    PROCESSORS_PER_NODE = 6
    ## Scratch free space requirements for the platform in percentage (%). If not specified, it won't be defined on the template.
    # SCRATCH_FREE_SPACE = 10
    ## Default Maximum number of jobs to be waiting in any platform queue
    ## Default = 3
    # MAX_WAITING_JOBS = 3
    ## Default maximum number of jobs to be running at the same time at any platform
    ## Default = 6
    # TOTAL_JOBS = 6

Make sure that the correct ``PROJECT``, ``BUDGET`` and ``USER`` are set.
In ``autosubmit_a001.conf`` the configuration of autosubmit itself is
located. The default setting should be fine for now. The same holds for
the ``jobs_a001.conf`` file, which will be replaced later. The
configuration file that needs the most work is ``expdef_a001.conf``.
Like all autosubmit configuration files, it is set up as unix
configuration file with sections embraced by brackets ([]) and options
with a name on the left and content on the right hand side of an equal
sign. First, find the ``[experiment]`` section and fill in the
information on your experiment. As an example we will run a ten year
test simulation with REMO in 0.44° resolution over Europe starting in
2000. To be consistent with the standard REMO output we will run each
month separately. The corresponding ``[experiment]`` section will look
like the following

::

    [experiment]
    # Supply the list of start dates. Available formats: YYYYMMDD YYYYMMDDhh YYYYMMDDhhmm
    # You can also use an abbreviated syntax for multiple dates with common parts: 200001[01 15] <=> 20000101 20000115
    # 200001[01-04] <=> 20000101 20000102 20000103 20000104
    # DATELIST = 19600101 19650101 19700101
    # DATELIST = 1960[0101 0201 0301]
    # DATELIST = 19[60-65]
    DATELIST = 20000101
    # Supply the list of members. Format fcX
    # You can also use an abreviated syntax for multiple members: fc[0 1 2] <=> fc0 fc1 fc2
    # fc[0-2] <=> fc0 fc1 fc2
    # MEMBERS = fc0 fc1 fc2 fc3 fc4
    # MEMBERS = fc[0-4]
    MEMBERS = eur044
    # Chunk size unit. STRING = hour, day, month, year
    CHUNKSIZEUNIT = month
    # Chunk size. NUMERIC = 4, 6, 12
    CHUNKSIZE = 1
    # Total number of chunks in experiment. NUMERIC = 30, 15, 10
    NUMCHUNKS = 120
    # Calendar used. LIST: standard, noleap
    CALENDAR = standard

where ``DATELIST`` is the start date, ``MEMBERS`` is the name of the
simulation, which in case of REMO has an additional meaning explained
later, ``CHUNKSIZEUNIT, CHUNKSIZE`` and ``NUMCHUNKS`` are the settings
for chunk and simulation length.

Next, we have to tell autosubmit in what kind of repository the REMO
code is stored and in which location. This will be configured by the
following two sections of the ``expdef_a001.conf`` file:

::

    [project]
    # Select project type. STRING = git, svn, local, none
    # If PROJECT_TYPE is set to none, Autosubmit self-contained dummy templates will be used
    PROJECT_TYPE = git
    # Destination folder name for project. type = STRING, default = leave empty,
    PROJECT_DESTINATION =

    # If PROJECT_TYPE is not git, no need to change
    [git]
    # Repository URL  STRING = 'https://github.com/torvalds/linux.git'
    PROJECT_ORIGIN = 'https://Kevin.Sieck@git.gerics.de/REMO/REMO_MPI.git'
    # Select branch or tag, STRING, default = 'master', help = {'master' (default), 'develop', 'v3.1b', ...}
    PROJECT_BRANCH = 'master'
    # type = STRING, default = leave empty, help = if model branch is a TAG leave empty
    PROJECT_COMMIT =

Under ``PROJECT_ORIGIN`` the git username needs be replaced and under
``PROJECT_BRANCH`` and ``PROJECT_COMMIT`` more fine grained settings can
be done. For our test simulation ``master`` is fine. All additional
configuration and script files used by autosubmit are located in the
REMO\_MPI repository under the ``autosubmit/conf`` and
``autosubmit/jobs`` directories, respectively. To include these files
automatically into the autosubmit structure, we have to make the
directories known to autosubmit by setting the following options:

::

    [project_files]
    # Where is PROJECT CONFIGURATION file location relative to project root path
    FILE_PROJECT_CONF = autosubmit/conf/proj_xxxx.conf
    # Where is JOBS CONFIGURATION file location relative to project root path
    FILE_JOBS_CONF = autosubmit/conf/jobs_xxxx.conf
    # Default job scripts type in the project. type = STRING, default = bash, supported = 'bash', 'python' or 'r'
    JOB_SCRIPTS_TYPE = 'bash'

The paths are relative to the root directory of the git project. After
this, we can create the experiment by running

::

    autosubmit create a001

on the command line. This should download the REMO code into an
``a001/proj`` directory, place additional configuration files into
``a001/conf``, and create a pdf showing the job flow of the experiment,
which most likely will look very basic. This is not the final experiment
flow and we will call ``autosubmit create`` later again to finalize the
experiment set-up.

In general, only the files located in the ``conf`` directory under the
autosubmit experiment root directory should be edited. The ``create``
command should have created the main configuration file for the REMO
experiment, which is ``proj_a001.conf``. In addition, ``jobs_a001.conf``
should have been created which defines dependencies of the entire job
chain.

First, we will have a closr look at ``proj_a001.conf``. It consists of
several sections where the most important ones for now are
``[BOUNDARY]`` and ``[REMO_RUN]``. The ``[BOUNDARY]`` sections contains
all important settings for the boundary data. In our example the section
should look like this:

::

    # Workflow relevant options
    [BOUNDARY]
    ## Usernumber of boundary data
    BOUND_USER = 058
    ## Experiment number of boundary data (if more than one member is calculated
    ## this is the base value)
    BOUND_EXP = 301
    ## Path to boundary data in archive
    BOUND_ARCH_PATH = /hpss/arch/ch0636/happi/nor-esm
    ## Style of experiment string: 1 = exp<EXP> | 2 = exp<USER><EXP>
    BOUND_EXP_STR_STYLE = 2
    ## Boundary data is stored in monthly|yearly tar-files
    BOUND_ARCH_STYLE = yearly
    ## Keep the boundary data on disk: true|false
    KEEP_BOUND = False
    ## Indicate if this will be using a warmstart. If false all WARM_... options
    ## can be omitted.
    WARMSTART = False
    ## Usernumber of warmstart data
    WARM_USER = 001
    ## Experiment number of warmstart data
    WARM_EXP = 068
    ## Path to warmstart file
    WARM_PATH = /work/ch0636/m212075/warmstart

Most of the settings should be self explaining, but one important aspect
is the handling of the warmstart. Here, the scripts expects a
restartfile (e.g., e001068f2000010100) with the given user and
experiment number and start date of the run. It is possible to just take
any restartfile (for the domain you are running on) and rename it
accordingly. Internally all relevant codes for a warmstart extracted
from the restartfile and put into the first boundary file (e.g.,
a058001a2000010100) overwriting any duplicate codes.

The ``[REMO_RUN]`` section defines the configuration of the REMO run. In
our example it will read

::

    [REMO_RUN]
    ## Skip the first time step or not. True|False
    ## Needed for model runs that start at 06 instead of 00
    SKIP_FIRST = False
    ## Will the model be compiled or not. True|False
    ## The following COMP_... options have to be set accordingly 
    COMPILE = True
    ## Filename of the tar-archive to be stored or retrieved without extension.
    COMP_NAME = test_remo_eur044
    ## Path where the project directory with compiled executable will be or is
    ## stored. Depending on the setting of COMPILE.
    COMP_ARCH = /work/ch0636/m212075/
    ## Comma seperated list of timesteps. First timestep is the default.
    ## The number of entries for DT_LIST should be equal to the number of
    ## retries for the simulation job!
    DT_LIST = 240.0, 200.0, 120.0
    ## Usernumber for REMO run
    REMO_USER = 058
    ## Experiment number for REMO run (if more than one member is calculated
    ## this is the base value)
    REMO_EXP = 301
    ## Working directory for data. Will be expanded by experiment string.
    WORK_DIR = /work/ch0636/m212075
    ## Path to store output data in archive
    REMO_ARCH_PATH = /hpss/arch/ch0636/happi/echam6
    ## Style of experiment string: 1 = exp<EXP> | 2 = exp<USER><EXP>
    REMO_EXP_STR_STYLE = 2
    ## Data directory names for output on scratch: default|empty.
    ## If left empty the options PATH_A, PATH_E, PATH_F, PATH_M, PATH_N
    ## and PATH_T have to be set!
    DATA_DIRS = default
    ## Paths that need to be set, if DATA_DIRS is not default.
    PATH_A =
    PATH_E =
    PATH_F =
    PATH_M =
    PATH_N =
    PATH_T =

The ``COMPILE`` option will define if the model will be compiled or if
an existing executable will be used. If ``COMPILE`` is set to True the
entire REMO\_MPI folder (including the executable) will be put into an
tar-archive using the name given by ``COMP_NAME`` and copied to
``COMP_ARCH``. In case ``COMPILE`` is set to False, the script expects
an tar-archive with the name given by ``COMP_NAME`` under path
``COMP_ARCH``. In this way one can reuse the same executable for a
series of experiments. As the time step is often the cause for model
crashes, one very useful setting is the option ``DT_LIST`` in the
``[REMO_RUN]`` section of ``proj_a001.conf``. This allows automatic
resubmission of the REMO job with different (preferably shorter) time
steps in the listed order. The first time step in the list is the
default for all jobs. Alternative time steps will only be used for the
current chunk (usual length is one month) and changed back to default
for the following. This mimics the standard procedure when dealing with
crashes with preceding warning messages such as ``WARNING: VMAX=...``.
By default the folders ``xa``, ``xt``, ``xe``, etc., are created in the
experiment folder. These can be changed but we will stick to the default
for now.

Some settings in ``expdef_a001.conf`` have important implications for
the behavior of settings in ``proj_a001.conf`` and the execution of the
job scripts. One of these important settings is the ``MEMBERS`` option
in the ``[experiment]`` section of ``expdef_a001.conf``. If configured
for multiple members the ``BOUND_EXP`` and ``REMO_EXP`` values in
``proj_a001.conf`` are interpreted as starting values for the
experiment. In addition, if the value of ``MEMBERS`` contains a
substring with the same name as one of the domain configuration files
(without extension) in ``REMO_MPI/autosubmit/conf`` (e.g., eur044 or
eur011), some default settings defined in the respective configuration
file are used in the NAMELIST. This offers some convenience when setting
up default experiments, because all important options for the NAMELIST
are already set, i.e. there is no need to add NAMELIST sections and
options to the ``proj_a001.conf`` file. The ``proj_a001.conf`` file
offers the opportunity to edit REMO NAMELIST setting individually
though, but this should be seen as advanced options and will not be
covered here.

Now we can run

::

    autosubmit create a001

again to properly configure the experiment. The experiment flow graph in
the pdf should confirm this by showing the complete tree now. Finally,
the experiment can be started by running

::

    autosubmit run a001

on the command line. It is usefull to start this a ``screen`` session in
order to be able to logout while the experiment is running. With running

::

    autosubmit monitor a001

on the command line you can create a pdf at any time, showing the
progress in the experiment flow graph. For more details on how to set up
an autosubmit experiment please refer to the autosubmit documentation.

Postprocessing during Model Run
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Usually, after the end of the model run, the output data is
postprocessed in some way or the other. First of all, the output data is
copied from the ``SCRATCH`` partition and tarred under certain
conventions into a few number of tar-files and the postprocessing script
that is managed by the run script should take care of this. For details,
have a look at Section [sec:data\_management]. However, there are two
python scripts which are used for further postprocessing and data
management.

Pressure Interpolation
^^^^^^^^^^^^^^^^^^^^^^

The first script is called ``pressure_interpolation.py`` which is script
that manages the pressure interpolation of t-files and it is usually
started after one year of model time has been completed. The pressure
interpolation script is configured by a config file called
``config_durint.txt`` which should be located in the current run script
directory. The python script is usually started by run script is a
complete year has been computed. The python script is managing the input
for and starts a FORTRAN program which then actually does the
computation. However, the python script is able to manage the data in
and output and creates the ``INPUT`` file for the FORTRAN pressure
interpolation program called ``druint`` (see Section [sec:druint]).

Archiving
^^^^^^^^^

| There is another python script called ``putscript.py`` which manages
  the packing and uploading of data to an HPSS tape archive. This script
  is also called after a complete year of model time has been computed
  so that all data for this year is stored under certain conventions in
  the tape archive. The script accepts some command line inputs which
  are usually managed by the run script.
| Both scripts, the ``pressure_interpolation.py`` and the
  ``putscript.py`` are called by another batch script called
  ``put_$UE$EE_$Y.sh`` that is submitted yearly by the main
  postprocessing script. It is created from the template
  ``put_output_template.sh`` in the ``template`` directory of the main
  run script. If you have a look at the template, you can see that it
  also is another batch script that should be run on a serial partition,
  e.g., ``shared`` on MISTRAL.
| If you look closely in the header of the two scripts
  ``pressure_interpolation.py`` and ``putscript.py``, you can see that
  these involve using the PyRemo tools, a library of python scripts
  designed to work with REMO data (see Appendix [sec:pyremo]) You need
  to install this libary and add it to you ``PYTHONPATH`` environment
  variable. If you work on MISTRAL, you can use the common installation
  of the GERICS project by adding this line to your profile (e.g.,
  ``.profile`` or ``.bashrc``):

::

    export PYTHONPATH=/work/ch0636/sw/python/PyRemo-1.0.0:\${PYTHONPATH}

Model Spin Up
-------------

| Usually, a special procedure is necessary for model initialisation.
  The general philosophy behind the driving fields (a-files) is that
  these files do not only contain the prognostic atmospheric parameters
  for the lateral driving of REMO but also the surface/soil parameters
  (see Table [tab:soil\_fields]) that are necessary for model
  initialisation (soil temperatures, soil wetness, snow cover, etc., see
  Section [sec:creating\_a-files]). Hence, all a-files can in principle
  also be used for model initialisation.
| However, for the model run with REMO, the surface and soil fields
  initialized with data from the forcing files might not be in an
  equilibrium state with the atmosphere in the beginning of the model
  run. Hence, the REMO model might run into a different equilibrium
  state for the surface and soil fields than present in the forcing
  files. The reason for this mainly lies in the increased resolution and
  different surface and soil models incorporated in the regional
  downscaling approach in REMO in contrast to the global model data from
  which the forcing files are derived.
| Consequently, to avoid trends in the soil conditions, the soil needs a
  *spin-up period* to reach a stable state. This period could range from
  a few years to a few decades depending on the model domain. If a so
  called *warm start* of the model run should be performed, the initial
  conditions for the state of the surface and soil fields should be
  updated with the corresponding fields after the spin-up period.

|  \| l \| l \| p7cm \| Code & Name & Description
| 84 & QDBL & specific humidity surface (land)
| 140 & WS & soil wetness
| 141 & SN & snow depth
| 170 & TD & deep soil temperature
| 183 & TDCL & soil temperature
| 206 & TSN & snow temperature
| 207 & TD3 & soil temperature
| 208 & TD4 & soil temperature
| 209 & TD5 & soil temperature
| 194 & WL & skin reservoir content
| 54 & TSL & surface temperature (land)
| 55 & TSW & surface temperature (water)
| 56 & TSI & surface temperature (ice)

Cold Start
~~~~~~~~~~

You can carry out a *cold start* (slowly reacting surface/soil fields
are not in equilibrium yet and need about 5 years for spin up) then just
launch the model with the very first forcing file that was created for
00:00 o’clock of the very first day.

Warm Start
~~~~~~~~~~

In case you want to carry out a warm start (surface/soil fields in
equilibrium), you need to choose a dataset for the surface and soil
fields that is in equilibrium with the atmospheric fields. This could
simply be the results of these fields of a cold start model run after
the spin-up period. However, a surface and soil dataset from a different
run on the same model domain and resolution should also provide
sufficient initial conditions for the surface and soil fields if they
reached an equilibrium state after the spin up period.

Replacing Surface and Soil Data in a Forcing File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Extracting and replacing certain variables or fields in a REMO in or
output file can be done in several ways. Here, we will show how to
extract the surface and soil fields from a REMO output file and replace
them in a forcing file using ``cdo``. Typically, one would extract these
fields from a REMO t-file (which should contain all neccessary surface
and soil fields) after a certain spin-up period of 5-10 years. We assume
that the usual preprocessing during the model run create a tar archive
of t-files. First, we extract an output t-file of the same date as the
start date of the entire run but from a year in which the surface and
soil fields have reached equilibrium, e.g.

::

    tar -xvf e056002t198901.tar e056002t1989010100 

Second, we extract the surface and soil fields (see Table
[tab:soil\_fields]) from the resulting file into an intermediate file
using the ``cdo`` operator ``selcode``

::

    cdo selcode,54,55,56,206,207,208,209,170,183,84,140,194,141 e056002t1990010100 warm_soil

The file ``warm_soil`` now contains the surface and soil fields in
equilibrium. Now, we change the date of this file to the start date of
the run, e.g., 1st of January 1979 (10 year spin up period) using a
script called ``ieg_setdate``

::

    ieg8_setdate warm_soil 
     IEG8_SETDATE
     Enter the new year: YYYY
    1979
     Enter the new month: MM
    01
     Enter the new day: DD
    01
     Enter the new hour: HH
    00

which creates the file out8.ieg. In the next step, we extract the very
first forcing file from the tar archive using:

::

    tar -xvf a056002a197901.tar a056002a1979010100

and delete all surface and soil fields from this file using the ``cdo``
operator ``delcode``:

::

    cdo delcode,54,55,56,206,207,208,209,170,183,84,140,194,141 a056002a1979010100 a056002a1979010100\_without\_soil

Finally, we have to concatenate the forcing file without surface and
soil fields (``a056002a1979010100_without_soil``) with the file
containing the spin up fields (``warm_soil``) using

::

    cat a056002a1979010100_without_soil out8.ieg > a056002a1979010100

This creates a new forcing file ``a056002a1979010100`` now containing
surface and soil fields in equilibrium state. We recommend to create a
new tar archive for the first month of the model run and replace the
first forcing file in this archive. This can be achieved, e.g., using

::

    cp a056002a197901.tar a056002a197901_warm_soil.tar
    tar --delete a056002a197901_warm_soil.tar a056002a1979010100 # delete old file
    tar --append a056002a197901_warm_soil.tar a056002a1979010100 # append new file

| Finally, the new tar archive should be uploaded to the HPSS archive if
  available so that either a cold or a warm start is possible. Now the
  model run can be restarted from the beginning with an equilibrium
  state in the surface and soil fields.
| [sec:spinup]
