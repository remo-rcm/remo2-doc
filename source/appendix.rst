Tips, Tricks and Troubleshooting
================================

Working with data
-----------------

The IEG tool
~~~~~~~~~~~~

There is a tool available that can read IEG files and give information
especially related to REMO data. The tool is available here:

::

    http://git.gerics.de/REMO/ieg.git

| The ieg tool is a simple command line interface tool to quickly read
  IEG files. It also supports non-standard IEG files with more than 50
  levels (e.g., for g-files).
| To install the tool, go to the build directory and adapt Makefile.h
  for your system. To use one of the default headers, just copy it over
  Makefile.h.

Then just try:

::

    make

and afterwards:

::

    make install

The final installation process will copy the executable to your personal
``$HOME/bin`` directory. To use ieg, the ``$HOME/bin`` should be in your
``PATH`` environment variable.

You can then use the command, e.g., using the ``info`` option to get
REMO related information about the file. The program contains a REMO
table to get some meta information about codes in an IEG file.

::

    ieg info file

PyRemo and PyPlot Tools
~~~~~~~~~~~~~~~~~~~~~~~

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

Missing Files
~~~~~~~~~~~~~

If working with forcing data, it might sometimes be the case that some
files are missing. E.g., if you apply double-nesting and you create
high-resolution forcing data from an earlier low-resolution REMO run,
you are probably missing the very first a-file at 0 hours. You can then
simply copy the 6 hour forcing file to a 0 hour forcing file and change
the date to 0 hours. The same holds for the very last forcing file.

Some more Tricks...
-------------------

Working with HPSS
^^^^^^^^^^^^^^^^^

On MISTRAL, you have to load the pftp module to use it. You also need to
create a .netrc file in your home directory so that a run script can
automatically login to the archive. According to the DKRZ page, it
should look like this:

::

    machine tape login <user> password <password>
    machine tape.dkrz.de login <user> password <password>

Remember that this file must not be readable by anyone but the user! So
you need to set permissions of this file, using, e.g., ``chmod 600``.

Working with tar-files
^^^^^^^^^^^^^^^^^^^^^^

Did you know that you can easily delete and add files from and to a
tar-archive? To delete a file from a tar-archive, you can use the
command

::

    tar --append --file=collection.tar rock

or to remove a file, use

::

    tar --delete --file=collection.tar rock

These commands are very useful when working with tar archives of forcing
files of output data.

Working with GIT
^^^^^^^^^^^^^^^^

It seems, that you only can clone a gerics git repository per http on
MISTRAL. You also need to disable ssl verification by using the flag “-c
http.sslVerify=false” or by adding it to your .gitconfig in your home
directory, e.g.:

::

    [http]
        sslVerify = false

If the flag does not work, you will probably have to load a newer git
version with the module command. You can also add your gitlab
credentials to your .netrc file in your home directory, e.g.,

::

    machine git.gerics.de login <user> password <password>

See only the history of your branch, e.g., when it was created

::

    git log <branch> --not master

Troubleshooting
---------------

Endianness
^^^^^^^^^^

Sometimes REMO might crash when trying to read an IEG file with some
error output. This is very often due to a problem with the endianness of
the forcing file. As a default, REMO is usually compiled with a convert
flag that force REMO to read and write binary files in the Big Endian
format. If REMO fails to read a binary file, you should check the
endianess of the input file using the ``cdo sinfo`` command. Then you
should check in the Makefile if the compiler options contain some
*convert* flag like, e.g., ``-convert big_endian`` and if it is
consistent with the endianness of the input file.

Naming Conventions [1]_
-----------------------

-  a002155aYYYYMM.tar : These files contain the 6-hourly forcing data
   for REMO (“a” = “Antrieb” = forcing). The individual ieg files in the
   archive are organised by time and are named a002155aYYYYMMDDHH, one
   file every 6 hours.

-  e002155eYYYYMM.tar: These are the main result files (“e” = “Ergebnis”
   = result) for the 2D fields, usually in hourly resolution (hourly
   means). The individual ieg files in this archive are organised by
   CODES (3- digit code number CCC) and are named
   e002155e\_cCCC\_YYYYMM, one file for each code.

-  e002155tYYYYMM.tar : These archives contain 6-hourly model results
   (instantaneous values) for a number 2D surface fields and for the 3D
   atmospheric fields. The individual ieg files in the archive are
   organised by time and are named e002155tYYYYMMDDHH, one file every 6
   hours. Among other purposes, the t-files are used for computing the
   forcing for finer-resolution REMO experiments.

-  e002155mYYYYMM.tar : These archives contain monthly means (“m”) and
   monthly standard deviations (“s”) of the most important 2D fields
   Each archive consists of two individual files: e002155mYYYYMM
   (monthly means) e002155sYYYYMM (monthly standard deviations).

-  e002155fYYYYMM.tar : These archives contain the model restart files,
   i.e. the prognostic fields at the last two time steps of the month.
   They are written at the end of each month’s calculation and are read
   by the model at the beginning of the next month’s calculation for
   initialisation. Each archive consists of two individual files:
   e002155fYYYYMM and e002155gYYYYMM (two time steps).

The IEG Format [2]_
-----------------------

| The generic data format used by REMO is a “Max Planck -internal”
  FORTRAN binary format which is usually referred to as “ieg-format”
  (ieg). Both the model input data (surface library, forcing fields) and
  the model output are based on this format. Sometimes, a reduced format
  called “service-format” (srv) is used which contains the same binary
  data fields but a reduced header (see below). Both the ieg and the srv
  format have the following sequential block structure:
| HEADER 1, FIELD 1
| HEADER 2, FIELD 2
| HEADER 3, FIELD 3
| ...
| HEADER n, FIELD n
| The header precedes the following binary 2D-field (nx\*ny) and
  contains information on its basic characteristics (code number, date
  and time, level, etc.). The header consists of 58 INTEGER and 101 REAL
  values (ieg format) and 8 INTEGER values (srv format) while the binary
  field consists of (nx\*ny) REAL values.
| A typical FORTRAN routine that opens an ieg model result file (nx=121,
  ny=121) and reads in the first header and the first field would look
  like this:

::

    PROGRAM READ_IEG
    INTEGER, PARAMETER :: NX = 121, NY = 121
    INTEGER IPDB(37), IGDB(22)
    REAL REF, AK(50), BK(50)
    REAL FIELD(NX,NY)
    OPEN(20,file='infile.ieg', FORM="UNFORMATTED")
    READ(20) IPDB,(IGDB(I),I=1,18),REF,(IGDB(J),J=20,22),AK,BK
    READ(20) FIELD
    CLOSE(20)
    END PROGRAM

For the reduced srv format the program looks more simple:

::

    PROGRAM READ_SRV
    INTEGER, PARAMETER :: NX = 121, NY = 121
    INTEGER IHEAD(8)
    REAL FIELD(NX,NY)
    OPEN(20,file='infile.srv', FORM="UNFORMATTED")
    READ(20) IHEAD
    READ(20) FIELD
    CLOSE(20)
    END PROGRAM

In the case of ieg, the IPDB header field contains the “Product
Definition Block”, the IGDB field the “Grid Definition Block”, and the
AK and BK arrays define the vertical grid on which the model results are
based (hybrid sigma coordinate system; NLEV+1 pairs for NLEV levels).
The real variable REF ist usually set to 0.0. The components of the IPDB
and IGDB arrays are listed in Table [tab:igdb] and [tab:ipdb]

+-----+------------------------------------------------------------------------------------------------------------+
| 1   | Length of the Grid Definition Block in Octets                                                              |
+=====+============================================================================================================+
| 2   | Number of unused Bits at the end of the GDB                                                                |
+-----+------------------------------------------------------------------------------------------------------------+
| 3   | Reserved                                                                                                   |
+-----+------------------------------------------------------------------------------------------------------------+
| 4   | Data Representation (10 for rotated Grid)                                                                  |
+-----+------------------------------------------------------------------------------------------------------------+
| 5   | **Number of grid points in x-direction                                                                     |
|     | 6 & **Number of grid points in y-direction                                                                 |
|     | 7 & Latitude :math:`\varphi` of lower left corner in the rotated system in :math:`10^{-3}` degree          |
|     | 8 & Longitude :math:`\lambda` of lower left corner in the rotated system in :math:`10^{-3}` degree         |
|     | 9 & Flag                                                                                                   |
|     | 10 & Latitude :math:`\varphi` of upper right corner in :math:`10^{-3}` degree                              |
|     | 11 & Longitude :math:`\lambda` of upper right corner in :math:`10^{-3}` degree                             |
|     | 12 & Latitude increment (resolution) :math:`\Delta\varphi ` in x-direction in :math:`10^{-3}` degree       |
|     | 13 & Longitude increment (resolution) :math:`\Delta\lambda ` in y-direction in :math:`10^{-3}` degree      |
|     | 14 & Flag                                                                                                  |
|     | 15 & Scanning Mode                                                                                         |
|     | 16 & Reserved                                                                                              |
|     | 17 & Latitude :math:`\varphi` of rotated south pole in the non-rotated system in :math:`10^{-3}` degree    |
|     | 18 & Longitude :math:`\lambda` of rotated south pole in the non-rotated system in :math:`10^{-3}` degree   |
|     | 19 & Rotation angle saved in REF                                                                           |
|     | 20 & Not used                                                                                              |
|     | 21 & Not used                                                                                              |
|     | 22 & Not used                                                                                              |
|     | from 23 on & Vertical Coordinate Parameters saved in AK and BK                                             |
|     | ****                                                                                                       |
+-----+------------------------------------------------------------------------------------------------------------+

Table: IEG Format; Grid Definition Block contained in IGDB, REF, AK and
BK.

+------+----------------------------------------------------+
| 1    | Length of the Product Definition Block in Octets   |
+======+====================================================+
| 2    | Edition number                                     |
+------+----------------------------------------------------+
| 3    | Station ID                                         |
+------+----------------------------------------------------+
| 4    | Type ID                                            |
+------+----------------------------------------------------+
| 5    | Catalouge number                                   |
+------+----------------------------------------------------+
| 6    | Block flag, shows if more blocks are following     |
+------+----------------------------------------------------+
| 7    | Code Number                                        |
+------+----------------------------------------------------+
| 8    | Level type                                         |
+------+----------------------------------------------------+
| 9    | Level type, depends on nr. 8                       |
+------+----------------------------------------------------+
| 10   | Level height                                       |
+------+----------------------------------------------------+
| 11   | Year                                               |
+------+----------------------------------------------------+
| 12   | Month                                              |
+------+----------------------------------------------------+
| 13   | Day                                                |
+------+----------------------------------------------------+
| 14   | Hour                                               |
+------+----------------------------------------------------+
| 15   | Minutes                                            |
+------+----------------------------------------------------+
| 16   | Time unit                                          |
+------+----------------------------------------------------+
| 17   | Not used                                           |
+------+----------------------------------------------------+
| 18   | Not used                                           |
+------+----------------------------------------------------+
| 19   | 10: Prediction time,                               |
+------+----------------------------------------------------+
|      | 4: Sum for a prediction time,                      |
+------+----------------------------------------------------+
|      | 3: mean for a prediction time                      |
+------+----------------------------------------------------+
| 20   | For mean values                                    |
+------+----------------------------------------------------+
| 21   | Reserved                                           |
+------+----------------------------------------------------+
| 22   | Free                                               |
+------+----------------------------------------------------+

Table: IEG Format; Product Definition Block contained in IPDB.

| Within each binary field, the first value represents the lower left
  grid box, with columns varying faster than lines (i.e., the lowermost
  line is filled at first from left to right before starting with the
  second line). Regarding the sequence of the fields, the date and time
  are the parameters that vary slowest, then the code and then the
  level. For instance, in a model results file containing two parameters
  (code 1 and code 2, both defined on 27 levels) for two points in time
  (date 1 and date 2) the sequence would look like this:
| HEADER date 1, code 1, level 1
| FIELD date 1, code 1, level 1
| HEADER date 1, code 1, level 2
| FIELD date 1, code 1, level 2
| ...
| HEADER date 1, code 1, level 27
| FIELD date 1, code 1, level 27
| HEADER date 1, code 2, level 1
| FIELD date 1, code 2, level 1
| HEADER date 1, code 2, level 2
| FIELD date 1, code 2, level 2
| HEADER date 1, code 2, level 27
| FIELD date 1, code 2, level 27
| HEADER date 2, code 1, level 1
| FIELD date 2, code 1, level 1
| HEADER date 2, code 1, level 2
| FIELD date 2, code 1, level 2
| ...
| HEADER date 2, code 1, level 27
| FIELD date 2, code 1, level 27
| HEADER date 2, code 2, level 1
| FIELD date 2, code 2, level 1
| HEADER date 2, code 2, level 2
| FIELD date 2, code 2, level 2
| ...
| HEADER date 2, code 2, level 27
| FIELD date 2, code 2, level 27
| needs to be translated
| Das IEG-Format ist im IEEE Binärformat abgespeichert, wobei zwei
  computerabhängige Formatierungen vorliegen, die sich durch die
  Anordnungen der Bytes unterscheiden, das sogenannte Big-Endian und
  Little-Endian Format. Alle Daten in der CERA Datenbank liegen im
  Big-Endian Format vor und können mit FORTRAN Programmen unter UNIX
  problemlos eingelesen werden. Linux und Windows Computer verwenden das
  Little-Endian Format. Daher müssen die Daten evt. vorher umformatiert
  werden, nur dann nicht, wenn der FORTRAN Compiler in der Lage ist auch
  Big-Endian Formate einzulesen. Die CDOs können mit beiden
  Datenformaten umgehen, so dass hier keine Konvertiereung notwendig
  ist.

Code List of Variables
----------------------

.. csv-table:: Code List
   :file: code_list.csv
   :widths: 5 5 5 5 20 5 5
   :header-rows: 1


Namelist of Input Parameters
----------------------------

Some variables are version specific. Please mention the dependence on
specific REMO versions in the variable description below. All namelist
variables are contained in the routine medea.f (without explanations,
though).

| \| l \| l \| p9cm \| l \| [tab:remo\_input] Namelist & Parameter &
  Description & Datatype
| & & &

Namelist for the Preprocessor
-----------------------------

Constants in REMO Physics
-------------------------

Model Domains and Grids
-----------------------

REMO was originally developed for the European model domain. Through
several projects and cooperations, REMO was further developed and used
in other domains such as South America, Africa, North America, Asia (see
Table [tab:model\_domains]). In the evaluation studies, several model
parameters were adapted to reproduce the present regional climate (Table
[tab:model\_domains\_parameters]).

+--------------+-------------+-------+-------+------+-----------------------------+
| Resolution   | Name        | nx    | ny    | nz   | Description                 |
+==============+=============+=======+=======+======+=============================+
| 0.44         | CORDEX044   | 129   | 121   | 27   | Eurocordex Domain           |
+--------------+-------------+-------+-------+------+-----------------------------+
| 0.11         | EUROPA011   | 433   | 433   | 27   | Eurocordex Domain           |
+--------------+-------------+-------+-------+------+-----------------------------+
| 0.44         | EAS044      | 217   | 181   | 27   | East Asia Cordex Domain     |
+--------------+-------------+-------+-------+------+-----------------------------+
| 0.22         | EAS022      | 433   | 361   | 27   | East Asia Cordex Domain     |
+--------------+-------------+-------+-------+------+-----------------------------+
| 0.44         | CLPB044     | 151   | 181   | 31   | South America CLPB Domain   |
+--------------+-------------+-------+-------+------+-----------------------------+

Table: Available Model Grids

+-----------------+--------+------------+------------+
| Domain          | Res.   | ZDLAND-L   | ZDLAND-O   |
+=================+========+============+============+
| Europe          | 0.44   | 7.5E+3     | 7.5E+3     |
+-----------------+--------+------------+------------+
| Europe          | 0.11   | 7.5E+3     | 7.5E+3     |
+-----------------+--------+------------+------------+
| Africa          | 0.44   | 2.0E+4     | 1.5E+4     |
+-----------------+--------+------------+------------+
| South Asia      | 0.44   | 2.5E+4     | 3.0E+4     |
+-----------------+--------+------------+------------+
| South America   | 0.44   | 7.5E+3     | 7.5E+3     |
+-----------------+--------+------------+------------+
| North America   | 0.44   | 7.5E+3     | 7.5E+3     |
+-----------------+--------+------------+------------+

Table: REMO Model Parameters

In addition to the tables indicated, some domain-specific functions were
introduced to reduce model biases in regions such as South America and
West Asia.

South America
~~~~~~~~~~~~~

In vdiff.f, Line 515 is commented out:

::

    > C      ZPLMIN=0.35
    > C ARCR: changed wilting point to locate forested region e.g. in the Amazon basin 

And the following lines were added after Line 783:

::

    > C condition for ZPLMIN from 0.35 to 0.1 (in forested region)
    > C arcr, 2014-08-19 changed the ZPLMIN to lowest possible (0.01)
    >           IF (PVLT(JL).GE.8.) THEN
    >               ZPLMIN=0.01
    >           ELSE
    >               ZPLMIN=0.35
    >           ENDIF

    > C condition for ZPLMIN          
    >           IF (PVLT(JL).GE.8.) THEN
    >               ZPLMIN=0.1
    >           ELSE
    >               ZPLMIN=0.35
    >           ENDIF

West Asia
~~~~~~~~~

In init.f, Line 108 to 139 were modified:

::

    108,109
    >           RGCGN(I)=1.28E+06
    >           TLAMBDA(I)=2.34E-7
    114,115
    >           RGCGN(I)=1.35E+06
    >           TLAMBDA(I)=2.07E-7
    120,121
    >           RGCGN(I)=1.42E+06
    >           TLAMBDA(I)=1.76E-7
    126,127
    >           RGCGN(I)=1.5E+06
    >           TLAMBDA(I)=1.4E-7
    132,133
    >           RGCGN(I)=1.63E+06
    >           TLAMBDA(I)=1.1E-7
    138,139
    >           RGCGN(I)=0.58E+06
    >           TLAMBDA(I)=1.03E-7

Grid Decomposition
------------------

(0,0) grid (12,12); (0,0) – (12,0); (0,0) – (0,12); (12,0) – (12,12);
(0,12) – (12,12);

REMO Model Parameters
---------------------

Possible domain sizes
---------------------

Here you will find a list of possible domain sizes up to 600 gridboxes
per dimension (these are the so called magic numbers). Due to a fft
function in REMO only domain sizes with certain characteristics are
allowed. The formula is the following:

.. math::

   l=2^{n} 3^{m} 5^{o} +1
   \label{equ:domain}

with :math:`l` the number of gridboxes in x or y direction, :math:`n` =
1,2,3,... , :math:`m` and :math:`o` = 0,1,2,3,...

Possible numbers of gridboxes are:

3 5 7 9 11 13 17 19 21 25 31 33 37 41 49 51 55 61 65 73 81 91 97 101 109
121 129 145 151 161 163 181 193 201 217 241 251 257 271 289 301 321 325
361 385 401 433 451 481 487 501 513 541 577 601 641 649 721 751 769 801
811 865 901 961 973 1001 1025 1081 1153 1201 1251 1281 1297 1351 1441
1459 1501 1537 1601 1621 1729 1801 1921 1945 2001 2049 2161 2251 2305
2401 2431 2501 2561 2593 2701 2881 2917 3001 3073 3201 3241 3457 3601
3751 3841 3889 4001 4051 4097 4321 4375 4501 4609 4801 4861 5001 5121
5185 5401 5761 5833 6001 6145 6251 6401 6481 6751 6913 7201 7291 7501
7681 7777 8001 8101 8641 8749 9001 9217 9601 9721 10001

(script for creating magic numbers is available in the BodLibKit)

.. [1]
   based on “Running REMO at Ouranos” by Sven Kotlarski

.. [2]
   based on “Running REMO at Ouranos” by Sven Kotlarski and “Hinweise
   für REMO Datenutzer”
