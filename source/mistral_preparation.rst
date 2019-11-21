Preparation
===========

| Before running the REMO model on the machine MISTRAL at DKRZ, it is
  useful to pay some attention to the configuration of your personal
  accout at DKRZ. We recommend to vist the User Portal of the Mistral
  for a lot of helpful advices concerning the workflow on MISTRAL.
  However, we will summarize the most important steps here to make sure,
  you are able to run the REMO Tutorial.
| If you have access to the MISTRAL, you can login to one of the login
  nodes using ssh:

.. code:: bash

    ssh  -X <userid>@mistral.dkrz.de 

| By default, you should be using the bash shell which you can configure
  by using the hidden file ``.bashrc`` in your home directory. To
  compile and run the REMO model for this tutorial, you will need a
  Fortran and MPI wrapper compiler. On the Mistral, these are managed by
  the ``module`` command and we will use the Intel Fortran compiler
  together with the Bullx MPI wrapper compiler to compile REMO for
  parallel runs. You get an overwiew of available software modules using
  ``module avail`` and to load a specific module, you can use the
  ``module load`` command.
| For this tutorial, you will need to add the following line to your
  ``.bashrc``

.. code:: bash

    # Use the default versions of Intel compiler and Bullx MPI with Mellanox MXM and FCA tools
    # Also add ncview and pftp
    module load intel mxm fca bullxmpi_mlx ncview pftp

| to make sure, you have all modules loaded when compiling and running
  the REMO model. Remember to relogin once you have edited your
  ``.bashrc`` file. You will also need a text editor to edit text files
  (e.g., a bash script). On Mistral, all common editors are available,
  e.g., ``emacs``, ``gedit`` or ``vim``. Remember to login to Mistral
  with X11-Forwarding using ``ssh -X`` if you want to use a text editor
  that requires a graphical user interface.
| Projects on MISTRAL are managed using account numbers, e.g., the
  account number for this tutorial is ``ch0636`` for internal use at
  GERICS. This account number is used to manage computing time and
  storage capacities, e.g., for this tutorial we will work in ``WORK``
  partition of the ``ch0636`` account which is located at

.. code:: bash

    /work/ch0636

where you should create a personal directory for your own data using
your user id, e.g.,

.. code:: bash

    mkdir /work/ch0636/\$USER

if this directory does not yet exists.

Setting up the Tutorial Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally you will need the tutorial data in your personal working
directory. The data is available at

.. code:: bash

    /work/ch0636/remo_tutorial/remo_tutorial.tar.gz 

Copy this data to you run directory and untar it, e.g., by typing this:

.. code:: bash

    cp /work/ch0636/remo_tutorial/remo_tutorial.tar.gz .
    tar -xvzf remo_tutorial.tar.gz

This will create a directory called ``remo_tutorial`` in your personal
working directory containing the REMO model source code
(``ModelRun/REMO_MPI``), a suite of software tools to create surface
boundary conditions (``BodLibKit``) and a script for running the
tutorial on MISTRAL (``ModelRun/test_remo.ksh``).
