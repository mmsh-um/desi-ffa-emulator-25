Last update: 17/04/2023

Given a lightcone, the code finds friend-of-friends (FOF) halos in angular space (i.e. the linking length is an arbitrary angle, the radial position is irrelevant).

The main file is fof_lightcone_anf.f90.

To compile you can run COMPILE.sh (if you copy the code in a new directory you need to change the script with name of the new directory).

The most important settings can be changed via the INI_fof_lightcone_ang.txt file, which should be given as input when running the code:

  ./fof_lightcone_ang INI_fof_lightcone_ang.txt

A few other parameters are contained in PRM_* files (when you change one of these PRM_* files you need to recompile)

At the moment the code is not parallel (there are a couple OpenMP statements it in the routine get_halos_ang.f90, but their are not ready for use yet)

Once complied, to run the code on NERSC you can run the script SUBMIT.sl

You can find a brief description of how the halo information is encoded in a comment at the beginning of the main file itself 
  

