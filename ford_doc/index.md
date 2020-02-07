title: README
This set of programs was created owing to the lack of available open-source interfaces to the C81 airfoil table format. This package contains Fortran subroutines to read, write and create from scratch C81 formatted files.

### C81 File
A C81 airfoil performance table is a text file that lists coefficients of lift, drag, and pitching moment of an airfoil as functions of angle of attack for a range of Mach numbers. This file is often used as input to reduced-order flight dynamics simulation programs for estimation of aerodynamic forces and moments. The data obtained using this format is typically used to implement lookup tables with linear interpolation. The angles of attack are generally specified from -180 to +180 degrees and the Mach numbers over a desired range. Lift, drag and moment coefficients are each tabulated one below the other as two-dimensional tables in a fixed spacing format. Data for these tables are usually obtained from CFD or experiments.

The C81 format provided below was obtained from [Stanko, J. D.](https://etda.libraries.psu.edu/catalog/14464jds5668) in his 2017 thesis titled _Automated Design and Evaluation of Airfoils for Rotorcraft Applications_.


##### C81 File Format
```
                                              Read/Write Format
--------------------------------------       ------------------
AIRFOIL_NAME        ML,NL,MD,ND,MM,NM          A30,6I2,6I2 
       M(1)     ...   ...    M(ML)             7X,9F7.0 
AL(1)  CL(1,1)  ...   ...    CL(1,NL)          10F7.0/(7X,9F7.0) 
.      .                     .                 .
.      .                     .                 .
.      .                     .                 .
AL(NL) CL(NL,1) ...   ...    CL(NL,ML)         10F7.0/(7X,9F7.0) 
       M(1)     ...   ...    M(MD)             7X,9F7.0 
AD(1) CD(1,1)   ...   ...    CD(1,ND)          10F7.0/(7X,9F7.0) 
.      .                     .                 .
.      .                     .                 .
.      .                     .                 .
AD(ND) CD(ND,1) ...   ...    CD(ND,MD)         10F7.0/(7X,9F7.0) 
       M(1)     ...   ...    M(MM)             7X,9F7.0 
AM(1)  CM(1,1)  ...   ...    CM(1,NM)          10F7.0/(7X,9F7.0) 
.      .                     .                 .
.      .                     .                 .
.      .                     .                 .
AM(NM) CM(NM,1) ...   ...    CM(NM,MM)         10F7.0/(7X,9F7.0) 

AL = Lift coefficient angles of attack 
AD = Drag coefficient angles of attack 
AM = Pitching moment coefficient angles of attack 
ML = Number of lift coefficient Machs 
NL = Number of lift coefficient alphas 
MD = Number of drag coefficient Machs 
```

### Contents of the repository
This repository contains a module file named libC81.f90 containing the subroutines and functions for interfacing with C81 files. It also contains a few demo fortran programs that exemplify the usage of the module. Detailed information is available in the sections of this documentation corresponding to each file.
