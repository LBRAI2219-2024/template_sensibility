The fastest way to try this compatible version of CRootBox with MARSHAL is to read the examples.

In the main.cpp file, you can select which example you want, then compile and run it.

# CRootBox June 2017 for Windows 

The code should compile with any c++11 compiler, e.g. for g++: MinGW has been tested.
Then create a new system environment variable. Path --> C:\MinGW\bin

- Open the terminal:

> cd ~/GitHub/marshal-pipeline/17_06 CRootBox

> g++ *.cpp -std=c++11

> a.exe

# CRootBox June 2017 for Mac and Linux

- Open the terminal:

> cd ~/GitHub/marshal-pipeline/17_06 CRootBox

> g++ *.cpp -std=c++11

> a.out

***

## The latest CRootBox Version

[CRootBox](https://github.com/Plant-Root-Soil-Interactions-Modelling/CRootBox)

# Folder structure:

- CRootBox C++ codes
* examples 		Some examples how to use the CRootBox
* modelparameter		Some root parameter, and a plant parameter files
* scripts 		Pyhthon scripts for visualization with Paraview, and Matlab scripts for parameter export
* www 		


# Documentation

Create the documentation by running doxygen in the folder 
$ doxygen doxy_config

The documentation should now be located in the folder /doc

To build the shared library py_rootbox for coupling with Python pleaser refer to 'python building guide.txt'

