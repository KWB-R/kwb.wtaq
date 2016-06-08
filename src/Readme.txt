README.TXT


                           WTAQ - Version: 2.1
  A Computer Program for Calculating Drawdowns and Estimating Hydraulic
           Properties for Confined and Water-Table Aquifers

NOTE: Any use of trade, product or firm names is for descriptive
      purposes only and does not imply endorsement by the U.S. 
      Government.

This version of WTAQ is packaged for personal computers using the
Microsoft Windows operating system. An executable file for personal
computers is provided as well as the source code.  The source code can
be compiled to run on other computers.


Instructions for installation, execution, and testing of WTAQ are
provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING WTAQ
                         D. TESTING
                         E. COMPILING
                         

A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

          WTAQ_2.1.exe

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows you to specify the directory in which the files should
be restored.  The default installation directory is C:\WRDAPP.  You
have the opportunity to specify an alternate installation directory
during extraction of the software. The following directory structure
will be created in the installation directory:

 |--WTAQ_2.1
    |--bin            : WTAQ executable for personal computers
    |--doc            : Documentation reports for WTAQ
    |--sample-problems: Input and output data for sample problems
    |--src            : WTAQ source code for use on any computer

It is recommended that no user files are kept in the WTAQ_2.1 directory
structure.

Included in directory WTAQ_2.1\doc are the documentation reports for WTAQ
(Barlow and Moench, 1999 and 2011). The reports are Portable Document 
Format (PDF) files. The PDF files are readable and printable on various 
computer platforms using Acrobat Reader from Adobe. The Acrobat Reader is 
freely available from the following World Wide Web site:
      http://www.adobe.com/


B. INSTALLING

To make the executable version of WTAQ accessible from any
directory, the directory containing the executable (WTAQ_2.1\bin)
should be included in the PATH environment variable.  Also, if a
prior release of WTAQ is installed on your system, the
directory containing the executable for the prior release should
be removed from the PATH environment variable.

To add a PATH environment variable on Windows XP Systems, do the
following: From the Start menu, select Settings and then Control Panel.
Double click System and select the Advanced tab.  Click on Environment
Variables. If a PATH user variable already is defined, click on it in
the User Variables pane, then click Edit. In the Edit User Variable
window, add the path name to the \bin directory (for example,
";C:\WTAQ\WTAQ_2.1\bin)" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.
If a PATH user variable is not already defined in the User variables
pane of the Environment Variables window, click New.  In the New User
Variable window, define a new variable name PATH and a new variable
value (for example, "C:\WTAQ\WTAQ_2.1\bin").  Click OK.  Click OK in
the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command window.


As an alternative, the executable file wtaq.2.1.exe in the 
WTAQ_2.1\bin directory can be copied into a directory already included
in the PATH environment variable.


C. EXECUTING WTAQ

WTAQ has been compiled using the Intel Visual Fortran 9.0 Compiler,
Standard Edition, for Windows. The executable file (wtaq.2.1.exe) is
located in the \bin directory.

After the executable file in the bin directory is installed in a 
directory that is included in your PATH, WTAQ is initiated in
a Windows Command-Prompt window with the following command:

          wtaq.2.1.exe

The program then prompts the user for the name of the input, results,
and plot files.

Alternatively, the user can copy the executable into the directory 
in which the input file is located and then double click on the 
executable to initiate the simulation.

D. TESTING

Test data sets are provided in the 'sample_problems' subdirectory to
verify that WTAQ is correctly installed and running on the system.
The subdirectory includes a readme file (Readme.samples.txt) that 
describes the four sample problems.

E. COMPILING

Although an executable version of the program is provided, the source
code also is provided in the WTAQ_2.1\src directory so that WTAQ can
be recompiled if necessary. However, the USGS cannot provide assistance
to those compiling WTAQ. In general, the requirements are a Fortran
compiler and the knowledge to use the compiler.





