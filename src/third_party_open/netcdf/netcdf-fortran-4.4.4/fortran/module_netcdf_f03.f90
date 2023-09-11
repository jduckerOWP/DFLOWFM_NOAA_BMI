Module netcdf_f03

!! Module to provide inheritance of data and interfaces from nf_data and
!! nf_interfaces modules. Not really needed by netCDF but provided for
!! folks writing new code or updating old code who would prefer using
!! a module instead of the old netcdf.inc include file.

!! Written by: Richard Weed, Ph.D.
!!             Center for Advanced Vehicular Systems
!!             Mississippi State University
!!             rweed@cavs.msstate.edu

!! License (and other Lawyer Language)

!! This software is released under the Apache 2.0 Open Source License. The
!! full text of the License can be viewed at :
!!
!!   http:www.apache.org/licenses/LICENSE-2.0.html
!!
!! The author grants to the University Corporation for Atmospheric Research
!! (UCAR), Boulder, CO, USA the right to revise and extend the software
!! without restriction. However, the author retains all copyrights and
!! intellectual property rights explicitly stated in or implied by the
!! Apache license

!! Version 1.0 - Sept. 2005 - Initial Cray X1 version
!! Version 2.0 - April 2009 - Added netcdf4 interfaces

!! This module can be used as a replacement for an include to netcdf.inc
!! The module is named netcdf_f03 to avoid a conflict with the netcdf
!! module generated by the Fortran 90 interfaces

 USE netcdf_nf_data        ! Brings in the nf interface error flags etc.
 USE netcdf_nf_interfaces  ! Brings in explicit interfaces to nf_ routines

 Implicit NONE

!!------------------------------- End of Module netcdf_f03 ---------------------
 End Module netcdf_f03
