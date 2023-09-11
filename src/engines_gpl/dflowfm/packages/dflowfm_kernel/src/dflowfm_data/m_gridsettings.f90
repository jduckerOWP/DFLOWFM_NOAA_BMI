!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Regular grid generation settings. All orthogonalisation settings are in
!! module m_orthosettings.
MODULE M_GRIDSETTINGS
implicit none

integer :: MFAC  = 2000 !< M-refinement factor for regular grid generation.
integer :: NFAC  = 40 !< N-refinement factor for regular grid generation.
integer :: ITSMO = 10 !< Nr. of inner iterations in regular grid smoothing.
integer :: ITSMA      !< Not in use, old rgfgrid
integer :: JADEPDESIGN = 0
integer :: MDESIGN
double precision :: BFAC=1d0, CSMO = 0.5d0, RFAC
double precision :: SRM,SRN,DEPSLO,FSMA, ALINEN, ALINEM
INTEGER :: KEEPSTARTDIR = 1
double precision :: BAAS2 = 0.5d0, FACMIR = 1.2d0
double precision :: SPLFAC, SPLFAC2
INTEGER :: JDEMO = 0

! Pillar grid settings
double precision :: pil_rad  = 0d0  !< pillar radius
double precision :: pil_x    = 0d0  !< pillar center point x-coordinate
double precision :: pil_y    = 0d0  !< pillar center point y-coordinate
double precision :: pil_grow = 1d0   !< pillar grid growth factor *not used*

END MODULE M_GRIDSETTINGS
