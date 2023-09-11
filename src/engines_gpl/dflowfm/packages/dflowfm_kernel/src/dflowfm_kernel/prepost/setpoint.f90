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

  SUBROUTINE SETPOINT(XP,YP,ZP,K1)

  use m_netw

  implicit none
  double precision :: XP, YP, ZP
  integer :: K1
  integer :: jav
  integer :: jview
  double precision :: xyz

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  CALL TWEEDRIE(XP,YP,XK(K1),YK(K1),ZK(K1))
  IF (JVIEW .EQ. 1) THEN
     ZK(K1) = zp ! XYZ
  ELSE IF (JVIEW .EQ. 2) THEN
     XK(K1) = XYZ
  ELSE IF (JVIEW .EQ. 3) THEN
     YK(K1) = XYZ
  ENDIF
  IF (KC(K1) .EQ. 0) KC(K1) = 1
  RETURN
  END SUBROUTINE SETPOINT
