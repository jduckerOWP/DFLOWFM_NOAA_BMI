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

      SUBROUTINE D1ARROWS(X,Y,Z,U,V,W,PSI0,VFAC)
      use gridoperations
      implicit none
      double precision :: psi0
      double precision :: vfac
      double precision :: X,Y,Z,U,V,W
      DOUBLE PRECISION XD,YD,ZD,XP,YP,ZP, &
                       UD,VD,WD,UR,VR,WR
      XD = X
      YD = Y
      ZD = Z
      UD = U
      VD = V
      WD = W
      CALL DRIETWEE(XD,YD,ZD,XP,YP,ZP)
      CALL DRIETWEE(UD,VD,WD,UR,VR,WR)
      CALL ARROWS(XP,YP,UR,VR,PSI0,VFAC)
      RETURN
      END SUBROUTINE D1ARROWS
