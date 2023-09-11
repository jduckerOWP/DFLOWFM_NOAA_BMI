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

      SUBROUTINE DISPNODE (MP)
      use m_devices
      use m_netw, only : zk
      implicit none
      integer :: mp
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'NODE NOT FOUND        '
         CALL KTEXT(TEX,IWS-22,4,15)
      ELSE
         TEX = 'NODE NR:              '
         WRITE(TEX (10:),'(I10)') MP
         CALL KTEXT(TEX,IWS-22,4,15)

!         TEX = 'ZK Lev :           (m)'
!         WRITE(TEX (10:18),'(F9.3)') zk(mp)
!         CALL KTEXT(TEX,IWS-22,5,15)
      ENDIF

      RETURN
      END
