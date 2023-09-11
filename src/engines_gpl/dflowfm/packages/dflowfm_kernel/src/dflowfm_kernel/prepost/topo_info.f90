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

!> link-based mesh-topology information
double precision function topo_info(L)
   use m_netw
   use m_landboundary
   use m_missing

   implicit none

   integer :: L   !< link number

   integer                  :: k1, k2, kL, kR
   integer                  :: icellL, icellR
   integer                  :: k, n

   integer                  :: jalandbound     ! take landboundary into account (1) or not (0)

   logical                  :: Lproceed

   integer, external        :: nmk_opt         ! optimal nmk for the for nodes involved

!  default
   topo_info = DMISS

!  check if administration is in order
   if ( L.gt.ubound(lnn,1) ) goto 1234

!  check if the landboundary can be taken into account (not necessarily the up-to-date)
   if ( ubound(lanseg_map,1).ge.numk ) then
      jalandbound = 1
   else
      jalandbound = 0
   end if

   call comp_ntopo(L, jalandbound, k1, k2, kL, kR, icellL, icellR, n)

   topo_info = -dble(n)

   if ( topo_info.le.0d0 ) topo_info=DMISS

   return

1234 continue  ! error handling
   return

end function topo_info
