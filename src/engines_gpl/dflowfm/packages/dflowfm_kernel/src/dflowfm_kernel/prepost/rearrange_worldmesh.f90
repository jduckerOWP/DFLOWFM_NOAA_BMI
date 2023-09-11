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

!> rearrange netnodes for spherical, periodic coordinates
!>    net nodes at the left are preferred
subroutine rearrange_worldmesh(xboundmin, xboundmax)
   use m_sferic
   use network_data
   implicit none

   double precision, intent(in) :: xboundmin, xboundmax  !< mesh bounding box x-coordinates

   integer                      :: k

   if ( jsferic.eq.1 .and. xboundmax-xboundmin.gt.180d0) then
      do k=1,numk
         if ( xk(k)-360d0.ge.xboundmin ) then
            xk(k) = xk(k)-360d0
         end if

         if ( xk(k).lt.xboundmin ) then
            xk(k) = xk(k)+360d0
         end if
      end do
   end if

   return
end subroutine rearrange_worldmesh
