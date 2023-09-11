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

   !> fill initial salinity and temperature with nudge variables
   subroutine set_saltem_nudge()
      use m_flowgeom
      use m_flow ! , only: sa1, tem1, kmxn, layertype, keepzlayeringatbed, jabaroczlaybed, zws, zslay, numtopsig
      use m_transport
      use m_nudge
      use m_missing
      implicit none

      integer :: k, kk, KB, KT



      do kk=1,Ndx
         call getkbotktop(kk,kb,kt)
         do k=kb,kt
            if ( ITEMP.gt.0 .and. nudge_tem(k).ne.DMISS ) then
               tem1(k) = nudge_tem(k)
            end if

            if ( ISALT.gt.0 .and. nudge_sal(k).ne.DMISS ) then
               sa1(k) = nudge_sal(k)
           end if
         end do

         do k = kt+1, kb + kmxn(kk) - 1
            if ( ITEMP.gt.0) tem1(k) = tem1(kt)
            if ( ISALT.gt.0) sa1 (k) = sa1 (kt)
         enddo

      end do

 
   end subroutine set_saltem_nudge
