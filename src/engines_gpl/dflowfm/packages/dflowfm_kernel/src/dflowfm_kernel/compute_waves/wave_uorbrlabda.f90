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

   subroutine wave_uorbrlabda()
   use m_waves, only: uorb, wlenwav, uorbwav, twav, hwav, hwavcom, gammax, rlabda, jauorb, jauorbfromswan
   use m_flow, only: s1
   use m_flowgeom, only: ndx, bl
   use m_physcoef, only: ag
   use m_sferic, only: pi
   use m_flowtimes, only: time1

   implicit none

   integer                            :: k
   integer                            :: uorbwav_from_SWAN=0
   integer                            :: wlenwav_from_SWAN=0

   double precision                   :: hss, per, omeg, k0, k0h, rk, uorb1

   do k = 1,ndx
      hss = max(1d-2, s1(k)-bl(k))
      per = max(1d-2, twav(k))                   ! wave period

      omeg       = 2d0*pi/per
      k0         = omeg*omeg/ag
      k0h        = k0*hss
      if (k0h>pi) then                ! if deep water
         rk = k0
      elseif (k0h<5d-3) then          ! if very shallow water
         rk = omeg/sqrt(ag*hss)
      else
         call getwavenr(hss,per,rk)
      endif
      if (wlenwav_from_SWAN.eq.1) then
         rlabda(k) = wlenwav(k)
      else
         rlabda(k) = 2d0*pi/rk
      endif
      if (rk*hss<80d0) then            ! if not very deep water
         if (jauorbfromswan.eq.1) then
            Uorb(k)    = uorbwav(k)
         else
            uorb(k)      = 0.5d0*hwav(k)*omeg/sinh(rk*hss)
            if (jauorb==0) then                  ! old d3d convention
               uorb(k) = uorb(k)*sqrt(pi)/2d0    ! only on hrms derived value, not on SWAN read uorb
            end if
         endif
      else
         Uorb(k) = 0d0
      endif
   enddo

   end subroutine wave_uorbrlabda
