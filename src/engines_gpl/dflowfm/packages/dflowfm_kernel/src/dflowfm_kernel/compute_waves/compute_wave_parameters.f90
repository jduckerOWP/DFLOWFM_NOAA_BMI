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

   ! compute uorb, rlabda for input in other subroutines
   subroutine compute_wave_parameters()
      use m_xbeach_data
      use m_waves
      use m_flow
      use m_flowgeom
      use m_sferic
      use m_flowtimes
      use mathconsts, only: sqrt2_hp

      use unstruc_display

      implicit none

      integer                                     :: k1, k2, k, L
      double precision                            :: hh, hw, tw, cs, sn, uorbi, rkw, ustt, uwi

      ! Fetch models
      !
      if (jawave < 3 .and. .not. flowWithoutWaves) then ! Every timestep, not only at getfetch updates, as waterdepth changes
         ! get ustokes, vstokes for 2D, else in update_verticalprofiles getustwav
         hwav = min(hwav,gammax*max(s1-bl,0d0))
         if (kmx==0) then
            do L=1,lnx
               k1=ln(1,L); k2=ln(2,L)
               hh = hu(L); 
               if (hh<=epshu) then
                  ustokes(L) = 0d0; vstokes(L) = 0d0
               else
                  hw=0.5d0*(hwav(k1)+hwav(k2));tw=.5d0*(twav(k1)+twav(k2))
                  uwi = sqrt(wx(L)*wx(L) + wy(L)*wy(L) )
                  if (uwi > 0d0) then
                     cs = wx(L)/uwi
                     sn = wy(L)/uwi
                  else
                     cs = 1d0 ; sn = 0d0
                  endif
                  call tauwavehk(hw, tw, hh, uorbi, rkw, ustt)
                  ustokes(L) = ustt*(csu(L)*cs + snu(L)*sn)
                  vstokes(L) = ustt*(-snu(L)*cs + csu(L)*sn)
               endif
            enddo
         endif
         ! get uorb, rlabda
         call wave_uorbrlabda()
      endif

      ! SWAN
      if ((jawave==3 .or. jawave==6) .and. .not. flowWithoutWaves) then
         if (jawave == 6) then
           ! HSIG is read from SWAN NetCDF file. Convert to HRMS
           hwav = hwavcom / sqrt2_hp
         else
           hwav = hwavcom
         endif
         hwav = min(hwav, gammax*hs)
         call wave_uorbrlabda()
         if(kmx == 0) then
            call wave_comp_stokes_velocities()
         end if
      end if
      !
      if ((jawave==3 .or. jawave==6) .and. flowWithoutWaves) then
        ! Exceptional situation: use wave info not in FLOW, only in WAQ
        ! Only compute uorb
        ! Works both for 2D and 3D
        if (jawave == 6) then
          ! HSIG is read from SWAN NetCDF file. Convert to HRMS
          hwav = hwavcom / sqrt2_hp
        else
          hwav = hwavcom
        endif
        hwav = min(hwav, gammax*hs)
        call wave_uorbrlabda()                       ! hwav gets depth-limited here
      end if
      !
      ! Surfbeat model
      if (jawave.eq.4) then
         ! pro memore
      end if
      !
      ! Uniform wave field
      if (jawave==5 .and. .not. flowWithoutWaves) then
         do k=1,ndx
            hwav(k) = min(hwavuni, gammax*(s1(k)-bl(k)))
         enddo
         if (kmx==0) then
            do L=1,lnx
               k1=ln(1,L); k2=ln(2,L)
               hh = hu(L); 
               if (hh<=epshu) then
                  ustokes(L) = 0d0; vstokes(L) = 0d0
               else
                  hw=0.5d0*(hwav(k1)+hwav(k2));tw=.5d0*(twav(k1)+twav(k2))
                  cs = 0.5*(cos(phiwav(k1)*dg2rd)+cos(phiwav(k2)*dg2rd))
                  sn = 0.5*(sin(phiwav(k1)*dg2rd)+sin(phiwav(k2)*dg2rd))
                  call tauwavehk(hw, tw, hh, uorbi, rkw, ustt)
                  ustokes(L) = ustt*(csu(L)*cs + snu(L)*sn)
                  vstokes(L) = ustt*(-snu(L)*cs + csu(L)*sn)
               endif
            enddo
            call wave_uorbrlabda()
         endif
      endif

      ! shortcut to switch off stokes influence
      if (jawavestokes==0) then
         ustokes = 0d0; vstokes = 0d0
      endif

1234 continue
     return
   end subroutine compute_wave_parameters
