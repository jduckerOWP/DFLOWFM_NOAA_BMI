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

   subroutine settaubxu_nowave()
   use m_flowgeom
   use m_flow
   use m_physcoef
   implicit none

   integer            :: L , Lb, Lt
   double precision   :: cz, cwall, rz, umod2

   taubxu = 0d0
   
   do L = 1, lnx
      call getLbotLtop(L,Lb,Lt)
      if (Lt<Lb) cycle
      if (hu(L)>epshu) then
         if (frcu(L)>0d0) then       ! input, or result from trachytopes
            call getcz(hu(L), frcu(L), ifrcutp(L), cz, L)
            z0urou(L) = max(1d-200,hu(L)*exp(-1d0 - vonkar*cz/sag))  ! getczz0
            rz        = max(hu(Lb),epshu)/ee/z0urou(L)               ! cz/sag, jaustarint=1, compatible with getustbcfuhi
            cz        = log(rz)/vonkar
            cwall     = 1d0/(cz**2)
            umod2     = u1(LB)*u1(LB) + v(Lb)*v(Lb)
            taubxu(L) = rhomean*cwall*umod2                     ! Note that taubxu for 3D without waves is based on bottom layer velocity, whereas 
            ! comment HK: dit blijft een merkwaardig verhaal, zowel in 2D als in 3D  
            ! Verder wordt deze code altijd doorlopen, maar is dat zelden nodig.  
         endif   
     endif
   enddo

   end subroutine
