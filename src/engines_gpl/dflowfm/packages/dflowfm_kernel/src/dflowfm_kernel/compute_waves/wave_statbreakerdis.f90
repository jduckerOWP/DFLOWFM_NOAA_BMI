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

subroutine wave_statbreakerdis(h, hrms, tp, k, D)
      !
      ! Baldock
      !
      use m_physcoef, only: ag, rhomean
      use m_waves, only: gammax

      implicit none

      double precision, intent(in)   :: h
      double precision, intent(in)   :: hrms
      double precision, intent(in)   :: tp
      double precision, intent(in)   :: k
      double precision, intent(out)  :: D

      double precision               :: alpha, Hb

      alpha = 1d0    ! can be slope dependent, see work of Bertin et al

      Hb=0.88d0/k*tanh(gammax*k*h/0.88d0)
      D=0.25d0*alpha*rhomean*ag/tp*exp(-(Hb/hrms)**2)*(Hb**3+hrms**3)/gammax/h

   end subroutine wave_statbreakerdis
