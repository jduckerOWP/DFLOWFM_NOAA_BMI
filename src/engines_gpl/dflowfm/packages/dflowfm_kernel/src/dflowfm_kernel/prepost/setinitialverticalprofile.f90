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

 subroutine setinitialverticalprofile(yy,ny,filename) ! polyfil
 use m_flowgeom
 use m_flow
 use m_polygon
 implicit none
 integer                   :: ny
 double precision          :: xx(kmxx)
 double precision          :: yy(ny)
 character(*),  intent(in) :: filename              ! file name for polygonfile

 integer :: minp0, n, k, kb, kt, ktx, nlayb,nrlay

 call oldfil(minp0, filename)
 call savepol()
 call reapol(minp0, 0)

 do n=1,ndxi
    call getkbotktop(n,kb,kt)
    do k = kb, kt
       xx(k-kb+1) = 0.5d0*( zws(k) + zws(k-1) )
    enddo
    ktx = kt-kb + 1
    if (layertype == 2 .and. keepzlayeringatbed .ne. 1 .and. jabaroczlaybed == 1) then 
       call getzlayerindices(n,nlayb,nrlay)
       xx(1) = 0.5d0*(zslay(nlayb-1,1) + zslay(nlayb,1) )
       if (kt > kb .and. keepzlayeringatbed == 2) then ! only 2
          xx(2) = 0.5d0*(zslay(nlayb+1,1) + zslay(nlayb,1) )
       endif 
    endif
    call lineinterp(xx, yy(kb:), ktx, xpl, ypl, npl)
 enddo

 call restorepol()

 end subroutine setinitialverticalprofile

 ! 2 subroutines in 1 file, yes we can ! 

 subroutine keepzlayering()
 use m_flowgeom
 use m_flow
 implicit none
 
 integer :: n, k, kb, kt, nlayb,nrlay, Ltn

 do n=1,ndxi
    call getkbotktop(n,kb,kt)
    call getzlayerindices(n,nlayb,nrlay)
    Ltn = laydefnr(n)
    zws(kb)   = zslay(nlayb,Ltn)
    if (nlayb == 1) then
       zws(kb-1) = 2*zslay(nlayb,Ltn) - zslay(nlayb+1,Ltn)
    else
       zws(kb-1) = zslay(nlayb-1,Ltn)
    endif
    if (keepzlayeringatbed == 2) then
       zws(kb) = zslay(nlayb,Ltn)
    endif
 enddo
 end subroutine keepzlayering
