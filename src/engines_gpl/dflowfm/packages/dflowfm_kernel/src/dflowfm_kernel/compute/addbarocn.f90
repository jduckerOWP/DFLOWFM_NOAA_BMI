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

 subroutine addbarocn(n) ! rho at cell centers
 use m_flowgeom
 use m_flow

 implicit none
 integer, intent(in) :: n

 integer             :: k,kb,kt
 double precision    :: pu, pd, gr, dzz
 double precision    :: fuu , fud, fdu, fdd, dzu, dzd, roup, rodo, rvk

 call getkbotktop(n,kb,kt)
 ! if (kt < kb) return
 if (zws(kt) - zws(kb-1) < epshu) then
     grn(kb:kt)  = 0d0
     rvdn(kb:kt) = 1d-10
     return
 endif
 
 grn(kt)  = 0d0
 rvdn(kt) = 0d0
 pd       = 0d0
 do k = kt, kb, -1
    dzz = zws(k)   - zws(k-1)
    if (kb == kt) then
       roup = rho(k)- rhomean
       rodo = rho(k)- rhomean
    else if (k > kb .and. k < kt) then 
       dzu  = zws(k+1) - zws(k)
       dzd  = zws(k-1) - zws(k-2)
       fuu  = dzu/(dzu+dzz) ; fud = 1d0-fuu
       fdu  = dzz/(dzd+dzz) ; fdd = 1d0-fdu
       roup = fuu*rho(k+1)  + fud*rho(k)   - rhomean
       rodo = fdu*rho(k)    + fdd*rho(k-1) - rhomean
    else if (k == kb) then 
       dzu  = zws(k+1) - zws(k)
       fuu  = dzu/(dzu+dzz) ; fud = 1d0-fuu
       roup = fuu*rho(k+1)  + fud*rho(k)   - rhomean
       rodo = 2d0*(rho(k)-rhomean) - roup
    else if (k == kt) then 
       dzd  = zws(k-1) - zws(k-2)
       fdu  = dzz/(dzd+dzz) ; fdd = 1d0-fdu
       rodo = fdu*rho(k)    + fdd*rho(k-1) - rhomean
       roup = 2d0*(rho(k)-rhomean) - rodo
    endif
    rvk     = 0.5d0*( roup + rodo )*dzz 
    pu      = pd
    pd      = pu     + rvk 
    rvdn(k) = pd
    gr      = pu*dzz + 0.5d0*( (2D0*roup + rodo)/3d0 )*dzz*dzz     ! your left  wall
    grn(k)  = gr

 enddo

 end subroutine addbarocn

 subroutine addbarocnrho_w(n) ! rho at interfaces (w points) 
 use m_flowgeom
 use m_flow
 use m_transport, only : NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents
 use m_physcoef , only : rhomean

 implicit none
 integer, intent(in) :: n

 integer             :: k,kb,kt,i,maxit
 double precision    :: saw(0:kmxx), tmw(0:kmxx) ! rho at pressure point layer interfaces
 double precision    :: fzu , fzd, alf, pu, pd, gr, dzz, rvn, p0d, pdb, rhosk
 double precision, external :: densfm

 call getkbotktop(n,kb,kt)
 ! if (kt < kb) return
 if (zws(kt) - zws(kb-1) < epshu) then
     grn(kb:kt)  = 0d0
     rvdn(kb:kt) = 1d-10
     return
 endif

 if (kt > kb) then
    do k = kb, kt-1
       fzu         = (zws(k+1) - zws(k)) / (zws(k+1) - zws(k-1)) ; fzd = 1d0 - fzu
       saw(k-kb+1) = fzu*constituents(isalt,k+1) + fzd*constituents(isalt,k)
       tmw(k-kb+1) = fzu*constituents(itemp,k+1) + fzd*constituents(itemp,k)
    enddo
    saw(0)       = 2d0*constituents(isalt,kb) - saw(1)
    tmw(0)       = 2d0*constituents(itemp,kb) - tmw(1)
    saw(kt-kb+1) = 2d0*constituents(isalt,kt) - saw(kt-kb)    
    tmw(kt-kb+1) = 2d0*constituents(itemp,kt) - tmw(kt-kb)
 else
    saw(0)       = constituents(isalt,kb)
    tmw(0)       = constituents(itemp,kb)
    saw(1)       = saw(0)
    tmw(1)       = tmw(0)
 endif

 pd       = 0d0 ! baroclinic pressure/ag
 pdb      = 0d0 ! barotropic pressure/ag
 
 rhosww(kt) = densfm(saw(kt-kb+1),tmw(kt-kb+1),pd) - rhomean ! rho at interface

 do k = kt, kb, -1
    dzz  = zws(k) - zws(k-1)
    pu   =  pd
    if (idensform < 10) then 
       rhosww(k-1) = densfm(saw(k-kb),tmw(k-kb),0d0) - rhomean 
    else  
       pdb  =  pdb + rhomean*dzz
       do i = 1,maxitpresdens 
          pd  = pu  + 0.5d0*( rhosww(k)+rhosww(k-1) )*dzz                ! start with previous step estimate
          p0d = ag*(pd + pdb)                                            ! total pressure
          rhosww(k-1) = densfm(saw(k-kb),tmw(k-kb),p0d) - rhomean
       enddo
    endif
    rhosk   =           0.5d0*(     rhosww(k) + rhosww(k-1) ) 
    rho (k) =  rhomean  +           rhosk                                ! instead of setrho
    pd      =  pu       + dzz*      rhosk
    rvdn(k) =  pd
    grn (k) = (pu + 0.5d0*dzz*( 2d0*rhosww(k) + rhosww(k-1) )/3d0 )*dzz  ! wall contribution
 enddo

 end subroutine addbarocnrho_w

subroutine addbarocnorg(n)
 use m_flowgeom
 use m_flow

 implicit none
 integer, intent(in) :: n

 integer             :: k,kb,kt
 double precision    :: rhosw(0:kmxx) ! rho at pressure point layer interfaces
 double precision    :: fzu , fzd, alf, pu, pd, gr, dzz, rvn

 call getkbotktop(n,kb,kt)
 ! if (kt < kb) return
 if (zws(kt) - zws(kb-1) < epshu) then
     grn(kb:kt)  = 0d0
     rvdn(kb:kt) = 1d-10
     return
 endif

 if (kt > kb) then
    do k = kb, kt-1
       fzu           = (zws(k+1) - zws(k)) / (zws(k+1) - zws(k-1)) ; fzd = 1d0 - fzu
       rhosw(k-kb+1) = fzu*rho(k+1) + fzd*rho(k) - rhomean
    enddo
    rhosw(0)       = 2d0*(rho(kb) - rhomean) - rhosw(1)
    rhosw(kt-kb+1) = 2d0*(rho(kt) - rhomean) - rhosw(kt-kb)
 else
    rhosw(0)       = rho(kb) - rhomean
    rhosw(1)       = rhosw(0)
 endif

 grn(kt)  = 0d0
 rvdn(kt) = 0d0
 pd       = 0d0
 rvn      = 0d0
 do k = kt, kb, -1
    dzz     = zws(k) - zws(k-1)
    rvn     = rvn + 0.5d0*( rhosw(k-kb+1) + rhosw(k-kb) ) * dzz
    rvdn(k) = rvn
    alf = rhosw(k-kb) - rhosw(k-kb+1)
    pu  = pd
    pd  = pu + rhosw(k-kb+1)*dzz + 0.5d0*alf*dzz
    gr  = pu*dzz + 0.5d0*rhosw(k-kb+1)*dzz*dzz + alf*dzz*dzz/6d0             ! your left  wall
    grn(k) = gr
 enddo

 end subroutine addbarocnorg

