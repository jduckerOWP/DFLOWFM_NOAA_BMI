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
module m_mergenodes

implicit none

contains

SUBROUTINE MERGENODES(K1,K2,JA,check_connected_input)  ! KNOOP 1 WORDT OPGENOMEN IN KNOOP 2

use m_netw,         only: NMK, KN, NOD, KC, XK
use m_missing,      only: dxymis
use gridoperations, only: OTHERNODE

implicit none
integer,           intent(in   ) :: K1,K2                 ! netnode indices of the 2 nodes to be merged
integer,           intent(  out) :: JA                    ! Status integer, always 1
logical, optional, intent(in   ) :: check_connected_input ! Do expensive connected check, default = .true.

!locals
integer :: L2, L12, NN, K22, NM22, L2A, K22A, N1
integer :: l
integer :: n, nm, nm1, nm2
INTEGER :: NODLIN(200)
logical :: check_connected

if(present(check_connected_input)) then 
  check_connected = check_connected_input
else
  check_connected = .true.
endif

NM1 = NMK(K1)
NM2 = NMK(K2)

! Deze check is duur dus doe hem niet als je weet dat de nodes unconnected zijn.
if (check_connected) then 

    CALL GIVELINKNUM(K1,K2,L12)
    if (L12 .ne. 0) then
      kn(1,L12) = 0 ; kn(2,L12) = 0; kn(3,L12) = 0
    endif
  
    DO NM = 1,NM1 ! CHECK OF JE NIET VIA EEN ANDER PAD OOK BIJ K2 UIT KAN KOMEN. ZO JA, VERWIJDER LINK
      L2 = NOD(K1)%LIN(NM)
      CALL OTHERNODE(K1,L2,K22)
      IF (K22 .NE. 0 .AND. K22 .NE. K2) THEN
        NM22 = NMK(K22)
        DO NN  = 1,NM22
          L2A  = NOD(K22)%LIN(NN)
          CALL OTHERNODE(K22,L2A,K22A)
          IF (K22A == K2) THEN
            kn(1,L2A) = 0 ; kn(2,L2A) = 0; kn(3,L2A) = 0
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  endif


N = 0
DO NM = 1,NM2
  L  = NOD(K2)%LIN(NM)
  IF (KN(1,L) .NE. 0) THEN
    N = N + 1 ; NODLIN(N) = L
  ENDIF
ENDDO

DO NM = 1,NM1
  L = NOD(K1)%LIN(NM)
  IF (KN(1,L) .NE. 0) THEN
    N = N + 1 ; NODLIN(N) = L
    IF (KN(1,L) == K1) KN(1,L) = K2
    IF (KN(2,L) == K1) KN(2,L) = K2
  ENDIF
ENDDO
NMK(K2) = N


if ( allocated(nod(k2)%lin) ) DEALLOCATE(NOD(K2)%LIN)
if ( allocated(nod(k1)%lin) ) deallocate(NOD(K1)%lin)

ALLOCATE ( NOD(K2)%LIN(NMK(K2)) )
NOD(K2)%LIN(1:NMK(K2)) = NODLIN(1:NMK(K2))

KC (K1)     = 0
NMK(K1)     = 0
XK (K1)     = dxymis
ja = 1

RETURN
END SUBROUTINE MERGENODES

end module m_mergenodes