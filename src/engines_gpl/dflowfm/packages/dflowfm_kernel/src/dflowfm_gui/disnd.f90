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

 SUBROUTINE DISND(NN, netorflow)   ! print net or flow node values
 use m_devices
 use m_flowgeom
 use network_data, only: rnod, netcell, xk, yk, nump
 use m_save_ugrid_state, only: nodeids
 
 implicit none
 integer, intent(in   ) :: nn !< Node number (either net or flow node)
 integer, intent(in   ) :: netorflow !< Whether to display net node info (0) or flow node info (1)

 CHARACTER TEX*23
 character(len=8)  :: nodetype
 character(len=23) :: idtext
 DOUBLE PRECISION, external :: ZNOD
 
 double precision :: x, y, val

 if (netorflow == 0) then
    nodetype = 'NET NODE'
 else
    nodetype = 'FLOWNODE'
 end if
 idtext = ''

 IF (NN .LE. 0) THEN
    TEX = 'NO '//nodetype//' FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
    CALL KTEXT('                      *',IWS-22,5,15)
    CALL KTEXT('                      *',IWS-22,6,15)
    CALL KTEXT('                      *',IWS-22,7,15)
    CALL KTEXT('                      *',IWS-22,8,15)
 ELSE
    if (netorflow == 0) then
       x = xk(NN)
       y = yk(NN)
       val = dble(rnod(NN))
       idtext = '                (no id)'
       if (allocated(nodeids)) then
          if (NN <= size(nodeids)) then
             idtext = 'ID ='
             write(idtext(6:), '(A18)') trim(nodeids(NN))
          end if
       end if
    else
       x = xz(NN)
       y = yz(NN)
       val = znod(NN)
       if (NN <= nump) then
          idtext = '2D node         (no id)'
       else if (allocated(nodeids)) then
          idtext = 'ID ='
          write(idtext(6:), '(A18)') trim(nodeids(netcell(NN)%nod(1)))
       else
          idtext = '1D node         (no id)'
       end if
    end if
    TEX = nodetype//' NR:         '
    WRITE(TEX (14:),'(I10)') NN
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') val
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'X  =                  '
    WRITE(TEX(6:), '(E18.11)') x
    CALL KTEXT(TEX,IWS-22,6,15)
    TEX = 'Y  =                  '
    WRITE(TEX(6:), '(E18.11)') y
    CALL KTEXT(TEX,IWS-22,7,15)
    WRITE(TEX(1:), '(A23)') trim(idtext)
    CALL KTEXT(TEX,IWS-22,8,15)
 ENDIF

 RETURN
 END SUBROUTINE DISND
