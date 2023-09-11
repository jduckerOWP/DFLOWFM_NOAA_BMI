module m_structures

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
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

use properties
use unstruc_channel_flow, only: network
use MessageHandling
implicit none

type(tree_data), pointer, public :: strs_ptr !< A property list with all input structure specifications of the current model. Not the actual structure set.
integer :: jaoldstr !< tmp backwards comp: we cannot mix structures from EXT and from structure-input files. Use one or the other.

! COMMON indices for all structure types:
 integer, parameter :: NUMVALS_COMMON = 11 !< Number of common variables for all structure types except for pump and gate (new)
 integer, parameter :: IVAL_WIDTH   = 1    !< Index of total width
 integer, parameter :: IVAL_WIDTHWET= 2    !< Index of total width of wet links
 integer, parameter :: IVAL_WIDTHUP = 3    !< Index of wet flow link width on upstream side
 integer, parameter :: IVAL_WIDTHDN = 4    !< Index of wet flow link width on downstream side
 integer, parameter :: IVAL_WIDTHUPDN = 5  !< Index of width of wet flow links that have both upstream and downstream nodes wet
 integer, parameter :: IVAL_DIS = 6        !< Index of discharge
 integer, parameter :: IVAL_S1UP = 7       !< Index of water level at upstream
 integer, parameter :: IVAL_S1DN = 8       !< Index of water level at downstream
 integer, parameter :: IVAL_HEAD = 9       !< Index of head of the structure
 integer, parameter :: IVAL_AREA = 10      !< Index of flow area of the structure
 integer, parameter :: IVAL_VEL = 11       !< Index of flow velocity

 !! For general structure, weir, orifice, because they share some common output variables
 ! Followings are extra variables for general structure, weir and orifice:
 integer, parameter :: IVAL_S1ONCREST  = NUMVALS_COMMON + 1  !< Index of water level on crest
 integer, parameter :: IVAL_CRESTL     = NUMVALS_COMMON + 2  !< Index of crest level
 integer, parameter :: IVAL_CRESTW     = NUMVALS_COMMON + 3  !< Index of crest width
 integer, parameter :: IVAL_STATE      = NUMVALS_COMMON + 4  !< Index of state (0: closed, 1: free weir, 2: drowned/submerged weir)
 integer, parameter :: IVAL_FORCEDIF   = NUMVALS_COMMON + 5  !< Index of force difference per unit width
 ! Followings are extra variables for general structure and orifice:
 integer, parameter :: IVAL_OPENW      = NUMVALS_COMMON + 6  !< Index of gate opening width
 integer, parameter :: IVAL_EDGEL      = NUMVALS_COMMON + 7  !< Index of gate lower edge level
 integer, parameter :: IVAL_OPENH      = NUMVALS_COMMON + 8  !< Index of gate opening height
 ! Followings are extra variables only for general structure:
 integer, parameter :: IVAL_UPPL       = NUMVALS_COMMON + 9  !< Index of gate upper edge level
 integer, parameter :: IVAL_DIS_OPEN   = NUMVALS_COMMON + 10 !< Index of discharge through gate opening
 integer, parameter :: IVAL_DIS_OVER   = NUMVALS_COMMON + 11 !< Index of discharge over gate
 integer, parameter :: IVAL_DIS_UNDER  = NUMVALS_COMMON + 12 !< Index of discharge under gate
 integer, parameter :: IVAL_AREA_OPEN  = NUMVALS_COMMON + 13 !< Index of flow area through gate opening
 integer, parameter :: IVAL_AREA_OVER  = NUMVALS_COMMON + 14 !< Index of flow area over gate
 integer, parameter :: IVAL_AREA_UNDER = NUMVALS_COMMON + 15 !< Index of flow area under gate
 integer, parameter :: IVAL_VEL_OPEN   = NUMVALS_COMMON + 16 !< Index of velocity through gate opening
 integer, parameter :: IVAL_VEL_OVER   = NUMVALS_COMMON + 17 !< Index of velocity over gate
 integer, parameter :: IVAL_VEL_UNDER  = NUMVALS_COMMON + 18 !< Index of velocity under gate
 integer, parameter :: IVAL_COUNT      = NUMVALS_COMMON + 19 !< Index of counters of partitions for parallel

 integer, parameter :: NUMEXTVALS_GENSTRU = 19 ! Number of extra variables for general structure, including last one as a counter
 integer, parameter :: NUMEXTVALS_WEIRGEN = 6  ! Number of extra variables for weir, including last one as a counter
 integer, parameter :: NUMEXTVALS_ORIFGEN = 9  ! Number of extra variables for orifice, including last one as a counter
 integer, parameter :: NUMVALS_GENSTRU = NUMVALS_COMMON + NUMEXTVALS_GENSTRU  !< Total number of variables for general structure (new exe file)
 integer, parameter :: NUMVALS_WEIRGEN = NUMVALS_COMMON + NUMEXTVALS_WEIRGEN  !< Total number of variables for weir
 integer, parameter :: NUMVALS_ORIFGEN = NUMVALS_COMMON + NUMEXTVALS_ORIFGEN  !< Total number of variables for orifice

 double precision, dimension(:,:), allocatable :: valgenstru   !< Array for general structure, (1:NUMVALS_GENSTRU,:), the first index include 1:NUMVALS_COMMON (see definitation at top),
                                                               !< and extra varaibles have indices: IVAL_S1ONCREST, IVAL_CRESTL, IVAL_CRESTW, IVAL_STATE,
                                                               !<                                   IVAL_FORCEDIF, IVAL_OPENW, IVAL_EDGEL, IVAL_OPENH,
                                                               !<                                   IVAL_UPPL, IVAL_DIS_OPEN, IVAL_DIS_OVER, IVAL_DIS_UNDER,
                                                               !<                                   IVAL_AREA_OPEN, IVAL_AREA_OVER, IVAL_AREA_UNDER, IVAL_VEL_OPEN, IVAL_VEL_OVER,
                                                               !<                                   IVAL_VEL_UNDER, IVAL_COUNT.
 double precision, dimension(:,:), allocatable :: valweirgen   !< Array for weir, (1:NUMVALS_WEIRGEN,:), the first index include 1:NUMVALS_COMMON (see definitation at top),
                                                               !< and extra varaibles have indices: IVAL_S1ONCREST, IVAL_CRESTL, IVAL_CRESTW, IVAL_STATE,
                                                               !<                                   IVAL_FORCEDIF, NUMVALS_WEIRGEN is the counter
 double precision, dimension(:,:), allocatable :: valorifgen   !< Array for orifice, (1:NUMVALS_ORIFGEN,:), the first index include 1:NUMVALS_COMMON (see definitation at top),
                                                               !< and extra varaibles have indices: IVAL_S1ONCREST, IVAL_CRESTL, IVAL_CRESTW, IVAL_STATE,
                                                               !<                                   IVAL_FORCEDIF, IVAL_OPENW, IVAL_EDGEL, IVAL_OPENH, the last one NUMVALS_ORIFGEN is the counter
 ! Bridge, extra variables:
 integer, parameter :: IVAL_BLUP         = NUMVALS_COMMON+1                   !< Index of bed level up
 integer, parameter :: IVAL_BLDN         = NUMVALS_COMMON+2                   !< Index of bed level down
 integer, parameter :: IVAL_BLACTUAL     = NUMVALS_COMMON+3                   !< Index of actual bed level (crest)
 integer, parameter :: NUMEXTVALS_BRIDGE = 3                                  !< Number of extra variables for bridge
 integer, parameter :: NUMVALS_BRIDGE    = NUMVALS_COMMON + NUMEXTVALS_BRIDGE !< Total number of variables for bridge
 double precision, dimension(:,:), allocatable :: valbridge                   !< Array for bridge(1:NUMVALS_BRIDGE,:), the first dimension of this array contains
                                                                              !< NUMVALS_COMMON common variables (see definitation at top) and NUMEXTVALS_BRIDGE extra variables here.

 ! Dambreak, extra variables:
 integer, parameter :: IVAL_DB_CRESTH   = NUMVALS_COMMON+1                     !< Index of crest level for dambreak
 integer, parameter :: IVAL_DB_CRESTW   = NUMVALS_COMMON+2                     !< Index of crest width for dambreak
 integer, parameter :: IVAL_DB_JUMP     = NUMVALS_COMMON+3                     !< Index of water level jump for dambreak
 integer, parameter :: IVAL_DB_TIMEDIV  = NUMVALS_COMMON+4                     !< Index of breach width time derivative for dambreak
 integer, parameter :: IVAL_DB_DISCUM   = NUMVALS_COMMON+5                     !< Index of cumulative discharge for dambreak
 integer, parameter :: NUMEXTVALS_DAMBREAK = 5                                 !< Number of extra variables for dambreak
 integer, parameter :: NUMVALS_DAMBREAK = NUMVALS_COMMON + NUMEXTVALS_DAMBREAK !< Total number of variables for dambreak
 double precision, dimension(:,:), allocatable, target :: valdambreak          !< Array for dambreak, (1:NUMVALS_DAMBREAK,:), the first dimension of this array contains
                                                                               !< NUMVALS_COMMON common variables (see definitation at top) and NUMEXTVALS_DAMBREAK extra variables here.
 ! Culvert, extra variables:
 integer, parameter :: IVAL_CL_CRESTL  = NUMVALS_COMMON + 1                  !< Index of culvert crest level
 integer, parameter :: IVAL_CL_STATE   = NUMVALS_COMMON + 2                  !< Index of culvert state (0: closed, 1: free weir, 2: drowned/submerged weir)
 integer, parameter :: IVAL_CL_EDGEL   = NUMVALS_COMMON + 3                  !< Index of culvert gate lower edge level
 integer, parameter :: IVAL_CL_OPENH   = NUMVALS_COMMON + 4                  !< Index of culvert gate opening height
 integer, parameter :: NUMEXTVALS_CULVERT = 4                                !< Number of extra variables for culvertt
 integer, parameter :: NUMVALS_CULVERT = NUMVALS_COMMON + NUMEXTVALS_CULVERT !< Total number of variables for culvert
 double precision, dimension(:,:), allocatable :: valculvert                 !< Array for culvert(1:NUMVALS_CULVERT,:), the first dimension of this array contains
                                                                             !< NUMVALS_COMMON common variables (see definitation at top) and above extra variables.

 ! Univeral weir, extra variables:
 integer, parameter :: IVAL_UW_CRESTL = NUMVALS_COMMON + 1                   !< Index of universal weir crest level
 integer, parameter :: NUMEXTVALS_UNIWEIR = 1                                !< Number of extra variables for universal weir
 integer, parameter :: NUMVALS_UNIWEIR = NUMVALS_COMMON + NUMEXTVALS_UNIWEIR !< Total number of variables for universal weir
 double precision, dimension(:,:), allocatable :: valuniweir                 !< Array for universal weir(1:NUMVALS_UNIWEIR,:), the first dimension of this array contains
                                                                             !< NUMVALS_COMMON common variables (see definitation at top) and above extra variables.
 
 ! gate (new),  extra variables:
 integer, parameter :: NUMVALS_COMMON_GATE = 8                                    !< Number of common variables shared by gate
 integer, parameter :: IVAL_GATE_FLOWH    = NUMVALS_COMMON_GATE + 1               !< Upstream average water level
 integer, parameter :: IVAL_GATE_COUNT    = NUMVALS_COMMON_GATE + 2               !< Counter
 integer, parameter :: IVAL_GATE_OPENW    = NUMVALS_COMMON_GATE + 3               !< Gate opening width
 integer, parameter :: IVAL_GATE_EDGEL    = NUMVALS_COMMON_GATE + 4               !< Gate lower edge level
 integer, parameter :: IVAL_GATE_SILLH    = NUMVALS_COMMON_GATE + 5               !< Gate crest level (via general structure)
 integer, parameter :: IVAL_GATE_WIDTHWET = NUMVALS_COMMON_GATE + 6               !< Width of wet links at upstream (used for IVAL_GATE_FLOWH)
 integer, parameter :: NUMEXTVALS_GATE    = 6                                     !< Number of extra variables for gate
 integer, parameter :: NUMVALS_GATEGEN    = NUMVALS_COMMON_GATE + NUMEXTVALS_GATE !< Total number of variables for gate
 double precision, dimension(:,:), allocatable :: valgategen                   !< Array for (new) gate (1:NUMVALS_GATEGEN,:), the first dimension of this array contains
                                                                               !< NUMVALS_COMMON_GATE common variables (see definitation at top) and NUMEXTVALS_GATE extra variables.
 
 ! Compound structure
 integer, parameter :: NUMVALS_CMPSTRU = NUMVALS_COMMON       !< Total number of variables for compound structure, no extra variables.
 double precision, dimension(:,:), allocatable :: valcmpstru  !< Array for compound structure(1:NUMVALS_CMPSTRU,:)

 ! Pump shares the first 9 indices in common indices, extra variables are as follows:
 integer, parameter :: NUMVALS_COMMON_PUMP = 9                              !< Number of common variables shared by pump
 integer, parameter :: IVAL_PP_CAP    = NUMVALS_COMMON_PUMP + 1               !< Pump capacity
 integer, parameter :: IVAL_PP_STAG   = NUMVALS_COMMON_PUMP + 2               !< Actual pump stage
 integer, parameter :: IVAL_PP_HEAD   = NUMVALS_COMMON_PUMP + 3               !< Pump head
 integer, parameter :: IVAL_PP_RED    = NUMVALS_COMMON_PUMP + 4               !< Pump reduction factor
 integer, parameter :: IVAL_PP_S1DEL  = NUMVALS_COMMON_PUMP + 5               !< Pump water level at delivery side
 integer, parameter :: IVAL_PP_S1SUC  = NUMVALS_COMMON_PUMP + 6               !< Pump water level at suction side
 integer, parameter :: IVAL_PP_DISDIR = NUMVALS_COMMON_PUMP + 7               !< Pump discharge w.r.t. pumping orientation (same sign as capacity)
 integer, parameter :: NUMEXTVALS_PUMP = 7                                  !< Number of extra variables for pump
 integer, parameter :: NUMVALS_PUMP = NUMVALS_COMMON_PUMP + NUMEXTVALS_PUMP !< Total number of variables for pump
 double precision, dimension(:,:), allocatable :: valpump                   !< Array for pump, (1:NUMVALS_PUMP,:), the first dimension of this array contains
                                                                            !< NUMVALS_COMMON_PUMP common variables (see definitation at top) and NUMEXTVALS_PUMP extra variables.

 ! Long culvert
 integer, parameter :: IVAL_LC_VALVE       = NUMVALS_COMMON + 1                   !< long culvert valve relative opening
 integer, parameter :: NUMEXTVALS_LONGCULV = 1                                    !< Number of extra variables for long culvert
 integer, parameter :: NUMVALS_LONGCULVERT = NUMVALS_COMMON + NUMEXTVALS_LONGCULV !< Number of variables for long culvert
 double precision, dimension(:,:), allocatable :: vallongculvert                  !< Array for long culvert, (1:NUMVALS_LONGCULVERT,:), the first dimension of this array contains
                                                                                  !< NUMVALS_COMMON common variables (see definitation at top)and above extra variables.
 ! For old stype structures
 integer                           :: NUMVALS_GATE = 5        !< Number of variables for gate
 integer                           :: NUMVALS_CDAM = 4        !< Number of variables for controble dam
 integer                           :: NUMVALS_CGEN = 4        !< Number of variables for general structure (old ext file)
 double precision, dimension(:,:), allocatable :: valgate     !< Array for gate;      (1,:) discharge through gate
 double precision, dimension(:,:), allocatable :: valcdam     !< Array for cdam;      (1,:) discharge through controlable dam
                                                              !<                      (2,:) Upstream average water levels
                                                              !<                      (3,:) downstream average water level
                                                              !<                      (4,0) width of dam
 double precision, dimension(:,:), allocatable :: valcgen     !< Array for general structure (old ext), (1,:) discharge

 ! His output keywords
 integer                           :: jahiscgen               !< Write structure parameters to his file, 0: n0, 1: yes
 integer                           :: jahispump               !< Write pump      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisgate               !< Write gate      parameters to his file, 0: n0, 1: yes
 integer                           :: jahiscdam               !< Write dam       parameters to his file, 0: n0, 1: yes
 integer                           :: jahisweir               !< Write weir      parameters to his file, 0: n0, 1: yes
 integer                           :: jahisdambreak           !< Write dambreak  parameters to his file, 0: n0, 1: yes
 integer                           :: jahisorif               !< Write orifice   parameters to his file, 0: no, 1: yes
 integer                           :: jahisbridge             !< Write bridge    parameters to his file, 0: no, 1: yes
 integer                           :: jahisculv               !< Write culvert   parameters to his file, 0: no, 1: yes
 integer                           :: jahisuniweir            !< Write univeral weir parameters to his file, 0: no, 1: yes
 integer                           :: jahiscmpstru            !< Write compound structure parameters to his file, 0: no, 1: yes
 integer                           :: jahislongculv           !< Write long culverts parameters to his file, 0: no, 1:yes
 
 !! Geometry variables
 ! weir
 integer                               :: nNodesWeir, nNodesWeirInput           !< [-] Total number of nodes for all weirs
 integer,          allocatable, target :: nodeCountWeir(:)     !< [-] Count of nodes per weir.
 integer,          allocatable, target :: nodeCountWeirInput(:)!< [-] Input Count of nodes per weir.
 double precision, allocatable, target :: geomXWeir(:)         !< [m] x coordinates of weirs.
 double precision, allocatable, target :: geomYWeir(:)         !< [m] y coordinates of weirs.
 double precision, allocatable, target :: geomXWeirInput(:)         !< [m] x coordinates of weirs.
 double precision, allocatable, target :: geomYWeirInput(:)         !< [m] y coordinates of weirs.
 ! general structure
 integer                               :: nNodesGenstru        !< [-] Total number of nodes for all general structures
 integer,          allocatable, target :: nodeCountGenstru(:)  !< [-] Count of nodes per general structure.
 double precision, allocatable, target :: geomXGenstru(:)      !< [m] x coordinates of general structures.
 double precision, allocatable, target :: geomYGenstru(:)      !< [m] y coordinates of general structures.
 ! orifice
 integer                               :: nNodesOrif           !< [-] Total number of nodes for all orifices
 integer,          allocatable, target :: nodeCountOrif(:)     !< [-] Count of nodes per orifice.
 double precision, allocatable, target :: geomXOrif(:)         !< [m] x coordinates of orifices.
 double precision, allocatable, target :: geomYOrif(:)         !< [m] y coordinates of orifices.
 ! universal weir
 integer                               :: nNodesUniweir        !< [-] Total number of nodes for all universal weirs
 integer,          allocatable, target :: nodeCountUniweir(:)  !< [-] Count of nodes per universal weir.
 double precision, allocatable, target :: geomXUniweir(:)      !< [m] x coordinates of universal weirs.
 double precision, allocatable, target :: geomYUniweir(:)      !< [m] y coordinates of universal weirs.
 ! culvert
 integer                               :: nNodesCulv           !< [-] Total number of nodes for all culverts
 integer,          allocatable, target :: nodeCountCulv(:)     !< [-] Count of nodes per culvert.
 double precision, allocatable, target :: geomXCulv(:)         !< [m] x coordinates of culverts.
 double precision, allocatable, target :: geomYCulv(:)         !< [m] y coordinates of culverts.
 ! pump
 integer                               :: nNodesPump           !< [-] Total number of nodes for all pumps
 integer,          allocatable, target :: nodeCountPump(:)     !< [-] Count of nodes per pump.
 double precision, allocatable, target :: geomXPump(:)         !< [m] x coordinates of pumps.
 double precision, allocatable, target :: geomYPump(:)         !< [m] y coordinates of pumps.
 ! bridge
 integer                               :: nNodesBridge         !< [-] Total number of nodes for all bridges
 integer,          allocatable, target :: nodeCountBridge(:)   !< [-] Count of nodes per bridge.
 double precision, allocatable, target :: geomXBridge(:)       !< [m] x coordinates of bridges.
 double precision, allocatable, target :: geomYBridge(:)       !< [m] y coordinates of bridges.
 ! long culvert
 integer                               :: nNodesLongCulv       !< [-] Total number of nodes for all long culverts
 integer,          allocatable, target :: nodeCountLongCulv(:) !< [-] Count of nodes per long culverts.
 double precision, allocatable, target :: geomXLongCulv(:)     !< [m] x coordinates of long culverts.
 double precision, allocatable, target :: geomYLongCulv(:)     !< [m] y coordinates of long culverts.
 
 integer, parameter :: IOPENDIR_FROMLEFT  = -1 !< Gate door opens/closes from left side.
 integer, parameter :: IOPENDIR_FROMRIGHT =  1 !< Gate door opens/closes from right side.
 integer, parameter :: IOPENDIR_SYMMETRIC =  0 !< Gate door opens/closes symmetrically (from center).

 type tgate                                          !< Gate structure type, before it gets evaluated as a general structure.
    !double precision :: sill_level       !< Not used: stored in zcgen(1,igen)
    !double precision :: lower_edge_level !< Not used: stored in zcgen(2,igen)
    !double precision :: opening_width    !< Not used: stored in zcgen(3,igen)
    double precision :: door_height       !< Height of the door, used for 'double-barrier' overflow. Time-INDEPENDENT.
    double precision :: sill_width        !< Width of the sill, may be larger than the opening width, such that in open part we have weir flow and in closed part we have gate flow. Time-INDEPENDENT.
    integer          :: opening_direction !< Direction from which the gate opens/closes, IOPENDIR_FROMLEFT|FROMRIGHT|SYMMETRIC.
 end type tgate

 ! TIDAL TURBINES: Insert allocatable of type structure_turbines here

 type(tgate), allocatable :: gates(:)
   contains


   !> Allocates and initializes all "valstruct"(:,:) arrays.
   !! Used for history output and/or restart file output for hydraulic structures.
   subroutine init_structure_hisvalues()
      use m_flowexternalforcings , only: npumpsg, ncgensg, ngatesg, ncdamsg, ngategen, ngenstru, nweirgen, ndambreaksg
      !use m_structures, only: NUMVALS_PUMP, NUMVALS_GATE, NUMVALS_CDAM, NUMVALS_CGEN, &
      !                        NUMVALS_GATEGEN, NUMVALS_WEIRGEN, NUMVALS_GENSTRU
      use m_alloc
      use m_flowtimes, only: ti_rst
      use m_longculverts, only: nlongculverts
      implicit none

      if((ti_rst > 0 .or. jahispump > 0) .and. npumpsg > 0) then
         if( allocated( valpump ) ) deallocate( valpump )
         allocate( valpump(NUMVALS_PUMP,npumpsg) ) ; valpump = 0d0
      endif
      if(ti_rst > 0 .or. jahiscgen > 0 ) then
         if( ncgensg > 0 ) then
            if( allocated( valcgen ) ) deallocate( valcgen )
            allocate( valcgen(NUMVALS_CGEN,ncgensg) ) ; valcgen = 0d0
         endif
         
         if (ngenstru == 0) then ! If it is new general structure, then it is stored in the network type
            ngenstru = network%sts%numGeneralStructures
         end if
         if( ngenstru > 0 ) then
            if( allocated( valgenstru ) ) deallocate( valgenstru )
            allocate( valgenstru(NUMVALS_GENSTRU,ngenstru) ) ; valgenstru  = 0d0
         endif
      endif
      if( jahisgate > 0 ) then
         if( ngatesg > 0 ) then
            if( allocated( valgate ) ) deallocate( valgate )
            allocate( valgate(NUMVALS_CGEN,ngatesg) ) ; valgate = 0d0
         endif
         if( ngategen > 0 ) then
            if( allocated( valgategen ) ) deallocate( valgategen )
            allocate( valgategen(NUMVALS_GATEGEN,ngategen) ) ; valgategen = 0d0
         endif
      endif
      if( jahiscdam > 0 .and. ncdamsg > 0) then
         if( allocated( valcdam) ) deallocate( valcdam )
         allocate( valcdam(NUMVALS_CDAM,ncdamsg) ) ; valcdam = 0d0
      endif
      if (nweirgen == 0) then ! If it is new 1D weir, the weir is stored in the network type
         nweirgen = network%sts%numWeirs
      end if
      
      if((ti_rst > 0 .or. jahisweir > 0) .and. nweirgen > 0) then
         if( allocated( valweirgen) ) deallocate( valweirgen )
         allocate( valweirgen(NUMVALS_WEIRGEN,nweirgen) ) ; valweirgen = 0d0
      endif
      if( jahisdambreak > 0 .and. ndambreaksg > 0) then
         if( allocated( valdambreak ) ) deallocate( valdambreak )
         allocate( valdambreak(NUMVALS_DAMBREAK,ndambreaksg) ) ; valdambreak = 0d0
      endif
      if((ti_rst > 0 .or. jahisorif > 0) .and. network%sts%numOrifices > 0) then
         if( allocated( valorifgen) ) deallocate( valorifgen )
         allocate( valorifgen(NUMVALS_ORIFGEN,network%sts%numOrifices) ) ; valorifgen = 0d0
      endif
      if( jahisbridge > 0 .and. network%sts%numBridges > 0) then
         if( allocated( valbridge) ) deallocate( valbridge )
         allocate( valbridge(NUMVALS_BRIDGE,network%sts%numBridges) ) ; valbridge = 0d0
      endif
      if( (ti_rst > 0 .or. jahisculv > 0) .and. network%sts%numCulverts > 0) then
         if( allocated( valculvert) ) deallocate( valculvert )
         allocate( valculvert(NUMVALS_CULVERT,network%sts%numCulverts) ) ; valculvert = 0d0
      endif
      if( jahisuniweir > 0 .and. network%sts%numUniWeirs > 0) then
         if( allocated( valuniweir ) ) deallocate( valuniweir )
         allocate( valuniweir(NUMVALS_UNIWEIR,network%sts%numUniWeirs) ) ; valuniweir = 0d0
      endif
      if( jahiscmpstru > 0 .and. network%cmps%count > 0) then
         if( allocated( valcmpstru ) ) deallocate( valcmpstru )
         allocate( valcmpstru(NUMVALS_CMPSTRU,network%cmps%count) ) ; valcmpstru = 0d0
      endif
      if( jahislongculv > 0 .and. nlongculverts > 0) then
         if( allocated( vallongculvert) ) deallocate( vallongculvert )
         allocate( vallongculvert(NUMVALS_LONGCULVERT,nlongculverts) ) ; vallongculvert = 0d0
      endif

! TIDAL TURBINES: Insert init_turbines here

   end subroutine init_structure_hisvalues

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_structures() instead.
subroutine default_structures()

call tree_destroy(strs_ptr)

call reset_structures()

! TIDAL TURBINES: Insert calls to deallocate_turbines and init_turbines here

   ! default settings for structure output to history file
   jahiscgen = 1
   jahispump = 1
   jahisgate = 1
   jahiscdam = 1
   jahisweir = 1
   jahisorif = 1
   jahisculv = 1
   jahisbridge   = 1
   jahisdambreak = 1
   jahisuniweir = 1
   jahiscmpstru = 1
   jahislongculv = 1

end subroutine default_structures


!> Resets only structures variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_structures() instead.
subroutine reset_structures()
   if (allocated(gates)) deallocate(gates)
end subroutine reset_structures

!> Fills the valstruct array for one given structure on a given link L.
!! All values are filled, both the generic ones, as well as the type-specific ones.
!! Note: old-style structures may call this with istrtypein = ST_UNSET.
subroutine fill_valstruct_perlink(valstruct, L, dir, istrtypein, istru, L0)
   use m_missing, only: dmiss
   use m_flow, only: q1, s1, au, hu, hs
   use m_flowgeom, only: wu, ln, teta, bl
   use m_1d_structures, only: get_discharge_under_compound_struc
   use m_General_Structure
   use m_GlobalParameters
   use m_longculverts
   use m_flowparameters, only: epshs, epshu
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valweirgen(:)):
                                                                !< (IVAL_WIDTH) total width, no matter dry or not
                                                                !< (IVAL_WIDTHWET) total width of wet links
                                                                !< (IVAL_WIDTHUP) total width of wet flowlinks on upstream side
                                                                !< (IVAL_WIDTHDN) total width of wet flowlinks on downstream side
                                                                !< (IVAL_WIDTHUPDN) total width of flowlinks when both upstream and downstream are wet
                                                                !< (IVAL_DIS) structure discharge
                                                                !< (IVAL_S1UP) structure water level up
                                                                !< (IVAL_S1DN) structure water level down
                                                                !< (IVAL_HEAD) structure head
                                                                !< (IVAL_AREA) flow area
                                                                !< (IVAL_VEL) velocity
                                                                !< (IVAL_S1ONCREST) water level on crest, or valve relative opening if type is long culvert
                                                                !< (IVAL_CRESTL) crest level
                                                                !< (IVAL_CRESTW) crest width
                                                                !< (IVAL_STATE) state
                                                                !< (IVAL_FORCEDIF) force difference per unit width
                                                                !< (IVAL_OPENW) gate opening width
                                                                !< (IVAL_EDGEL) gate lower edge level
                                                                !< (IVAL_OPENH) gate opening height
                                                                !< (IVAL_UPPL) gate upper edge level
                                                                !< (IVAL_DIS_OPEN) discharge through gate opening
                                                                !< (IVAL_DIS_OVER) discharge over gate
                                                                !< (IVAL_DIS_UNDER) discharge under gate
                                                                !< (IVAL_AREA_OPEN) flow area in gate opening
                                                                !< (IVAL_AREA_OVER) flow area over gate
                                                                !< (IVAL_AREA_UNDER) flow area under gate
                                                                !< (IVAL_VEL_OPEN) velocity through gate opening
                                                                !< (IVAL_VEL_OVER) velocity over gate
                                                                !< (IVAL_VEL_UNDER) velocity under gate
   integer,                        intent(in   ) :: L           !< Flow link number.
   double precision,               intent(in   ) :: dir         !< Direction of flow link w.r.t. structure orientation (1.0 for same direction, -1.0 for opposite).
   integer,                        intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                                                !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                        intent(in   ) :: istru       !< Structure index in network%sts set or in longculverts.
   integer,                        intent(in   ) :: L0          !< Local flow link index in the struct%linknumbers array.

   integer :: ku, kd, k1, k2
   type(t_GeneralStructure), pointer :: genstr
   double precision :: qcmp
   logical :: in_compound

   in_compound = .false.

   if (istrtypein /= ST_LONGCULVERT) then
      if (dir > 0) then
         ku = ln(1,L)
         kd = ln(2,L)
      else
         ku = ln(2,L)
         kd = ln(1,L)
      end if
   else 
      ku = longculverts(istru)%flownode_up
      kd = longculverts(istru)%flownode_dn
   end if

   ! 1. Generic values that apply to all structure types
   valstruct(IVAL_WIDTH) = valstruct(IVAL_WIDTH) + wu(L)

   if (istru > 0 .and. (istrtypein /= ST_LONGCULVERT) ) then ! When it is not old weir and not old general structure and not a compound structure
      in_compound = (network%sts%struct(istru)%compound > 0)
   end if

   if (hs(ku) > epshs) then
      valstruct(IVAL_WIDTHUP) = valstruct(IVAL_WIDTHUP) + wu(L)
      valstruct(IVAL_S1UP)    = valstruct(IVAL_S1UP) + s1(ku)*wu(L)
   end if
   if (hs(kd) > epshs) then
      valstruct(IVAL_WIDTHDN) = valstruct(IVAL_WIDTHDN) + wu(L)
      valstruct(IVAL_S1DN)    = valstruct(IVAL_S1DN) + s1(kd)*wu(L)
   end if
   if (hs(ku) > epshs .and. hs(kd) > epshs) then
      valstruct(IVAL_WIDTHUPDN) = valstruct(IVAL_WIDTHUPDN) + wu(L)
      valstruct(IVAL_HEAD)      = valstruct(IVAL_HEAD) + (s1(ku) - s1(kd))*wu(L)
   end if


   if (hu(L) > epshu) then ! when link L is wet
      valstruct(IVAL_WIDTHWET) = valstruct(IVAL_WIDTHWET) + wu(L)

      if (in_compound) then ! for a structure that belongs to a compound structure
         k1 = ln(1,L)
         k2 = ln(2,L)
         qcmp = get_discharge_under_compound_struc(network%sts%struct(istru), L0, s1(k1), s1(k2), teta(L))
         valstruct(IVAL_DIS) = valstruct(IVAL_DIS) + qcmp*dir
      else
         valstruct(IVAL_DIS) = valstruct(IVAL_DIS) + q1(L)*dir
      end if

      if (istrtypein /= ST_PUMP) then ! Compute flow area for structures except for pump
         if (istru > 0) then ! When it is not old weir and not old general structure and not a compound structure
            if (in_compound) then ! for a structure that belongs to a compound structure
               valstruct(IVAL_AREA) = valstruct(IVAL_AREA) + network%sts%struct(istru)%au(L0)
            else
               valstruct(IVAL_AREA) = valstruct(IVAL_AREA) + au(L)
            end if
         else
            valstruct(IVAL_AREA) = valstruct(IVAL_AREA) + au(L)
         end if
      end if

      ! 2. More specific valus that apply to certain structure types only

      ! 2a. General structure-based structures with a crest.
      if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
         valstruct(IVAL_S1ONCREST) = valstruct(IVAL_S1ONCREST) + network%sts%struct(istru)%generalst%sOnCrest(L0)*wu(L)
         valstruct(IVAL_FORCEDIF)  = valstruct(IVAL_FORCEDIF) + get_force_difference(istru, L)*wu(L)
      end if

      ! 2b. General structure-based structures with a (gate) door.
      if (any(istrtypein == (/ ST_GENERAL_ST /))) then ! TODO: ST_GATE
         k1 = ln(1,L)
         k2 = ln(2,L)
         genstr => network%sts%struct(istru)%generalst

         valstruct(IVAL_DIS_OPEN)  = valstruct(IVAL_DIS_OPEN)  + get_discharge_through_gate_opening(genstr, L0, s1(k1), s1(k2))*dir
         valstruct(IVAL_DIS_OVER)  = valstruct(IVAL_DIS_OVER)  + get_discharge_over_gate(genstr, L0, s1(k1), s1(k2))*dir
         valstruct(IVAL_DIS_UNDER) = valstruct(IVAL_DIS_UNDER) + get_discharge_under_gate(genstr, L0, s1(k1), s1(k2))*dir

         valstruct(IVAL_AREA_OPEN)  = valstruct(IVAL_AREA_OPEN)  + genstr%au(3,L0) ! flow area through gate opening
         valstruct(IVAL_AREA_OVER)  = valstruct(IVAL_AREA_OVER)  + genstr%au(2,L0) ! flow area over gate
         valstruct(IVAL_AREA_UNDER) = valstruct(IVAL_AREA_UNDER) + genstr%au(1,L0) ! flow area under gate
      end if

      ! 2c. More specific value that applies to long culvert
      if (istrtypein == ST_LONGCULVERT) then
         valstruct(IVAL_LC_VALVE) = longculverts(istru)%valve_relative_opening
      end if
   end if ! hu(L) > epshu

   ! 2d. More specific values that apply to bridge
   if (istrtypein == ST_BRIDGE) then
      valstruct(IVAL_BLUP)     = valstruct(IVAL_BLUP) + bl(ku)*wu(L)
      valstruct(IVAL_BLDN)     = valstruct(IVAL_BLDN) + bl(kd)*wu(L)
      valstruct(IVAL_BLACTUAL) = valstruct(IVAL_BLACTUAL) + network%sts%struct(istru)%bridge%bedLevel_actual*wu(L)
   end if

end subroutine fill_valstruct_perlink


!> Averages the values on one structure across all links or all wet links,
!! where needed taking care of partition models.
!! Note 1: fill_valstructs_perlink must have been called in
!! a loop prior to calling this averaging routine.
!! Note 2: if in parallel computing, MPI reduction must be done before calling this subroutine.
subroutine average_valstruct(valstruct, istrtypein, istru, nlinks)
   use m_missing, only: dmiss
   use m_partitioninfo, only: jampi
   use m_1d_structures
   use m_General_Structure, only: t_GeneralStructure
   use m_GlobalParameters
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct   !< Output values on structure (e.g. valpump(:)):
                                                                !< (IVAL_WIDTH) total width
                                                                !< (IVAL_WIDTHWET) total width of wet links
                                                                !< (IVAL_WIDTHUP) total width of wet flowlinks on upstream side
                                                                !< (IVAL_WIDTHDN) total width of wet flowlinks on downstream side
                                                                !< (IVAL_WIDTHUPDN) total width of flowlinks when both upstream and downstream are wet
                                                                !< (IVAL_DIS) structure discharge
                                                                !< (IVAL_S1UP) structure water level up
                                                                !< (IVAL_S1DN) structure water level down
                                                                !< (IVAL_HEAD) structure head
                                                                !< (IVAL_AREA) flow area
                                                                !< (IVAL_VEL) velocity
                                                                !< (IVAL_S1ONCREST) water level on crest, or valve relative opening if type is long culvert
                                                                !< (IVAL_CRESTL) crest level
                                                                !< (IVAL_CRESTW) crest width
                                                                !< (IVAL_STATE) state
                                                                !< (IVAL_FORCEDIF) force difference per unit width
                                                                !< (IVAL_OPENW) gate opening width
                                                                !< (IVAL_EDGEL) gate lower edge level
                                                                !< (IVAL_OPENH) gate opening height
                                                                !< (IVAL_UPPL) gate upper edge level
                                                                !< (IVAL_DIS_OPEN) discharge through gate opening
                                                                !< (IVAL_DIS_OVER) discharge over gate
                                                                !< (IVAL_DIS_UNDER) discharge under gate
                                                                !< (IVAL_AREA_OPEN) flow area in gate opening
                                                                !< (IVAL_AREA_OVER) flow area over gate
                                                                !< (IVAL_AREA_UNDER) flow area under gate
                                                                !< (IVAL_VEL_OPEN) velocity through gate opening
                                                                !< (IVAL_VEL_OVER) velocity over gate
                                                                !< (IVAL_VEL_UNDER) velocity under gate
   integer,                        intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                                                !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                        intent(in   ) :: istru       !< Structure index in network%sts set or in longculverts
   integer,                        intent(in   ) :: nlinks      !< Number of flow links for this structure (on the current partition)

   type(t_structure), pointer :: pstru

   ! 1. Generic values that apply to all structure types
   ! 1a. average for waterlevel upstream, downstream and head
   if (valstruct(IVAL_WIDTHUP) > 0) then
      valstruct(IVAL_S1UP) = valstruct(IVAL_S1UP) / valstruct(IVAL_WIDTHUP)
   else 
      valstruct(IVAL_S1UP) = dmiss
   end if
   if (valstruct(IVAL_WIDTHDN) > 0) then
      valstruct(IVAL_S1DN) = valstruct(IVAL_S1DN) / valstruct(IVAL_WIDTHDN)
   else 
      valstruct(IVAL_S1DN) = dmiss
   end if
   if (valstruct(IVAL_WIDTHUPDN) > 0) then
      valstruct(IVAL_HEAD) = valstruct(IVAL_HEAD) / valstruct(IVAL_WIDTHUPDN)
   else 
      valstruct(IVAL_HEAD) = dmiss
   end if
   ! 1b. other generic variables
   if (valstruct(IVAL_WIDTH) == 0d0) then
      valstruct(IVAL_CRESTL)    = dmiss ! crest level
      valstruct(IVAL_CRESTW)    = dmiss ! crest width
   end if

   if (valstruct(IVAL_WIDTHWET) == 0d0 ) then ! zero width
      valstruct(IVAL_DIS) = dmiss  ! discharge
      if (istrtypein /= ST_PUMP) then
         valstruct(IVAL_AREA) = dmiss ! flow area
         valstruct(IVAL_VEL) = dmiss  ! velocity
      end if

      if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
         valstruct(IVAL_S1ONCREST) = dmiss ! water level on crest
         valstruct(IVAL_STATE)     = dmiss ! state
         valstruct(IVAL_FORCEDIF)  = dmiss ! force difference per unit width
      end if
   else

      if (istrtypein /= ST_PUMP) then
         if (valstruct(IVAL_AREA) > 0d0) then ! non-zero flow area
            valstruct(IVAL_VEL) = valstruct(IVAL_DIS) / valstruct(IVAL_AREA)  ! velocity
         else
            valstruct(IVAL_VEL) = 0d0
         end if
      end if

      if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
         pstru => network%sts%struct(istru)
         valstruct(IVAL_S1ONCREST) = valstruct(IVAL_S1ONCREST) / valstruct(IVAL_WIDTHWET)     ! water level on crest
         valstruct(IVAL_FORCEDIF)  = valstruct(IVAL_FORCEDIF) / valstruct(IVAL_WIDTHWET)      ! force difference per unit width
      end if
   endif

   ! 2. More specific valus that apply to certain structure types only
   ! General structure-based structures with a (gate) door.
   if (any(istrtypein == (/ ST_GENERAL_ST, ST_ORIFICE /))) then ! TODO: ST_GATE
      if (valstruct(IVAL_WIDTH) == 0d0) then ! zero width
         valstruct(IVAL_OPENW:) = dmiss
      end if
      if (valstruct(IVAL_WIDTHWET) == 0d0) then ! zero wet width, info. on gate is not changed to dmiss
         valstruct(IVAL_DIS_OPEN:) = dmiss
      else
         ! only for general structure
         if (istrtypein == ST_GENERAL_ST) then 
            if (valstruct(IVAL_AREA_OPEN) > 0) then ! flow area in gate opening
               valstruct(IVAL_VEL_OPEN) = valstruct(IVAL_DIS_OPEN) / valstruct(IVAL_AREA_OPEN) ! velocity through gate opening
            end if
            if (valstruct(IVAL_AREA_OVER) > 0) then ! flow area over gate
               valstruct(IVAL_VEL_OVER) = valstruct(IVAL_DIS_OVER) / valstruct(IVAL_AREA_OVER) ! velocity over gate
            end if
            if (valstruct(IVAL_AREA_UNDER) > 0) then ! flow area under gate
               valstruct(IVAL_VEL_UNDER) = valstruct(IVAL_DIS_UNDER) / valstruct(IVAL_AREA_UNDER) ! velocity under gate
            end if
         end if
      end if
   end if

   ! 3. More specific values that apply to bridge
   if (istrtypein == ST_BRIDGE) then
      if (valstruct(IVAL_WIDTH) == 0d0 ) then ! zero width
         valstruct(IVAL_BLUP)    = dmiss
         valstruct(IVAL_BLDN)    = dmiss
         valstruct(IVAL_BLACTUAL)= dmiss
      else
         valstruct(IVAL_BLUP)     = valstruct(IVAL_BLUP) / valstruct(IVAL_WIDTH)
         valstruct(IVAL_BLDN)     = valstruct(IVAL_BLDN) / valstruct(IVAL_WIDTH)
         valstruct(IVAL_BLACTUAL) = valstruct(IVAL_BLACTUAL)/ valstruct(IVAL_WIDTH)
      end if
   end if

   ! 4. More specific values that apply to culvert
   if (istrtypein == ST_CULVERT) then
      if (valstruct(IVAL_WIDTH) == 0d0) then
         valstruct(IVAL_CL_CRESTL:NUMVALS_CULVERT) = dmiss
      else if (valstruct(IVAL_WIDTHWET) == 0d0) then
         valstruct(IVAL_CL_STATE) = dmiss
      end if
   end if

end subroutine average_valstruct


!!> Gets force difference per unit width over structure (weir, gate, general structure) per link.
double precision function get_force_difference(istru, L)
   use m_missing
   use m_flowgeom, only: ln
   use m_flow, only: s1
   use m_1d_structures, only: get_crest_level
   use m_GlobalParameters
   implicit none   
   integer, intent(in   )   :: istru !< structure index
   integer, intent(in   )   :: L     !< current link L
   
   double precision  :: s1up   !< water level up
   double precision  :: s1dn   !< water level down
   double precision  :: crestl
   integer           :: k1, k2
   double precision  :: rholeft, rhoright
   
   crestl = get_crest_level(network%sts%struct(istru))
  
   k1 = ln(1,L)
   k2 = ln(2,L)
   s1up = max(s1(k1), s1(k2))
   s1dn = min(s1(k1), s1(k2))
   if (crestl > dmiss + 0.1d0) then
      rholeft  = 1000.0d0
      rhoright = 1000.0d0
      
      get_force_difference =  max((s1up - crestl), 0.0d0)**2 * rholeft  * gravity / 2.0d0 -  &
                            max((s1dn - crestl), 0.0d0)**2 * rhoright * gravity / 2.0d0
   else
      get_force_difference = dmiss
   end if

end function get_force_difference


!> Gets discharge through gate opening per link.
double precision function get_discharge_through_gate_opening(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information.
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1 
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction < 1d0-gatefrac_eps) then
      u1L = genstr%ru(3,L0) - genstr%fu(3,L0)*dsL
      get_discharge_through_gate_opening = genstr%au(3,L0) * u1L
   else
      get_discharge_through_gate_opening = 0d0
   end if

end function get_discharge_through_gate_opening

!> Gets discharge over gate opening per link.
double precision function get_discharge_over_gate(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction > gatefrac_eps) then
      u1L = genstr%ru(2,L0) - genstr%fu(2,L0)*dsL
      get_discharge_over_gate = genstr%au(2,L0) * u1L
   else
      get_discharge_over_gate = 0d0
   end if

end function get_discharge_over_gate

!> Gets discharge under gate per link.
double precision function get_discharge_under_gate(genstr, L0, s1m1, s1m2)
   use m_missing
   use m_General_Structure
   implicit none   
   type(t_GeneralStructure), pointer, intent(in   ) :: genstr !< Derived type containing general structure information
   integer,                           intent(in   ) :: L0     !< Local link index in genstr%..(:) link-based arrays.
   double precision,                  intent(in   ) :: s1m1   !< (geometrical) upstream water level.
   double precision,                  intent(in   ) :: s1m2   !< (geometrical) downstream water level.
   double precision  :: u1L, dsL, gatefraction
   
   dsL = s1m2 - s1m1
   gatefraction = genstr%gateclosedfractiononlink(L0)
   
   if (gatefraction > gatefrac_eps) then
      u1L = genstr%ru(1,L0) - genstr%fu(1,L0)*dsL
      get_discharge_under_gate = genstr%au(1,L0) * u1L
   else
      get_discharge_under_gate = 0d0
   end if

end function get_discharge_under_gate

!> Updates structure parameters for the output to restart file.
!! Only computes the needed values, and
!! only when they are not computed for history output.
!! Values are stored in the val*(:,:) arrays, shared with history output.
subroutine structure_parameters_rst()
   use m_1d_structures
   use m_flowexternalforcings
   implicit none
   integer :: n, istru
   type(t_structure), pointer    :: pstru

   do n = 1, network%sts%numCulverts
      istru = network%sts%culvertIndices(n)
      pstru => network%sts%struct(istru)
      valculvert(11,n) = get_opening_height(pstru)
   end do

   do n = 1, network%sts%numGeneralStructures
      istru = network%sts%generalStructureIndices(n)
      pstru => network%sts%struct(istru)
      valgenstru(9,n)  = get_crest_level(pstru)
      valgenstru(10,n) = get_width(pstru)
      valgenstru(14,n) = get_gle(pstru)
      valgenstru(13,n) = network%sts%struct(istru)%generalst%gateopeningwidth_actual
      ! fu, ru, au have been computed in each computational time step, so skip computing them again
   end do
   
   do n = 1, network%sts%numWeirs
      istru = network%sts%weirIndices(n)
      pstru => network%sts%struct(istru)
      valweirgen(9,n)  = get_crest_level(pstru)
      valweirgen(10,n) = get_width(pstru)
      ! fu, ru have been computed in each computational time step, so skip computing them again
   end do
   
   do n = 1, network%sts%numOrifices
      istru = network%sts%orificeIndices(n)
      pstru => network%sts%struct(istru)
      valorifgen(9,n)  = get_crest_level(pstru)
      valorifgen(10,n) = get_width(pstru)
      valorifgen(14,n) = get_gle(pstru)
      valorifgen(13,n) = network%sts%struct(istru)%generalst%gateopeningwidth_actual
      ! fu, ru have been computed in each computational time step, so skip computing them again
   end do

   do n = 1, network%sts%numPumps
      istru = network%sts%pumpIndices(n)
      pstru => network%sts%struct(istru)
      valpump(6,n) = GetPumpCapacity(pstru)
   end do

end subroutine structure_parameters_rst

!> Get the maximal number of links of all general structures/weir/orifice, when given the type and the total number of the structure
integer function get_max_numLinks(istrtypein, nstru)
   use m_1d_structures
   use m_GlobalParameters

   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: nstru       !< Total number of this structure

   integer :: i
   
   get_max_numLinks = 0
   do i = 1, nstru
      select case (istrtypein)
      case (ST_WEIR)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%weirIndices(i))%numlinks)
      case (ST_ORIFICE)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%orificeIndices(i))%numlinks)
      case (ST_GENERAL_ST)
         get_max_numLinks = max(get_max_numLinks, network%sts%struct(network%sts%generalStructureIndices(i))%numlinks)
      end select
   end do

end function get_max_numLinks

!!> Gets istru when given a structure type and structure index
integer function get_istru(istrtypein, i)
   use m_1d_structures
   use m_GlobalParameters
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: i           !< Structure index

   select case (istrtypein)
   case (ST_WEIR)
      get_istru = network%sts%weirIndices(i)
   case (ST_ORIFICE)
      get_istru = network%sts%orificeIndices(i)
   case (ST_GENERAL_ST)
      get_istru = network%sts%generalStructureIndices(i)
   case (ST_CULVERT)
      get_istru = network%sts%culvertIndices(i)
   case (ST_UNI_WEIR)
      get_istru = network%sts%uniweirIndices(i)
   case (ST_BRIDGE)
      get_istru = network%sts%bridgeIndices(i)
   case (ST_PUMP)
      get_istru = network%sts%pumpIndices(i)
   end select
end function get_istru

!> Gets number of geometry nodes for a single structure type and structure index.
!! Geometry nodes can be used in a (multi-) polyline representation of the placement
!! of a structure on flow links.
integer function get_number_of_geom_nodes(istrtypein, i)
   use m_1d_structures
   use m_longculverts
   use m_GlobalParameters, only: ST_LONGCULVERT
   use m_partitioninfo, only: my_rank, jampi, idomain, link_ghostdata
   use m_flowgeom, only: ln
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: i           !< Structure index for this structure type.

   integer :: istru, nLinks, nLinksTmp, jaghost, idmn_ghost, L, Lf, La, Ls
   type(t_structure), pointer    :: pstru

   if (istrtypein == ST_LONGCULVERT) then
      nLinks = longculverts(i)%numlinks - 2 ! exclude the two 1D2D links
      Ls = 1
   else
      istru = get_istru(istrtypein, i)

      pstru => network%sts%struct(istru)
      nLinks = pstru%numlinks
   end if

   ! In a parallel simulation, count the links that are valid (>0) and non-ghost.
   if (jampi > 0) then
      nLinksTmp = nLinks
      do L = 1, nLinksTmp
         if (istrtypein == ST_LONGCULVERT) then
            Lf = longculverts(i)%flowlinks(Ls+L)
         else
            Lf = pstru%linknumbers(L)
         end if
         La = abs(Lf)
         if (La > 0) then
            call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
            if ( jaghost.eq.1 ) then
               nLinks = nLinks - 1
            end if
         else
            nLinks = nLinks - 1
         end if
      enddo
   end if
   if (nLinks > 0) then
      ! "2D" representation: nLinks+1 polyline points.
      ! TODO: for multiple 1D links in a single structure, we could consider
      !       a multi-part polyline. That would mean: get_number_of_geom_nodes = 2*nLinks
      get_number_of_geom_nodes = nLinks + 1
   else 
      ! When no links: empty geometry.
      get_number_of_geom_nodes = 0
   end if

end function get_number_of_geom_nodes

!> Gets total number of geometry nodes for a given structure type and total number of the structures.
!! Geometry nodes can be used in a (multi-) polyline representation of the placement
!! of structures on flow links.
integer function get_total_number_of_geom_nodes(istrtypein, nstru)
   use m_1d_structures
   implicit none
   integer, intent(in   ) :: istrtypein  !< The type of the structure. May differ from the struct%type, for example:
                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer, intent(in   ) :: nstru       !< Total number of structures of this structure type

   integer :: i, istru, nNodes, nLinks
   type(t_structure), pointer    :: pstru

   get_total_number_of_geom_nodes = 0
   do i = 1, nstru
      nNodes = get_number_of_geom_nodes(istrtypein, i)
      get_total_number_of_geom_nodes = get_total_number_of_geom_nodes + nNodes
   end do

end function get_total_number_of_geom_nodes

!> Gets geometry coordinates of a structure.
!! Geometry coordinates can be used in a (multi-) polyline representation of the placement
!! of structures on flow links.
!! Currently one structure lying on multiple subdomains is only supported for long culverts.
!! TODO: Support other structures lying on multiple subdomains.
subroutine get_geom_coordinates_of_structure(istrtypein, i, nNodes, x, y, maskLocalStartEnd)
   use m_1d_structures
   use m_alloc
   use m_flowgeom, only: lncn
   use network_data, only: xk, yk
   use m_longculverts
   use m_GlobalParameters, only: ST_LONGCULVERT
   use m_partitioninfo, only: jampi, idomain, my_rank, link_ghostdata
   use m_flowgeom, only: ln
   implicit none
   integer,                       intent(in   ) :: istrtypein            !< The type of the structure. May differ from the struct%type, for example:
                                                                         !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
   integer,                       intent(in   ) :: i                     !< Structure index for this structure type.
   integer,                       intent(in   ) :: nNodes                !< Number of geometry nodes in this structure (as computed by get_number_of_geom_nodes()).
   double precision, allocatable, intent(  out) :: x(:)                  !< x-coordinates of the structure (will be reallocated when needed)
   double precision, allocatable, intent(  out) :: y(:)                  !< y-coordinates of the structure (will be reallocated when needed)
   integer,          allocatable, intent(  out) :: maskLocalStartEnd(:)  !< Mask array of local start and end nodes on current subdomain (now only used for long culverts)

   integer :: istru, nLinks, L, L0, Ls, k1, k2, k3, k4, k, nLinksTmp, jaghost, idmn_ghost, Lf, La
   double precision :: dtmp
   type(t_structure), pointer    :: pstru
   integer, allocatable :: links(:)

   if (istrtypein == ST_LONGCULVERT) then
      nLinks = longculverts(i)%numlinks - 2 ! exclude the two 1D2D links
      Ls = 1
   else
      istru = get_istru(istrtypein, i)
      pstru => network%sts%struct(istru)
      nLinks = pstru%numlinks
   end if

   ! In a parallel simulation, array links stores the link number of all valid (>0) and non-ghost links.
   if (jampi > 0) then
      call realloc(links, nLinks, KeepExisting=.false., fill = 0)
      nLinksTmp = nLinks
      nLinks    = 0
      do L = 1, nLinksTmp
         if (istrtypein == ST_LONGCULVERT) then
            Lf = longculverts(i)%flowlinks(Ls+L)
         else
            Lf = pstru%linknumbers(L)
         end if
         La = abs(Lf)
         if (La > 0) then ! All links in array links are valid
            call link_ghostdata(my_rank,idomain(ln(1,La)), idomain(ln(2,La)), jaghost, idmn_ghost)
            if ( jaghost == 0 ) then
               nLinks = nLinks + 1
               links(nLinks) = Lf
            end if
         end if
      enddo
   end if


   if (nNodes > 0) then
      call realloc(x, nNodes, keepExisting = .false.)
      call realloc(y, nNodes, keepExisting = .false.)

      if (jampi > 0) then
         L = abs(links(1))
      else
         if (istrtypein == ST_LONGCULVERT) then
            L = abs(longculverts(i)%flowlinks(Ls+1))
         else
            L = abs(pstru%linknumbers(1))
         end if
      end if

      k1 = lncn(1,L)
      k2 = lncn(2,L)

      x(1) = xk(k1)
      x(2) = xk(k2)
      y(1) = yk(k1)
      y(2) = yk(k2)

      if (jampi > 0) then
         if (istrtypein == ST_LONGCULVERT) then
            ! maskLocalStartEnd will be used for the situation that the structure lying on multiple subdomains.
            ! Determine the 1st local start/end node (it is only useful for parallel simulations).
            call realloc(maskLocalStartEnd, nNodes, keepExisting = .false., fill = 0)
            if (nLinks == 1) then
               ! If there is only one link, then the two nodes are the start/end nodes.
               maskLocalStartEnd(1) = 1
               maskLocalStartEnd(2) = 1
            else
               ! If there are more than one link, then (x(1),y(1)) is a start/end node.
               ! NOTE: here assumes that the 1st node of a 1D link of the long culvert is always the
               ! starting node, and the 2nd node is always the ending node.
               maskLocalStartEnd(1) = 1
            end if
         else
            ! For other structures, lying on multiple subdomains is not supported yet.
            ! When we support it, then we can also use the codes as above.
            call realloc(maskLocalStartEnd, 1, keepExisting = .false., fill = 0)
         end if
      end if

      k = 3
      do L0 = 2, nLinks
         if (jampi > 0) then
            L = abs(links(L0))
         else
            if (istrtypein == ST_LONGCULVERT) then
               L = abs(longculverts(i)%flowlinks(L0+Ls))
            else
               L = abs(pstru%linknumbers(L0))
            end if
         end if
         k3 = lncn(1,L)
         k4 = lncn(2,L)
         if (L0 == 2) then
            if (k1 == k3 .or. k1 == k4) then
               dtmp = x(2)
               x(2) = x(1)
               x(1) = dtmp
               dtmp = y(2)
               y(2) = y(1)
               y(1) = dtmp
            endif
         endif
         if (k1 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k1 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         else if (k2 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k2 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         endif
         k1 = k3
         k2 = k4

         if (jampi > 0 .and. L0 == nlinks .and. istrtypein == ST_LONGCULVERT) then ! The last node is a boundary node.
            maskLocalStartEnd(k) = 1
         end if
         k = k+1
      end do
   end if
end subroutine get_geom_coordinates_of_structure

!> Gets geometry coordinates of a structure, aligned along structure.
!! Geometry coordinates can be used in a polyline representation of the placement
!! of structures on flow links.
subroutine get_geom_coordinates_of_structure_old(i, nNodes, x, y)
   use m_alloc
   use m_flowexternalforcings, only: ncgensg, kcgen, L1cgensg, L2cgensg
   use m_flowgeom, only: lncn
   use network_data, only: xk, yk
   implicit none
   integer,                       intent(in   ) :: i           !< Structure index for this structure type.
   integer,                       intent(in   ) :: nNodes      !< Number of geometry nodes in this structure.
   double precision, allocatable, intent(  out) :: x(:)   !< x-coordinates of the structure (will be reallocated when needed)
   double precision, allocatable, intent(  out) :: y(:)   !< y-coordinates of the structure (will be reallocated when needed)

   integer :: L, L0, k1, k2, k3, k4, k
   double precision :: dtmp

   if (nNodes > 0) then
      call realloc(x, nNodes)
      call realloc(y, nNodes)

      L0 = L1cgensg(i)
      L = abs(kcgen(3,L0))
      k1 = lncn(1,L)
      k2 = lncn(2,L)
      x(1) = xk(k1)
      x(2) = xk(k2)
      y(1) = yk(k1)
      y(2) = yk(k2)
                      
      k = 3
      do L0 = L1cgensg(i)+1, L2cgensg(i)
         L = abs(kcgen(3,L0))
         k3 = lncn(1,L)
         k4 = lncn(2,L)
         if (L0 == 2) then
            if (k1 == k3 .or. k1 == k4) then
               dtmp = x(2)
               x(2) = x(1)
               x(1) = dtmp
               dtmp = y(2)
               y(2) = y(1)
               y(1) = dtmp
            endif
         endif
         if (k1 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k1 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         else if (k2 == k3) then
            x(k) = xk(k4)
            y(k) = yk(k4)
         else if (k2 == k4) then
            x(k) = xk(k3)
            y(k) = yk(k3)
         endif
         k1 = k3
         k2 = k4
         k = k+1
      end do
   end if
end subroutine get_geom_coordinates_of_structure_old

!> Fills in the geometry arrays of a structure type for history output
subroutine fill_geometry_arrays_structure(istrtypein, nstru, nNodesStru, nodeCountStru, geomXStru, geomYStru)
   use m_alloc
   use m_partitioninfo
   use m_GlobalParameters
   use m_flowparameters, only: eps6
   use precision_basics
   implicit none
   integer,                       intent(in   ) :: istrtypein       !< The type of the structure. May differ from the struct%type
   integer,                       intent(in   ) :: nstru            !< Number of this structure type
   integer,                       intent(  out) :: nNodesStru       !< Total number of nodes of this structure type
   integer,          allocatable, intent(  out) :: nodeCountStru(:) !< Node count of this structure type
   double precision, allocatable, intent(  out) :: geomXStru(:)     !< [m] x coordinate of nodes of this structure type
   double precision, allocatable, intent(  out) :: geomYStru(:)     !< [m] y coordinate of nodes of this structure type

   double precision, allocatable :: xGat(:), yGat(:)     ! Coordinates that are gatherd data from all subdomains
   integer,          allocatable :: nodeCountStruMPI(:)  ! Count of nodes per structure after mpi communication.
   double precision, allocatable :: geomXStruMPI(:)      ! [m] x coordinates of structures after mpi communication.
   double precision, allocatable :: geomYStruMPI(:)      ! [m] y coordinates of structures after mpi communication.
   integer,          allocatable :: nodeCountStruGat(:), nNodesStruGat(:), displs(:)
   double precision, allocatable :: geomX(:), geomY(:)
   integer                       :: i, j, j1, k, k1, ierror, is, ie, n, ii, nNodes, nNodesStruMPI, jaexist, ke, ks, kk, nLocalStartEnd, nLocalStartEndLast, npar
   double precision              :: xNew, yNew, xOld, yOld
   integer,          allocatable :: maskLocalStartEnd(:), maskLocalStartEndAll(:), maskLocalStartEndGat(:), indLocalStartEndMPI(:) ! Arrays for local start/end nodes, only used in parallel run
   integer,          allocatable :: jacoincide(:)

   ! Allocate and construct geometry variable arrays (on one subdomain)
   call realloc(nodeCountStru,   nstru, keepExisting = .false., fill = 0  )
   do i = 1, nstru
      nNodes = get_number_of_geom_nodes(istrtypein, i)
      nodeCountStru(i) = nNodes
   end do
   nNodesStru = sum(nodeCountStru)
   call realloc(geomXStru,       nNodesStru,   keepExisting = .false., fill = 0d0)
   call realloc(geomYStru,       nNodesStru,   keepExisting = .false., fill = 0d0)
   if (jampi > 0 .and. istrtypein == ST_LONGCULVERT) then
      ! In parallel runs, one structure might lie on multiple subdomains. To handle this situation,
      ! we will need to know which nodes are local start/end nodes of a structure on each subdomain, and the local start/end nodes will be handled separately.
      ! This will avoid having duplicated (local start/end) nodes in the arrays of coordinates of a structure among all subdomains.
       call realloc(maskLocalStartEndAll, nNodesStru, keepExisting = .false., fill = 0) ! If the node is a local start/end node then the value will be 1
   end if
   is = 0
   ie = 0
   do i = 1, nstru
      nNodes = nodeCountStru(i)
      if (nNodes > 0) then
         call get_geom_coordinates_of_structure(istrtypein, i, nNodes, geomX, geomY, maskLocalStartEnd)
         is = ie + 1
         ie = is + nNodes - 1
         geomXStru(is:ie) = geomX(1:nNodes)
         geomYStru(is:ie) = geomY(1:nNodes)
         if (jampi > 0 .and. istrtypein == ST_LONGCULVERT) then
            maskLocalStartEndAll(is:ie) = maskLocalStartEnd(1:nNodes)
         end if
      end if
   end do

   !! The codes below are similar to subroutine "fill_geometry_arrays_lateral".
   !! They work for 1D structures, but are supposed to work when more links are contained in a structure.
   ! For parallel simulation: since only process 0000 writes the history output, the related arrays
   ! are only made on 0000.
   if (jampi > 0) then
      call reduce_int_sum(nNodesStru, nNodesStruMPI) ! Get total number of nodes among all subdomains

      if (my_rank == 0) then
         ! Allocate arrays
         call realloc(nodeCountStruMPI, nstru,  keepExisting = .false., fill = 0  )
         call realloc(geomXStruMPI,     nNodesStruMPI, keepExisting = .false., fill = 0d0)
         call realloc(geomYStruMPI,     nNodesStruMPI, keepExisting = .false., fill = 0d0)

         ! Allocate arrays that gather information from all subdomains
         ! Data on all subdomains will be gathered in a contiguous way
         call realloc(nodeCountStruGat, nstru*ndomains, keepExisting = .false., fill = 0  )
         call realloc(xGat,             nNodesStruMPI,  keepExisting = .false., fill = 0d0)
         call realloc(yGat,             nNodesStruMPI,  keepExisting = .false., fill = 0d0)
         call realloc(displs,           ndomains,       keepExisting = .false., fill = 0  )
         call realloc(nNodesStruGat,    ndomains,       keepExisting = .false., fill = 0  )
         if (istrtypein == ST_LONGCULVERT) then
            call realloc(maskLocalStartEndGat, nNodesStruMPI,  keepExisting = .false., fill = 0  )
         end if
      else
         ! NOTE: dummy allocate to prevent crash in Debug-model on Intel MPI, even though receive buffers are officially not needed on non-root.
         allocate(nodeCountStruGat(0), xGat(0), yGat(0), displs(0), nNodesStruGat(0), maskLocalStartEndGat(0))
      end if

      ! Gather integer data, where the same number of data, i.e. nstru, are gathered from each subdomain to process 0000
      call gather_int_data_mpi_same(nstru, nodeCountStru, nstru*ndomains, nodeCountStruGat, nstru, 0, ierror)

      if (my_rank == 0) then
         ! To use mpi gather call, construct displs, and nNodesStruGat (used as receive count for mpi gather call)
         displs(1) = 0
         do i = 1, ndomains
            is = (i-1)*nstru+1 ! Starting index in nodeCountStruGat
            ie = is+nstru-1    ! Endding index in nodeCountStruGat
            nNodesStruGat(i) = sum(nodeCountStruGat(is:ie)) ! Total number of nodes on subdomain i
            if (i > 1) then
               displs(i) = displs(i-1) + nNodesStruGat(i-1)
            end if
         end do
      end if

      ! Gather double precision data, here, different number of data can be gatherd from different subdomains to process 0000
      call gatherv_double_data_mpi_dif(nNodesStru, geomXStru, nNodesStruMPI, xGat, ndomains, nNodesStruGat, displs, 0, ierror)
      call gatherv_double_data_mpi_dif(nNodesStru, geomYStru, nNodesStruMPI, yGat, ndomains, nNodesStruGat, displs, 0, ierror)
      if (istrtypein == ST_LONGCULVERT) then
         call gatherv_int_data_mpi_dif(nNodesStru, maskLocalStartEndAll, nNodesStruMPI, maskLocalStartEndGat, ndomains, nNodesStruGat, displs, 0, ierror)
      end if

      if (my_rank == 0) then
         ! Construct nodeCountStruMPI for history output
         do i = 1, nstru
            do n = 1, ndomains
               k = (n-1)*nstru+i
               nodeCountStruMPI(i) = nodeCountStruMPI(i) + nodeCountStruGat(k) ! Total number of nodes for structure i among all subdomains
            end do
         end do

         ! Construct geomXStruMPI and geomYStruMPI for history output
         ! Below seperate long culverts with other structures, because the we support a long culvert lying
         ! on multiple subdomains, but do not support other structures lying on multiple subdomains yet.
         ! TODO: enable this for other structures as well.
         if (istrtypein == ST_LONGCULVERT) then
            j = 0
            do i = 1, nstru                    ! for each structure
               nPar = 0                        ! Number of subdomains that contain this structure
               nLocalStartEnd = 0              ! Number of local start/end nodes for this structure
               nLocalStartEndLast = 0          ! Number of local start/end nodes for this structure in the previous subdomains
               call realloc(indLocalStartEndMPI, nodeCountStruMPI(i), keepExisting = .false., fill = 0)
               call realloc(jaCoincide,nodeCountStruMPI(i), keepExisting = .false., fill = 0)
               do n = 1, ndomains              ! on each sudomain
                  k = (n-1)*nstru+i            ! index in nodeCountStruGat
                  nNodes = nodeCountStruGat(k) ! structure i on sumdomain n has nNodes nodes
                  if (nNodes > 0) then
                     nPar = nPar + 1
                     ii = (n-1)*nstru
                     is = sum(nNodesStruGat(1:n-1)) + sum(nodeCountStruGat(ii+1:ii+i-1))! starting index in xGat
                     ks = 1
                     ke = nNodes
                     if (nPar > 1) then ! This structure lies on multiple subdomains.
                        ! Select and add the nodes of this structure on the current subdomain
                        do k1 = ks, ke
                           kk = is+k1
                           if (maskLocalStartEndGat(kk) == 1) then ! If it is a local start/end node, need to check if it already exists in
                                                                   ! the coordinate arrays, i.e. GeomXstruMPI and GeomYStruMPI
                              xNew = xGat(kk)
                              yNew = yGat(kk)
                              jaexist = 0
                              do j1 = 1, nLocalStartEndLast ! Loop over all local start/end nodes of the previous subdomains that have been added in the coordinate arrays
                                 if (jaCoincide(j1) == 0) then
                                    ! If the j1 start/end node is not coincide with any start/end node, then check if node (xNew,yNew) is coincide with it or not.
                                    ! If jaCoincide(j1) == 1, then no need to check this node because one start/end node can be
                                    ! coincide with another start/end node maximal ONCE.
                                    xOld = geomXStruMPI(indLocalStartEndMPI(j1))
                                    yOld = geomYStruMPI(indLocalStartEndMPI(j1))
                                    if (comparereal(xNew, xOld, eps6)==0 .and. comparereal(xNew, xOld, eps6)==0) then
                                       jaexist = 1
                                       jaCoincide(j1) = 1
                                       exit
                                    end if
                                 end if
                              end do
                              if (jaexist == 0) then ! If the new candidate node does not exist in the coordinate arrays, then add it
                                 j = j + 1
                                 geomXStruMPI(j) = xNew
                                 geomYStruMPI(j) = yNew
                                 nLocalStartEnd = nLocalStartEnd + 1         ! add one local start/end node
                                 indLocalStartEndMPI(nLocalStartEnd) = j     ! store its index in geomXStruMPI (and geomYStruMPI)
                              else
                                 nodeCountStruMPI(i) = nodeCountStruMPI(i) - 1 ! adjust the node counter
                                 nNodesStruMPI = nNodesStruMPI - 1
                              end if
                           else ! If it is not a start/end node, then add it directly
                              j = j + 1
                              geomXStruMPI(j) = xGat(kk)
                              geomYStruMPI(j) = yGat(kk)
                           end if
                        end do
                     else
                        do k1 = ks, ke
                           j = j + 1
                           kk = is + k1
                           geomXStruMPI(j) = xGat(kk)
                           geomYStruMPI(j) = yGat(kk)
                           if (maskLocalStartEndGat(kk) == 1) then
                              nLocalStartEnd = nLocalStartEnd + 1
                              indLocalStartEndMPI(nLocalStartEnd) = j ! store the index in geomXStruMPI for the local start/end nodes
                           end if
                        end do
                     end if
                     nLocalStartEndLast = nLocalStartEnd ! update nLocalStartEndLast when the current subdomain is finished.
                  end if
               end do
            end do
         else
            j = 1
            do i = 1, nstru    ! for each structure
               do n = 1, ndomains ! on each sudomain
                  k = (n-1)*nstru+i        ! index in nodeCountStruGat
                  nNodes = nodeCountStruGat(k)  ! structure i on sumdomain n has nNodes nodes
                  if (nNodes > 0) then
                     ii = (n-1)*nstru
                     is = sum(nNodesStruGat(1:n-1)) + sum(nodeCountStruGat(ii+1:ii+i-1))! starting index in xGat
                     do k1 = 1, nNodes
                        geomXStruMPI(j) = xGat(is+k1)
                        geomYStruMPI(j) = yGat(is+k1)
                        j = j + 1
                     end do
                  end if
               end do
            end do
         end if

         ! Copy the MPI-arrays to nodeCoutLat, geomXStru and geomYStru for the his-output
         nNodesStru = nNodesStruMPI
         nodeCountStru(1:nstru) = nodeCountStruMPI(1:nstru)
         call realloc(geomXStru, nNodesStru, keepExisting = .false., fill = 0d0)
         call realloc(geomYStru, nNodesStru, keepExisting = .false., fill = 0d0)
         geomXStru(1:nNodesStru) = geomXStruMPI(1:nNodesStru)
         geomYStru(1:nNodesStru) = geomYStruMPI(1:nNodesStru)
      end if
   end if
end subroutine fill_geometry_arrays_structure

!> Fill in array valstruct for a givin general structure, weir or orifice.
subroutine fill_valstruct_per_structure(valstruct, istrtypein, istru, nlinks)
   use m_missing, only: dmiss
   use m_1d_structures
   use m_General_Structure, only: t_GeneralStructure
   use m_GlobalParameters
   implicit none
   double precision, dimension(:), intent(inout) :: valstruct     !< Output values on structure (e.g. valweirgen(:)):
                                                                  !< (IVAL_WIDTH) total width, no matter dry or not
                                                                  !< (IVAL_WIDTHWET) total width of wet links
                                                                  !< (IVAL_WIDTHUP) total width of wet flowlinks on upstream side
                                                                  !< (IVAL_WIDTHDN) total width of wet flowlinks on downstream side
                                                                  !< (IVAL_WIDTHUPDN) total width of flowlinks when both upstream and downstream are wet
                                                                  !< (IVAL_DIS) structure discharge
                                                                  !< (IVAL_S1UP) structure water level up
                                                                  !< (IVAL_S1DN) structure water level down
                                                                  !< (IVAL_HEAD) structure head
                                                                  !< (IVAL_AREA) flow area
                                                                  !< (IVAL_VEL) velocity
                                                                  !< (IVAL_S1ONCREST) water level on crest, or valve relative opening if type is long culvert
                                                                  !< (IVAL_CRESTL) crest level
                                                                  !< (IVAL_CRESTW) crest width
                                                                  !< (IVAL_STATE) state
                                                                  !< (IVAL_FORCEDIF) force difference per unit width
                                                                  !< (IVAL_OPENW) gate opening width
                                                                  !< (IVAL_EDGEL) gate lower edge level
                                                                  !< (IVAL_OPENH) gate opening height
                                                                  !< (IVAL_UPPL) gate upper edge level
                                                                  !< (IVAL_DIS_OPEN) discharge through gate opening
                                                                  !< (IVAL_DIS_OVER) discharge over gate
                                                                  !< (IVAL_DIS_UNDER) discharge under gate
                                                                  !< (IVAL_AREA_OPEN) flow area in gate opening
                                                                  !< (IVAL_AREA_OVER) flow area over gate
                                                                  !< (IVAL_AREA_UNDER) flow area under gate
                                                                  !< (IVAL_VEL_OPEN) velocity through gate opening
                                                                  !< (IVAL_VEL_OVER) velocity over gate
                                                                  !< (IVAL_VEL_UNDER) velocity under gate
   integer,                           intent(in   ) :: istrtypein !< Structure type
   integer,                           intent(in   ) :: istru      !< Structure index in network%sts set or in longculverts
   integer,                           intent(in   ) :: nlinks     !< Number of links for the structure
   
   double precision :: tmp
   integer          :: jadif,i
   type(t_structure), pointer :: pstru
   type(t_GeneralStructure), pointer :: genstr

   if (any(istrtypein == (/ ST_GENERAL_ST, ST_WEIR, ST_ORIFICE /))) then ! TODO: ST_GATE
      pstru => network%sts%struct(istru)
      valstruct(IVAL_CRESTL) = get_crest_level(pstru)     ! crest level
      valstruct(IVAL_CRESTW)= get_width(pstru)            ! crest width

      ! determine state
      tmp = maxval(pstru%generalst%state(1:3,1))
      jadif = 0
      do i = 2, nlinks
         if (tmp /= maxval(pstru%generalst%state(1:3,i))) then
            jadif = 1
            exit
         end if
      end do
      if (jadif == 0) then
         valstruct(IVAL_STATE) = dble(tmp)
      else
         valstruct(IVAL_STATE) = dmiss
      end if
   end if

   if (any(istrtypein == (/ ST_GENERAL_ST, ST_ORIFICE /))) then ! TODO: ST_GATE
      if (nlinks > 0) then ! If it is a new general structure, and there are links
         genstr => network%sts%struct(istru)%generalst
         valstruct(IVAL_OPENW) = genstr%gateopeningwidth_actual                ! gate opening width
         valstruct(IVAL_EDGEL) = get_gle(pstru)                                ! gate lower edge level
         valstruct(IVAL_OPENH) = get_opening_height(pstru)                     ! gate opening height
         valstruct(IVAL_UPPL)  = valstruct(IVAL_EDGEL) + genstr%gatedoorheight ! gate upper edge level
      end if
   end if

end subroutine fill_valstruct_per_structure

! This subroutine fills two arrays with the input x- and y- coordinates of the chosen structure type. 
subroutine get_input_coordinates_of_structure(structuretype, geomXStructInput, geomYStructInput, nNodesStructInput,nNodeTot,numstructs)

use m_network
use m_globalparameters
use odugrid
use m_sferic, only: jsferic

integer,                               intent(in   ) :: structuretype               !< Structure type, see: m_globalparameters
double precision, allocatable, target, intent(  out) :: geomXStructInput(:)         !< [m] array with input x coordinates of structures.
double precision, allocatable, target, intent(  out) :: geomYStructInput(:)         !< [m] array with input y coordinates of structures.
integer,          allocatable, target, intent(  out) :: nNodesStructInput(:)        !< Array with number of coordinates for each structure
integer,                               intent(out)   :: numstructs                  !< Number of structures of structure type
integer,                               intent(out)   :: nNodeTot                    !< Total number of nodes of structure type,

integer           ::  i, n, j, ierr, nodes
integer,  pointer :: structindex(:)

select case (structuretype)
case (ST_WEIR)
   numstructs = network%sts%numweirs
   if (numstructs > 0) structindex => network%sts%WEIRINDICES
case (ST_UNI_WEIR)
   numstructs = network%sts%numuniweirs
   if (numstructs > 0) structindex => network%sts%uniWEIRINDICES
case (ST_CULVERT)
   numstructs = network%sts%numculverts
   if (numstructs > 0) structindex => network%sts%culvertINDICES
case (ST_BRIDGE)   
   numstructs = network%sts%numBRIDGEs
   if (numstructs > 0) structindex => network%sts%BRIDGEINDICES
case (ST_PUMP)
   numstructs = network%sts%numPUMPs
   if (numstructs > 0) structindex => network%sts%PUMPINDICES
case (ST_ORIFICE)
   numstructs = network%sts%numORIFICEs
   if (numstructs > 0) structindex => network%sts%ORIFICEINDICES
case (ST_GATE)  
   numstructs = network%sts%numGATEs
   if (numstructs > 0) structindex => network%sts%GATEINDICES
case (ST_GENERAL_ST) 
   numstructs = network%sts%numgeneralstructures
   if (numstructs > 0) structindex => network%sts%generalstructureINDICES
case default
   return
end select

if (allocated(geomXStructInput) ) deallocate(geomXStructInput)
if (allocated(geomYStructInput) ) deallocate(geomYStructInput)
if (allocated(nNodesStructInput)) deallocate(nNodesStructInput)


nNodeTot = 0
i = 1

if (numstructs > 0) then 
  
   allocate(nNodesStructInput(numstructs))
   do n = 1, numstructs
      j = structindex(n)
      nodes = network%sts%struct(j)%NUMCOORDINATES
      if (nodes > 0) then
         nNodeTot = nNodeTot + nodes
      else if (network%sts%struct(j)%ibran > -1) then
         nNodeTot = nNodeTot + 1
      endif
   enddo

   allocate(geomXStructInput(nNodeTot),geomYStructInput(nNodeTot))
   do n = 1, numstructs
      j = structindex(n)
      nodes = network%sts%struct(j)%NUMCOORDINATES
      if (nodes > 0) then

         geomXStructInput(i:i+nodes-1)= network%sts%struct(j)%XCOORDINATES
         geomYStructInput(i:i+nodes-1)= network%sts%struct(j)%YCOORDINATES
         nNodesStructInput(n) = nodes

         i = i + nodes
      else if (network%sts%struct(j)%ibran > -1) then

         ASSOCIATE ( branch => network%brs%branch(network%sts%struct(j)%IBRAN ))
            if (branch%GRIDPOINTSCOUNT > 0) then
            ierr = odu_get_xy_coordinates( (/ 1 /) ,  network%sts%struct(j:j)%CHAINAGE , branch%xs , branch%ys , &
               (/ branch%gridpointscount /),(/ branch%length /), jsferic  , geomXStructInput(i:i) , geomYStructInput(i:i) )
            else
            ierr = odu_get_xy_coordinates( (/ 1 /) ,  network%sts%struct(j:j)%CHAINAGE ,(/ branch%FROMNODE%X, branch%TONODE%X /) , (/ branch%FROMNODE%Y, branch%TONODE%Y /) , &
               (/ 2 /),(/ branch%length /), jsferic  , geomXStructInput(i:i) , geomYStructInput(i:i) )
            endif
            nNodesStructInput(n) = 1
            i = i + 1
         end ASSOCIATE

      endif
   enddo
endif

end subroutine get_input_coordinates_of_structure

end module m_structures