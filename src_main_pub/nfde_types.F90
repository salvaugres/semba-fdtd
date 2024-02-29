!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MIT License
! 
! Copyright (c) 2023 University of Granada
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
MODULE NFDETypes
   !
   USE FDETYPES
   USE mtln_types_mod, MTLN_t => parsed_t
   !
   IMPLICIT NONE
   INTEGER (KIND=4), PARAMETER :: RK = RKIND
   !------------------------------------------------------------------------------
   ! CONSTANTS FOR THE PARSER
   !------------------------------------------------------------------------------
   ! global variable stochastic
   ! MATERIALS
   REAL (KIND=RK), PARAMETER :: SIGMA_PEC = 1e19_RK
   REAL (KIND=RK), PARAMETER :: SIGMA_PMC = 1e19_RK 
   ! PROBES
   !!!!
   INTEGER (KIND=4), PARAMETER :: NP_T1_PLAIN = 0
   INTEGER (KIND=4), PARAMETER :: NP_T1_AMBOS = 2
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIME = 0
   INTEGER (KIND=4), PARAMETER :: NP_T2_FREQ = 1
   INTEGER (KIND=4), PARAMETER :: NP_T2_TRANSFER = 2
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIMEFREQ  = 3
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIMETRANSF = 4
   INTEGER (KIND=4), PARAMETER :: NP_T2_FREQTRANSF = 5
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIMEFRECTRANSF = 6
   INTEGER (KIND=4), PARAMETER :: NP_COR_EX = 0
   INTEGER (KIND=4), PARAMETER :: NP_COR_EY = 1
   INTEGER (KIND=4), PARAMETER :: NP_COR_EZ = 2
   INTEGER (KIND=4), PARAMETER :: NP_COR_HX = 3
   INTEGER (KIND=4), PARAMETER :: NP_COR_HY = 4
   INTEGER (KIND=4), PARAMETER :: NP_COR_HZ = 5
   INTEGER (KIND=4), PARAMETER :: NP_COR_WIRECURRENT = 6
   INTEGER (KIND=4), PARAMETER :: NP_COR_DDP = 7
   LOGICAL, PARAMETER :: BcELECT = .TRUE.
   LOGICAL, PARAMETER :: BcMAGNE = .FALSE.
   ! THIN WIRES
   INTEGER (KIND=4), PARAMETER :: MATERIAL_CONS = 0
   INTEGER (KIND=4), PARAMETER :: MATERIAL_absorbing = 100
   INTEGER (KIND=4), PARAMETER :: Parallel_CONS = 1
   INTEGER (KIND=4), PARAMETER :: SERIES_CONS = 2
   INTEGER (KIND=4), PARAMETER :: DISPERSIVE_CONS = 3
   ! BORDERS
   INTEGER (KIND=4), PARAMETER :: F_PEC = 1
   INTEGER (KIND=4), PARAMETER :: F_PMC = 2
   INTEGER (KIND=4), PARAMETER :: F_PER = 4
   INTEGER (KIND=4), PARAMETER :: F_MUR = 7
   INTEGER (KIND=4), PARAMETER :: F_PML = 9
   INTEGER (KIND=4), PARAMETER :: F_XL = 1
   INTEGER (KIND=4), PARAMETER :: F_XU = 2
   INTEGER (KIND=4), PARAMETER :: F_YL = 3
   INTEGER (KIND=4), PARAMETER :: F_YU = 4
   INTEGER (KIND=4), PARAMETER :: F_ZL = 5
   INTEGER (KIND=4), PARAMETER :: F_ZU = 6
   INTEGER (KIND=4), PARAMETER :: F_TIMEFRECTRANSF = 0
   ! rlc y diodos
   INTEGER (KIND=4), PARAMETER :: inductor = 20
   INTEGER (KIND=4), PARAMETER :: capacitor = 21
   INTEGER (KIND=4), PARAMETER :: resistor = 22
   INTEGER (KIND=4), PARAMETER :: diodo = 23
   INTEGER (KIND=4), PARAMETER :: Dielectric = 24
   INTEGER (KIND=4), PARAMETER :: PMLbody = 25

   !------------------------------------------------------------------------------
   ! TYPES
   !------------------------------------------------------------------------------
   !-----------------> Cordinate Types
   !------------------------------------------------------------------------------
   ! Basic cordinate type for two points and orientation
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: coords
      INTEGER (KIND=4) :: Xi = - 1
      INTEGER (KIND=4) :: Xe = - 1
      INTEGER (KIND=4) :: Yi = - 1
      INTEGER (KIND=4) :: Ye = - 1
      INTEGER (KIND=4) :: Zi = - 1
      INTEGER (KIND=4) :: Ze = - 1
      INTEGER (KIND=4) :: Xtrancos = 1
      INTEGER (KIND=4) :: Ytrancos = 1
      INTEGER (KIND=4) :: Ztrancos = 1
      INTEGER (KIND=4) :: Or = 0 !f1eld orientation
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE coords
   TYPE, PUBLIC :: coords_scaled
      INTEGER (KIND=4) :: Xi = - 1
      INTEGER (KIND=4) :: Xe = - 1
      INTEGER (KIND=4) :: Yi = - 1
      INTEGER (KIND=4) :: Ye = - 1
      INTEGER (KIND=4) :: Zi = - 1
      INTEGER (KIND=4) :: Ze = - 1
      REAL (KIND=RK) :: xc = 0.0_RKIND
      REAL (KIND=RK) :: yc = 0.0_RKIND
      REAL (KIND=RK) :: zc = 0.0_RKIND
          INTEGER (KIND=4) :: Or = 0 !field orientation nuevo 2015
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE coords_scaled
   !-----------------> Material Types
   !------------------------------------------------------------------------------
   ! Basic constants for materials
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Material
      REAL (KIND=RK) :: eps = 0.0_RKIND
      REAL (KIND=RK) :: mu = 0.0_RKIND
      REAL (KIND=RK) :: sigma = 0.0_RKIND
      REAL (KIND=RK) :: sigmam = 0.0_RKIND
      INTEGER (KIND=4) :: id = 0
   END TYPE Material
   !------------------------------------------------------------------------------
   ! New Class which is a collection of different materials
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Materials
      INTEGER (KIND=4) :: n_Mats = 0
      INTEGER (KIND=4) :: n_Mats_max = 0
      TYPE (Material), DIMENSION (:), POINTER :: Mats => NULL ()
   END TYPE Materials
   !------------------------------------------------------------------------------
   ! Locates all the different PEC media found
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: PECRegions
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nSurfs = 0
      INTEGER (KIND=4) :: nLins = 0
      INTEGER (KIND=4) :: nVols_max = 0
      INTEGER (KIND=4) :: nSurfs_max = 0
      INTEGER (KIND=4) :: nLins_max = 0
      TYPE (coords), DIMENSION (:), POINTER :: Vols => NULL ()
      TYPE (coords), DIMENSION (:), POINTER :: Surfs => NULL ()
      TYPE (coords), DIMENSION (:), POINTER :: Lins => NULL ()
   END TYPE PECRegions
   !------------------------------------------------------------------------------
   ! Defines a Non Metal Body
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Dielectric_t
      TYPE (coords), DIMENSION (:), POINTER :: c1P => NULL ()
      TYPE (coords), DIMENSION (:), POINTER :: c2P => NULL ()
      REAL (KIND=RK) :: sigma = 0.0_RKIND
      REAL (KIND=RK) :: eps = 0.0_RKIND
      REAL (KIND=RK) :: mu = 0.0_RKIND
      REAL (KIND=RK) :: sigmam = 0.0_RKIND
      INTEGER (KIND=4) :: n_C1P = 0
      INTEGER (KIND=4) :: n_C2P = 0
!!!reaprovecho para meter aqui los lumped
      REAL (KIND=RK)   :: Rtime_on = 0.0_RKIND, Rtime_off = 0.0_RKIND
      REAL (KIND=RK) :: R = 0.0_RKIND
      REAL (KIND=RK) :: L = 0.0_RKIND
      REAL (KIND=RK) :: C = 0.0_RKIND
      !stoch 201022
      REAL (KIND=RK) :: R_devia = 0.0_RKIND
      REAL (KIND=RK) :: L_devia = 0.0_RKIND
      REAL (KIND=RK) :: C_devia = 0.0_RKIND
      !
      REAL (KIND=RK) :: DiodB = 0.0_RKIND
      REAL (KIND=RK)   :: DiodIsat = 0.0_RKIND
      INTEGER (KIND=4) :: DiodOri = 0
!!!reaprovecho para meter aqui los waveports de Berenger
      INTEGER (KIND=4) :: orient = 0
!!!!!!!!!
      logical :: resistor=.false. , inductor=.false. , capacitor=.false. , diodo=.false. , plain=.false. , PMLbody=.false.
!!!!fin 270815
   END TYPE Dielectric_t
   !------------------------------------------------------------------------------
   ! Locates all the different Non Metal Media found
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: DielectricRegions
      TYPE (Dielectric_t), DIMENSION (:), POINTER :: Vols => NULL ()
      TYPE (Dielectric_t), DIMENSION (:), POINTER :: Surfs => NULL ()
      TYPE (Dielectric_t), DIMENSION (:), POINTER :: Lins => NULL ()
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nSurfs = 0
      INTEGER (KIND=4) :: nLins = 0
      INTEGER (KIND=4) :: nVols_max = 0
      INTEGER (KIND=4) :: nSurfs_max = 0
      INTEGER (KIND=4) :: nLins_max = 0
      INTEGER (KIND=4) :: n_C1P_max = 0
      INTEGER (KIND=4) :: n_C2P_max = 0
   END TYPE DielectricRegions
   !------------------------------------------------------------------------------
   ! TYPE that defines the information of a frequency depENDent material,
   ! it inherits from the material class and it adds the possible values needed
   ! in the frequency depENDent section of the
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: FreqDepenMaterial
      COMPLEX, DIMENSION (:), POINTER ::  a11 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b11 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am11 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm11 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  a12 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b12 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am12 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm12 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  a13 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b13 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am13 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm13 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  a22 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b22 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am22 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm22 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  a23 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b23 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am23 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm23 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  a33 => NULL ()
      COMPLEX, DIMENSION (:), POINTER ::  b33 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: am33 => NULL ()
      COMPLEX, DIMENSION (:), POINTER :: bm33 => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: alpha => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: beta => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: gamma => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: alpham => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: betam => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: gammam => NULL ()
      TYPE (coords), DIMENSION (:), POINTER :: c => NULL ()
      REAL (KIND=RK) ::    eps11 = 0.0_RKIND ,    eps12 = 0.0_RKIND ,    eps13 = 0.0_RKIND ,    eps22 = 0.0_RKIND ,    eps23 = 0.0_RKIND ,    eps33 = 0.0_RKIND
      REAL (KIND=RK) ::     mu11 = 0.0_RKIND ,     mu12 = 0.0_RKIND ,     mu13 = 0.0_RKIND ,     mu22 = 0.0_RKIND ,     mu23 = 0.0_RKIND ,     mu33 = 0.0_RKIND
      REAL (KIND=RK) ::  sigma11 = 0.0_RKIND ,  sigma12 = 0.0_RKIND ,  sigma13 = 0.0_RKIND ,  sigma22 = 0.0_RKIND ,  sigma23 = 0.0_RKIND ,  sigma33 = 0.0_RKIND
      REAL (KIND=RK) :: sigmam11 = 0.0_RKIND , sigmam12 = 0.0_RKIND , sigmam13 = 0.0_RKIND , sigmam22 = 0.0_RKIND , sigmam23 = 0.0_RKIND , sigmam33 = 0.0_RKIND
      INTEGER (KIND=4) ::  K11 = 0
      INTEGER (KIND=4) :: Km11 = 0
      INTEGER (KIND=4) ::  K12 = 0
      INTEGER (KIND=4) :: Km12 = 0
      INTEGER (KIND=4) ::  K13 = 0
      INTEGER (KIND=4) :: Km13 = 0
      INTEGER (KIND=4) ::  K22 = 0
      INTEGER (KIND=4) :: Km22 = 0
      INTEGER (KIND=4) ::  K23 = 0
      INTEGER (KIND=4) :: Km23 = 0
      INTEGER (KIND=4) ::  K33 = 0
      INTEGER (KIND=4) :: Km33 = 0
      INTEGER (KIND=4) :: L = 0
      INTEGER (KIND=4) :: Lm = 0
      INTEGER (KIND=4) :: n_c = 0
      CHARACTER (LEN=BUFSIZE) :: files = ' ' !2015 si esta presente lee los polos/residuos desde fichero
   END TYPE FreqDepenMaterial
   !------------------------------------------------------------------------------
   ! TYPE that defines the list of frequency depedent materials
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: FreqDepenMaterials
      TYPE (FreqDepenMaterial), DIMENSION (:), POINTER :: Vols => NULL ()
      TYPE (FreqDepenMaterial), DIMENSION (:), POINTER :: Surfs => NULL ()
      TYPE (FreqDepenMaterial), DIMENSION (:), POINTER :: Lins => NULL ()
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nSurfs = 0
      INTEGER (KIND=4) :: nLins = 0
      INTEGER (KIND=4) :: nVols_max = 0
      INTEGER (KIND=4) :: nSurfs_max = 0
      INTEGER (KIND=4) :: nLins_max = 0
      INTEGER (KIND=4) :: n_c_max = 0 !cota superior
   END TYPE FreqDepenMaterials
   !------------------------------------------------------------------------------
   ! Type for the ANISOTROPIC body, surface and lines since they will contain
   ! the same information
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ANISOTROPICbody_t
      TYPE (coords), DIMENSION (:), POINTER :: c1P => NULL ()
      TYPE (coords), DIMENSION (:), POINTER :: c2P => NULL ()
      REAL (KIND=RK), DIMENSION (3, 3) :: sigma, eps, mu, sigmam
      INTEGER (KIND=4) :: n_C1P = 0
      INTEGER (KIND=4) :: n_C2P = 0
   END TYPE ANISOTROPICbody_t
   !------------------------------------------------------------------------------
   ! Type that contains the elements found in the nfde File
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ANISOTROPICelements_t
      TYPE (ANISOTROPICbody_t), DIMENSION (:), POINTER :: Vols => NULL ()
      TYPE (ANISOTROPICbody_t), DIMENSION (:), POINTER :: Surfs => NULL ()
      TYPE (ANISOTROPICbody_t), DIMENSION (:), POINTER :: Lins => NULL ()
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nSurfs = 0
      INTEGER (KIND=4) :: nLins = 0
      INTEGER (KIND=4) :: nVols_max = 0
      INTEGER (KIND=4) :: nSurfs_max = 0
      INTEGER (KIND=4) :: nLins_max = 0
      INTEGER (KIND=4) :: n_C1P_max = 0 !cota superior de c1p y c2p en vols,sufs,lins
      INTEGER (KIND=4) :: n_C2P_max = 0
   END TYPE ANISOTROPICelements_t
   !------------------------------------------------------------------------------
   ! Defines a Comp Surface
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: LossyThinSurface
      TYPE (coords), DIMENSION (:), POINTER :: c => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: sigma
      REAL (KIND=RK), DIMENSION (:), POINTER :: eps
      REAL (KIND=RK), DIMENSION (:), POINTER :: mu
      REAL (KIND=RK), DIMENSION (:), POINTER :: sigmam
      REAL (KIND=RK), DIMENSION (:), POINTER :: thk
      !for_devia
      
      REAL (KIND=RK), DIMENSION (:), POINTER :: sigma_devia
      REAL (KIND=RK), DIMENSION (:), POINTER :: eps_devia
      REAL (KIND=RK), DIMENSION (:), POINTER :: mu_devia
      REAL (KIND=RK), DIMENSION (:), POINTER :: sigmam_devia
      REAL (KIND=RK), DIMENSION (:), POINTER :: thk_devia
      !
      INTEGER (KIND=4) :: nc = 0
      CHARACTER (LEN=BUFSIZE) :: files = ' ' !2011 tag nombre fichero
      INTEGER (KIND=4)  :: numcapas  !2014 multicapas
   END TYPE LossyThinSurface
   !------------------------------------------------------------------------------
   ! Locates all the different Comp media found
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: LossyThinSurfaces
      TYPE (LossyThinSurface), DIMENSION (:), POINTER :: cs => NULL ()
      INTEGER (KIND=4) :: length = 0
      INTEGER (KIND=4) :: length_max = 0
      INTEGER (KIND=4) :: nC_max = 0 !cota de todos los nc de LossyThinSurface
   END TYPE LossyThinSurfaces
   !------------------------------------------------------------------------------
   ! Component for Thin Wires there is a list of this inside the component
   ! that defines the whole Thin Wire Reference
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinWireComp
      CHARACTER (LEN=BUFSIZE) :: srctype, srcfile
      INTEGER (KIND=4) :: i = - 1
      INTEGER (KIND=4) :: j = - 1
      INTEGER (KIND=4) :: K = - 1
      INTEGER (KIND=4) :: nd = - 1
      INTEGER (KIND=4) :: d = - 1
      REAL (KIND=RK) :: m = 0.0_RKIND
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE ThinWireComp
   !------------------------------------------------------------------------------
   ! ThinWire component that defines the overall properties of the definition
   ! of ThinWires
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinWire
      TYPE (ThinWireComp), DIMENSION (:), POINTER :: twc => NULL ()
      REAL (KIND=RK) :: rad = 0 , rad_devia = 0
      LOGICAL :: disp = .false.
      CHARACTER (LEN=BUFSIZE) :: dispfile
      REAL (KIND=RK) :: res = 0 , res_devia = 0
      REAL (KIND=RK) :: ind = 0 , ind_devia = 0
      REAL (KIND=RK) :: cap = 0 , cap_devia = 0
      REAL (KIND=RK) :: P_res = 0
      REAL (KIND=RK) :: P_ind = 0
      REAL (KIND=RK) :: P_cap = 0
      CHARACTER (LEN=BUFSIZE) :: dispfile_LeftEnd
      REAL (KIND=RK) :: R_LeftEnd = 0 , R_LeftEnd_devia = 0
      REAL (KIND=RK) :: L_LeftEnd = 0 , L_LeftEnd_devia = 0
      REAL (KIND=RK) :: C_LeftEnd = 0 , C_LeftEnd_devia = 0
      CHARACTER (LEN=BUFSIZE) :: dispfile_RightEnd
      REAL (KIND=RK) :: R_RightEnd = 0 , R_RightEnd_devia = 0
      REAL (KIND=RK) :: L_RightEnd = 0 , L_RightEnd_devia = 0
      REAL (KIND=RK) :: C_RightEnd = 0 , C_RightEnd_devia = 0
      INTEGER (KIND=4) :: LeftEnd = 0
      INTEGER (KIND=4) :: RightEnd = 0
      ! Components
      INTEGER (KIND=4) :: tl = 0
      INTEGER (KIND=4) :: tr = 0
      INTEGER (KIND=4) :: n_twc = 0
      INTEGER (KIND=4) :: n_twc_max = 0
   END TYPE ThinWire
   !------------------------------------------------------------------------------
   ! List of the different thin wires that were found in the file
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinWires
      TYPE (ThinWire), DIMENSION (:), POINTER :: tw => NULL ()
      INTEGER (KIND=4) :: n_tw = 0
      INTEGER (KIND=4) :: n_tw_max = 0
   END TYPE ThinWires
   !------------------------------------------------------------------------------
   ! Component for Slanted Wires there is a list of this inside the component
   ! that defines the whole Slanted Wire Reference
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SlantedWireComp
      CHARACTER (LEN=BUFSIZE) :: srctype, srcfile
      REAL (KIND=RK) :: x = - 1.0_RKIND
      REAL (KIND=RK) :: y = - 1.0_RKIND
      REAL (KIND=RK) :: z = - 1.0_RKIND
      INTEGER (KIND=4) :: nd = - 1
      REAL (KIND=RK) :: m = 0.0_RKIND
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE SlantedWireComp
   !------------------------------------------------------------------------------
   ! ThinWire component that defines the overall properties of the definition
   ! of ThinWires
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SlantedWire
      TYPE (SlantedWireComp), DIMENSION (:), POINTER :: swc => NULL ()
      REAL (KIND=RK) :: rad = 0
      LOGICAL :: disp = .false.
      CHARACTER (LEN=BUFSIZE) :: dispfile
      REAL (KIND=RK) :: res = 0
      REAL (KIND=RK) :: ind = 0
      REAL (KIND=RK) :: cap = 0
      REAL (KIND=RK) :: P_res = 0
      REAL (KIND=RK) :: P_ind = 0
      REAL (KIND=RK) :: P_cap = 0
      CHARACTER (LEN=BUFSIZE) :: dispfile_LeftEnd
      REAL (KIND=RK) :: R_LeftEnd = 0
      REAL (KIND=RK) :: L_LeftEnd = 0
      REAL (KIND=RK) :: C_LeftEnd = 0
      CHARACTER (LEN=BUFSIZE) :: dispfile_RightEnd
      REAL (KIND=RK) :: R_RightEnd = 0
      REAL (KIND=RK) :: L_RightEnd = 0
      REAL (KIND=RK) :: C_RightEnd = 0
      INTEGER (KIND=4) :: LeftEnd = 0
      INTEGER (KIND=4) :: RightEnd = 0
      ! Components
      INTEGER (KIND=4) :: tl = 0
      INTEGER (KIND=4) :: tr = 0
      INTEGER (KIND=4) :: n_swc = 0
      INTEGER (KIND=4) :: n_swc_max = 0
   END TYPE SlantedWire
   !------------------------------------------------------------------------------
   ! List of the different thin wires that were found in the file
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SlantedWires
      TYPE (SlantedWire), DIMENSION (:), POINTER :: sw => NULL ()
      INTEGER (KIND=4) :: n_sw = 0
      INTEGER (KIND=4) :: n_sw_max = 0
   END TYPE SlantedWires
   !--------------------------------------------------------------------------
   ! Component for Thin Slots there is a list of this inside the component
   ! that defines the whole Thin Slot Reference
   !--------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinSlotComp
      INTEGER (KIND=4) :: i = 0
      INTEGER (KIND=4) :: j = 0
      INTEGER (KIND=4) :: K = 0
      INTEGER (KIND=4) :: node = 0
      INTEGER (KIND=4) :: dir = - 1
      INTEGER (KIND=4) :: Or = - 1 !added by 2011 and filled in inside PREPROCESS
      !since the orientation of the plane info is not on .nfde, but foud afterwards
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE ThinSlotComp
   !--------------------------------------------------------------------------
   ! ThinSlot component that defines the overall properties of the definition
   ! of ThinSlots in ORIGINAL
   !--------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinSlot
      TYPE (ThinSlotComp), DIMENSION (:), POINTER :: tgc => NULL ()
      REAL (KIND=RK) :: width = 0
      INTEGER (KIND=4) :: n_tgc = 0
      INTEGER (KIND=4) :: n_tgc_max = 0
   END TYPE ThinSlot
   !--------------------------------------------------------------------------
   ! List of the different thin Slots that were found in the file
   !--------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinSlots
      TYPE (ThinSlot), DIMENSION (:), POINTER :: tg => NULL ()
      INTEGER (KIND=4) :: n_tg = 0
      INTEGER (KIND=4) :: n_tg_max = 0
   END TYPE ThinSlots
   !-----------------> Border Types
   !------------------------------------------------------------------------------
   ! PML Border Type
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: FronteraPML
      REAL (KIND=RK) :: orden = 2.0_RK
      REAL (KIND=RK) :: refl = 1e-3_RK
      INTEGER (KIND=4) :: numCapas = 8
   END TYPE FronteraPML
   !------------------------------------------------------------------------------
   ! Tipo de la frontera
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Frontera
      INTEGER (KIND=4), DIMENSION (6) :: tipoFrontera
      TYPE (FronteraPML), DIMENSION (6) :: propiedadesPML
   END TYPE Frontera
   !-----------------> Probe Types
   !------------------------------------------------------------------------------
   ! TYPE to define the new probe object which contains the TYPE of calculation
   ! the TYPE of analysis, time and frequency step and the filename where
   ! it should be saved
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: MasSonda
      CHARACTER (LEN=BUFSIZE) :: filename
      TYPE (coords), DIMENSION (:), POINTER :: cordinates => NULL ()
      REAL (KIND=RK) :: tstart, tstop, tstep
      REAL (KIND=RK) :: fstart, fstop, fstep
      INTEGER (KIND=4) :: type1, type2
      INTEGER (KIND=4) :: len_cor = 0
      CHARACTER (LEN=BUFSIZE) :: outputrequest
   END TYPE MasSonda
   !------------------------------------------------------------------------------
   ! TYPE that defines a list of probes to be appended and accesed
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: MasSondas
      TYPE (MasSonda), DIMENSION (:), POINTER :: collection => NULL ()
      INTEGER (KIND=4) :: length = 0
      INTEGER (KIND=4) :: length_max = 0
      INTEGER (KIND=4) :: len_cor_max = 0 !cota
   END TYPE MasSondas
   !------------------------------------------------------------------------------
   ! This TYPE contains the basic information in nearly all the different PROBES
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Sonda
      CHARACTER (LEN=BUFSIZE) :: grname
      INTEGER (KIND=4), DIMENSION (:), POINTER :: i => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: j => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: K => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: node => NULL ()
      INTEGER (KIND=4) :: n_cord = 0
      INTEGER (KIND=4) :: n_cord_max = 0
      REAL (KIND=RK) :: tstart, tstop, tstep
      CHARACTER (LEN=BUFSIZE) :: outputrequest
      !por si se precisa para el Far Field
      REAL (KIND=RK) :: fstart, fstop, fstep
      REAL (KIND=RK) :: phistart, phistop, phistep
      REAL (KIND=RK) :: thetastart, thetastop, thetastep
      CHARACTER (LEN=BUFSIZE) :: FileNormalize
   END TYPE Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric far field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: FarField_Sonda
      TYPE (Sonda) :: probe
   END TYPE FarField_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Electric_Sonda
      TYPE (Sonda) :: probe
   END TYPE Electric_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the magnetic field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Magnetic_Sonda
      TYPE (Sonda) :: probe
   END TYPE Magnetic_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the normal electric field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NormalElectric_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE NormalElectric_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the normal magnetic field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NormalMagnetic_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE NormalMagnetic_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric surface current density
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SurfaceElectricCurrent_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE SurfaceElectricCurrent_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the magnetic surface current density
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SurfaceMagneticCurrent_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE SurfaceMagneticCurrent_Sonda
   !------------------------------------------------------------------------------
   ! Abstract class which performs the dynamic dispatching
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: abstractSonda
      INTEGER (KIND=4) :: n_FarField = 0 
      INTEGER (KIND=4) :: n_Electric = 0
      INTEGER (KIND=4) :: n_Magnetic = 0
      INTEGER (KIND=4) :: n_NormalElectric = 0
      INTEGER (KIND=4) :: n_NormalMagnetic = 0
      INTEGER (KIND=4) :: n_SurfaceElectricCurrent = 0
      INTEGER (KIND=4) :: n_SurfaceMagneticCurrent = 0
      !
      INTEGER (KIND=4) :: n_FarField_max = 0
      INTEGER (KIND=4) :: n_Electric_max = 0
      INTEGER (KIND=4) :: n_Magnetic_max = 0
      INTEGER (KIND=4) :: n_NormalElectric_max = 0
      INTEGER (KIND=4) :: n_NormalMagnetic_max = 0
      INTEGER (KIND=4) :: n_SurfaceElectricCurrent_max = 0
      INTEGER (KIND=4) :: n_SurfaceMagneticCurrent_max = 0
      TYPE (FarField_Sonda), DIMENSION (:), POINTER :: FarField => NULL ()
      TYPE (Electric_Sonda), DIMENSION (:), POINTER :: Electric => NULL ()
      TYPE (Magnetic_Sonda), DIMENSION (:), POINTER :: Magnetic => NULL ()
      TYPE (NormalElectric_Sonda), DIMENSION (:), POINTER :: NormalElectric => NULL ()
      TYPE (NormalMagnetic_Sonda), DIMENSION (:), POINTER :: NormalMagnetic => NULL ()
      TYPE (SurfaceElectricCurrent_Sonda), DIMENSION (:), POINTER :: SurfaceElectricCurrent => NULL ()
      TYPE (SurfaceMagneticCurrent_Sonda), DIMENSION (:), POINTER :: SurfaceMagneticCurrent => NULL ()
   END TYPE abstractSonda
   !------------------------------------------------------------------------------
   ! Class to account as a list for all the probes
   ! that might be required during the parsing process
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Sondas
      TYPE (abstractSonda), DIMENSION (:), POINTER :: probes => NULL ()
      INTEGER (KIND=4) :: n_probes = 0
      INTEGER (KIND=4) :: n_probes_max = 0
   END TYPE Sondas
   !------------------------------------------------------------------------------
   ! Object TYPE defined for the Bloque current probe
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: BloqueProbe
      REAL (KIND=RK) :: tstart, tstop, tstep
      REAL (KIND=RK) :: fstart, fstop, fstep
      CHARACTER (LEN=BUFSIZE) :: FileNormalize
      INTEGER (KIND=4) :: type2
      INTEGER (KIND=4) :: i1, i2, j1, j2, k1, k2, skip
      INTEGER (KIND=4) :: nml
      LOGICAL :: t
      CHARACTER (LEN=BUFSIZE) :: outputrequest
      CHARACTER (LEN=BUFSIZE) :: tag
   END TYPE BloqueProbe
   ! Object made for the collection of defined Bloque probes
   TYPE, PUBLIC :: BloqueProbes
      TYPE (BloqueProbe), DIMENSION (:), POINTER :: bp => NULL ()
      INTEGER (KIND=4) :: n_bp = 0
      INTEGER (KIND=4) :: n_bp_max = 0
   END TYPE BloqueProbes

   !------------------------------------------------------------------------------
   ! Object TYPE defined for the Volumic probes
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: VolProbe
      TYPE (coords), DIMENSION (:), POINTER :: cordinates => NULL ()
      REAL (KIND=RK) :: tstart, tstop, tstep
      CHARACTER (LEN=BUFSIZE) :: outputrequest
      INTEGER (KIND=4) :: len_cor = 0
      !para freq domain
      REAL (KIND=RK) :: fstart, fstop, fstep
      INTEGER (KIND=4) ::  type2
      CHARACTER (LEN=BUFSIZE) :: filename
   END TYPE VolProbe
   ! Object made for the collection of defined Volumic probes
   TYPE, PUBLIC :: VolProbes
      TYPE (VolProbe), DIMENSION (:), POINTER :: collection => NULL ()
      INTEGER (KIND=4) :: length = 0
      INTEGER (KIND=4) :: length_max = 0
      INTEGER (KIND=4) :: len_cor_max = 0 !cota
   END TYPE VolProbes

   !-----------------> Source Types
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Box
      CHARACTER (LEN=BUFSIZE) :: nombre_fichero
      INTEGER (KIND=4), DIMENSION (3) :: coor1, coor2
   END TYPE Box
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Boxes
      TYPE (Box), DIMENSION (:), POINTER :: Vols => NULL ()
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nVols_max = 0
   END TYPE Boxes
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: PlaneWave
      CHARACTER (LEN=BUFSIZE) :: nombre_fichero
      CHARACTER (LEN=BUFSIZE) :: atributo
      INTEGER (KIND=4), DIMENSION (3) :: coor1, coor2
      REAL (KIND=RK) :: theta, phi, alpha, beta
      logical :: isRC !for reververation chambers
      REAL (KIND=RK) :: INCERTMAX
      INTEGER (KIND=4) :: numModes !for reververation chambers
   END TYPE PlaneWave
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: PlaneWaves
      TYPE (PlaneWave), DIMENSION (:), POINTER :: collection => NULL ()
      INTEGER (KIND=4) :: nc = 0
      INTEGER (KIND=4) :: nC_max = 0
   END TYPE PlaneWaves
   !------------------------------------------------------------------------------
   ! Definicin de los tipos current density que existirn en el ficero
   ! nfde
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Curr_Field_Src
      TYPE (coords_scaled), DIMENSION (:), POINTER :: c1P => NULL ()
      TYPE (coords_scaled), DIMENSION (:), POINTER :: c2P => NULL ()
      CHARACTER (LEN=BUFSIZE) :: nombre
      INTEGER (KIND=4) :: n_C1P = 0
      INTEGER (KIND=4) :: n_C2P = 0
      LOGICAL :: isElec, isMagnet
      LOGICAL :: isCurrent, isField, isInitialValue
   END TYPE Curr_Field_Src
   !------------------------------------------------------------------------------
   ! Definicin de las Nodal Source global
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NodSource
      TYPE (Curr_Field_Src), DIMENSION (:), POINTER :: NodalSource => NULL ()
      INTEGER (KIND=4) :: n_nodSrc = 0
      INTEGER (KIND=4) :: n_nodSrc_max = 0
      INTEGER (KIND=4) :: n_C1P_max = 0
      INTEGER (KIND=4) :: n_C2P_max = 0
   END TYPE NodSource
   !-----------------> General Types
   !------------------------------------------------------------------------------
   ! Matrix attributes.
   ! Total[XYZ] -> Is the cell number for each axis.
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: MatrizMedios
      INTEGER (KIND=4) :: totalX, totalY, totalZ
   END TYPE MatrizMedios
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   TYPE NFDEGeneral
      REAL (KIND=RK) :: dt
      INTEGER (KIND=4) :: nmax
   END TYPE NFDEGeneral
   !------------------------------------------------------------------------------
   ! Definition of the type. Three vectors are defined, for each axis X,Y,Z. If
   ! their size is equal to 1 then there is a constant increment. If it is not
   ! then it will be one for each cell position.
   !! WARNING
   !! Even though, Type MatrizMedios defines the total number of cell for each
   !! axis. n[XYZ] defines it too partially. Meaning that if there is a
   !! constant increment, the pointer will be a scalar. However, when it is
   !! variable the pointer will have the same size as total[XYZ] in the
   !! MatrizMedios type and for each vector position the increment for those
   !! Cells
   !------------------------------------------------------------------------------
   TYPE Desplazamiento
      REAL (KIND=RK), DIMENSION (:), POINTER :: desX => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: desY => NULL ()
      REAL (KIND=RK), DIMENSION (:), POINTER :: desZ => NULL ()
      INTEGER (KIND=4) :: nX = 0, mx1 = 0, mx2 = 0 !2012
      INTEGER (KIND=4) :: nY = 0, my1 = 0, my2 = 0 !2012
      INTEGER (KIND=4) :: nZ = 0, mz1 = 0, mz2 = 0 !2012
      real (KIND=RK) ::originx= 0.0_RKIND  !2012
      real (KIND=RK) ::originy= 0.0_RKIND  !2012
      real (KIND=RK) ::originz= 0.0_RKIND  !2012
   END TYPE Desplazamiento
   !-----------------> Program Types
   !------------------------------------------------------------------------------
   ! Parameters needed for the parser
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Parseador
      character (len=BUFSIZE) :: switches=' '  
      ! Basics
      TYPE (NFDEGeneral), POINTER ::           general => NULL ()
      TYPE (MatrizMedios), POINTER ::          matriz => NULL ()
      TYPE (Desplazamiento), POINTER ::        despl => NULL ()
      TYPE (Frontera), POINTER ::              front => NULL ()
      ! Materials
      TYPE (Materials), POINTER ::             Mats => NULL ()
      TYPE (PECRegions), POINTER ::            pecRegs => NULL ()
      TYPE (PECRegions), POINTER ::            pmcRegs => NULL ()
      TYPE (DielectricRegions), POINTER ::     DielRegs => NULL ()
      TYPE (LossyThinSurfaces), POINTER ::     LossyThinSurfs => NULL ()
      TYPE (FreqDepenMaterials), POINTER ::    frqDepMats => NULL ()
      TYPE (ANISOTROPICelements_t), POINTER :: aniMats => NULL ()
      ! Sources
      TYPE (Boxes), POINTER ::                 boxSrc => NULL ()
      TYPE (PlaneWaves), POINTER ::            plnSrc => NULL ()
      TYPE (NodSource), POINTER ::             nodSrc => NULL ()
      ! Probes
      TYPE (Sondas), POINTER ::                oldSONDA => NULL ()
      TYPE (MasSondas), POINTER ::             Sonda => NULL ()
      TYPE (BloqueProbes), POINTER ::          BloquePrb => NULL ()
      TYPE (VolProbes), POINTER ::             VolPrb => NULL ()
      ! Thin Elements                         
      TYPE (ThinWires), POINTER ::             tWires => NULL ()
      TYPE (SlantedWires), POINTER ::          sWires => NULL ()
      TYPE (ThinSlots), POINTER ::             tSlots => NULL ()
      TYPE (mtln_t), POINTER ::                mtln => NULL ()
   END TYPE Parseador
   
   !---> definicion de tipos
   TYPE, PUBLIC :: t_linea
      INTEGER (KIND=4) :: LEN
      CHARACTER (LEN=BUFSIZE) :: dato
   END TYPE t_linea
   !--->
   TYPE, PUBLIC :: t_NFDE_FILE
      INTEGER (KIND=4) mpidir !x=1,y=2,z=3
      INTEGER (KIND=8) :: targ
      !--->
      INTEGER (KIND=8) :: numero
      TYPE (t_linea), DIMENSION (:), POINTER :: lineas
      logical :: thereare_stoch
   END TYPE t_NFDE_FILE
!--->

END MODULE NFDETypes

    
    
!!AUX  ROUTINES TO CREATE THE TYPES USED BY THE PRIVATE PARSER    
MODULE BoxSourceClass

   !
   USE NFDETypes
   !
CONTAINS
   !
   SUBROUTINE new_box (MPIDIR,this, nombre, i1, j1, k1, i2, j2, k2)
      TYPE (Box), INTENT (INOUT) :: this
      CHARACTER (LEN=*), INTENT (IN) :: nombre
      INTEGER (KIND=4) :: i1, j1, k1, i2, j2, k2, mpidir
      INTEGER (KIND=4)   :: OXI, OXE, OYI, OYE,OZI, OZE
      !
      this%coor1 (1) = i1
      this%coor1 (2) = j1
      this%coor1 (3) = k1
      this%coor2 (1) = i2
      this%coor2 (2) = j2
      this%coor2 (3) = k2
      this%nombre_fichero = nombre
      !MPI  ROTATE BOX
      IF (MPIDIR==2 ) THEN
         OXI=  this%coor1 (1)
         OXE=  this%coor2 (1)
         OYI=  this%coor1 (2)
         OYE=  this%coor2 (2)
         OZI=  this%coor1 (3)
         OZE=  this%coor2 (3)
         !
         this%coor1 (1) =OZI
         this%coor2 (1) =OZE
         this%coor1 (2) =OXI
         this%coor2 (2) =OXE
         this%coor1 (3) =OYI
         this%coor2 (3) =OYE
      ELSEIF (MPIDIR==1 ) THEN
         OXI=  this%coor1 (1)
         OXE=  this%coor2 (1)
         OYI=  this%coor1 (2)
         OYE=  this%coor2 (2)
         OZI=  this%coor1 (3)
         OZE=  this%coor2 (3)
         !
         this%coor1 (1) =OYI
         this%coor2 (1) =OYE
         this%coor1 (2) =OZI
         this%coor2 (2) =OZE
         this%coor1 (3) =OXI
         this%coor2 (3) =OXE
      ENDIF
      !!!!!
      !
      RETURN
   END SUBROUTINE new_box
   !
   FUNCTION incluirBox (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (Box), INTENT (IN) :: n_Volume
      TYPE (Box), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (Box), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION incluirBox
   !
   
END MODULE BoxSourceClass
!------------------------------------------------------------------------------
! File :    BloqueProbe.f90
! Created on April 24, 2009, 12 : 52 AM
! This MODULE defines the class to deal with the Bloque probes in the
! nfde input files. It encodes the information provided by it and provides
! methods to generate them as outputs.
!------------------------------------------------------------------------------
MODULE BloqueProbeClass

   !
   USE NFDETypes
   !
   IMPLICIT NONE
   !
CONTAINS
   !------------------------------------------------------------------------------
   ! Add probes to the collection of probes object
   !------------------------------------------------------------------------------
   FUNCTION addBloqueCurrentProbes (n_BloqueProbe, o_BloqueProbes, steps) RESULT (BloqueProbes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (BloqueProbe), INTENT (INOUT) :: n_BloqueProbe
      TYPE (BloqueProbe), DIMENSION (steps-1), INTENT (IN) :: o_BloqueProbes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (BloqueProbe), DIMENSION (:), POINTER :: BloqueProbes
      !--> Allocate space for the new array
      ALLOCATE (BloqueProbes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         BloqueProbes (n) = o_BloqueProbes (n)
      END DO
      !--> Add new value found
      BloqueProbes (steps) = n_BloqueProbe
      !
      RETURN
   END FUNCTION addBloqueCurrentProbes
   !
   
END MODULE BloqueProbeClass

!------------------------------------------------------------------------------
! File :    Frontera.f90
! Created on March 16, 2009, 13 : 03 AM
! Definicin de la clase frontera definida para trabajar con las condiciones
! frontera del fichero nfde
!------------------------------------------------------------------------------
MODULE FronteraClass

   USE NFDETypes
CONTAINS
   !------------------------------------------------------------------------------
   ! Mtodo que establece el tipo de frontera para la posicin sealada
   ! introduciendo el tipo sealado
   !------------------------------------------------------------------------------
   SUBROUTINE establecerFrontera (this, posicion, tipo)
      TYPE (Frontera), INTENT (INOUT) :: this
      INTEGER (KIND=4) :: posicion, tipo
      this%tipoFrontera (posicion) = tipo
   END SUBROUTINE establecerFrontera
   !------------------------------------------------------------------------------
   ! Mtodo para asignar propiedades en la posicin indicadada
   ! con el valor dado
   !------------------------------------------------------------------------------
   SUBROUTINE asignarPropiedades (this, valor, posicion)
      TYPE (Frontera), INTENT (INOUT) :: this
      INTEGER (KIND=4) :: posicion, valor
      this%tipoFrontera (posicion) = valor
   END SUBROUTINE asignarPropiedades
   !------------------------------------------------------------------------------
   ! Mtodo por el cual se establecen las propiedades PML en la posicin
   ! sealada por el mtodo
   !------------------------------------------------------------------------------
   SUBROUTINE asignarPropiedadesPML (this, pml, posicion)
      TYPE (Frontera), INTENT (INOUT) :: this
      TYPE (FronteraPML) :: pml
      INTEGER (KIND=4) :: posicion
      this%propiedadesPML (posicion) = pml
   END SUBROUTINE asignarPropiedadesPML
   !------------------------------------------------------------------------------
   ! Constructor de la clase de propiedades del PML
   ! necesario introducir siempre los datos
   !------------------------------------------------------------------------------
   SUBROUTINE new_pml (this, numCapas, orden, refl)
      TYPE (FronteraPML), INTENT (INOUT) :: this
      REAL (KIND=RK) ::  orden, refl
      REAL (KIND=RK) :: numCapas
      this%numCapas = nint(numCapas) !por algun misterio ORIGINAL pone las capas como reales
      this%orden = orden
      this%refl = refl
   END SUBROUTINE new_pml
   !
   
END MODULE FronteraClass
!------------------------------------------------------------------------------
! File :    MasSonda.f90
! Created on April 24, 2009, 12 : 52 AM
! This MODULE defines the class to deal with the new probes in the
! nfde input files. It encodes the information provided by it and provides
! methods to generate them as outputs.
! Despite it not being the most efficient solution, the following code uses
! dynamic dispatching
!------------------------------------------------------------------------------
!
MODULE MasSondaClass

   !
   USE NFDETypes
   IMPLICIT NONE
   !
CONTAINS
   !------------------------------------------------------------------------------
   ! SUBROUTINE to generate the construction of the
   ! new probe class
   !------------------------------------------------------------------------------
   SUBROUTINE new_MasSonda (this, type1, type2, filename, tstart, tstop, tstep, fstart, fstop, fstep, outputrequest)
      TYPE (MasSonda), INTENT (INOUT) :: this
      INTEGER (KIND=4), INTENT (INOUT) :: type1, type2
      CHARACTER (LEN=*), INTENT (INOUT) :: filename
      REAL (KIND=RK), INTENT (INOUT) :: tstart, tstop, tstep
      REAL (KIND=RK), INTENT (INOUT) :: fstart, fstop, fstep
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      this%type1 = type1
      this%type2 = type2
      this%filename = filename
      this%tstart = tstart
      this%tstop = tstop
      this%tstep = tstep
      this%fstart = fstart
      this%fstop = fstop
      this%fstep = fstep
      this%len_cor = 0
      NULLIFY (this%cordinates)
      !
      RETURN
   END SUBROUTINE new_MasSonda
   !
   
END MODULE MasSondaClass
!
!------------------------------------------------------------------------------
! File :    MasSonda.f90
! Created on April 24, 2009, 12 : 52 AM
! This MODULE defines the class to deal with the new probes in the
! nfde input files. It encodes the information provided by it and provides
! methods to generate them as outputs.
! Despite it not being the most efficient solution, the following code uses
! dynamic dispatching
!------------------------------------------------------------------------------
!
MODULE VolProbeClass

   !
   USE NFDETypes
   IMPLICIT NONE
   !
CONTAINS
   !------------------------------------------------------------------------------
   ! SUBROUTINE to generate the construction of the
   ! Scr probe class
   !------------------------------------------------------------------------------
   SUBROUTINE New_VolProbe (this, tstart, tstop, tstep, fstart, fstop, fstep, type2, filename, outputrequest)
      TYPE (VolProbe), INTENT (INOUT) :: this
      REAL (KIND=RK), INTENT (INOUT) :: tstart, tstop, tstep,fstart, fstop, fstep
      !sgg'10
      INTEGER (KIND=4) :: comi,type2
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      CHARACTER (LEN=BUFSIZE) :: filename
      comi = index (outputrequest, '*') + 1
      this%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      this%tstart = tstart
      this%tstop = tstop
      this%tstep = tstep
      this%len_cor = 0

      this%fstart=fstart
      this%fstop=fstop
      this%fstep=fstep
      this%type2=type2
      this%filename=filename

      NULLIFY (this%cordinates)
      !
      RETURN
   END SUBROUTINE New_VolProbe
   !
   
END MODULE VolProbeClass
!------------------------------------------------------------------------------
! File :    planeWave.f90
! Created on March 19, 2009, 10 : 59 AM
!------------------------------------------------------------------------------
!
MODULE PlaneWaveClass

   !
   USE NFDETypes
   !
CONTAINS
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   !------------------------------------------------------------------------------
   SUBROUTINE new_planewave (mpidir,this, nombre, attribute, i1, j1, k1, i2, j2, k2, theta, phi, alpha, beta,isRC,numModes,INCERTMAX)
      TYPE (PlaneWave), INTENT (INOUT) :: this
      CHARACTER (LEN=*), INTENT (IN) :: nombre, attribute
      INTEGER (KIND=4), INTENT (IN)  :: i1, j1, k1, i2, j2, k2,numModes
      logical, INTENT (IN)  :: isRC
      REAL (KIND=RK), INTENT (IN) :: theta, phi, alpha, beta, INCERTMAX
      INTEGER (KIND=4)   :: OXI, OXE, OYI, OYE,OZI, OZE,mpidir
      !
      this%coor1 (1) = i1
      this%coor1 (2) = j1
      this%coor1 (3) = k1
      this%coor2 (1) = i2
      this%coor2 (2) = j2
      this%coor2 (3) = k2
      this%nombre_fichero = nombre
      this%atributo = attribute
      this%theta = theta
      this%phi = phi
      this%alpha = alpha
      this%beta = beta
      this%isRC = isRC
      this%INCERTMAX = INCERTMAX
      this%numModes = numModes

      !MPI  ROTATE PLANEWAVE
      IF (MPIDIR==2 ) THEN
         OXI=  this%coor1 (1)
         OXE=  this%coor2 (1)
         OYI=  this%coor1 (2)
         OYE=  this%coor2 (2)
         OZI=  this%coor1 (3)
         OZE=  this%coor2 (3)
         !
         this%coor1 (1) =OZI
         this%coor2 (1) =OZE
         this%coor1 (2) =OXI
         this%coor2 (2) =OXE
         this%coor1 (3) =OYI
         this%coor2 (3) =OYE

         this%theta = atan2(Sqrt(Cos(theta)**2.0_RKIND+ Cos(phi)**2*Sin(theta)**2),Sin(phi)*Sin(theta))
         this%phi =   atan2(Cos(phi)*Sin(theta),Cos(theta))
         this%alpha = atan2(Sqrt(Cos(alpha)**2.0_RKIND+ Cos(beta)**2*Sin(alpha)**2),Sin(beta)*Sin(alpha))
         this%beta =  atan2(Cos(beta)*Sin(alpha),Cos(alpha))

      ELSEIF (MPIDIR==1 ) THEN
         OXI=  this%coor1 (1)
         OXE=  this%coor2 (1)
         OYI=  this%coor1 (2)
         OYE=  this%coor2 (2)
         OZI=  this%coor1 (3)
         OZE=  this%coor2 (3)
         !
         this%coor1 (1) =OYI
         this%coor2 (1) =OYE
         this%coor1 (2) =OZI
         this%coor2 (2) =OZE
         this%coor1 (3) =OXI
         this%coor2 (3) =OXE

         this%theta = atan2(Sqrt(Cos(theta)**2.0_RKIND+ Sin(phi)**2*Sin(theta)**2),Cos(phi)*Sin(theta))
         this%phi =   atan2(Cos(theta),Sin(phi)*Sin(theta))
         this%alpha = atan2(Sqrt(Cos(alpha)**2.0_RKIND+ Sin(beta)**2*Sin(alpha)**2),Cos(beta)*Sin(alpha))
         this%beta =  atan2(Cos(alpha),Sin(beta)*Sin(alpha))
      ENDIF
      !!!!!
      RETURN
   END SUBROUTINE new_planewave
   !
   FUNCTION incluirPlaneWave (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (PlaneWave), INTENT (IN) :: n_Volume
      TYPE (PlaneWave), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (PlaneWave), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION incluirPlaneWave
   !
   
END MODULE PlaneWaveClass
!------------------------------------------------------------------------------
! File :    ThinSlots.f90
! MODULE to cover the information provided by thin Slots in the development of
! the parsing application
!------------------------------------------------------------------------------
MODULE ThinSlotsClass

   !
   USE NFDETypes
   !
CONTAINS
   !--------------------------------------------------------------------------
   ! Adds a ThinSlotComponent inside the ThinSlot object given as a
   ! parameter
   !--------------------------------------------------------------------------
   FUNCTION addThinSlotComp (n_thinSlotComp, o_thinSlotsComp, steps) RESULT (thinSlotsComp)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (ThinSlotComp), INTENT (IN) :: n_thinSlotComp
      TYPE (ThinSlotComp), DIMENSION (steps-1), INTENT (IN) :: o_thinSlotsComp
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (ThinSlotComp), DIMENSION (:), POINTER :: thinSlotsComp
      !--> Allocate space for the new array
      ALLOCATE (thinSlotsComp(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         thinSlotsComp (n) = o_thinSlotsComp (n)
      END DO
      !--> Add new value found
      thinSlotsComp (steps) = n_thinSlotComp
      !
      RETURN
   END FUNCTION addThinSlotComp
   !--------------------------------------------------------------------------
   ! Adds a ThinSlot inside the ThinSlots object given as a parameter
   !--------------------------------------------------------------------------
   FUNCTION addThinSlot (n_thinSlot, o_thinSlots, steps) RESULT (ThinSlots)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (ThinSlot), INTENT (IN) :: n_thinSlot
      TYPE (ThinSlot), DIMENSION (steps-1), INTENT (IN) :: o_thinSlots
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (ThinSlot), DIMENSION (:), POINTER :: ThinSlots
      !--> Allocate space for the new array
      ALLOCATE (ThinSlots(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         ThinSlots (n) = o_thinSlots (n)
      END DO
      !--> Add new value found
      ThinSlots (steps) = n_thinSlot
      !
      RETURN
   END FUNCTION addThinSlot
   !

END MODULE ThinSlotsClass
!------------------------------------------------------------------------------
! File :    ThinWires.f90
! Created on August 10, 2009, 12 : 27 AM
! MODULE to cover the information provided by thin wires in the development of
! the parsing application
!------------------------------------------------------------------------------
MODULE ThinWiresClass

   !
   USE NFDETypes
   !----------------------------------------------------------------------------
   ! Adds a ThinWireComponent inside the ThinWire object given as a
   ! parameter
   !----------------------------------------------------------------------------
CONTAINS
   FUNCTION addThinWireComp (n_thinWireComp, o_thinWiresComp, steps) RESULT (thinWiresComp)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (ThinWireComp), INTENT (IN) :: n_thinWireComp
      TYPE (ThinWireComp), DIMENSION (steps-1), INTENT (IN) :: o_thinWiresComp
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (ThinWireComp), DIMENSION (:), POINTER :: thinWiresComp
      !--> Allocate space for the new array
      ALLOCATE (thinWiresComp(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         thinWiresComp (n) = o_thinWiresComp (n)
      END DO
      !--> Add new value found
      thinWiresComp (steps) = n_thinWireComp
      !
      RETURN
   END FUNCTION addThinWireComp
   !----------------------------------------------------------------------------
   ! Adds a ThinWireComponent inside the ThinWire object given as a
   ! parameter
   !----------------------------------------------------------------------------
   FUNCTION addThinWire (n_thinWire, o_thinWires, steps) RESULT (ThinWires)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (ThinWire), INTENT (IN) :: n_thinWire
      TYPE (ThinWire), DIMENSION (steps-1), INTENT (IN) :: o_thinWires
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (ThinWire), DIMENSION (:), POINTER :: ThinWires
      !--> Allocate space for the new array
      ALLOCATE (ThinWires(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         ThinWires (n) = o_thinWires (n)
      END DO
      !--> Add new value found
      ThinWires (steps) = n_thinWire
      !
      RETURN
   END FUNCTION addThinWire
   !
   
END MODULE ThinWiresClass
!------------------------------------------------------------------------------
! File :    SlantedWires.f90
! Created on September 28, 2015, 18 : 51 AM
! MODULE to cover the information provided by slanted wires in the development of
! the parsing application
!------------------------------------------------------------------------------
MODULE SlantedWiresClass

   !
   USE NFDETypes
   !----------------------------------------------------------------------------
   ! Adds a SlantedWireComponent inside the SlantedWire object given as a
   ! parameter
   !----------------------------------------------------------------------------
CONTAINS
   FUNCTION addSlantedWireComp (n_slantedWireComp, o_slantedWiresComp, steps) RESULT (slantedWiresComp)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (SlantedWireComp), INTENT (IN) :: n_slantedWireComp
      TYPE (SlantedWireComp), DIMENSION (steps-1), INTENT (IN) :: o_slantedWiresComp
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (SlantedWireComp), DIMENSION (:), POINTER :: slantedWiresComp
      !--> Allocate space for the new array
      ALLOCATE (slantedWiresComp(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         slantedWiresComp (n) = o_slantedWiresComp (n)
      END DO
      !--> Add new value found
      slantedWiresComp (steps) = n_slantedWireComp
      !
      RETURN
   END FUNCTION addSlantedWireComp
   !----------------------------------------------------------------------------
   ! Adds a SlantedWireComponent inside the SlantedWire object given as a
   ! parameter
   !----------------------------------------------------------------------------
   FUNCTION addSlantedWire (n_slantedWire, o_slantedWires, steps) RESULT (SlantedWires)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (SlantedWire), INTENT (IN) :: n_slantedWire
      TYPE (SlantedWire), DIMENSION (steps-1), INTENT (IN) :: o_slantedWires
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (SlantedWire), DIMENSION (:), POINTER :: SlantedWires
      !--> Allocate space for the new array
      ALLOCATE (SlantedWires(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         SlantedWires (n) = o_slantedWires (n)
      END DO
      !--> Add new value found
      SlantedWires (steps) = n_slantedWire
      !
      RETURN
   END FUNCTION addSlantedWire
   !
   
END MODULE SlantedWiresClass
!------------------------------------------------------------------------------
! File :    SondaClass.f90
! Created on April 24, 2009, 12 : 52 AM
! This MODULE defines the class to deal with the probes in the
! nfde input files. It encodes the information provided by it and provides
! methods to generate them as outputs.
! Despite it not being the most efficient solution, the following code uses
! dynamic dispatching
!------------------------------------------------------------------------------
!
MODULE SondaClass

   !------------------------------------------------------------------------------
   ! External classes used
   !------------------------------------------------------------------------------
   !
   USE NFDETypes
   !------------------------------------------------------------------------------
   ! PRIVATE methods
   !------------------------------------------------------------------------------
   PRIVATE :: FarField_Sonda_new
   PRIVATE :: Electric_Sonda_new, Magnetic_Sonda_new
   PRIVATE :: NormalElectric_Sonda_new, NormalMagnetic_Sonda_new
   PRIVATE :: SurfaceElectricCurrent_Sonda_new, SurfaceMagneticCurrent_Sonda_new
   PRIVATE :: assign_FarField,assign_Electric, assign_Magnetic, assign_NormalElectric, assign_NormalMagnetic, assign_SurfaceMagneticCurrent, assign_SurfaceElectricCurrent
   PRIVATE :: addCoordinates_1, addCoordinates_2
   !------------------------------------------------------------------------------
   ! INTERFACE for the constructor of classes
   !------------------------------------------------------------------------------
   INTERFACE new_SONDA
      MODULE PROCEDURE FarField_Sonda_new
      MODULE PROCEDURE Electric_Sonda_new
      MODULE PROCEDURE Magnetic_Sonda_new
      MODULE PROCEDURE NormalElectric_Sonda_new
      MODULE PROCEDURE NormalMagnetic_Sonda_new
      MODULE PROCEDURE SurfaceElectricCurrent_Sonda_new
      MODULE PROCEDURE SurfaceMagneticCurrent_Sonda_new
      MODULE PROCEDURE abstractProbe_new
   END INTERFACE new_SONDA
   !------------------------------------------------------------------------------
   ! INTERFACE for assigning to the abstract class a non-abstract
   ! object
   !------------------------------------------------------------------------------
   INTERFACE assign
      MODULE PROCEDURE assign_FarField
      MODULE PROCEDURE assign_Electric
      MODULE PROCEDURE assign_Magnetic
      MODULE PROCEDURE assign_NormalElectric
      MODULE PROCEDURE assign_NormalMagnetic
      MODULE PROCEDURE assign_SurfaceElectricCurrent
      MODULE PROCEDURE assign_SurfaceMagneticCurrent
   END INTERFACE assign
   !------------------------------------------------------------------------------
   ! INTERFACE to distinguish from different add coordinates
   !------------------------------------------------------------------------------
   INTERFACE addCoordinates
      MODULE PROCEDURE addCoordinates_1
      MODULE PROCEDURE addCoordinates_2
   END INTERFACE addCoordinates
   !------------------------------------------------------------------------------
   ! Begining of the subroutines and functions
   !------------------------------------------------------------------------------
CONTAINS
   !------------------------------------------------------------------------------
   ! Initialices the object for abstract probes
   !------------------------------------------------------------------------------
   SUBROUTINE abstractProbe_new (this)
      TYPE (abstractSonda), INTENT (INOUT) :: this
      !
      this%n_FarField = 0
      this%n_Electric = 0
      this%n_Magnetic = 0
      this%n_NormalElectric = 0
      this%n_NormalMagnetic = 0
      this%n_SurfaceElectricCurrent = 0
      this%n_SurfaceMagneticCurrent = 0
      this%n_FarField_max = 0
      this%n_Electric_max = 0
      this%n_Magnetic_max = 0
      this%n_NormalElectric_max = 0
      this%n_NormalMagnetic_max = 0
      this%n_SurfaceElectricCurrent_max = 0
      this%n_SurfaceMagneticCurrent_max = 0
      NULLIFY (this%FarField,this%Electric, this%Magnetic, this%NormalElectric, this%NormalMagnetic, this%SurfaceElectricCurrent, this%SurfaceMagneticCurrent)
      !
      RETURN
   END SUBROUTINE abstractProbe_new
   !------------------------------------------------------------------------------
   ! Constructor for the FAR electric field probe
   !------------------------------------------------------------------------------
   SUBROUTINE FarField_Sonda_new (MPIDIR,this, name, i, j, K, node, fstart, fstop, fstep, &
   thetastart, thetastop, thetastep,phistart, phistop, phistep, FileNormalize,&
   outputrequest)

      TYPE (FarField_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*) :: name,FileNormalize
      INTEGER (KIND=4) :: i, j, K, node,MPIDIR
      REAL (KIND=RK) :: fstart, fstop, fstep
      REAL (KIND=RK) :: thetastart, thetastop, thetastep,phistart, phistop, phistep
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%probe%grname = name
      this%probe%fstart = fstart
      this%probe%fstop =  fstop
      this%probe%fstep =  fstep
      this%probe%thetastart = thetastart
      this%probe%thetastop  =  thetastop
      this%probe%thetastep  =  thetastep
      this%probe%phistart = phistart
      this%probe%phistop  = phistop
      this%probe%phistep  = phistep

      !!!mpirotate angulos farfield .... las coordenadas se rotan luego
      IF (MPIDIR==2 ) THEN
         this%probe%thetastart = atan2(Sqrt(Cos(thetastart)**2.0_RKIND+ Cos(phistart)**2*Sin(thetastart)**2),Sin(phistart)*Sin(thetastart))
         this%probe%phistart   = atan2(Cos(phistart)*Sin(thetastart),Cos(thetastart))
      ELSEIF (MPIDIR==1 ) THEN
         this%probe%thetastart = atan2(Sqrt(Cos(thetastart)**2.0_RKIND+ Sin(phistart)**2*Sin(thetastart)**2),Cos(phistart)*Sin(thetastart))
         this%probe%phistart   = atan2(Cos(thetastart),Sin(phistart)*Sin(thetastart))
      ENDIF
      !!!!!!


      this%probe%FileNormalize=trim(adjustl(FileNormalize))

      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      !
      RETURN
   END SUBROUTINE FarField_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the electric field probe
   !------------------------------------------------------------------------------
   SUBROUTINE Electric_Sonda_new (MPIDIR,this, name, i, j, K, node, tstart, tstop, tstep, outputrequest)
      TYPE (Electric_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node
      REAL (KIND=RK) :: tstart, tstop, tstep
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      !
      RETURN
   END SUBROUTINE Electric_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the magnetic field probe
   !------------------------------------------------------------------------------
   SUBROUTINE Magnetic_Sonda_new (MPIDIR,this, name, i, j, K, node, tstart, tstop, tstep, outputrequest)
      TYPE (Magnetic_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*), INTENT (IN) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node
      REAL (KIND=RK) :: tstart, tstop, tstep
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      !
      RETURN
   END SUBROUTINE Magnetic_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the normal electric field probe
   !------------------------------------------------------------------------------
   SUBROUTINE NormalElectric_Sonda_new (MPIDIR,this, name, i, j, K, node, nml, tstart, tstop, tstep, outputrequest)
      TYPE (NormalElectric_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*), INTENT (IN) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node, nml
      REAL (KIND=RK) :: tstart, tstop, tstep
      INTEGER (KIND=4), DIMENSION (:), POINTER :: aux_f
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%n_nml = 1
      NULLIFY (this%nml)
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      aux_f => addNml (nml, this%nml, this%n_nml)
      IF (ASSOCIATED(this%nml)) DEallocate (this%nml)
      this%nml => aux_f
      !
      RETURN
   END SUBROUTINE NormalElectric_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the magnetic current surface probe object
   !------------------------------------------------------------------------------
   SUBROUTINE SurfaceMagneticCurrent_Sonda_new (MPIDIR,this, name, i, j, K, node, nml, tstart, tstop, tstep, outputrequest)
      TYPE (SurfaceMagneticCurrent_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node, nml
      REAL (KIND=RK) :: tstart, tstop, tstep
      INTEGER (KIND=4), DIMENSION (:), POINTER :: aux_f
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%n_nml = 1
      NULLIFY (this%nml)
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      aux_f => addNml (nml, this%nml, this%n_nml)
      IF (ASSOCIATED(this%nml)) DEallocate (this%nml)
      this%nml => aux_f
      !
      RETURN
   END SUBROUTINE SurfaceMagneticCurrent_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the normal magnetic field probe
   !------------------------------------------------------------------------------
   SUBROUTINE NormalMagnetic_Sonda_new (MPIDIR,this, name, i, j, K, node, nml, tstart, tstop, tstep, outputrequest)
      TYPE (NormalMagnetic_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node, nml
      REAL (KIND=RK) :: tstart, tstop, tstep
      INTEGER (KIND=4), DIMENSION (:), POINTER :: aux_f
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%n_nml = 1
      NULLIFY (this%nml)
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      aux_f => addNml (nml, this%nml, this%n_nml)
      IF (ASSOCIATED(this%nml)) DEallocate (this%nml)
      this%nml => aux_f
      !
      RETURN
   END SUBROUTINE NormalMagnetic_Sonda_new
   !------------------------------------------------------------------------------
   ! Constructor for the electric current surface probe object
   !------------------------------------------------------------------------------
   SUBROUTINE SurfaceElectricCurrent_Sonda_new (MPIDIR,this, name, i, j, K, node, nml, tstart, tstop, tstep, outputrequest)
      TYPE (SurfaceElectricCurrent_Sonda), INTENT (INOUT) :: this
      CHARACTER (LEN=*) :: name
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node, nml
      REAL (KIND=RK) :: tstart, tstop, tstep
      INTEGER (KIND=4), DIMENSION (:), POINTER :: aux_f
      !sgg'10
      INTEGER (KIND=4) :: comi
      CHARACTER (LEN=*), INTENT (INOUT) :: outputrequest
      comi = index (outputrequest, '*') + 1
      this%probe%outputrequest = trim (adjustl(outputrequest(comi:)))
      !! fin sgg'10
      !
      this%n_nml = 1
      NULLIFY (this%nml)
      this%probe%grname = name
      this%probe%tstart = tstart
      this%probe%tstop = tstop
      this%probe%tstep = tstep
      this%probe%n_cord = 1
      this%probe%n_cord_max = 1
      NULLIFY (this%probe%i, this%probe%j, this%probe%K, this%probe%node)
      CALL addSondaCoordinates (MPIDIR,this%probe, i, j, K, node, this%probe%n_cord)
      aux_f => addNml (nml, this%nml, this%n_nml)
      IF (ASSOCIATED(this%nml)) DEallocate (this%nml)
      this%nml => aux_f
      !
      RETURN
   END SUBROUTINE SurfaceElectricCurrent_Sonda_new
   !------------------------------------------------------------------------------
   ! Add probes to the list of probes
   !------------------------------------------------------------------------------
   FUNCTION addSONDA (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (abstractSonda), INTENT (IN) :: n_Volume
      TYPE (abstractSonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (abstractSonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !--> Assigning new values
      !  IF ( ASSOCIATED( o_Volumes ) ) DEallocate (o_Volumes)
      !
      RETURN
   END FUNCTION addSONDA
   !------------------------------------------------------------------------------
   ! Selector to add coordinates 
   !------------------------------------------------------------------------------
   SUBROUTINE addCoordinates_1 (MPIDIR,this, i, j, K, node, nml, precounting)
      TYPE (abstractSonda), INTENT (INOUT) :: this
      INTEGER (KIND=4), INTENT (INOUT) :: MPIDIR,i, j, K, node, nml
      INTEGER (KIND=4) :: precounting
      !
      IF (ASSOCIATED(this%NormalElectric)) THEN
         this%NormalElectric(1)%probe%n_cord_max = this%NormalElectric(1)%probe%n_cord_max + 1
         this%NormalElectric(1)%probe%n_cord = this%NormalElectric(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%NormalElectric(1)%probe, i, j, K, node, this%NormalElectric(1)%probe%n_cord)
         this%NormalElectric(1)%n_nml_max = this%NormalElectric(1)%n_nml_max + 1
         this%NormalElectric(1)%n_nml = this%NormalElectric(1)%n_nml * precounting + 1
         this%NormalElectric(1)%nml => addNml (nml, this%NormalElectric(1)%nml, this%NormalElectric(1)%n_nml)
      ELSE IF (ASSOCIATED(this%NormalMagnetic)) THEN
         this%NormalMagnetic(1)%probe%n_cord_max = this%NormalMagnetic(1)%probe%n_cord_max + 1
         this%NormalMagnetic(1)%probe%n_cord = this%NormalMagnetic(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%NormalMagnetic(1)%probe, i, j, K, node, this%NormalMagnetic(1)%probe%n_cord)
         this%NormalMagnetic(1)%n_nml_max = this%NormalMagnetic(1)%n_nml_max + 1
         this%NormalMagnetic(1)%n_nml = this%NormalMagnetic(1)%n_nml * precounting + 1
         this%NormalMagnetic(1)%nml => addNml (nml, this%NormalMagnetic(1)%nml, this%NormalMagnetic(1)%n_nml)
      ELSE IF (ASSOCIATED(this%SurfaceElectricCurrent)) THEN
         this%SurfaceElectricCurrent(1)%probe%n_cord_max = this%SurfaceElectricCurrent(1)%probe%n_cord_max + 1
         this%SurfaceElectricCurrent(1)%probe%n_cord = this%SurfaceElectricCurrent(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%SurfaceElectricCurrent(1)%probe, i, j, K, node, this%SurfaceElectricCurrent(1)%probe%n_cord)
         this%SurfaceElectricCurrent(1)%n_nml_max = this%SurfaceElectricCurrent(1)%n_nml_max + 1
         this%SurfaceElectricCurrent(1)%n_nml = this%SurfaceElectricCurrent(1)%n_nml * precounting + 1
         this%SurfaceElectricCurrent(1)%nml => addNml (nml, this%SurfaceElectricCurrent(1)%nml, this%SurfaceElectricCurrent(1)%n_nml)
      ELSE IF (ASSOCIATED(this%SurfaceMagneticCurrent)) THEN
         this%SurfaceMagneticCurrent(1)%probe%n_cord_max = this%SurfaceMagneticCurrent(1)%probe%n_cord_max + 1
         this%SurfaceMagneticCurrent(1)%probe%n_cord = this%SurfaceMagneticCurrent(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%SurfaceMagneticCurrent(1)%probe, i, j, K, node, this%SurfaceMagneticCurrent(1)%probe%n_cord)
         this%SurfaceMagneticCurrent(1)%n_nml_max = this%SurfaceMagneticCurrent(1)%n_nml_max + 1
         this%SurfaceMagneticCurrent(1)%n_nml = this%SurfaceMagneticCurrent(1)%n_nml * precounting + 1
         this%SurfaceMagneticCurrent(1)%nml => addNml (nml, this%SurfaceMagneticCurrent(1)%nml, this%SurfaceMagneticCurrent(1)%n_nml)
      END IF
      !
      RETURN
   END SUBROUTINE addCoordinates_1
   !------------------------------------------------------------------------------
   ! Selector to add coordinates 
   !------------------------------------------------------------------------------
   SUBROUTINE addCoordinates_2 (MPIDIR,this, i, j, K, node, precounting)
      TYPE (abstractSonda), INTENT (INOUT) :: this
      INTEGER (KIND=4) :: MPIDIR,i, j, K, node
      INTEGER (KIND=4) :: precounting
      !
      IF (ASSOCIATED(this%FarField)) THEN
         this%FarField(1)%probe%n_cord_max = this%FarField(1)%probe%n_cord_max + 1
         this%FarField(1)%probe%n_cord = this%FarField(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%FarField(1)%probe, i, j, K, node, this%FarField(1)%probe%n_cord)
      ELSEIF (ASSOCIATED(this%Electric)) THEN
         this%Electric(1)%probe%n_cord_max = this%Electric(1)%probe%n_cord_max + 1
         this%Electric(1)%probe%n_cord = this%Electric(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%Electric(1)%probe, i, j, K, node, this%Electric(1)%probe%n_cord)
      ELSE IF (ASSOCIATED(this%Magnetic)) THEN
         this%Magnetic(1)%probe%n_cord_max = this%Magnetic(1)%probe%n_cord_max + 1
         this%Magnetic(1)%probe%n_cord = this%Magnetic(1)%probe%n_cord * precounting + 1
         CALL addSondaCoordinates (MPIDIR,this%Magnetic(1)%probe, i, j, K, node, this%Magnetic(1)%probe%n_cord)
      END IF
      !
      RETURN
   END SUBROUTINE addCoordinates_2
   !------------------------------------------------------------------------------
   ! Method to include the coordinates introduced as parameters
   ! into the Sonda object included as a parameter
   ! as well.
   !------------------------------------------------------------------------------
   SUBROUTINE addSondaCoordinates (mpidir,this, n_i, n_j, n_k, n_node, steps)
      !-------------> Parameters
      TYPE (Sonda), INTENT (INOUT) :: this
      INTEGER (KIND=4), INTENT (INOUT) :: n_i, n_j, n_k, n_node,mpidir
      INTEGER (KIND=4), INTENT (INOUT) :: steps
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      INTEGER (KIND=4), DIMENSION (:), POINTER :: i, j, K, node
      !--> Allocate space for the new array
      ALLOCATE (i(steps), j(steps), K(steps), node(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         i (n) = this%i(n)
         J (n) = this%j(n)
         K (n) = this%K(n)
         node (n) = this%node(n)
      END DO
      !--> Add new value found
      i (steps) = n_i
      j (steps) = n_j
      K (steps) = n_k
      node (steps) = n_node
      !--> Assigning new values
      NULLIFY (this%i, this%j, this%K, this%node)
      !
      this%i => i
      this%j => j
      this%K => K
      this%node => node
      !!!ROTATE MPI
      IF (MPIDIR==2 ) THEN
         this%i => K
         this%j => I
         this%K => J
      ELSEIF (MPIDIR==1 ) THEN
         this%i => J
         this%j => K
         this%K => I
      ENDIF
      !!!FIN

      !
      RETURN
   END SUBROUTINE addSondaCoordinates
   !------------------------------------------------------------------------------
   ! Method to include the nml attribute into the normal electric and magnetic
   ! field probe.
   !------------------------------------------------------------------------------
   FUNCTION addNml (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      INTEGER (KIND=4), INTENT (IN) :: n_Volume
      INTEGER (KIND=4), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      INTEGER (KIND=4), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION addNml
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_FarField (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (FarField_Sonda), INTENT (IN) :: n_Volume
      TYPE (FarField_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (FarField_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_FarField
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_Electric (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (Electric_Sonda), INTENT (IN) :: n_Volume
      TYPE (Electric_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (Electric_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_Electric
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_Magnetic (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (Magnetic_Sonda), INTENT (IN) :: n_Volume
      TYPE (Magnetic_Sonda), DIMENSION (:), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (Magnetic_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_Magnetic
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_NormalElectric (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (NormalElectric_Sonda), INTENT (IN) :: n_Volume
      TYPE (NormalElectric_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (NormalElectric_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_NormalElectric
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_NormalMagnetic (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (NormalMagnetic_Sonda), INTENT (IN) :: n_Volume
      TYPE (NormalMagnetic_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (NormalMagnetic_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_NormalMagnetic
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_SurfaceElectricCurrent (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (SurfaceElectricCurrent_Sonda), INTENT (IN) :: n_Volume
      TYPE (SurfaceElectricCurrent_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (SurfaceElectricCurrent_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_SurfaceElectricCurrent
   !------------------------------------------------------------------------------
   ! Routine to assign the abstract class to the non-abstract Electric
   !------------------------------------------------------------------------------
   FUNCTION assign_SurfaceMagneticCurrent (n_Volume, o_Volumes, steps) RESULT (Volumes)
      !-------------> Parameters
      INTEGER (KIND=4), INTENT (IN) :: steps
      TYPE (SurfaceMagneticCurrent_Sonda), INTENT (IN) :: n_Volume
      TYPE (SurfaceMagneticCurrent_Sonda), DIMENSION (steps-1), INTENT (IN) :: o_Volumes
      !-------------> Used inside
      INTEGER (KIND=4) :: n
      !-------------> Output
      TYPE (SurfaceMagneticCurrent_Sonda), DIMENSION (:), POINTER :: Volumes
      !--> Allocate space for the new array
      ALLOCATE (Volumes(steps))
      !--> Copy of the old array into the new one
      DO n = 1, steps - 1
         Volumes (n) = o_Volumes (n)
      END DO
      !--> Add new value found
      Volumes (steps) = n_Volume
      !
      RETURN
   END FUNCTION assign_SurfaceMagneticCurrent
   !
   
END MODULE SondaClass
