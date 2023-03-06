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
   USE REPORT
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
   INTEGER (KIND=4), PARAMETER :: NP_T1_NUMER = 0
   INTEGER (KIND=4), PARAMETER :: NP_T1_BOTH = 2
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIME = 0
   INTEGER (KIND=4), PARAMETER :: NP_T2_FREQ = 1
   INTEGER (KIND=4), PARAMETER :: NP_T2_TRAN = 2
   INTEGER (KIND=4), PARAMETER :: NP_T2_TIFR = 3
   INTEGER (KIND=4), PARAMETER :: NP_T2_TITR = 4
   INTEGER (KIND=4), PARAMETER :: NP_T2_FRTR = 5
   INTEGER (KIND=4), PARAMETER :: NP_T2_ALL = 6
   INTEGER (KIND=4), PARAMETER :: NP_COR_EX = 0
   INTEGER (KIND=4), PARAMETER :: NP_COR_EY = 1
   INTEGER (KIND=4), PARAMETER :: NP_COR_EZ = 2
   INTEGER (KIND=4), PARAMETER :: NP_COR_HX = 3
   INTEGER (KIND=4), PARAMETER :: NP_COR_HY = 4
   INTEGER (KIND=4), PARAMETER :: NP_COR_HZ = 5
   INTEGER (KIND=4), PARAMETER :: NP_COR_IW = 6
   INTEGER (KIND=4), PARAMETER :: NP_COR_VG = 7
   LOGICAL, PARAMETER :: BcELECT = .TRUE.
   LOGICAL, PARAMETER :: BcMAGNE = .FALSE.
   ! THIN WIRES
   INTEGER (KIND=4), PARAMETER :: MATERIAL_CONS = 0
   INTEGER (KIND=4), PARAMETER :: MATERIAL_absorbing = 100
   INTEGER (KIND=4), PARAMETER :: PARALLEL_CONS = 1
   INTEGER (KIND=4), PARAMETER :: SERIES_CONS = 2
   INTEGER (KIND=4), PARAMETER :: DISPERSIVE_CONS = 3
   ! BORDERS
   INTEGER (KIND=4), PARAMETER :: F_PEC = 1
   INTEGER (KIND=4), PARAMETER :: F_PMC = 2
   INTEGER (KIND=4), PARAMETER :: F_SYM_ETAN = 3
   INTEGER (KIND=4), PARAMETER :: F_SYM_HTAN = 10
   INTEGER (KIND=4), PARAMETER :: F_PER = 4
   INTEGER (KIND=4), PARAMETER :: F_LOF = 5
   INTEGER (KIND=4), PARAMETER :: F_ELO = 6
   INTEGER (KIND=4), PARAMETER :: F_MUR = 7
   INTEGER (KIND=4), PARAMETER :: F_FAN = 8
   INTEGER (KIND=4), PARAMETER :: F_PML = 9
   INTEGER (KIND=4), PARAMETER :: F_XL = 1
   INTEGER (KIND=4), PARAMETER :: F_XU = 2
   INTEGER (KIND=4), PARAMETER :: F_YL = 3
   INTEGER (KIND=4), PARAMETER :: F_YU = 4
   INTEGER (KIND=4), PARAMETER :: F_ZL = 5
   INTEGER (KIND=4), PARAMETER :: F_ZU = 6
   INTEGER (KIND=4), PARAMETER :: F_ALL = 0
   ! rlc y diodos
   INTEGER (KIND=4), PARAMETER :: inductor = 20
   INTEGER (KIND=4), PARAMETER :: capacitor = 21
   INTEGER (KIND=4), PARAMETER :: resistor = 22
   INTEGER (KIND=4), PARAMETER :: diodo = 23
   INTEGER (KIND=4), PARAMETER :: plainNonMetal = 24
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
      CHARACTER (LEN=MAX_LINEA) :: tag
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
      CHARACTER (LEN=MAX_LINEA) :: tag
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
   TYPE, PUBLIC :: NonMetalBody
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
   END TYPE NonMetalBody
   !------------------------------------------------------------------------------
   ! Locates all the different Non Metal Media found
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NonMetalRegions
      TYPE (NonMetalBody), DIMENSION (:), POINTER :: Vols => NULL ()
      TYPE (NonMetalBody), DIMENSION (:), POINTER :: Surfs => NULL ()
      TYPE (NonMetalBody), DIMENSION (:), POINTER :: Lins => NULL ()
      INTEGER (KIND=4) :: nVols = 0
      INTEGER (KIND=4) :: nSurfs = 0
      INTEGER (KIND=4) :: nLins = 0
      INTEGER (KIND=4) :: nVols_max = 0
      INTEGER (KIND=4) :: nSurfs_max = 0
      INTEGER (KIND=4) :: nLins_max = 0
      INTEGER (KIND=4) :: n_C1P_max = 0
      INTEGER (KIND=4) :: n_C2P_max = 0
   END TYPE NonMetalRegions
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
      CHARACTER (LEN=MAX_LINEA) :: files = ' ' !2015 si esta presente lee los polos/residuos desde fichero
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
   TYPE, PUBLIC :: CompSurface
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
      CHARACTER (LEN=MAX_LINEA) :: files = ' ' !2011 tag nombre fichero
      INTEGER (KIND=4)  :: numcapas  !2014 multicapas
   END TYPE CompSurface
   !------------------------------------------------------------------------------
   ! Locates all the different Comp media found
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: CompSurfaces
      TYPE (CompSurface), DIMENSION (:), POINTER :: cs => NULL ()
      INTEGER (KIND=4) :: length = 0
      INTEGER (KIND=4) :: length_max = 0
      INTEGER (KIND=4) :: nC_max = 0 !cota de todos los nc de compsurface
   END TYPE CompSurfaces
   !------------------------------------------------------------------------------
   ! Component for Thin Wires there is a list of this inside the component
   ! that defines the whole Thin Wire Reference
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinWireComp
      CHARACTER (LEN=MAX_LINEA) :: srctype, srcfile
      INTEGER (KIND=4) :: i = - 1
      INTEGER (KIND=4) :: j = - 1
      INTEGER (KIND=4) :: K = - 1
      INTEGER (KIND=4) :: nd = - 1
      INTEGER (KIND=4) :: d = - 1
      REAL (KIND=RK) :: m = 0.0_RKIND
      CHARACTER (LEN=MAX_LINEA) :: tag
   END TYPE ThinWireComp
   !------------------------------------------------------------------------------
   ! ThinWire component that defines the overall properties of the definition
   ! of ThinWires
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: ThinWire
      TYPE (ThinWireComp), DIMENSION (:), POINTER :: twc => NULL ()
      REAL (KIND=RK) :: rad = 0 , rad_devia = 0
      LOGICAL :: disp = .false.
      CHARACTER (LEN=MAX_LINEA) :: dispfile
      REAL (KIND=RK) :: res = 0 , res_devia = 0
      REAL (KIND=RK) :: ind = 0 , ind_devia = 0
      REAL (KIND=RK) :: cap = 0 , cap_devia = 0
      REAL (KIND=RK) :: P_res = 0
      REAL (KIND=RK) :: P_ind = 0
      REAL (KIND=RK) :: P_cap = 0
      CHARACTER (LEN=MAX_LINEA) :: dispfile_TL
      REAL (KIND=RK) :: R_TL = 0 , R_TL_devia = 0
      REAL (KIND=RK) :: I_TL = 0 , I_TL_devia = 0
      REAL (KIND=RK) :: C_TL = 0 , C_TL_devia = 0
      CHARACTER (LEN=MAX_LINEA) :: dispfile_TR
      REAL (KIND=RK) :: R_TR = 0 , R_TR_devia = 0
      REAL (KIND=RK) :: I_TR = 0 , I_TR_devia = 0
      REAL (KIND=RK) :: C_TR = 0 , C_TR_devia = 0
      INTEGER (KIND=4) :: enl = 0
      INTEGER (KIND=4) :: enr = 0
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
      CHARACTER (LEN=MAX_LINEA) :: srctype, srcfile
      REAL (KIND=RK) :: x = - 1.0_RKIND
      REAL (KIND=RK) :: y = - 1.0_RKIND
      REAL (KIND=RK) :: z = - 1.0_RKIND
      INTEGER (KIND=4) :: nd = - 1
      REAL (KIND=RK) :: m = 0.0_RKIND
      CHARACTER (LEN=MAX_LINEA) :: tag
   END TYPE SlantedWireComp
   !------------------------------------------------------------------------------
   ! ThinWire component that defines the overall properties of the definition
   ! of ThinWires
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: SlantedWire
      TYPE (SlantedWireComp), DIMENSION (:), POINTER :: swc => NULL ()
      REAL (KIND=RK) :: rad = 0
      LOGICAL :: disp = .false.
      CHARACTER (LEN=MAX_LINEA) :: dispfile
      REAL (KIND=RK) :: res = 0
      REAL (KIND=RK) :: ind = 0
      REAL (KIND=RK) :: cap = 0
      REAL (KIND=RK) :: P_res = 0
      REAL (KIND=RK) :: P_ind = 0
      REAL (KIND=RK) :: P_cap = 0
      CHARACTER (LEN=MAX_LINEA) :: dispfile_TL
      REAL (KIND=RK) :: R_TL = 0
      REAL (KIND=RK) :: I_TL = 0
      REAL (KIND=RK) :: C_TL = 0
      CHARACTER (LEN=MAX_LINEA) :: dispfile_TR
      REAL (KIND=RK) :: R_TR = 0
      REAL (KIND=RK) :: I_TR = 0
      REAL (KIND=RK) :: C_TR = 0
      INTEGER (KIND=4) :: enl = 0
      INTEGER (KIND=4) :: enr = 0
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
      CHARACTER (LEN=MAX_LINEA) :: tag
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
      CHARACTER (LEN=MAX_LINEA) :: filename
      TYPE (coords), DIMENSION (:), POINTER :: cordinates => NULL ()
      REAL (KIND=RK) :: tstart, tstop, tstep
      REAL (KIND=RK) :: fstart, fstop, fstep
      INTEGER (KIND=4) :: type1, type2
      INTEGER (KIND=4) :: len_cor = 0
      CHARACTER (LEN=MAX_LINEA) :: outputrequest
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
   ! This TYPE contains the basic information in nearly all the different
   ! Traditional Probe TYPEs
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Sonda
      CHARACTER (LEN=MAX_LINEA) :: grname
      INTEGER (KIND=4), DIMENSION (:), POINTER :: i => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: j => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: K => NULL ()
      INTEGER (KIND=4), DIMENSION (:), POINTER :: node => NULL ()
      INTEGER (KIND=4) :: n_cord = 0
      INTEGER (KIND=4) :: n_cord_max = 0
      REAL (KIND=RK) :: tstart, tstop, tstep
      CHARACTER (LEN=MAX_LINEA) :: outputrequest
      !por si se precisa para el Far Field
      REAL (KIND=RK) :: fstart, fstop, fstep
      REAL (KIND=RK) :: phistart, phistop, phistep
      REAL (KIND=RK) :: thetastart, thetastop, thetastep
      CHARACTER (LEN=MAX_LINEA) :: FileNormalize
   END TYPE Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric far field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: FF_Sonda
      TYPE (Sonda) :: probe
   END TYPE FF_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: EF_Sonda
      TYPE (Sonda) :: probe
   END TYPE EF_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the magnetic field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: HF_Sonda
      TYPE (Sonda) :: probe
   END TYPE HF_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the normal electric field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NE_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE NE_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the normal magnetic field
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: NH_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE NH_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the electric surface current density
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: JS_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE JS_Sonda
   !------------------------------------------------------------------------------
   ! TYPE for the magnetic surface current density
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: MS_Sonda
      TYPE (Sonda) :: probe
      INTEGER (KIND=4), DIMENSION (:), POINTER :: nml => NULL ()
      INTEGER (KIND=4) :: n_nml = 0
      INTEGER (KIND=4) :: n_nml_max = 0
   END TYPE MS_Sonda
   !------------------------------------------------------------------------------
   ! Abstract class which performs the dynamic dispatching
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: abstractSonda
      INTEGER (KIND=4) :: n_FF = 0 !far field E
      INTEGER (KIND=4) :: n_EF = 0
      INTEGER (KIND=4) :: n_HF = 0
      INTEGER (KIND=4) :: n_NE = 0
      INTEGER (KIND=4) :: n_NH = 0
      INTEGER (KIND=4) :: n_JS = 0
      INTEGER (KIND=4) :: n_MS = 0
      !
      INTEGER (KIND=4) :: n_FF_max = 0
      INTEGER (KIND=4) :: n_EF_max = 0
      INTEGER (KIND=4) :: n_HF_max = 0
      INTEGER (KIND=4) :: n_NE_max = 0
      INTEGER (KIND=4) :: n_NH_max = 0
      INTEGER (KIND=4) :: n_JS_max = 0
      INTEGER (KIND=4) :: n_MS_max = 0
      TYPE (FF_Sonda), DIMENSION (:), POINTER :: FF => NULL ()
      TYPE (EF_Sonda), DIMENSION (:), POINTER :: EF => NULL ()
      TYPE (HF_Sonda), DIMENSION (:), POINTER :: HF => NULL ()
      TYPE (NE_Sonda), DIMENSION (:), POINTER :: NE => NULL ()
      TYPE (NH_Sonda), DIMENSION (:), POINTER :: NH => NULL ()
      TYPE (JS_Sonda), DIMENSION (:), POINTER :: JS => NULL ()
      TYPE (MS_Sonda), DIMENSION (:), POINTER :: MS => NULL ()
   END TYPE abstractSonda
   !------------------------------------------------------------------------------
   ! Class to account as a list for all the traditional probes
   ! that might be required during the parsing process
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: Sondas
      TYPE (abstractSonda), DIMENSION (:), POINTER :: probes => NULL ()
      INTEGER (KIND=4) :: n_probes = 0
      INTEGER (KIND=4) :: n_probes_max = 0
   END TYPE Sondas
   !------------------------------------------------------------------------------
   ! Object TYPE defined for the bulk current probe
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: bulkProbe
      REAL (KIND=RK) :: tstart, tstop, tstep
      REAL (KIND=RK) :: fstart, fstop, fstep
      CHARACTER (LEN=MAX_LINEA) :: FileNormalize
      INTEGER (KIND=4) :: type2
      INTEGER (KIND=4) :: i1, i2, j1, j2, k1, k2, skip
      INTEGER (KIND=4) :: nml
      LOGICAL :: t
      CHARACTER (LEN=MAX_LINEA) :: outputrequest
      CHARACTER (LEN=MAX_LINEA) :: tag
   END TYPE bulkProbe
   ! Object made for the collection of defined bulk probes
   TYPE, PUBLIC :: bulkProbes
      TYPE (bulkProbe), DIMENSION (:), POINTER :: bp => NULL ()
      INTEGER (KIND=4) :: n_bp = 0
      INTEGER (KIND=4) :: n_bp_max = 0
   END TYPE bulkProbes

   !------------------------------------------------------------------------------
   ! Object TYPE defined for the Volumic probes
   !------------------------------------------------------------------------------
   TYPE, PUBLIC :: VolProbe
      TYPE (coords), DIMENSION (:), POINTER :: cordinates => NULL ()
      REAL (KIND=RK) :: tstart, tstop, tstep
      CHARACTER (LEN=MAX_LINEA) :: outputrequest
      INTEGER (KIND=4) :: len_cor = 0
      !para freq domain
      REAL (KIND=RK) :: fstart, fstop, fstep
      INTEGER (KIND=4) ::  type2
      CHARACTER (LEN=MAX_LINEA) :: filename
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
      CHARACTER (LEN=MAX_LINEA) :: nombre_fichero
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
      CHARACTER (LEN=MAX_LINEA) :: nombre_fichero
      CHARACTER (LEN=MAX_LINEA) :: atributo
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
      CHARACTER (LEN=MAX_LINEA) :: nombre
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
      character (len=max_linea) :: switches=' '  !20221115
      !character (len=MAX_LINEA)                     ::  aEntrada !2011
      ! Basics
      TYPE (NFDEGeneral), POINTER :: general => NULL ()
      TYPE (MatrizMedios), POINTER :: matriz => NULL ()
      TYPE (Desplazamiento), POINTER :: despl => NULL ()
      TYPE (Frontera), POINTER :: front => NULL ()
      ! Materials
      TYPE (Materials), POINTER :: Mats => NULL ()
      TYPE (PECRegions), POINTER :: pecRegs => NULL ()
      TYPE (PECRegions), POINTER :: pmcRegs => NULL ()
      TYPE (NonMetalRegions), POINTER :: nMRegs => NULL ()
      TYPE (CompSurfaces), POINTER :: compSurfs => NULL ()
      TYPE (FreqDepenMaterials), POINTER :: frqDepMats => NULL ()
      TYPE (ANISOTROPICelements_t), POINTER :: aniMats => NULL ()
      ! Sources
      TYPE (Boxes), POINTER :: boxSrc => NULL ()
      TYPE (PlaneWaves), POINTER :: plnSrc => NULL ()
      TYPE (NodSource), POINTER :: nodSrc => NULL ()
      ! Probes
      TYPE (Sondas), POINTER :: traPrb => NULL ()
      TYPE (MasSondas), POINTER :: Sonda => NULL ()
      TYPE (bulkProbes), POINTER :: blkPrb => NULL ()
      TYPE (VolProbes), POINTER :: VolPrb => NULL ()
      ! Thin Elements
      TYPE (ThinWires), POINTER :: tWires => NULL ()
      TYPE (SlantedWires), POINTER :: sWires => NULL ()
      TYPE (ThinSlots), POINTER :: tSlots => NULL ()
   END TYPE Parseador
   
   !---> definicion de tipos
   TYPE, PUBLIC :: t_linea
      INTEGER (KIND=4) :: LEN
      CHARACTER (LEN=MAX_LINEA) :: dato
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