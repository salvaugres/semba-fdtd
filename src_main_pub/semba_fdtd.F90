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
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  SEMBA_FDTD LAUNCHER MODULE
!  Creation date Date :  April, 8, 2010
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! in Unix Systems checkt that ulimit -n gives high numbers (more than 2e6 files may be open at the same time!!!)
! in Ubuntu this is in /etc/security/limits.conf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

PROGRAM SEMBA_FDTD_launcher

   USE version
   USE Report
   USE Getargs
   !
   USE fdetypes
   USE Solver
   USE Resuming
   !nfde parser stuff
#ifdef CompilePrivateVersion  
   USE ParseadorClass
#endif   
   USE Preprocess
   USE storeData
   
#ifdef CompileWithXDMF
   USE xdmf_h5
#endif   
   !
#ifdef CompileWithMPI
   USE MPIcomm
   USE build_t_linea_mpi
#ifdef CompileWithStochastic
   use MPI_stochastic
#endif
#endif
   !

   !*************************************************
   !***[conformal] ******************
   !*************************************************
#ifdef CompileWithConformal
   USE CONFORMAL_INI_CLASS
   USE CONFORMAL_TOOLS
   USE CONFORMAL_MAPPED
   USE CONFORMAL_TYPES
   USE Conformal_TimeSteps_m
#endif
   use EpsMuTimeScale_m
   !*************************************************
   !*************************************************
   !*************************************************
   !
   IMPLICIT NONE
   !
   logical :: creditosyaprinteados
   logical :: hopf,experimentalVideal,forceresampled
   character (len=100) :: ficherohopf
!!!24118 pscaling
   REAL (KIND=RKIND)              ::  eps0,mu0
   REAL (KIND=RKIND)              ::  cluz
!!!241018 fin pscaling
   integer (KIND=IKINDMTAG) , allocatable , dimension(:,:,:) ::  sggMtag
   integer (KIND=INTEGERSIZEOFMEDIAMATRICES) , allocatable , dimension(:,:,:) ::  sggMiNo,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz
   character (len=5)  :: NFDEEXTENSION='.nfde',CONFEXTENSION='.conf',CMSHEXTENSION='.cmsh'
   CHARACTER (LEN=20) :: inductance_model,wiresflavor
   integer (kind=4)   :: inductance_order
   LOGICAL :: makeholes,connectendings, isolategroupgroups, dontsplitnodes,resume_fromold, pausar, l_aux,skindepthpre,groundwires,noguiffautcrecepelo ,mibc,ade,SGBC,SGBCDispersive,SGBCcrank, &
   conformalskin,CLIPREGION,boundwireradius,vtkindex,createh5bin,wirecrank,ignoreerrors,fatalerror,fatalerror_aux,dummylog,fatalerrornfde2sgg,fieldtotl,finishedwithsuccess,ignoresamplingerrors,l_auxinput, l_auxoutput, &
       ThereArethinslots
   !-------------------------------->
   CHARACTER (LEN=24) :: file_name
   LOGICAL :: ok
   CHARACTER (LEN=1024) :: dato
   !-------------------------------->
   integer (KIND=4) :: myunit,myunit11,jmed
   INTEGER (KIND=4) size, layoutnumber
   REAL (KIND=RKIND) :: ranio,rmes,rdia,attfactorc,attfactorw,cfl,cfltemp,pausetime,maxSourceValue
   REAL (KIND=8) ::time_begin,time_end
   !
   REAL (KIND=RKIND) :: dtantesdecorregir
   real (kind=RKIND_wires) :: factorradius,factordelta
   integer (kind=4) :: finaltimestepantesdecorregir,NEWfinaltimestep,thefileno
   integer (kind=4) :: precision
!
   type (nf2ff_T) :: facesNF2FF
   TYPE (Parseador), POINTER :: parser
   type (SGGFDTDINFO)   :: sgg
   TYPE (limit_t), DIMENSION (1:6) :: fullsize, SINPML_fullsize
   !
   LOGICAL :: resume, resume3, freshstart,run, forcesteps, createmap, createmapvtk, existe,MurAfterPML,mur_second,mur_first,mur_exist,forcecfl,mtlnberenger,stableradholland,NOcompomur,strictOLD, &
   TAPARRABOS,NF2FFDecim,verbose,hay_slanted_wires,existeputoconf
   REAL (KIND=RKIND) :: mindistwires,maxwireradius,SGBCFreq,SGBCresol   
   INTEGER (KIND=4) :: finaltimestep, length, status, n, i,j, p, field, donde,mpidir,SGBCdepth,newmpidir
   INTEGER (KIND=4) :: flushminutesFields, flushsecondsFields
   INTEGER (KIND=4) :: flushminutesData, flushsecondsData,idummy
   CHARACTER (LEN=1024) :: fichin = ' ', f = ' ', chain = ' ', chain2 = ' ', opcionestotales = ' ', chain3 = ' ',chain4 = ' ',  nEntradaRoot = ' ', fileFDE = ' ', fileH5 = ' ',chaindummy=' '
   CHARACTER (LEN=1024) :: licensee = ' ',nresumeable2 = ' ', prefix = ' ', geomfile = ' ', filenombre = ' '

   CHARACTER (LEN=65536) :: prefixopci = ' ', prefixopci1 = ' ',opcionespararesumeo = ' ', opcionesoriginales = ' ', &
   slicesoriginales = ' ', slices = ' ', chdummy = ' '
   CHARACTER (LEN=5) :: chari
   CHARACTER (LEN=14) :: whoami, whoamishort
   CHARACTER (LEN=1024) :: dubuf
   integer (kind=4) :: statuse

   LOGICAL :: saveall, existeNFDE,existeCONF, existeCMSH, existeh5, deleteintermediates, hayinput,createdotnfdefromdoth5=.false.,forcestop,updateshared,thereare_stoch
   integer(kind = 8)                :: my_host_id
   !
   TYPE (t_NFDE_FILE), POINTER :: NFDE_FILE
   TYPE (MedioExtra_t) :: MEDIOEXTRA
#ifdef CompileWithHDF
   LOGICAL :: thereareerrors = .FALSE.
#endif
   !
      !
   type (tagtype_t) :: tagtype
   REAL (KIND=RKIND)   ::  dxmin,dymin,dzmin,dtlay
   LOGICAL :: run_with_dmma, run_with_abrezanjas,flag_conf_sgg
   !!!!!!!PML params!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   REAL (KIND=RKIND)  ::  alphamaxpar,kappamaxpar,alphaOrden
   !!!!!!!
   integer (KIND=4) :: MAXyear, MAXmonth, MAXday,COMP_YEAR,COMP_DAY,COMP_MONTH
   integer (KIND=8) :: DIASCADUCIDAD
   integer (KIND=4) MINyear, MINmonth, MINday
   character (128) :: comp_date  
      
#ifdef CompileWithMPI
   TYPE (XYZlimit_t), DIMENSION (1:6) :: tempalloc
   INTEGER (KIND=4) :: ierr
#endif
   INTEGER (KIND=4) forced, maxCPUtime,num_nfdes,temp_numnfdes
   LOGICAL :: forcing, singlefilewrite,takeintcripte,relaunching,prioritizeCOMPOoverPEC,prioritizeISOTROPICBODYoverall,permitscaling,niapapostprocess,planewavecorr
   TYPE (tiempo_t) :: time_out2,time_comienzo
   CHARACTER (LEN=BUFSIZE) :: buff
   REAL (KIND=8) time_desdelanzamiento
   CHARACTER (LEN=1024) :: filename_h5bin ! File name

   !****************************************************************************
   !****************************************************************************
   !conformal existence flags   ref: ##Confflag##
   integer :: conf_err
#ifdef CompileWithConformal
   character (len=200) :: conformal_file_input_name=char(0);
   type (conf_conflicts_t), pointer  :: conf_conflicts
#endif
   type (EpsMuTimeScale_input_parameters_t) :: EpsMuTimeScale_input_parameters

   !****************************************************************************
   !****************************************************************************
   !****************************************************************************
!!!variables stoch
   logical :: simu_devia,stochastic,chosenyesornostochastic
!!! fin variables stoch

   
   logical :: lexis,lcreateh5filefromsinglebin,dontwritevtk
   integer :: my_iostat
!!!!!!!!!!!!!!!!comienzo instrucciones
    creditosyaprinteados=.false.
   !activate printing through screen
   CALL OnPrint
   !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!
   call EpsMuTimeScale_input_parameters%init0()

#ifdef CompileWithMPI
   CALL InitGeneralMPI (layoutnumber, size)
   SUBCOMM_MPI=MPI_COMM_WORLD !default el stochastic es el global a menos que luego se divida
#else
   size = 1
   layoutnumber = 0
#endif
    call setglobal(layoutnumber,size) !para crear variables globales con info MPI
    
   if (size.gt.maxcores) then
       print *,'Maximum cores ',maxcores,' reached.  to recompile'
       stop
   endif
       
   WRITE (whoamishort, '(i5)') layoutnumber + 1
   WRITE (whoami, '(a,i5,a,i5,a)') '(', layoutnumber + 1, '/', size, ') '

#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
   call get_secnds(time_out2)
   time_desdelanzamiento= time_out2%segundos
#ifndef keeppause
   if (layoutnumber==0) then
      OPEN (38, file='running')
      write (38,*) '!END'
      CLOSE (38,status='delete')
      OPEN (38, file='pause')
      write (38,*) '!END'
      CLOSE (38,status='delete')
      OPEN (38, file='relaunch')
      write (38,*) '!END'
      CLOSE (38,status='delete')
      OPEN (38, file='forcestop')
      write (38,*) '!END'
      CLOSE (38,status='delete')
   endif
#endif

  if (layoutnumber==0) then
      my_iostat=0
3443  if(my_iostat /= 0) write(*,fmt='(a)',advance='no'), '.' 
      OPEN (11, file='SEMBA_FDTD_temp.log',err=3443,iostat=my_iostat,action='write')
      write (11,*) '!END'
      CLOSE (11,status='delete')
      my_iostat=0
3447  if(my_iostat /= 0) write(*,fmt='(a)',advance='no'), '.' !!if(my_iostat /= 0) print '(i5,a1,i4,2x,a)',3447,'.',layoutnumber,'SEMBA_FDTD_temp.log' 
      OPEN (11, file='SEMBA_FDTD_temp.log',err=3447,iostat=my_iostat,status='new',action='write')
      call print_credits
      CLOSE (11)
  endif

#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif


652 continue

   CALL CLOSEWARNINGFILE(layoutnumber,size,dummylog,.false.,.false.) !aqui ya no se tiene en cuenta el fatalerror

   WRITE (opcionespararesumeo, '(a,i4,a)') 'mpirun -n ', size,' '
   call default_flags  !set all default flags

#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
   call get_secnds(time_comienzo)
   !temporarily until later
   IF (layoutnumber == 0) THEN
      OPEN (11, file='SEMBA_FDTD_temp.log',position='append')
      file11isopen=.true.
   END IF
   !

#ifdef CompileWithMPI
   !wait until everything comes out
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif

   !see if there is semaphore to pause continuing
   INQUIRE (file='pause', EXIST=pausar)
#ifdef CompileWithMPI
   l_aux = pausar
   CALL MPI_AllReduce (l_aux, pausar, 1_4, MPI_LOGICAL, MPI_LOR, SUBCOMM_MPI, ierr)
#endif
#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
   CALL get_secnds (time_out2)
   time_begin = time_out2%segundos
   WRITE (dubuf,*) 'Paused at              ', time_out2%fecha(7:8), '/', time_out2%fecha(5:6), '/', &
   &                time_out2%fecha(1:4), '  ', time_out2%hora(1:2), ':', time_out2%hora(3:4)
   IF (pausar) CALL print11 (layoutnumber, dubuf)
   DO while (pausar)
#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
      CALL get_secnds (time_out2)
      time_end = time_out2%segundos
      IF (time_end-time_begin > 10.0_RKIND) THEN
         INQUIRE (file='pause', EXIST=pausar)
#ifdef CompileWithMPI
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
         l_aux = pausar
         CALL MPI_AllReduce (l_aux, pausar, 1_4, MPI_LOGICAL, MPI_LOR, SUBCOMM_MPI, ierr)
         call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
         CALL get_secnds (time_out2)
         time_begin = time_out2%segundos
         WRITE (dubuf,*) 'Paused at              ', time_out2%fecha(7:8), '/', time_out2%fecha(5:6), '/', &
         &                time_out2%fecha(1:4), ' ', time_out2%hora(1:2), ':', time_out2%hora(3:4)
         IF (pausar) CALL print11 (layoutnumber, dubuf)
      END IF
   END DO
   !fin del semaphoro

#ifdef keeppause   
   INQUIRE (file='forcestop', EXIST=forcestop)
   if (forcestop) then
       if (layoutnumber==0) then
          OPEN (38, file='running')
          write (38,*) '!END'
          CLOSE (38,status='delete')
          OPEN (38, file='pause')
          write (38,*) '!END'
          CLOSE (38,status='delete')
          OPEN (38, file='relaunch')
          write (38,*) '!END'
          CLOSE (38,status='delete')
          OPEN (38, file='forcestop')
          write (38,*) '!END'
          CLOSE (38,status='delete')
       endif
#ifdef CompileWithMPI
        CALL MPI_Barrier (SUBCOMM_MPI, ierr)
        CALL MPI_FINALIZE (ierr)
#endif
        STOP
   endif
#endif

#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
   CALL get_secnds (time_out2)
   !
   ! mira el command_line y el fichero launch 251022
   CALL get_command (chain2, length, status)
   IF (status /= 0) then
      CALL stoponerror (layoutnumber, size, 'General error',.true.); goto 652
   endif

   chain2=trim(adjustl(chain2))
   !concatena con lo que haya en launch
   INQUIRE (file='launch', EXIST=hayinput)
   if (hayinput) then
      OPEN (9, file='launch', FORM='formatted',action='read')
      READ (9, '(a)') chain3
      chain3=trim(adjustl(chain3))
      CLOSE (9)
   endif
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif


   chain2=trim(adjustl(trim(adjustl(chain2))//' '//trim(adjustl(chain3))))

   call buscaswitchficheroinput(chain2,status)
   IF (status /= 0) then
       CALL stoponerror (layoutnumber, size, 'Error in searching input file. Correct and remove pause file',.true.); goto 652
   endif
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
   call print_credits 
#ifdef CompilePrivateVersion   
   call cargaNFDE
!!!!!!!!!!!!!!!!!!!!!!!
   sgg%extraswitches=parser%switches
!!!da preferencia a los switches por linea de comando
   CALL getcommandargument (chain2, 1, chaindummy, length, statuse)
#else
   print *,'The user must build by his own the nfde type'
   stop
#endif   
   
   chain2=trim(adjustl(chain2))
   chaindummy=trim(adjustl(chaindummy))
   length=len(trim(adjustl(chaindummy)))
   chain2=trim(adjustl(trim(adjustl(chaindummy))//' '//trim(adjustl(sgg%extraswitches))//' '//trim(adjustl(chain2(length+1:)))))
!!!!
   call interpretswitches(chain2,status)
!!!!tunel a lo bestia para crear el .h5 a 021219

#ifdef CompileWithXDMF   
#ifdef CompileWithHDF
   if (lcreateh5filefromsinglebin) then
     if (layoutnumber==0) then
       inquire(file=trim(adjustl(sgg%nEntradaRoot))//'_h5bin.txt',exist=lexis)
       if (.not.lexis) goto 9083
       open(newunit=myunit,file=trim(adjustl(sgg%nEntradaRoot))//'_h5bin.txt',form='formatted',err=9083) !lista de todos los .h5bin
       do 
           read (myunit,'(a)',end=84552) filename_h5bin
           call createh5filefromsinglebin(filename_h5bin,vtkindex) 
           print *, 'Processed '//trim(adjustl(filename_h5bin))
       end do
84552  close(myunit)
       print *, 'END: SUCCESS creating '//trim(adjustl(sgg%nEntradaRoot))//'_h5bin.txt'
       stop
9083   CALL stoponerror (0, size, 'Invalid _h5bin.txt file',.true.); statuse=-1; !return
     endif
#ifdef CompileWithMPI
      !wait until everything comes out
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      stop
   endif
#endif
#endif

   IF (status /= 0) then
      call print11(layoutnumber,'Remove running and pause files. If error persists check switches for error.  '//chain2,.true.)
      call print11(layoutnumber,' '); call print11(layoutnumber,' '); call print11(layoutnumber,' '); call print11(layoutnumber,' '); call print11(layoutnumber,' '); call print11(layoutnumber,' ');  goto 652
   endif
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!

   call set_priorities(prioritizeCOMPOoverPEC,prioritizeISOTROPICBODYoverall) !!! asigna las prioridades
   if (finaltimestep /= -2) then
      ! nfde part
      CALL print11 (layoutnumber, 'INIT conversion internal ASCII => Binary')
      CALL print11 (layoutnumber, SEPARADOR//SEPARADOR//SEPARADOR)

      CALL print11 (layoutnumber, SEPARADOR//SEPARADOR//SEPARADOR)
      !!!!!!!!!!!!!!!!!!!!!!
      call NFDE2sgg(fatalerrornfde2sgg)
      fatalerror=fatalerror.or.fatalerrornfde2sgg
      !!!!!!!!!!!!!!!!!!!!!
      !NOTE: md: necesito parser vivo hata el conformal ini, lo paso abajo
      ! CALL Destroy_Parser (parser)
      ! DEALLOCATE (NFDE_FILE%lineas)
      ! DEALLOCATE (NFDE_FILE)
      ! nullify (NFDE_FILE)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CompileWithMPI
      !wait until everything comes out
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      CALL print11 (layoutnumber, '[OK] Ended conversion internal ASCII => Binary')
      !release memory created by newPARSER
      if (fatalerror) then
!!intenta recuperarte
         if (allocated(sggMiEx)) deallocate (sggMiEx, sggMiEy, sggMiEz,sggMiHx, sggMiHy, sggMiHz,sggMiNo,sggMtag)
         CALL stoponerror (layoutnumber, size, 'Error in .nfde file syntax. Check all *Warnings* and *tmpWarnings* files, correct and remove pause file if any',.true.); goto 652
      endif
      !**********************************************
      !INIT DXF OUTPUT
      !
      !!!!    CALL INITdxfFILE (layoutnumber, size, nEntradaRoot)
      !************************************************
      !
      ! IF (createmap) CALL store_geomData (sgg,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, geomfile)
      ! Se hace otra vez luego (sgg 220817)

      !*************************************************************************
      !***[conformal] ******************************************
      !*************************************************************************
      !conformal conformal ini          ref: ##Confini##
#ifdef CompileWithConformal
    if (input_conformal_flag) then

         !md notes:
         ![1]      Todos los procesos parsean el archivo -conf completo.
         ![2]      El parseador es INDEPENDIENTE de del resto del problema (dimensiones,
         !         particion MPI, ... )
         ![3]      Posteriormente conf_mesh obtenido por el parseador sera tratado por cada
         !         proceso atendiedo al resto del porblema y la particion MPI

         conf_parameter%output_file_report_id = 47;
         !......................................................................
        write(dubuf,*) 'Init Searching for Conformal Mesh ...';  call print11(layoutnumber,dubuf)
#ifdef CompileWithMPI
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
         CALL conformal_ini (TRIM(conformal_file_input_name),trim(FILEFDE),parser,&
            &sgg, sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz,run_with_abrezanjas,&
            &fullsize,layoutnumber,mpidir, input_conformal_flag,conf_err,verbose)
#endif
         !......................................................................
#ifndef CompileWithMPI
         !CALL conformal_ini (TRIM(conformal_file_input_name),trim(FILEFDE),sgg,fullsize,0,conf_err,verbose)
        CALL conformal_ini (TRIM(conformal_file_input_name),trim(FILEFDE),parser,&
            &sgg, sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz,&
            &run_with_abrezanjas,fullsize,0,mpidir,input_conformal_flag,conf_err,verbose)
#endif
         if(conf_err/=0)then
            call WarnErrReport(Trim(buff),.true.)
         end if
      !NOTE: md: lo necesito despues del conformal init (antes se borraba mas arriba)
      !REVIEW: sgg

#ifdef CompilePrivateVersion           
      CALL Destroy_Parser (parser)
#endif      
      DEALLOCATE (NFDE_FILE%lineas)
      DEALLOCATE (NFDE_FILE)
      nullify (NFDE_FILE)
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CompileWithMPI
       !wait until everything comes out
       CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
         !tocado para reducir en MPI 1119 con sgg pa no se que demonios quiere
         !input_conformal_flag = .True. ! lo fuerzo para evitar deadlocks tengo que revisarlo
         !lo que sigue debe resolve los deadlocks 1119 sgg
#ifdef CompileWithMPI
         l_auxinput = input_conformal_flag
         call MPI_Barrier(SUBCOMM_MPI,ierr)
         call MPI_AllReduce( l_auxinput, l_auxoutput, 1_4, MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, ierr)
         input_conformal_flag = l_auxoutput
#endif
         !......................................................................
#ifdef CompileWithMPI
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif       
         if (resume.and.flag_conf_sgg) then
               CALL stoponerror (layoutnumber, size, 'RESUME -r currently unsupported by conformal solver',.true.); statuse=-1; !return
         end if
         if (input_conformal_flag.and.flag_conf_sgg) then
             write(dubuf,*) '----> Conformal Mesh found';  call print11(layoutnumber,dubuf)
         else   
             write(dubuf,*) '----> No Conformal Mesh found';  call print11(layoutnumber,dubuf)
         endif
    end if !FIN DEL: if (input_conformal_flag) then
    
#endif

      !*************************************************************************
      !*************************************************************************
      !*************************************************************************

#ifdef CompileWithConformal
      !*************************************************************************
      !***[conformal] ******************************************
      !*************************************************************************
      !conformal mapped reff: ##Confmapped##

      !call creamatricesdedibujoencadaslabmpi(sgg%alloc(iEx)%XI,....,sgg%Sweep(iEx)%...)

      if (input_conformal_flag) then
             write(dubuf,*) '----> Input_conformal_flag True and init';  call print11(layoutnumber,dubuf)
         call conf_geometry_mapped_for_UGRDTD (&
         &conf_conflicts, &
         &sgg,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, &
         &fullsize, SINPML_fullsize,layoutnumber,conf_err,verbose);
         !call conf_geometry_mapped_for_UGRDTD (sgg, fullsize, SINPML_fullsize,layoutnumber,conf_err,verbose); //refactor JUL15
         if(conf_err==0)then
         else
            buff=''; buff = 'Program aborted.';
            call WarnErrReport(Trim(buff),.true.)
         end if
             write(dubuf,*) '----> Input_conformal_flag True and exit';  call print11(layoutnumber,dubuf)
      end if

#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      !*************************************************************************
      !*************************************************************************
      !*************************************************************************
#endif

      !310715

      if (allocated(sggMiEx)) then !para el skindepthpre no se allocatea nada
#ifdef CompileWithConformal
        call AssigLossyOrPECtoNodes(sgg,sggMiNo,sggMiEx,sggMiEy,sggMiEz,&
                                    &conf_conflicts,input_conformal_flag)
#else
        call AssigLossyOrPECtoNodes(sgg,sggMiNo,sggMiEx,sggMiEy,sggMiEz)
#endif
        IF (createmap) CALL store_geomData (sgg,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, geomfile)
      endif
      !
#ifdef CompileWithMPI
      !wait until everything comes out
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   endif
   write(dubuf,*) '[OK] Ended Conformal Mesh';  call print11(layoutnumber,dubuf)
   if (finaltimestep==0) finaltimestep=sgg%TimeSteps !no quitar
   IF (forcesteps) then
      sgg%TimeSteps = finaltimestep
   else
      finaltimestep = sgg%TimeSteps
   endif
!aniadido correcion timesteps finales si no hay forcesteps 250417
   IF (.not.forcesteps) then
         finaltimestepantesdecorregir=finaltimestep
         finaltimestep=int(dtantesdecorregir/sgg%dt*finaltimestepantesdecorregir)
#ifdef CompileWithMPI
         call MPI_AllReduce( finaltimestep, NEWfinaltimestep, 1_4, MPI_INTEGER, MPI_MAX, SUBCOMM_MPI, ierr)
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
         finaltimestep=NEWfinaltimestep
#endif
         if (finaltimestepantesdecorregir/=finaltimestep) then
             write(dubuf,*) SEPARADOR//separador//separador
             call print11(layoutnumber,dubuf)
             write(dubuf,*) 'Original Final Time Step= ',finaltimestepantesdecorregir
             if (layoutnumber==0) call print11(layoutnumber,dubuf)
             write(dubuf,*) 'Corrected Final Time Step= ',finaltimestep
             if (layoutnumber==0) call print11(layoutnumber,dubuf)
         endif
   endif
!fin aniadido 250417
   !check that simulation can actually be done for the kind of media requested
   DO i = 1, sgg%nummedia
      IF (sgg%Med(i)%Is%ThinWire) THEN
#ifndef CompileWithWires
         CALL stoponerror (layoutnumber, size, 'Wires without wire support. Recompile!')
#endif
#ifndef CompileWithBerengerWires
    if  ((wiresflavor=='berenger')) then
         CALL stoponerror (layoutnumber, size, 'Berenger Wires without support. Recompile!')
    endif
#endif
#ifndef CompileWithSlantedWires
    if  ((wiresflavor=='guiffaut').or.(wiresflavor=='semistructured')) then
         CALL stoponerror (layoutnumber, size, 'Slanted Wires without support. Recompile!')
    endif
#endif
         CONTINUE
      END IF
      !
      IF (sgg%Med(i)%Is%EDispersive) THEN
#ifndef CompileWithEDispersives
         CALL stoponerror (layoutnumber, size, 'Edispersives without Edispersives support. Recompile!')
#endif
         CONTINUE
      END IF
      !
      IF (sgg%Med(i)%Is%MDispersive) THEN
#ifndef CompileWithEDispersives
         CALL stoponerror (layoutnumber, size, 'Mdispersives without Edispersives support. Recompile!')
#endif
         CONTINUE
      END IF
      !
      IF (sgg%Med(i)%Is%ThinSlot) THEN
#ifndef CompileWithDMMA
         CALL stoponerror (layoutnumber, size, 'Slots without Slots support. Recompile!')
#endif
#ifndef CompileWithAnisotropic
         CALL stoponerror (layoutnumber, size, 'Slots without Anisotropic support. Recompile!')
#endif
         CONTINUE
      END IF
      !
      IF (sgg%Med(i)%Is%Anisotropic) THEN
#ifndef CompileWithAnisotropic
         CALL stoponerror (layoutnumber, size, 'Anisotropics without Anisotropic support. Recompile!')
#endif
         CONTINUE
      END IF
      !
      IF ((sgg%Med(i)%Is%AnisMultiport) .OR. (sgg%Med(i)%Is%multiport).OR. (sgg%Med(i)%Is%SGBC)) THEN
#ifndef CompileWithNIBC
         if (mibc) CALL stoponerror (layoutnumber, size, 'MIBC Multiports without support. Recompile!')
#endif

#ifndef CompileWithSGBC
         if (SGBC) CALL stoponerror (layoutnumber, size, 'SGBC thin metals without support. Recompile!')
#endif
         if (.not.(mibc.or.SGBC)) &
         CALL stoponerror (layoutnumber, size, 'Choose some treatment for multiports (-mibc,-sgbc)')
         CONTINUE
      END IF
!altair no conformal sgbc 201119
#ifdef NoConformalSGBC
      IF (sgg%Med(i)%Is%SGBC .and. input_conformal_flag) THEN
         CALL stoponerror (layoutnumber, size, 'Conformal SGBC not allowed. ')
      END IF
#endif
!    
   END DO
   
   
   IF (thereare_stoch.and.(.not.chosenyesornostochastic)) THEN
      CALL stoponerror (layoutnumber, size, '!STOCH found in .nfde. Specify either -stoch or -nostoch')
   END IF
#ifndef CompileWithSlantedWires
   IF (hay_slanted_wires) THEN
      CALL stoponerror (layoutnumber, size, 'Slanted wires without slanted support. Recompile ()')
   END IF
#endif   
   IF (hay_slanted_wires .AND. ((trim(adjustl(wiresflavor))/='guiffaut').AND.(trim(adjustl(wiresflavor))/='semistructured'))) THEN
      CALL stoponerror (layoutnumber, size, 'Slanted wires require -wiresflavor guiffaut/semistructured')
   endif

   
!punietero error abrezanjas y no resume conformal !niapa 121020
          ThereArethinslots=.FALSE.
          do jmed=1,sgg%NumMedia
             if (sgg%Med(jmed)%Is%ThinSlot) ThereArethinslots=.true.
          end do
         if (resume.and.run_with_abrezanjas.and.ThereArethinslots) then   
             CALL stoponerror (layoutnumber, size, 'RESUME -r currently unsupported by conformal solver',.true.); statuse=-1; !return
         end if
!fin niapa  
   !
!!!SOME FINAL REPORTING

   if (layoutnumber==0) then
        WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
        CALL print11 (layoutnumber, dubuf)
        CALL print11 (layoutnumber, 'Solver launched with options:')
        write(dubuf,*) mibc          
        CALL print11 (layoutnumber, '---> MIBC    solver for NIBC multilayer: '//trim(adjustl(dubuf)))
        write(dubuf,*) ade         
        CALL print11 (layoutnumber, '---> ADE     solver for ADC multilayer: '//trim(adjustl(dubuf)))
        Write(dubuf,*) SGBC    
        CALL print11 (layoutnumber, '---> SGBC    solver for multilayer: '//trim(adjustl(dubuf)))
        if (SGBC) then
            write(dubuf,*) SGBCDispersive      
            CALL print11 (layoutnumber, '---> SGBC DISPERSIVE solver for multilayer: '//trim(adjustl(dubuf)))
            write(dubuf,*) SGBCCrank     
            CALL print11 (layoutnumber, '---> SGBC Crank-Nicolson solver for multilayer: '//trim(adjustl(dubuf)))
            write(dubuf,*) SGBCDepth
            CALL print11 (layoutnumber, '---> SGBC Depth: '//trim(adjustl(dubuf)))
            write(dubuf,*) SGBCFreq
            CALL print11 (layoutnumber, '---> SGBC Freq: '//trim(adjustl(dubuf)))
            write(dubuf,*) SGBCresol
            CALL print11 (layoutnumber, '---> SGBC Resol: '//trim(adjustl(dubuf)))
        endif
        write(dubuf,*) skindepthpre
        CALL print11 (layoutnumber, '---> SKINDEPTHPRE preprocessing for multilayer: '//trim(adjustl(dubuf)))
        write(dubuf,*) flag_conf_sgg
        CALL print11 (layoutnumber, '---> Conformal file external: '//trim(adjustl(dubuf)))
        write(dubuf,*) input_conformal_flag      
        CALL print11 (layoutnumber, '---> Conformal solver: '//trim(adjustl(dubuf)))
        write(dubuf,*) run_with_abrezanjas
        CALL print11 (layoutnumber, '---> Conformal thin-gap solver: '//trim(adjustl(dubuf)))
        write(dubuf,*) run_with_dmma
        CALL print11 (layoutnumber, '---> DMMA thin-gap solver: '//trim(adjustl(dubuf)))
        write(dubuf,*) wiresflavor
        CALL print11 (layoutnumber, '---> Wire model: '//trim(adjustl(dubuf)))
        write(dubuf,*) inductance_model
        CALL print11 (layoutnumber, '---> Inductance model: '//trim(adjustl(dubuf)))
        if (trim(adjustl(wiresflavor))=='berenger') then
            write(dubuf,*) mindistwires
            CALL print11 (layoutnumber, '---> Berenger minimum distance between wires: '//trim(adjustl(dubuf)))
            write(dubuf,*) mtlnberenger
            CALL print11 (layoutnumber, '---> Berenger -mtlnberenger MTLN switch: '//trim(adjustl(dubuf)))
        endif
        if (trim(adjustl(wiresflavor))=='holland') then
            write(dubuf,*) stableradholland                 
            CALL print11 (layoutnumber, '---> Holland -stableradholland automatic correction switch: '//trim(adjustl(dubuf)))
        endif
        write(dubuf,*) TAPARRABOS                
        CALL print11 (layoutnumber, '---> Thin-wire double-tails removed: '//trim(adjustl(dubuf)))
        write(dubuf,*) fieldtotl                
        CALL print11 (layoutnumber, '---> Thin-wire -fieldtotl experimental switch: '//trim(adjustl(dubuf)))
        WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
        CALL print11 (layoutnumber, dubuf)
    endif

   IF (layoutnumber == 0) THEN
      call erasesignalingfiles(simu_devia)
   endif
   
    if (layoutnumber==0) then
        
        open(newunit=thefileno,FILE = trim(adjustl(nEntradaRoot))//'_tag_paraviewfilters.txt')
            write(thefileno,'(a)') trim(adjustl('### FOR SLICE CURRENT VTK PROBES select the "current_t" or "current_f"                           '))   
            write(thefileno,'(a)') trim(adjustl('### FOR MAP VTK PROBES select the "mediatype" layer                                               '))   
            write(thefileno,'(a)') trim(adjustl('### Copy and paste the next as a programmable filter to select only one interval of tags           '))
            write(thefileno,'(a)') trim(adjustl('import vtk                                                                                        '))
            write(thefileno,'(a)') trim(adjustl('inp = self.GetInputDataObject(0, 0)                                                               '))
            write(thefileno,'(a)') trim(adjustl('outp = self.GetOutputDataObject(0)                                                                '))
            write(thefileno,'(a)') trim(adjustl('thresh = vtk.vtkThreshold()                                                                       '))
            write(thefileno,'(a)') trim(adjustl('thresh.SetInputData(inp)                                                                          '))
            write(thefileno,'(a)') trim(adjustl('thresh.SetInputArrayToProcess(0, 0, 0,vtk.vtkDataObject.FIELD_ASSOCIATION_CELLS, "tagnumber")     '))
            write(thefileno,'(a)') trim(adjustl('thresh.ThresholdBetween(64,127)                                                              '))
            write(thefileno,'(a)') trim(adjustl('thresh.Update()                                                              '))
            write(thefileno,'(a)') trim(adjustl('outp.ShallowCopy(thresh.GetOutput())    '))        
            write(thefileno,'(a)') trim(adjustl( '# Replace the thresh.ThresholdBetween numbers by tag intervals below to filter by tags           '))
            write(thefileno,'(a)')               '# ( -1e21    , -1e-3    ) '//trim(adjustl('Candidates for undesired free-space slots'))
            write(thefileno,'(a,i9,a,i9,a)')     '# (  0       ,  63      ) '//trim(adjustl('Nodal sources, etc.'))
            do i=1,tagtype%numertags
                write(thefileno,'(a,i9,a,i9,a)') '# (',i*64,' , ',i*64+63,') '//trim(adjustl(tagtype%tag(i))) !los shifteo 6 bits y les sumo 2**campo ! idea de los 3 bits de 151020
            end do
            !!
            write(thefileno,'(a)') trim(adjustl( '###    '))   
            write(thefileno,'(a)') trim(adjustl( '###    '))   
            write(thefileno,'(a)') trim(adjustl( '### FOR MAP VTK PROBES select the "mediatype" layer                                               '))   
            write(thefileno,'(a)') trim(adjustl( '### Copy and paste the next as a programmable filter to select only one types of media           '))
            write(thefileno,'(a)') trim(adjustl( 'import vtk                                                                                        '))
            write(thefileno,'(a)') trim(adjustl( 'inp = self.GetInputDataObject(0, 0)                                                               '))
            write(thefileno,'(a)') trim(adjustl( 'outp = self.GetOutputDataObject(0)                                                                '))
            write(thefileno,'(a)') trim(adjustl( 'thresh = vtk.vtkThreshold()                                                                       '))
            write(thefileno,'(a)') trim(adjustl( 'thresh.SetInputData(inp)                                                                          '))
            write(thefileno,'(a)') trim(adjustl( 'thresh.SetInputArrayToProcess(0, 0, 0,vtk.vtkDataObject.FIELD_ASSOCIATION_CELLS, "mediatype")     '))
            write(thefileno,'(a)') trim(adjustl( 'thresh.ThresholdBetween(0.0,0.5)                                                              '))
            write(thefileno,'(a)') trim(adjustl( 'thresh.Update()                                                              '))
            write(thefileno,'(a)') trim(adjustl( 'outp.ShallowCopy(thresh.GetOutput())  '))
            write(thefileno,'(a)') trim(adjustl( '# Replace the thresh.ThresholdBetween numbers by media types below to filter by media types           '))
            write(thefileno,'(a)') '# ( -100 , -100 ) '//trim(adjustl('Candidates for undesired free-space slots                               (Surface)'))
            write(thefileno,'(a)') '# (  0.0 ,  0.0 ) '//trim(adjustl('PEC                                                                     (Surface)'))
            write(thefileno,'(a)') '# (  0.5 ,  0.5 ) '//trim(adjustl('PEC                                                                     (Line)'))
            write(thefileno,'(a)') '# (  1.5 ,  1.5 ) '//trim(adjustl('Dispersive electric or magnetic isotropic or anisotropic                (Line)'))
            write(thefileno,'(a)') '# (  100 ,  199 ) '//trim(adjustl('Dispersive electric/magnetic isotropic/anisotropic (+indexmedium)       (Surface) '))
            write(thefileno,'(a)') '# (  2.5 ,  2.5 ) '//trim(adjustl('Dielectric isotropic or anisotropic                                     (Line)'))
            write(thefileno,'(a)') '# (  200 ,  299 ) '//trim(adjustl('Dielectric isotropic or anisotropic (+indexmedium)                      (Surface)'))
            write(thefileno,'(a)') '# (  3.5 ,  3.5 ) '//trim(adjustl('SGBC/MIBC Isotropic/anisotropic Multiport                               (Line)'))
            write(thefileno,'(a)') '# (  300 ,  399 ) '//trim(adjustl('SGBC/MIBC Isotropic/anisotropic Multiport (+indexmedium)                (Surface)'))
            write(thefileno,'(a)') '# (  4.5 ,  4.5 ) '//trim(adjustl('Thin slot                                                               (Line)'))
            write(thefileno,'(a)') '# (  5.0 ,  5.0 ) '//trim(adjustl('Already_YEEadvanced_byconformal                                         (Surface)'))
            write(thefileno,'(a)') '# (  5.5 ,  5.5 ) '//trim(adjustl('Already_YEEadvanced_byconformal                                         (Line)'))
            write(thefileno,'(a)') '# (  6.0 ,  6.0 ) '//trim(adjustl('Split_and_useless                                                       (Surface)'))
            write(thefileno,'(a)') '# (  6.5 ,  6.5 ) '//trim(adjustl('Split_and_useless                                                       (Line)'))
            write(thefileno,'(a)') '# (  7.0 ,  7.0 ) '//trim(adjustl('Edge Not colliding thin wires                                           (Line)'))
            write(thefileno,'(a)') '# (  8.0 ,  8.0 ) '//trim(adjustl('Thin wire segments colliding with structure                             (Line)'))
            write(thefileno,'(a)') '# (  8.5 ,  8.5 ) '//trim(adjustl('Soft/Hard Nodal CURRENT/FIELD ELECTRIC DENSITY SOURCE                   (Line)'))
            write(thefileno,'(a)') '# (  9.0 ,  9.0 ) '//trim(adjustl('Soft/Hard Nodal CURRENT/FIELD MAGNETIC DENSITY SOURCE                   (Line)'))
            write(thefileno,'(a)') '# (   10 ,   11 ) '//trim(adjustl('ENL/ENR/Ending  segment                                                 (Wire)'))
            write(thefileno,'(a)') '# (   20 ,   20 ) '//trim(adjustl('Intermediate segment +number_holland_parallel or +number_berenger       (Wire) '))
            write(thefileno,'(a)') '# (  400 ,  499 ) '//trim(adjustl('Thin slot (+indexmedium)                                                (Surface)'))
            write(thefileno,'(a)') '# ( -0.5 , -0.5 ) '//trim(adjustl('Other types of media                                                    (Line)'))
            write(thefileno,'(a)') '# ( -1.0 , -1.0 ) '//trim(adjustl('Other types of media                                                    (Surface)'))
        close(thefileno)
        
        
        
        
        !esto es el programmable filter para cargar estados con todos los materiales
!!!input = self.GetUnstructuredGridInput() 
!!!output = self.GetUnstructuredGridOutput()
!!!output.GetPointData().PassData(input.GetPointData())
!!!output.GetCellData().PassData(input.GetCellData()) 
!!!cellTypes = vtk.vtkUnsignedCharArray() 
!!!cellTypes.DeepCopy(input.GetCellTypesArray())
!!!cellTypes.SetName("Cell Types") 
!!!output.GetCellData().AddArray(cellTypes)

    endif



   ! call each simulation   !ojo que los layoutnumbers empiezan en 0
   IF (finaltimestep /= 0) THEN
#ifdef CompileWithMPI
      !wait until everything comes out
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      finishedwithsuccess=.false.
      if ((finaltimestep >= 0).and.(.not.skindepthpre)) then
         CALL launch_simulation (sgg,sggMtag,sggMiNo,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, cfl,SINPML_fullsize,fullsize, nEntradaRoot, finaltimestep, resume, saveall, &
         & makeholes,connectendings, isolategroupgroups,dontsplitnodes,stableradholland, flushsecondsFields,mtlnberenger, &
         & flushsecondsData, layoutnumber, size, createmap, inductance_model, inductance_order, maxCPUtime,time_desdelanzamiento, &
         & nresumeable2, resume_fromold,groundwires,noguiffautcrecepelo,SGBC,SGBCDispersive,mibc,attfactorc,attfactorw,&
         & alphamaxpar,alphaOrden,kappamaxpar,mur_second,MurAfterPML,MEDIOEXTRA,singlefilewrite,maxSourceValue,NOcompomur,ADE, &
         & conformalskin,strictOLD,TAPARRABOS,wiresflavor,mindistwires,facesNF2FF,NF2FFDECIM,vtkindex,createh5bin,wirecrank,opcionestotales,SGBCFreq,SGBCresol,SGBCcrank,SGBCDepth,fatalerror,fieldtotl,finishedwithsuccess,permitscaling, &
         & Eps0,Mu0, EpsMuTimeScale_input_parameters &
   , simu_devia &
   , stochastic,mpidir,verbose,precision,hopf,ficherohopf,niapapostprocess,planewavecorr,tagtype,dontwritevtk,experimentalVideal,forceresampled,factorradius,factordelta &
   )

         deallocate (sggMiEx, sggMiEy, sggMiEz,sggMiHx, sggMiHy, sggMiHz,sggMiNo,sggMtag)
      else
#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
      CALL get_secnds (time_out2)
      IF (layoutnumber == 0) THEN
          call print_credits
          WRITE (dubuf,*) 'BEGUN '//trim (adjustl(nEntradaRoot)),' at ', time_comienzo%fecha(7:8), &
          & '/', time_comienzo%fecha(5:6), '/', time_comienzo%fecha(1:4),' , ',  &
          & time_comienzo%hora(1:2), ':', time_comienzo%hora(3:4)
          CALL print11 (layoutnumber, dubuf)
          WRITE (dubuf,*) 'ENDED '//trim (adjustl(nEntradaRoot)),' at ', time_out2%fecha(7:8), &
          & '/', time_out2%fecha(5:6), '/', time_out2%fecha(1:4),' , ',  &
          & time_out2%hora(1:2), ':', time_out2%hora(3:4)
          CALL print11 (layoutnumber, dubuf)
          WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
          CALL print11 (layoutnumber, dubuf)
          CALL print11 (layoutnumber, dubuf)
       ENDIF
         !!!!!!!        CALL CLOSEdxfFILE(layoutnumber,size)
         CALL CLOSEWARNINGFILE(layoutnumber,size,dummylog,stochastic,simu_devia) !aqui ya no se tiene en cuenta el fatalerror
#ifdef CompileWithMPI
         !wait until everything comes out
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
#ifdef CompileWithMPI
         CALL MPI_FINALIZE (ierr)
#endif
         stop
      endif
   END IF
   !
#ifdef CompileWithMPI
   !wait until everything comes out
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !
   IF (layoutnumber == 0) THEN
      if (run) then
         OPEN (38, file='running')
         WRITE (38, '(a)') '!END'
         CLOSE (38,status='delete')
      endif
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) 'DONE :  ', trim (adjustl(nEntradaRoot)), ' UNTIL n=', finaltimestep
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      call erasesignalingfiles(simu_devia)

   END IF

#ifdef CompileWithMPI
   !wait until everything comes out
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !
   IF (deleteintermediates) THEN
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) 'Attempting to delete all intermediate data files'
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      INQUIRE (file=trim(adjustl(nEntradaRoot))//'_Outputrequests_'//trim(adjustl(whoamishort))//'.txt', EXIST=existe)
      IF (existe) THEN
         OPEN (19, file=trim(adjustl(nEntradaRoot))//'_Outputrequests_'//trim(adjustl(whoamishort))//'.txt')
         buscafile: DO
            READ (19, '(a)', end=76) filenombre
            IF (trim(adjustl(filenombre)) == '!END') THEN
               EXIT buscafile
            ELSE
               OPEN (34, file=trim(adjustl(filenombre)))
               WRITE (34,*) '!END'
               CLOSE (34, STATUS='delete')
            END IF
         END DO buscafile
76       CONTINUE
         CLOSE (19, STATUS='delete')
         IF (layoutnumber == 0) THEN
            OPEN (33, file=trim(adjustl(nEntradaRoot))//'_Outputlists.dat')
            WRITE (33,*) '!END'
            CLOSE (33, STATUS='delete')
         END IF
      END IF
   END IF
   !

   !**************************************************************************************************
   !***[conformal] *******************************************************************
   !**************************************************************************************************
   !delete conformal memory   reff: ##Conf_end##
#ifdef CompileWithConformal
   if(input_conformal_flag)then
      call conf_sMesh%delete
      call conf_timeSteps%delete;
      call delete_conf_tools();
   end if
#endif
   !**************************************************************************************************
   !**************************************************************************************************
   !**************************************************************************************************

#ifdef CompileWithMPI
   call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
   CALL get_secnds (time_out2)
   IF (layoutnumber == 0) THEN
      call print_credits
      WRITE (dubuf,*) 'BEGUN '//trim (adjustl(nEntradaRoot)),' at ', time_comienzo%fecha(7:8), &
      & '/', time_comienzo%fecha(5:6), '/', time_comienzo%fecha(1:4),' , ',  &
      & time_comienzo%hora(1:2), ':', time_comienzo%hora(3:4)
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) 'ENDED '//trim (adjustl(nEntradaRoot)),' at ', time_out2%fecha(7:8), &
      & '/', time_out2%fecha(5:6), '/', time_out2%fecha(1:4),' , ',  &
      & time_out2%hora(1:2), ':', time_out2%hora(3:4)
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      CALL print11 (layoutnumber, dubuf)
   ENDIF
   INQUIRE (file='relaunch', EXIST=relaunching)
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   ! Error reading check

#ifdef keeppause
   if (fatalerror) then
      fatalerror_aux=.true.
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
      call MPI_AllReduce(fatalerror_aux, fatalerror, 1_4, MPI_LOGICAL, MPI_LOR, SUBCOMM_MPI, ierr)
#else
      fatalerror = fatalerror_aux
#endif
     if (fatalerror) relaunching=.true.
#ifdef CompileWithMPI
     CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
  endif
#endif

   IF (relaunching.and.(.not.finishedwithsuccess)) THEN
      IF (layoutnumber == 0) THEN
         CALL print11 (layoutnumber, SEPARADOR//SEPARADOR)
         CALL print11 (layoutnumber, 'Not finishing solicited either manually or by an error condition. Edit of create launch file and remove pause file ')
         CALL print11 (layoutnumber, SEPARADOR//SEPARADOR)
         OPEN (9, file='pause', FORM='formatted')
         write (9, '(a)') ' '
         CLOSE (9)
         OPEN (9, file='relaunch', FORM='formatted')
         write (9, '(a)') ' '
         CLOSE (9,status='delete')
      endif
      !!!!!
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      IF (layoutnumber == 0) THEN
         CALL CloseReportingFiles
      endif
      GO TO 652
   END IF
!si ha acabado con exito sal borrando signal files
   IF (finishedwithsuccess) THEN
      IF (layoutnumber == 0) THEN
         OPEN (9, file='pause', FORM='formatted')
         write (9, '(a)') ' '
         CLOSE (9,status='delete')
         OPEN (9, file='relaunch', FORM='formatted')
         write (9, '(a)') ' '
         CLOSE (9,status='delete')
         OPEN (9, file='running', FORM='formatted')
         write (9, '(a)') ' '
         CLOSE (9,status='delete')
     endif
   endif

#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif

   IF (layoutnumber == 0) THEN
      CALL CloseReportingFiles
   endif
   !**************************************************************************************************

#ifdef CompileWithMPI
   CALL MPI_FINALIZE (ierr)
#endif
   STOP
   !

contains
!END PROGRAM SEMBA_FDTD_launcher
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine buscaswitchficheroinput(chaininput,statuse)
   CHARACTER (LEN=1024) ::  chaininput
   integer (kind=4) :: statuse

   statuse=0
   !!!!!!!!!!!!!!!
   n = commandargumentcount (chaininput)
   IF (n == 0) THEN
      call print_basic_help
      call stoponerror(layoutnumber,size,'Error: NO arguments neither command line nor in launch file. Correct and remove pause...',.true.)
      statuse=-1
      return
   END IF

   IF (n > 0) THEN
      num_nfdes=0
      i = 2
      DO while (i <= n)
         CALL getcommandargument (chaininput, i, chain, length, statuse)
         IF (statuse /= 0) THEN
            CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
            return
         END IF
         !
         SELECT CASE (trim(adjustl(chain)))
          CASE ('-mpidir')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            select case (trim (adjustl(f)))
             case ('x','X')
               mpidir=1  !!!lo cambie por error !161018
             case ('y','Y')
               mpidir=2   !!!lo cambie por error !161018
             case ('z','Z')
               mpidir=3
             CASE DEFAULT
               GOTO 1762
            END SELECT
            GO TO 2762
1762        CALL stoponerror (layoutnumber, size, 'Invalid -mpidir option',.true.)
            statuse=-1
            return
2762        CONTINUE
          CASE ('-h')
            call print_credits
            call print_help
            call print_credits
            STOP
          CASE ('-hh')
            call print_credits
            call print_help
            call print_credits
            STOP
          CASE ('-i')
            num_nfdes=num_nfdes + 1
         end select
         i = i + 1
      END DO
      if (num_nfdes > 1) then
         temp_numnfdes=0
         i = 2 ! se empieza en 2 porque el primer argumento es siempre el nombre del ejecutable
         DO while (i <= n)
            CALL getcommandargument (chaininput, i, chain, length, statuse)
             IF (statuse /= 0) THEN
                CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
                return
             END IF
            !
            SELECT CASE (trim(adjustl(chain)))
             CASE ('-i')
               temp_numnfdes=temp_numnfdes + 1
               i = i + 1
               CALL getcommandargument (chaininput, i, f, length,  statuse)
               p = LEN_trim (adjustl(f))
               IF ((p-4) >= 1) THEN
                  IF (f((p-4) :(p-4)) == NFDEEXTENSION(1:1)) THEN
                     NFDEEXTENSION= f((p-4) :p)
                     fichin = f (1:p-5)
                  ELSE
                     fichin = f (1:p)
                  END IF
               ELSE IF (p >= 1) THEN
                  fichin = f (1:p)
               ELSE
                  CALL stoponerror (layoutnumber, size, 'There is not a .nfde file for input',.true.)
                  statuse=-1
                  return
               END IF
               INQUIRE (file=trim(adjustl(fichin))//NFDEEXTENSION, EXIST=existeNFDE)
               IF ( .NOT. existeNFDE) THEN
                  buff='The input file was not found '//trim(adjustl(fichin))//NFDEEXTENSION
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  return  
               END IF
!aniadido para chequear que no haya .conf sin haber invocado el -conf 15/12/16 sgg
               INQUIRE (file=trim(adjustl(fichin))//CONFEXTENSION, EXIST=existeCONF)
               IF ((existeCONF).AND.(.not.(input_conformal_flag))) THEN
                  buff='No -conf issued but existing file '//trim(adjustl(fichin))//confEXTENSION//' . Either remove file or relaunch with -conf'
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  return  
               END IF
               INQUIRE (file=trim(adjustl(fichin))//CMSHEXTENSION, EXIST=existeCMSH)
               IF ((existeCMSH).AND.(.not.(input_conformal_flag))) THEN
                  buff='No -conf issued but existing file '//trim(adjustl(fichin))//CMSHEXTENSION//' . Either remove file or relaunch with -conf'
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  return  
               END IF
!
               if (temp_numnfdes ==1) then !solo el primero
                  IF (layoutnumber == 0) open (194,file='multi_'//trim(adjustl(fichin))//NFDEEXTENSION,form='formatted')
               endif
               IF (layoutnumber == 0) then
                  open (196,file=trim(adjustl(fichin))//NFDEEXTENSION,form='formatted')
                  do
                     read (196,'(a)',end=197) dato
                     if (trim(adjustl(dato)) /= '!END') then
                        write(194,'(a)') trim(adjustl(dato))
                     else
                        dato='***** End merging file: '//trim(adjustl(fichin))//NFDEEXTENSION//' ********'
                        write(194,'(a)') trim(adjustl(dato))
                     endif
                  end do
197               close(196)
               endif
               if (temp_numnfdes == num_nfdes) then !solo el primero
                  IF (layoutnumber == 0) then
                     write(194,'(a)') '!END'
                     close (194)
                  endif
               endif
            end select
            i = i + 1
         END DO
      endif
   endif
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !
   !concatenado multiples ORIGINAL 26/06/14
   
   !fin concatenado

   temp_numnfdes=0
   IF (n > 0) THEN
   i = 2  ! se empieza en 2 porque el primer argumento es siempre el nombre del ejecutable
      DO while (i <= n)
         CALL getcommandargument (chaininput, i, chain, length, statuse)
         IF (statuse /= 0) THEN
            CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
            return
         END IF
         !
         SELECT CASE (trim(adjustl(chain)))
            !
          CASE ('-i')
            temp_numnfdes=temp_numnfdes + 1
            i = i + 1
            if (temp_numnfdes == 1) then
               !
               CALL getcommandargument (chaininput, i, f, length,  statuse)
               p = LEN_trim (adjustl(f))
               IF ((p-4) >= 1) THEN
                  IF (f((p-4) :(p-4)) == NFDEEXTENSION(1:1)) THEN
                     NFDEEXTENSION= f((p-4) :p)
                     fichin = f (1:p-5)
                  ELSE
                     fichin = f (1:p)
                  END IF
               ELSE IF (p >= 1) THEN
                  fichin = f (1:p)
               ELSE
                  CALL stoponerror (layoutnumber, size, 'There is not a .nfde file for input',.true.)
                  statuse=-1
                  return
               END IF
               INQUIRE (file=trim(adjustl(fichin))//NFDEEXTENSION, EXIST=existeNFDE)
               IF ( .NOT. existeNFDE) THEN
                  buff='The input file was not found '//trim(adjustl(fichin))//NFDEEXTENSION
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  return
               END IF
            elseif (temp_numnfdes == 2) then
               fichin='multi_'//trim(adjustl(fichin))
            else
               fichin=fichin
            endif
            !!!          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
         END SELECT
         i = i + 1
      END DO
   endif
      !
    ! If no input is present we stop
    IF (len(trim(adjustl(fichin))) <= 0) THEN
        CALL stoponerror (layoutnumber, size, 'ERROR! -> No input file was specified. Use -i ****.nfde',.true.); statuse=-1; return
    END IF

   fileFDE = trim (adjustl(fichin)) // NFDEEXTENSION
   fileH5 = trim (adjustl(fichin)) // '.h5'
   CALL INITWARNINGFILE (layoutnumber, size, trim (adjustl(fichin))//'_tmpWarnings.txt',verbose,ignoreErrors)
   return
  end subroutine buscaswitchficheroinput

!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!

   subroutine interpretswitches(chaininput,statuse)
   CHARACTER (LEN=1024) ::  chaininput
   integer (kind=4) :: statuse
   logical :: existiarunningigual,mpidirset
   mpidirset=.false.
   existiarunningigual=.false.
   statuse=0
   !!!!!!!!!!!!!!!
   n = commandargumentcount (chaininput)
   IF (n == 0) THEN
      call print_basic_help
      call stoponerror(layoutnumber,size,'Error: NO arguments neither command line nor in launch file. Correct and remove pause...',.true.)
      statuse=-1
      !return
   END IF
   opcionestotales=''
   do i=2,n
      CALL getcommandargument (chaininput, i, chain, length, statuse)
      IF (statuse /= 0) THEN
         CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
          statuse=-1
          !return
      END IF
      opcionestotales=trim(adjustl(opcionestotales))//' '//trim(adjustl(chain))
   end do
   CALL print11 (layoutnumber, 'Switches '//trim(adjustl(opcionestotales)))

 
   IF (n > 0) THEN
      i = 2  ! se empieza en 2 porque el primer argumento es siempre el nombre del ejecutable
      DO while (i <= n)
         CALL getcommandargument (chaininput, i, chain, length, statuse)
         IF (statuse /= 0) THEN
            CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
          statuse=-1
          !return
         END IF
         SELECT CASE (trim(adjustl(chain)))
          CASE ('-i')
               i = i + 1
               CALL getcommandargument (chaininput, i, f, length,  statuse)
             continue !ya interpretado
          case ('-a')
               i = i + 1
               CALL getcommandargument (chaininput, i, f, length,  statuse)
             continue !ya interpretado
          CASE ('-mpidir')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            select case (trim (adjustl(f)))
             case ('x','X')
               newmpidir=1  !!!lo cambie por error !161018
             case ('y','Y')
               newmpidir=2   !!!lo cambie por error !161018
             case ('z','Z')
               newmpidir=3
             CASE DEFAULT
               GOTO 1762
            END SELECT 
            if (newmpidir.ne.mpidir) then
               GOTO 1762
            endif
            GO TO 2762
1762        CALL stoponerror (layoutnumber, size, 'Invalid or duplicate incoherent -mpidir option',.true.)
          statuse=-1
          return
2762      CONTINUE
          if (.not.mpidirset) then
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
            mpidirset=.true.
          endif

          case ('-pause')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=7312) pausetime
            GO TO 8312
7312        CALL stoponerror (layoutnumber, size, 'Invalid pause time',.true.)
          statuse=-1
          !return
8312        IF (pausetime <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pause time',.true.)
          statuse=-1
          !return
            END IF
            !
            pausar=.true.
#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
            CALL get_secnds (time_out2)
            time_begin = time_out2%segundos
            WRITE (dubuf,*) 'Paused for (secs) ', pausetime
            CALL print11 (layoutnumber, dubuf)
            DO while (pausar)
#ifdef CompileWithMPI
      call MPI_Barrier(SUBCOMM_MPI,ierr)
#endif
               CALL get_secnds (time_out2)
               time_end = time_out2%segundos
               IF (time_end-time_begin > pausetime) THEN
                  pausar =.false.
               END IF
            END DO
#ifdef CompileWithMPI
            CALL MPI_Barrier (SUBCOMM_MPI, ierr)
            l_aux = pausar
            CALL MPI_AllReduce (l_aux, pausar, 1_4, MPI_LOGICAL, MPI_LOR, SUBCOMM_MPI, ierr)
#endif

            !!!        CASE ('-maxmessages')
            !!!            i = i + 1
            !!!          CALL getcommandargument (chaininput, i, f, length,  statuse)
            !!!          READ (f,*, ERR=1012) maxmessages
            !!!          GO TO 2012
            !!!1012      CALL stoponerror (layoutnumber, size, 'Invalid Number of maxmessages',.true.)
          !!!statuse=-1
          !!!!return
            !!!2012      CONTINUE
            !!!          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))

          CASE ('-NF2FFdecim')
            NF2FFDecim=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-noNF2FF')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            select case (trim (adjustl(f)))
             case ('back','BACK')
               facesNF2FF%TR=.FALSE.
             case ('front','FRONT')
               facesNF2FF%FR=.FALSE.
             case ('left','LEFT')
               facesNF2FF%IZ=.FALSE.
             case ('right','RIGHT')
               facesNF2FF%DE=.FALSE.
             case ('down','DOWN')
               facesNF2FF%AB=.FALSE.
             case ('up','UP')
               facesNF2FF%AR=.FALSE.
             CASE DEFAULT
               GOTO 1712
            END SELECT
            GO TO 2712
1712        CALL stoponerror (layoutnumber, size, 'Invalid -noNF2FF option',.true.)
          statuse=-1
          !return
2712        CONTINUE
            !COMO LA RCS SE CALCULA SOLO AL FINAL NO OBLIGO A RESUMEAR CON IGUAL -NONFF2FF PARA PODER CALCULAR CON Y SIN ESTA OPCION resumeando
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-force')
            forcing = .TRUE.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f,*, ERR=412) forced
            GO TO 312
412         CALL stoponerror (layoutnumber, size, 'Invalid cut',.true.)
          statuse=-1
          !return
312         CONTINUE
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-singlefile')
            singlefilewrite = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-r')
            resume = .TRUE.
            forcesteps=.true.
#ifdef CompileWithOldSaving
          CASE ('-old')
            resume_fromold = .TRUE.
#endif
          CASE ('-cpumax')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=712) maxCPUtime
            GO TO 812
712         CALL stoponerror (layoutnumber, size, 'Invalid CPU maximum time',.true.)
          statuse=-1
          !return
812         IF (maxCPUtime <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPU maximum time',.true.)
          statuse=-1
          !return
            END IF
          CASE ('-run')
            run = .TRUE.
          CASE ('-s')
            freshstart = .TRUE.
          CASE ('-flush')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=300) flushminutesFields
            GO TO 400
300         CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !return
400         IF (flushminutesFields <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !return
            END IF
          CASE ('-flushdata')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=301) flushminutesData
            GO TO 401
301         CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !return
401         IF (flushminutesData <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !return
            END IF
          CASE ('-map')
            !dump the map files
            createmap = .TRUE.
!Gamusiono de las narices

          CASE ('-verbose')
            !dump the map files
            verbose = .TRUE.
          CASE ('-mapvtk')
            !dump the map files
#ifdef CompileWithVTK   
            createmapvtk = .TRUE.
#else
            createmapvtk = .FALSE.
#endif
#ifndef compileWithGamusino
          CASE ('-dontwritevtk')
            dontwritevtk=.true.
          CASE ('-vtkindex')
            vtkindex = .TRUE.
          CASE ('-ignoreerrors')
            ignoreerrors = .TRUE.
          CASE ('-ignoresamplingerrors')
            ignoresamplingerrors = .TRUE.
          CASE ('-prioritizeCOMPOoverPEC')
            prioritizeCOMPOoverPEC=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
            ignoreerrors = .TRUE.
          CASE ('-noshared')
            updateshared=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-prioritizeISOTROPICBODYoverall')
            prioritizeISOTROPICBODYoverall=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-wirecrank')
            wirecrank = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-clip')
            CLIPREGION = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))

!endif del compileWithGamusino
#endif 

!
          CASE ('-hopf')
            hopf=.true.
            i = i + 1;
            ficherohopf = char(0);
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ficherohopf = trim(adjustl(f));
            INQUIRE (file=trim(adjustl(f)), EXIST=existeNFDE)
            IF ( .NOT. existeNFDE) THEN
               hopf = .FALSE.;
               buff = 'The Hopf input file was not found '//trim(adjustl(ficherohopf));
               call WarnErrReport(Trim(buff),.true.)
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
!
#ifdef CompileWithDMMA
          CASE ('-dmma')
              run_with_dmma = .TRUE.
              run_with_abrezanjas = .FALSE.
              opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
!!              i = i + 1;
#endif
#ifdef CompileWithConformal
          CASE ('-abrezanjas') !Provisional FEB-2018

            !NOTE: Comento lo de abajo para debugear 
            !   print *,'Abrezanjas not available (290521).  '
            !   stop

            run_with_dmma = .FALSE.
            run_with_abrezanjas = .true.
            if (.NOT.input_conformal_flag) then
                conformal_file_input_name = char(0)
                input_conformal_flag = .true.
 !!               input_conformal_flag_abrezanjas = .true.;
            end if
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))

!!            i = i + 1;
          CASE ('-activateconf') !Provisional FEB-2018
            if (.NOT.input_conformal_flag) then
                conformal_file_input_name = char(0)
                input_conformal_flag = .true.
            end if
            i = i + 1;      
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))      
#endif            
          CASE ('-conf')
            flag_conf_sgg=.true.
            i = i + 1;
#ifdef CompileWithConformal              
            input_conformal_flag = .true.;
            conformal_file_input_name = char(0);

            CALL getcommandargument (chaininput, i, f, length,  statuse)
            conformal_file_input_name = trim(adjustl(f));

            INQUIRE (file=trim(adjustl(f)), EXIST=existeNFDE)
            IF ( .NOT. existeNFDE) THEN
               input_conformal_flag = .FALSE.;
               buff = 'The conformal input file was not found '//trim(adjustl(fichin));
               call WarnErrReport(Trim(buff),.true.)
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#endif
#ifndef CompileWithConformal
            IF (input_conformal_flag)THEN
               buff='';buff='Conformal without conformal support. Recompile!';
               call WarnErrReport(Trim(buff),.true.)
            END IF
#endif

          CASE ('-takeintcripte')
            takeintcripte=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#ifdef CompileWithNIBC
          CASE ('-skindepthpre')
            skindepthpre=.true.
!            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-mibc','-skindepth')
            mibc=.true.
            SGBC=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-conformalskin')
            conformalskin=.true.
            mibc=.true.
            SGBC=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-ade')
            ade=.true.
            mibc=.true.
            SGBC=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-nocompomur')
            NOcompomur=.true.
            mibc=.true.
            SGBC=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#endif
          CASE ('-mur2')
            MurAfterPML=.true.
            !mur_second=.true.
            mur_first=.true.
            !arreglar cuando resuelva el bug en mur segundo orden
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-mur1')
            MurAfterPML=.true.
            mur_first=.true.
            mur_second=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-pmlalpha')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7621) alphamaxpar
            GO TO 8621
7621        CALL stoponerror (layoutnumber, size, 'Invalid CPML alpha factor',.true.)
          statuse=-1
          !return
8621        IF (alphamaxpar < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML alpha factor',.true.)
          statuse=-1
          !return
            END IF
            i = i + 1
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7121) alphaOrden
            GO TO 8121
7121        CALL stoponerror (layoutnumber, size, 'Invalid CPML order factor',.true.)
          statuse=-1
          !return
8121        IF (alphaOrden < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML alpha factor',.true.)
          statuse=-1
          !return
            END IF
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-pmlkappa')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7622) kappamaxpar
            GO TO 8622
7622        CALL stoponerror (layoutnumber, size, 'Invalid CPML kappa factor',.true.)
          statuse=-1
          !return
8622        IF (kappamaxpar < 1.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML kappa factor',.true.)
          statuse=-1
          !return
            END IF
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-pmlcorr')
            medioextra%exists=.true.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7672) medioextra%sigma
            GO TO 8672
7672        CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr sigma factor',.true.)
          statuse=-1
          !return
8672        IF (medioextra%sigma < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr sigma factor',.true.)
          statuse=-1
          !return
            END IF
            medioextra%sigmam=-1.0_RKIND!voids it. later overriden
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7662) MEDIOEXTRA%size
            GO TO 8662
7662        CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr depth factor',.true.)
          statuse=-1
          !return
8662        IF (MEDIOEXTRA%size < 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr depth factor',.true.); statuse=-1; !return
            END IF
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-attc')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=766) attfactorc
            GO TO 866
766         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !return
866         IF ((attfactorc <= -1.0_RKIND ).or.(attfactorc > 1.0_RKIND)) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-sgbcdepth')
            mibc=.false.
            SGBC=.true.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7466) SGBCdepth
            GO TO 8466
7466        CALL stoponerror (layoutnumber, size, 'Invalid SGBC depth ',.true.); statuse=-1; !return
8466        IF (SGBCdepth < -1 ) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid SGBC depth',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-sgbcfreq')
            SGBC=.true.
            mibc=.false.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=74616) SGBCfreq
            GO TO 84616
74616       CALL stoponerror (layoutnumber, size, 'Invalid SGBC freq ',.true.); statuse=-1; !return
84616       IF (SGBCfreq < 0. ) THEN
            CALL stoponerror (layoutnumber, size, 'Invalid SGBC freq',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-sgbcresol')
            mibc=.false.
            SGBC=.true.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=74626) SGBCresol
            GO TO 84626
74626       CALL stoponerror (layoutnumber, size, 'Invalid SGBC decay ',.true.); statuse=-1; !return
84626       IF (SGBCresol < 0.0 ) THEN
            CALL stoponerror (layoutnumber, size, 'Invalid SGBC decay',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-sgbcyee')
            SGBC=.true.
            mibc=.false.
            SGBCcrank=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-sgbccrank') !es el default. Lo mantengo por compatibilidad con lanzamientos previos
            sgbccrank=.true.
            mibc=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-nosgbc') !opcion generica que aglutina varios switches que estan den default (sgbcresol, sgbccrank, sgbcfreq)
            sgbc=.false.
            mibc=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-sgbc') !opcion generica que aglutina varios switches que estan den default (sgbcresol, sgbccrank, sgbcfreq)
            sgbc=.true.
            mibc=.false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-sgbcdispersive') !opcion generica que aglutina varios switches que estan den default (sgbcresol, sgbccrank, sgbcfreq)
            sgbc=.true.
            mibc=.false.
            sgbcDispersive=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-saveall')
            saveall = .TRUE.
#ifdef CompileWithWires
          CASE ('-attw')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=732) attfactorw
            GO TO 832
732         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !return
832         IF ((attfactorw <= -1.0_RKIND ).or.(attfactorw > 1.0_RKIND)) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-maxwireradius')
            boundwireradius=.true.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=737) maxwireradius
            GO TO 837
737         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !return
837         IF ((maxwireradius <= 0.0_RKIND )) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid maximumwireradius',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-mindistwires')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=1732) mindistwires
            GO TO 1832
1732        CALL stoponerror (layoutnumber, size, 'Invalid minimum distance between wires',.true.); statuse=-1; !return
1832        IF (mindistwires <= 0.0_RKIND ) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid minimum distance between wires',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            !!CASE ('-wiresverbose')
            !!  verbose = .TRUE.
          CASE ('-makeholes')
            makeholes = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-connectendings')
            connectendings = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-nostrictOLD')
            strictOLD = .false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-nomtlnberenger')
            mtlnberenger = .false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-stableradholland')
            stableradholland = .true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-mtln')
              buff='-mtln option deprecated and ignored. Check -nomtlnberenger or -stableradholland'
              call WarnErrReport(Trim(buff),.false.)
          CASE ('-intrawiresimplify')
            strictOLD = .false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-notaparrabos')
            TAPARRABOS = .false.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          case ('-fieldtotl')
              fieldtotl=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          case ('-experimentalVideal')
              experimentalVideal=.true.
              opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          case ('-forceresampled') !a menos que se pida explicitamente, no se resamplea 120123
              forceresampled=.true.
              opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-wiresflavor')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
            READ (f, '(a)', ERR=3621) wiresflavor
            select case (trim(adjustl(wiresflavor)))
            case ('holland','old')
                wiresflavor='holland'
            case ('berenger','new')
                wiresflavor='berenger'
            case ('guiffaut','experimental','slanted')
                wiresflavor='guiffaut'
            case ('transition')
                wiresflavor='transition'
            case ('semistructured')
                wiresflavor='semistructured'
                !
                i = i + 1
                CALL getcommandargument (chaininput, i, f, length,  statuse)
                opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(f))
            ! Converts the characters to real
                READ (f,*, ERR=2561) precision
                GO TO 2562
2561            CALL stoponerror (layoutnumber, size, 'Invalid precision for semistructured',.true.); statuse=-1; !return
2562            IF (precision < 0 ) THEN
                    CALL stoponerror (layoutnumber, size, 'Invalid precision for semistructured',.true.); statuse=-1; !return
                END IF
            !   
            end select   
            GO TO 4621
3621        CALL stoponerror (layoutnumber, size, 'Invalid wires flavor',.true.); statuse=-1; !return
4621        IF ( ((trim(adjustl(wiresflavor)) /= 'holland')  .AND. &
                  (trim(adjustl(wiresflavor)) /= 'transition') .AND. &
                  (trim(adjustl(wiresflavor)) /= 'berenger') .AND.  &
                  (trim(adjustl(wiresflavor)) /= 'guiffaut').and. &
                  (trim(adjustl(wiresflavor))/='semistructured')) .or. &
             .not.((trim(adjustl(wiresflavor)) == 'holland')  .xor.  &
                  (trim(adjustl(wiresflavor)) == 'transition') .xor. &
                  (trim(adjustl(wiresflavor)) == 'berenger') .xor.  &
                  (trim(adjustl(wiresflavor)) == 'guiffaut').xor.  &
                  (trim(adjustl(wiresflavor)) =='semistructured')) )  THEN
               CALL stoponerror (layoutnumber, size, 'Invalid wires flavor->'//trim(adjustl(wiresflavor)),.true.); statuse=-1; !return
            END IF
#ifndef CompileWithWires
            select case (trim(adjustl(wiresflavor)))
            case ('holland','transition')
                CALL stoponerror (layoutnumber, size, 'Holland wire flavor not available in this compilation',.true.); statuse=-1; !return
            end select   
#endif
#ifndef CompileWithBerengerWires
            select case (trim(adjustl(wiresflavor)))
            case ('berenger')
                CALL stoponerror (layoutnumber, size, 'Berenger wire flavor not available in this compilation',.true.); statuse=-1; !return
            end select   
#endif
#ifndef CompileWithSlantedWires
            select case (trim(adjustl(wiresflavor)))
            case ('guiffaut','experimental')
                CALL stoponerror (layoutnumber, size, 'Experimental wire flavor not available in this compilation',.true.); statuse=-1; !return
            end select   
#endif
          CASE ('-isolategroupgroups')
            isolategroupgroups = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
            !!CASE ('-dontsplitnodes')
            !!  dontsplitnodes = .TRUE.
            !!  opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-groundwires')
            groundwires = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-noguiffautcrecepelo ') !opcion niapa excperimental 131219
            noguiffautcrecepelo  = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-inductance')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f, '(a)', ERR=361) inductance_model
            GO TO 461
361         CALL stoponerror (layoutnumber, size, 'Invalid inductance model',.true.); statuse=-1; !return
461         IF ((inductance_model /= 'ledfelt') .AND. (inductance_model /= 'berenger') .AND. &
            &    (inductance_model /= 'boutayeb')) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid inductance model',.true.); statuse=-1; !return
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-inductanceorder')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f,*, ERR=179) inductance_order
            GO TO 180
179         CALL stoponerror (layoutnumber, size, 'Invalid inductance order',.true.); statuse=-1; !return
180         opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
#endif
          CASE ('-prefix')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            prefix = '_' // trim (adjustl(f))
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-cfl')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=3762) cfltemp
            GO TO 3862
3762        CALL stoponerror (layoutnumber, size, 'Invalid Courant Number',.true.); statuse=-1; !return
3862        IF (cfltemp <= 0.0 ) THEN
               call print11(layoutnumber,'------> Ignoring negative or null CFL Courant Number')
!!!!!!!!!               CALL stoponerror (layoutnumber, size, 'Invalid negative or null CFL Courant Number',.true.); statuse=-1; !return !!!sgg 151216 para evitar el error cfl 0 del problem-type sigue como si no estuviera
               forcecfl=.false.
            else
               cfl=cfltemp
               forcecfl=.true.
               opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            END IF
          CASE ('-niapapostprocess')
            niapapostprocess=.true.
          CASE ('-planewavecorr')
            planewavecorr=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#ifdef CompileWithPrescale
!!!!210918 permit scaling
          CASE ('-pscale')
            permitscaling=.true.
            saveall=.true. !lo salvo todo en permit scaling para evitar errores
            i = i + 1
            buff=""
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f,*, ERR=33762) buff
            EpsMuTimeScale_input_parameters%electric=.False.
            EpsMuTimeScale_input_parameters%electric=.False.
            Select case (trim(adjustl(buff)))
            case("ee")
               EpsMuTimeScale_input_parameters%electric=.True.
            case("hh")
               EpsMuTimeScale_input_parameters%magnetic=.True.
            case("eh": "he")
               EpsMuTimeScale_input_parameters%electric=.True.
               EpsMuTimeScale_input_parameters%magnetic=.True.
            case default
               GO TO 33862
            end select
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            ! Converts the characters to real
            READ (f,*, ERR=33762) EpsMuTimeScale_input_parameters%tini
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(f))
            READ (f,*, ERR=33762) EpsMuTimeScale_input_parameters%tend
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(f))
            READ (f,*, ERR=33762) EpsMuTimeScale_input_parameters%alpha_max
            GO TO 33862
33762       CALL stoponerror (layoutnumber, size, 'Invalid pscale parameters',.true.); statuse=-1; !return
33862       IF (EpsMuTimeScale_input_parameters%checkError()/=0) THEN
                CALL stoponerror (layoutnumber, size, &
   &'Invalid -pscale parameters: some parameters have to be greater than 0.0: -pscale t0(>=0) tend slope(>0)'&
                  &,.true.); statuse=-1; !return
            else
               EpsMuTimeScale_input_parameters%are_there = .true.
            endif
#endif       
          CASE ('-n')
            forcesteps = .TRUE.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=602) finaltimestep
            GO TO 702
602         CALL stoponerror (layoutnumber, size, 'Invalid time step',.true.); statuse=-1; !return
702         IF (finaltimestep < -2) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid time step',.true.); statuse=-1; !return
            END IF      
!!!!!!     
          CASE ('-factorradius')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=6032) factorradius
            GO TO 7032
6032         CALL stoponerror (layoutnumber, size, 'Invalid factorradius',.true.); statuse=-1; !return
7032         continue
          CASE ('-factordelta')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=6072) factordelta
            GO TO 7072
6072         CALL stoponerror (layoutnumber, size, 'Invalid factordelta',.true.); statuse=-1; !return
7072         continue
!!!!!!!!!!!!!
          CASE ('-stoch')
            stochastic=.true.
            chosenyesornostochastic=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#ifndef CompileWithMPI
            CALL stoponerror (layoutnumber, size, 'Stochastic simulation unsupported without MPI compilation',.true.); statuse=-1; !return
#endif
          CASE ('-nostoch')
            stochastic=.false.
            chosenyesornostochastic=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))         
          case ('-forcecreateh5bin')         
            createh5bin=.true.
          CASE ('') !100615 para evitar el crlf del .sh
            continue
          CASE DEFAULT
            CALL stoponerror (layoutnumber, size, 'Wrong switch '//trim(adjustl(chain)),.true.); statuse=-1; !return
         END SELECT
         i = i + 1
      END DO

   END IF
   !some checkings
   !just to be sure that I do not have stupid errors
#ifdef CompileWithMPI
   IF ((sizeof(MPI_DOUBLE_PRECISION) /= 4) .OR. (sizeof(MPI_REAL) /= 4)) THEN
      CALL stoponerror (layoutnumber, size, 'SEVERE COMPILATION ERROR: MPI_REAL size is not 4. ')
   END IF
#endif

#ifdef CompileWithDMMA
#ifndef CompileWithAnisotropic
   CALL stoponerror (layoutnumber, size, 'ERROR: DMMA without Anisotropic support. Recompile!')
#endif
#endif

   IF (connectendings .AND. strictOLD) THEN
      CALL stoponerror (layoutnumber, size, 'strictOLD option not compatible with -connectendings',.true.); statuse=-1; !return
   END IF
   IF (taparrabos .AND. (.not.strictOLD)) THEN
      CALL stoponerror (layoutnumber, size, '-nostrictOLD option requires -notaparrabos ',.true.); statuse=-1; !return
   END IF
   IF (isolategroupgroups .AND. strictOLD) THEN
      CALL stoponerror (layoutnumber, size, '-intrawiresimplify option not compatible with -isolategroupgroups',.true.); statuse=-1; !return
   END IF

   IF ((sgbc .AND. mibc)) THEN
      CALL stoponerror (layoutnumber, size, 'Use only one of -sgbc -mibc',.true.); statuse=-1; !return
   END IF
   IF (freshstart .AND. resume) THEN
      CALL stoponerror (layoutnumber, size, 'Fresh Start option -s not compatible with restarting -r',.true.); statuse=-1; !return
   END IF
   IF (freshstart .AND. resume_fromold) THEN
      CALL stoponerror (layoutnumber, size, 'Fresh Start option -s not compatible with -old',.true.); statuse=-1; !return
   END IF
   IF (( .NOT. resume).and.(.not.run) .AND. resume_fromold) THEN
      CALL stoponerror (layoutnumber, size, 'Resume option -r must be used if issuing -old',.true.); statuse=-1; !return
   END IF
   IF ((flushminutesFields /= 0) .AND. (deleteintermediates)) THEN
      CALL stoponerror (layoutnumber, size, '-delete is not compatible with -flush',.true.); statuse=-1; !return
   END IF
   if (run_with_abrezanjas.and.run_with_dmma) then
      CALL stoponerror (layoutnumber, size, '-abrezanjas is not compatible with -dmma',.true.); statuse=-1; !return
   END IF
   if (stochastic.and.(trim(adjustl(wiresflavor))/='holland')) then
      CALL stoponerror (layoutnumber, size, 'Old wires flavor is the only supported with stochastic',.true.); statuse=-1; !return
   END IF
   if (stochastic.and.wirecrank) then
      CALL stoponerror (layoutnumber, size, 'Wires Crank Nicolson is unsupported with stochastic',.true.); statuse=-1; !return
   END IF
   !!!si esta soportado 170719
   !! if (permitscaling.and.resume) then
   !!   CALL stoponerror (layoutnumber, size, 'Resuming with Permittivity scaling unsupported',.true.); statuse=-1; !return
   !!END IF
   if (permitscaling.and.(kappamaxpar.gt.1.000001_rkind)) then
   !!!061118 no lo permito porque cpml toca los idxe, idye, idze en funcion del kappa y permittivity scaling conflicta
            CALL stoponerror (layoutnumber, size, 'Unsupported CPML kappa factor since 061118 because conflicts with Idxe...in permittivity scaling',.true.)
   endif
   if (stochastic) then
#ifndef CompileWithStochastic
        call StopOnError(layoutnumber,size,'Stochastic without compilation support. Recompile')
#endif
#ifdef CompileWithStochastic
#ifndef CompileWithMPI
        call StopOnError(layoutnumber,size,'Stochastic unsupported without MPI compilation. Recompile')
#endif
#endif 
        continue
   endif
   
!!!
       
   !
   prefixopci1=trim (adjustl(opcionespararesumeo))
   prefixopci=' '
   do i=1,len(trim (adjustl(prefixopci1)))
      prefixopci(i:i)=prefixopci1(i:i)
      j=ichar(prefixopci1(i:i))
      if (j <= 47) prefixopci(i:i)='_'
      if (j >= 123) prefixopci(i:i)='_'
      if ((j >= 58).and.(j <= 64)) prefixopci(i:i)='_'
      if ((j >= 91).and.(j <= 96)) prefixopci(i:i)='_'
      if (j == 46) prefixopci(i:i)='p'
   end do

   do i=1,len(trim (adjustl(prefixopci)))
      do while (prefixopci(i:i+1) =='__')
         prefixopci(i:)=prefixopci(i+1:)
      end do
   end do
   if (prefix(1:1)=='_') then
     !!!acortado 120219  nEntradaRoot = trim (adjustl(fichin)) // trim (adjustl(prefix))// trim (adjustl(prefixopci))
      nEntradaRoot = trim (adjustl(fichin)) //'_'// trim (adjustl(prefixopci))
   else
      nEntradaRoot = trim (adjustl(fichin))
   endif
!!!stochastic
#ifdef CompileWithStochastic
  if (stochastic) then  
    if (layoutnumber <= size/2-1) then !aun no se ha dividido el size
          nEntradaRoot=trim (adjustl(nEntradaRoot))
    else
          nEntradaRoot=trim (adjustl('devia_'//trim (adjustl(nEntradaRoot))))
    endif
  endif
#ifdef CompileWithMPI
  CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
#endif
!!!fin stochastic
   sgg%nEntradaRoot=trim (adjustl(nEntradaRoot))
   !
   WRITE (chari, '(i5)') layoutnumber + 1
   nresumeable2 = trim (adjustl(nEntradaRoot)) // '_' // trim (adjustl(chari)) // '.fields'
   !

   geomfile = trim (adjustl(nEntradaRoot)) // '_' // trim (adjustl(chari))
   !warning file management
   if (statuse/=-1) then
       CALL CLOSEWARNINGFILE(layoutnumber,size,fatalerror,.false.,.false.) !!cierra el temporal !todavia no se ha dividido el size
       !!!borra el tmp si no hay fatalerror y reabre el de verdad
       if ((.not.fatalerror).and.(layoutnumber==0)) then 
           open(unit=1320,file=trim (adjustl(fichin))//'_tmpWarnings.txt_Warnings.txt')
           close(unit=1320,status='delete')
       endif
       CALL INITWARNINGFILE (layoutnumber, size, nEntradaRoot,verbose,ignoreErrors)
   endif
   !
   !
   IF (resume_fromold) THEN
      INQUIRE (file=trim(adjustl(nresumeable2))//'.old', EXIST=resume3)
   ELSE
      INQUIRE (file=trim(adjustl(nresumeable2)), EXIST=resume3)
   END IF
   IF (resume) THEN
      IF ( .NOT. resume3) THEN
         CALL stoponerror (layoutnumber, size, 'Resume fields not present',.true.); statuse=-1; !return
      END IF
      WRITE (dubuf,*) 'RESUMING simulation ', trim (adjustl(nEntradaRoot)), ' until n= ', finaltimestep
      CALL print11 (layoutnumber, dubuf)
   ELSE
      IF (resume3 .AND. ( .NOT. freshstart).and.(.not.run)) THEN
         CALL stoponerror (layoutnumber, size, 'Restarting file exists. Either specify -r to RESUME, -s to do a fresh START, or -run to run in whatever the case',.true.); statuse=-1; !return
      ELSEIF (resume3.and.(run)) THEN
         resume=.true.
      ELSE
         OPEN (35, file=trim(adjustl(nresumeable2)))
         WRITE (35, '(a)') '!END'
         CLOSE (35, status='DELETE')
         OPEN (35, file=trim(adjustl(nresumeable2))//'.old')
         WRITE (35, '(a)') '!END'
         CLOSE (35, status='DELETE')
      END IF
   END IF
!
!
!
   if (((wiresflavor=='guiffaut').or.(wiresflavor=='semistructured')).AND.(mpidir/=3)) then
       continue !arreglado mpidir slanted 2019
       !         CALL stoponerror (layoutnumber, size, 'Slanted wires unsupported with -mpidir {x,y}',.true.); statuse=-1; !return
   endif
   if (input_conformal_flag.AND.(mpidir/=3)) then
        continue !arreglado mpidir conformal 2019
         !TODO: under test
         !26-sep-2018: lo comento 
         !CALL stoponerror (layoutnumber, size, 'CONFORMAL -conf  unsupported with -mpidir {x,y}',.true.); statuse=-1; !return
   endif
   if (run_with_abrezanjas.AND.(mpidir/=3)) then
        continue !arreglado mpidir conformal 2019
         !under test
         !26-sep-2018: lo comento 
         !CALL stoponerror (layoutnumber, size, 'New abrezanjas thin gaps unsupported with -mpidir {x,y}',.true.); statuse=-1; !return
   endif
   if (run_with_abrezanjas.AND.flag_conf_sgg) then
      !pass Mayo-2018
         !CALL stoponerror (layoutnumber, size, 'CONFORMAL -conf currently unsupported with new abrezanjas thin gaps (unsupported 2 simultaneous conformal meshes at this moment',.true.); statuse=-1; !return
         !se hace en otro sitio
   endif


   !
   IF (((forcesteps) .AND. ( .NOT. freshstart)).and.(statuse/=-1)) THEN
      !in case of option -n withouth the freshstart option -s, it will resume or do a fresh start
      !depending on wether the resuming files are present or not
      IF (resume_fromold) THEN
         INQUIRE (file=trim(adjustl(nresumeable2))//'.old', EXIST=resume)
      ELSE
         INQUIRE (file=trim(adjustl(nresumeable2)), EXIST=resume)
      END IF
      IF (resume) THEN
         IF ((layoutnumber==0).or.((layoutnumber == size/2).and.stochastic)) THEN
            !the temporary
            CLOSE (11)
            file11isopen=.false.
            OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted', POSITION='append')
            file11isopen=.true.
 !!!           if (layoutnumber==0) call insertalogtmp !ojo lo quito aqui porque borra el _log con la info de credits
            IF (resume_fromold) THEN
               CALL print11 (layoutnumber, 'Resuming from .fields.old files')
            ELSE
               CALL print11 (layoutnumber, 'Resuming from .fields files')
            END IF
         END IF
      ELSE
         !!!IF ((layoutnumber==0).or.((layoutnumber == size/2).and.stochastic)) THEN
         !!!   !the temporary
         !!!   CLOSE (11)
         !!!   file11isopen=.false.
         !!!   OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted')
         !!!   file11isopen=.true.
         !!!   if (layoutnumber==0) call insertalogtmp
         !!!   CALL print11 (layoutnumber, 'Doing a new simulation from n=1')
         !!!END IF
         freshstart = .TRUE.
         resume_fromold = .FALSE.
      END IF
      IF ( ((layoutnumber==0).or.((layoutnumber == size/2).and.stochastic)).and.file11isopen) close (11)
      file11isopen=.false.
   END IF
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if ((run)) then  !el modo run crea el semaforo pause para permiter encolados salvajes en ugrgrid
#ifdef keeppause
          !!!solo para el cluster
                  INQUIRE (file='running', EXIST=hayinput)
#ifdef CompileWithMPI
                      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
                  if (hayinput) then
                      OPEN (9, file='running', FORM='formatted',action='read')
                      READ (9, '(a)') chain4
                      chain4=trim(adjustl(chain4))
                      CLOSE (9)
                  !!!!!!!!
#ifdef CompileWithMPI
                      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
                      call removeintraspaces(opcionespararesumeo)
                      call removeintraspaces(chain4)
                      if (trim(adjustl(opcionespararesumeo))==trim(adjustl(chain4))) then
                          existiarunningigual=.true.
                      endif
                  endif
#endif
#ifdef CompileWithMPI
                      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      if (layoutnumber==0) then
         OPEN (38, file='running')
         WRITE (38, '(a)') trim(adjustl(opcionespararesumeo))
         CLOSE (38)
      endif
   endif
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !openReportingFiles
   !only the master

   IF (((layoutnumber==0).or.((layoutnumber == size/2).and.stochastic)).and.(statuse/=-1)) THEN
      print *,'Opening _Report.txt file'
      IF (resume) THEN
         !the temporary
         CLOSE (11)
         file11isopen=.false.
         OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted')
         file11isopen=.true.
         donde = 0
         DO while (donde ==  0)
            !the first one is a dummy read
            READ (11, '(a)') chdummy
            donde = index (chdummy, 'mpirun -n')
         END DO
         CLOSE (11)
         file11isopen=.false.
         opcionesoriginales = chdummy
!
         call removeintraspaces(opcionespararesumeo)
         call removeintraspaces(opcionesoriginales)
         IF (trim(adjustl(opcionesoriginales)) /= trim(adjustl(opcionespararesumeo))) THEN
            CALL stoponerror (layoutnumber, size, 'Different resumed/original switches: '//trim(adjustl(opcionespararesumeo))//' <> '//&
            & trim(adjustl(opcionesoriginales)),.true.); statuse=-1; !return
         END IF
         !
         !!!!!!!!!        CLOSE (11, status='delete')
         !!!!!!!!!        file11isopen=.false.
         OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted')
         file11isopen=.true.
         donde = 0
         DO while (donde ==  0)
            !the first one is a dummy read
            READ (11, '(a)') chdummy
            donde = index (chdummy, '!SLICES')
         END DO
         slicesoriginales = trim (adjustl(chdummy))
         CLOSE (11)
         file11isopen=.false.
         OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted', POSITION='append')
         file11isopen=.true.
         if (layoutnumber==0) call insertalogtmp
      ELSE
         CLOSE (11)
         file11isopen=.false.
         OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted')
         file11isopen=.true.
         if (layoutnumber==0) call insertalogtmp
      END IF
      !
      CALL get_secnds (time_out2)
      call print_credits
#ifdef CompileWithReal8
      WRITE (dubuf,*) 'Compiled with Double Precision (real*8)'
      CALL print11 (layoutnumber, dubuf)
#endif      
#ifdef CompileWithReal4
      WRITE (dubuf,*) 'Compiled with Single Precision (real*4)'
      CALL print11 (layoutnumber, dubuf)
#endif      
#ifdef CompileWithReal16
      WRITE (dubuf,*) 'Compiled with Quadruple Precision (real*16)'
      CALL print11 (layoutnumber, dubuf)
#endif      
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,*) 'Launched on              ', time_out2%fecha(7:8), '/', time_out2%fecha(5:6), '/', &
      &                time_out2%fecha(1:4), ' ', time_out2%hora(1:2), ':', time_out2%hora(3:4)
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,'(a)') 'Launched with total options '
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,*) trim (adjustl(opcionestotales))
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,'(a)') 'If later resuming use compulsory options '
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,*) trim (adjustl(opcionespararesumeo))
      !!!CALL print11 (layoutnumber, dubuf,.true.)
      write (11,'(a)') trim(adjustl(dubuf)) !a capon para que el stochastic pueda resumear
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
   END IF
   !
   !
   !in seconds
   flushsecondsFields = flushminutesFields * 60
   !in seconds
   flushsecondsData = flushminutesData * 60
   
   IF (( .NOT. existeNFDE) .AND. ( .NOT. existeh5)) THEN
      CALL stoponerror (layoutnumber, size, 'Some input file missing .h5/.nfde/.conf',.true.); statuse=-1; !return
   END IF
   !
   !
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !
#ifdef CompileWithMPI
   CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
   !
    if (existiarunningigual) then !lo pongo aqui pq si no no se escribe en el report
        CALL stoponerror (layoutnumber, size, 'Running flag file with same options than requested exist. ',.true.); statuse=-1;
    endif
   
    return !el unico return que he dejado !240817
   
   end subroutine interpretswitches
!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!

#ifdef CompilePrivateVersion   
subroutine cargaNFDE
   INTEGER (KIND=8) :: numero,i8,troncho,longitud
   integer (kind=4) :: mpi_t_linea_t,longitud4
   IF (existeNFDE) THEN
       WRITE (dubuf,*) 'INIT Reading file '//whoami//' ', trim (adjustl(fileFDE))
       CALL print11 (layoutnumber, dubuf)
!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
      if (layoutnumber==0) then
           NFDE_FILE => cargar_NFDE_FILE (fileFDE)
!!!ya se allocatea dentro
      else
           ALLOCATE (NFDE_FILE)
      endif
!
      write(dubuf,*) '[OK]';  call print11(layoutnumber,dubuf)
      !--->
      WRITE (dubuf,*) 'INIT Sharing file through MPI'; CALL print11 (layoutnumber, dubuf)
!
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
!
      numero=NFDE_FILE%numero
      call MPI_BCAST(numero, 1_4, MPI_INTEGER8, 0_4, SUBCOMM_MPI, ierr)      
      if (layoutnumber/=0) then
          NFDE_FILE%targ = 1
          NFDE_FILE%numero=numero
          ALLOCATE (NFDE_FILE%lineas(NFDE_FILE%numero))
      endif
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
!CREAMOS EL DERIVED TYPE y lo enviamos !para evitar el error de Marconi asociado a PSM2_MQ_RECVREQS_MAX 100617

      CALL build_derived_t_linea(mpi_t_linea_t)

!problema del limite de mandar mas de 2^29 bytes con MPI!!!  Los soluciono partiendo en maxmpibytes (2^27) (algo menos por prudencia)! 040716
      troncho=ceiling(maxmpibytes*1.0_8/(max_linea*1.0_8+8.0_8),8)
     !!! print *,'numero,troncho ',numero,troncho
      do i8=1,numero,troncho
          longitud=min(troncho,numero-i8+1)
          CALL MPI_Barrier (SUBCOMM_MPI, ierr)
          if ((longitud>huge(1_4)).or.(longitud>maxmpibytes)) then
              print *,'Stop. Buggy error: MPI longitud greater that greatest integer*4'
              stop
          else
              longitud4=int(longitud,4)
          endif
          call MPI_BCAST(NFDE_FILE%lineas(i8),longitud4,mpi_t_linea_t,0_4,SUBCOMM_MPI,ierr)    
          CALL MPI_Barrier (SUBCOMM_MPI, ierr)
       !!!  if (layoutnumber==1) print *,'layoutnumber-->',layoutnumber, i8,i8+longitud-1 
       !!!  if (layoutnumber==1) print *,NFDE_FILE%lineas(i8)%len,' ',trim(adjustl(NFDE_FILE%lineas(i8)%dato)) 
       !!!  if (layoutnumber==1) print *,NFDE_FILE%lineas(i8+longitud-1)%len,' ',trim(adjustl(NFDE_FILE%lineas(i8+longitud-1)%dato))
          CALL MPI_Barrier (SUBCOMM_MPI, ierr)
    !      do i=1,numero
    !          call MPI_BCAST(NFDE_FILE%lineas(i)%len, 1_4, MPI_INTEGER4, 0_4, SUBCOMM_MPI, ierr)
    !          call MPI_BCAST(NFDE_FILE%lineas(i)%dato, MAX_LINEA, MPI_CHARACTER, 0_4, SUBCOMM_MPI, ierr)
    !          CALL MPI_Barrier (SUBCOMM_MPI, ierr) !para evitar el error de Marconi asociado a PSM2_MQ_RECVREQS_MAX 100617
    !      end do
      end do
!solo para debugeo
      !!!open(6729,file='comprob_'//trim(adjustl(dubuf))//'.nfde',form='formatted')
      !!!write(6729,'(2i12)') NFDE_FILE%numero,NFDE_FILE%targ
      !!!do i=1,numero
      !!!   write(6729,'(i6,a)') NFDE_FILE%lineas(i)%len,trim(adjustl(NFDE_FILE%lineas(i)%dato))
      !!!end do
      !!!close (6729)
!!!!!!
#else
    NFDE_FILE => cargar_NFDE_FILE (fileFDE)
#endif
      write(dubuf,*) '[OK]';  call print11(layoutnumber,dubuf)
      !--->
   END IF
   NFDE_FILE%mpidir=mpidir
!!!!!!!!!!!!!!!!!!!
   WRITE (dubuf,*) 'INIT interpreting geometrical data from ', trim (adjustl(fileFDE))
   CALL print11 (layoutnumber, dubuf)
!!!!!!!!!!
   parser => newparser (NFDE_FILE)
   thereare_stoch=nfde_file%thereare_stoch
   mpidir=NFDE_FILE%mpidir !bug 100419
!!!!!!!!!!!
   write(dubuf,*) '[OK]';  call print11(layoutnumber,dubuf)
   return

end subroutine cargaNFDE
#endif
 !del CompilePrivateVersion

!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!






   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine NFDE2sgg(fatalerror)
      real (kind=rkind) :: dt,finaldt
      logical fatalerror
      ! parser now holds all the .nfde info
      !first read the limits
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      CALL read_limits_nogeom (layoutnumber,size, sgg, fullsize, SINPML_fullsize, parser,MurAfterPML,mur_exist)


      dtantesdecorregir=sgg%dt
      !!!!!corrige el delta de t si es necesario !sgg15 310715 bug distintos sgg%dt !!!!!!!!!!

      dxmin=minval(sgg%DX)
      dymin=minval(sgg%DY)
      dzmin=minval(sgg%DZ)
      !!!
      dtlay=(1.0_RKIND/(cluz*sqrt(((1.0_RKIND / dxmin)**2.0_RKIND )+((1.0_RKIND / dymin)**2.0_RKIND )+((1.0_RKIND / dzmin)**2.0_RKIND ))))
      dt=dtlay
#ifdef CompileWithMPI
      call MPIupdateMin(dtlay,dt)
#endif

      !!!write(dubuf,*) SEPARADOR//separador//separador
      !!!call print11(layoutnumber,dubuf)
      !!!write(dubuf,*) '--->dt,dxmin,dymin,dzmin,sgg%dt  ',dt,dxmin,dymin,dzmin,sgg%dt
      !!!call print11(layoutnumber,dubuf)
      !!!write(dubuf,*) SEPARADOR//separador//separador
      !!!call print11(layoutnumber,dubuf)

      if (forceCFL) then
         sgg%dt=dt*cfl
         write(dubuf,*) SEPARADOR//separador//separador
         call print11(layoutnumber,dubuf)
         write(dubuf,*) 'Correcting sgg%dt with -cfl switch. New time step: ',sgg%dt
         call print11(layoutnumber,dubuf)
         write(dubuf,*) SEPARADOR//separador//separador
         call print11(layoutnumber,dubuf)
      else
          if (sgg%dt > dt*heurCFL) then
             write(dubuf,*) SEPARADOR//separador//separador
             call print11(layoutnumber,dubuf)
             write(dubuf,*) 'Automatically correcting dt for stability reasons: '
             call print11(layoutnumber,dubuf)
             write(dubuf,*) 'Original dt: ',sgg%dt
             call print11(layoutnumber,dubuf)
             sgg%dt=dt*heurCFL
             write(dubuf,*) 'New dt: ',sgg%dt
             call print11(layoutnumber,dubuf)
             write(dubuf,*) SEPARADOR//separador//separador
             call print11(layoutnumber,dubuf)
          endif
      endif
      !!!!!!!!!!!!No es preciso re-sincronizar pero lo hago !!!!!!!!!!!!!!!!!!!!!!!!!!
      finaldt=sgg%dt
#ifdef CompileWithMPI
      call MPIupdateMin(real(sgg%dt,RKIND),finaldt)
#endif
      !!!!!!!!!!!!!!
      cfl=sgg%dt/dtlay
      write(dubuf,*) SEPARADOR//separador//separador
      call print11(layoutnumber,dubuf)
      write(dubuf,*) 'CFLN= ',cfl
      call print11(layoutnumber,dubuf)
      write(dubuf,*) SEPARADOR//separador//separador
      call print11(layoutnumber,dubuf)

      write(dubuf,*) SEPARADOR//separador//separador
      call print11(layoutnumber,dubuf)
      write(dubuf,*) 'Deltat= ',sgg%dt
      if (layoutnumber==0) call print11(layoutnumber,dubuf)
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      write(dubuf,*) SEPARADOR//separador//separador
      call print11(layoutnumber,dubuf)
      if (mur_exist.and.mur_first) then
         mur_second=.false.
      else
         mur_second=.false. !arreglar cuando se arregle el bug de las mur second
         mur_first=.true. !arreglar cuando se arregle el bug de las mur second
      endif
#ifdef CompileWithMPI
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      !LATER OVERRRIDEN BY MPI
      !ALLOCATED ONE MORE TO KEEP PMC INFO FOR THE HX,HY,HZ FIELDS
      sgg%Alloc(1:6)%XI = fullsize(1:6)%XI - 1
      sgg%Alloc(1:6)%XE = fullsize(1:6)%XE + 1
      sgg%Alloc(1:6)%YI = fullsize(1:6)%YI - 1
      sgg%Alloc(1:6)%YE = fullsize(1:6)%YE + 1
      !REDUCE THE SWEEP AREA BY 1
      sgg%Sweep(1:6)%XI = fullsize(1:6)%XI
      sgg%Sweep(1:6)%XE = fullsize(1:6)%XE
      sgg%Sweep(1:6)%YI = fullsize(1:6)%YI
      sgg%Sweep(1:6)%YE = fullsize(1:6)%YE
      !
      IF (size == 1) THEN
         sgg%Alloc(1:6)%ZI = fullsize(1:6)%ZI - 1
         sgg%Alloc(1:6)%ZE = fullsize(1:6)%ZE + 1
         !REDUCE THE SWEEP AREA BY 1
         sgg%Sweep(1:6)%ZI = fullsize(1:6)%ZI
         sgg%Sweep(1:6)%ZE = fullsize(1:6)%ZE
         !!incluido aqui pq se precisa para clip 16/07/15
         DO field = iEx, iHz
            sgg%SINPMLSweep(field)%XI = Max (SINPML_fullsize(field)%XI, sgg%Sweep(field)%XI)
            sgg%SINPMLSweep(field)%XE = Min (SINPML_fullsize(field)%XE, sgg%Sweep(field)%XE)
            sgg%SINPMLSweep(field)%YI = Max (SINPML_fullsize(field)%YI, sgg%Sweep(field)%YI)
            sgg%SINPMLSweep(field)%YE = Min (SINPML_fullsize(field)%YE, sgg%Sweep(field)%YE)
            sgg%SINPMLSweep(field)%ZI = Max (SINPML_fullsize(field)%ZI, sgg%Sweep(field)%ZI)
            sgg%SINPMLSweep(field)%ZE = Min (SINPML_fullsize(field)%ZE, sgg%Sweep(field)%ZE)
         END DO
         !!fin 16/07/15
         WRITE (dubuf,*) 'INIT NFDE --------> GEOM'
         CALL print11 (layoutnumber, dubuf)
         CALL read_geomData (sgg,sggMtag,sggMiNo,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, fichin, layoutnumber, size, SINPML_fullsize, fullsize, parser, &
         groundwires,attfactorc,mibc,sgbc,sgbcDispersive,MEDIOEXTRA,maxSourceValue,skindepthpre,createmapvtk,input_conformal_flag,CLIPREGION,boundwireradius,maxwireradius,updateshared,run_with_dmma, &
         eps0,mu0,.false.,hay_slanted_wires,verbose,ignoresamplingerrors,tagtype)
         WRITE (dubuf,*) '[OK] ENDED NFDE --------> GEOM'
         CALL print11 (layoutnumber, dubuf)
         !writing
         slices = '!SLICES'
         WRITE (buff, '(i7)') sgg%Sweep(iHz)%ZE - sgg%Sweep(iHz)%ZI
         slices = trim (adjustl(slices)) // '_' // trim (adjustl(buff))
         IF (resume .AND. (slices /= slicesoriginales)) THEN
            buff='Different resumed/original MPI slices: '//trim(adjustl(slices))//' '//&
            & trim(adjustl(slicesoriginales))
            CALL stoponerror (layoutnumber, size, buff)
         END IF
         CALL print11 (layoutnumber, trim(adjustl(slices)))
         !end writing
         WRITE (buff, '(a,i7,a,i7)') '_________Spanning from z=', sgg%Sweep(iHz)%ZI, ' to z=', sgg%Sweep(iHz)%ZE
         CALL print11 (layoutnumber, trim(adjustl(buff)))
#ifdef CompileWithMPI
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#ifdef CompileWithStochastic
         if (stochastic) then
            buff='Stochastic uncompatible with MPI size smaller than 2'
            CALL stoponerror (layoutnumber, size, buff)
         endif
#endif
#endif
      ELSE !del size==1
#ifdef CompileWithMPI
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#ifdef CompileWithStochastic
         if (stochastic) then
            call HalvesStochasticMPI(layoutnumber,size,simu_devia)
         endif
#endif
!!!ahora divide el espacio computacional
         CALL MPIdivide (sgg, fullsize, SINPML_fullsize, layoutnumber, size, forcing, forced, slicesoriginales, resume,fatalerror)
         !
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
         if (fatalerror) then
!intenta recuperarte
            return
         endif

         ! if the layout is pure PML then take at least a line of non PML to build the PML data insider read_geomDAta
         ! Uses extra memory but later matrix sggm is deallocated in favor of smaller sggMIEX, etc
         DO field = iEx, iHz
            tempalloc(field)%ZE = sgg%Alloc(field)%ZE
            tempalloc(field)%ZI = sgg%Alloc(field)%ZI
            sgg%Alloc(field)%ZE = Max (sgg%Alloc(field)%ZE, SINPML_fullsize(field)%ZI+1)
            sgg%Alloc(field)%ZI = Min (sgg%Alloc(field)%ZI, SINPML_fullsize(field)%ZE-1)
         END DO
         !
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
         !!incluido aqui pq se precisa para clip 16/07/15
         DO field = iEx, iHz
            sgg%SINPMLSweep(field)%XI = Max (SINPML_fullsize(field)%XI, sgg%Sweep(field)%XI)
            sgg%SINPMLSweep(field)%XE = Min (SINPML_fullsize(field)%XE, sgg%Sweep(field)%XE)
            sgg%SINPMLSweep(field)%YI = Max (SINPML_fullsize(field)%YI, sgg%Sweep(field)%YI)
            sgg%SINPMLSweep(field)%YE = Min (SINPML_fullsize(field)%YE, sgg%Sweep(field)%YE)
            sgg%SINPMLSweep(field)%ZI = Max (SINPML_fullsize(field)%ZI, sgg%Sweep(field)%ZI)
            sgg%SINPMLSweep(field)%ZE = Min (SINPML_fullsize(field)%ZE, sgg%Sweep(field)%ZE)
         END DO
         !!fin 16/07/15
         WRITE (dubuf,*) 'INIT NFDE --------> GEOM'
         CALL print11 (layoutnumber, dubuf)
         CALL read_geomData (sgg,sggMtag,sggMiNo,sggMiEx,sggMiEy,sggMiEz,sggMiHx,sggMiHy,sggMiHz, fichin, layoutnumber, size, SINPML_fullsize, fullsize, parser, &
         groundwires,attfactorc,mibc,sgbc,sgbcDispersive,MEDIOEXTRA,maxSourceValue,skindepthpre,createmapvtk,input_conformal_flag,CLIPREGION,boundwireradius,maxwireradius,updateshared,run_with_dmma, &
         eps0,mu0,simu_devia,hay_slanted_wires,verbose,ignoresamplingerrors,tagtype)
#ifdef CompileWithMPI
         !wait until everything comes out
         CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
         WRITE (dubuf,*) '[OK] ENDED NFDE --------> GEOM'
         CALL print11 (layoutnumber, dubuf)
         !restore back the indexes
         DO field = iEx, iHz
            sgg%Alloc(field)%ZE = tempalloc(field)%ZE
            sgg%Alloc(field)%ZI = tempalloc(field)%ZI
         END DO
#endif
         CONTINUE
      END IF !del size==1
      !
#ifdef CompileWithMPI
      !wait until everything comes out
      CALL MPI_Barrier (SUBCOMM_MPI, ierr)
#endif
      !!!!!!!!!!!!!lo dejo aqui debajo tambien aunque ya se ha calculado antes para lo del clipping
      DO field = iEx, iHz
         sgg%SINPMLSweep(field)%XI = Max (SINPML_fullsize(field)%XI, sgg%Sweep(field)%XI)
         sgg%SINPMLSweep(field)%XE = Min (SINPML_fullsize(field)%XE, sgg%Sweep(field)%XE)
         sgg%SINPMLSweep(field)%YI = Max (SINPML_fullsize(field)%YI, sgg%Sweep(field)%YI)
         sgg%SINPMLSweep(field)%YE = Min (SINPML_fullsize(field)%YE, sgg%Sweep(field)%YE)
         sgg%SINPMLSweep(field)%ZI = Max (SINPML_fullsize(field)%ZI, sgg%Sweep(field)%ZI)
         sgg%SINPMLSweep(field)%ZE = Min (SINPML_fullsize(field)%ZE, sgg%Sweep(field)%ZE)
      END DO
      return
   end subroutine
   !

   subroutine print_credits
      if (creditosyaprinteados) return
      creditosyaprinteados=.true.
      CALL print11 (layoutnumber, '=========================')
      CALL print11 (layoutnumber, 'SEMBA_FDTD SOLVER')
      CALL print11 (layoutnumber, '=========================')
      
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) trim (adjustl(dataversion))
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      CALL print11 (layoutnumber, 'All rights reserved by the University of Granada (Spain)')
      CALL print11 (layoutnumber, '       Contact person: Salvador G. Garcia <salva@ugr.es>')
      
      !*******************************************************************************


      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
#ifdef CompileWithMPI
      CALL print11 (layoutnumber, 'Compiled WITH MPI support')
#else
      CALL print11 (layoutnumber, 'Compiled without MPI support')
#endif
#ifdef CompileWithHDF
      CALL print11 (layoutnumber, 'Compiled WITH .h5 HDF support')
#else
      CALL print11 (layoutnumber, 'Compiled without .h5 HDF support')
#endif
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      CALL get_secnds (time_out2)
!      WRITE (dubuf,*) 'Launched on              ', time_out2%fecha(7:8), '/', time_out2%fecha(5:6), '/', &
!      &                time_out2%fecha(1:4), ' ', time_out2%hora(1:2), ':', time_out2%hora(3:4)
!      CALL print11 (layoutnumber, dubuf)
!      print *, 'Highest integer ',huge(1_4)
      return
   end subroutine print_credits

   subroutine print_help
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, 'Command line arguments: ')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, '-i geometryfile        : Simulates the Native format input file            ')
      !!!          CALL print11 (layoutnumber, '-wiresverbose          : Enforce writing wire warnings in file (only in MPI)') !deprecated 07/03/15
      CALL print11 (layoutnumber, '-r                     : Restarts a previous execution until a given step. ')
      CALL print11 (layoutnumber, '&                        Needs -n                                          ')
      CALL print11 (layoutnumber, '-run                   : Uses a semaphore running file and automatically   ')
      CALL print11 (layoutnumber, '&                        relaunches simulation if ended or aborted (cluter)')
#ifdef CompileWithOldSaving
      CALL print11 (layoutnumber, '-old                   : Jointly with -r restarts from .fields.old files   ')
      CALL print11 (layoutnumber, '&                        instead (for safety .fields.old fields are saved  ')
      CALL print11 (layoutnumber, '&                        too if -flush is issued)                          ')
#endif
      CALL print11 (layoutnumber, '-cfl number            : Courant number (suggested<=0.8)  overriding input ')
      CALL print11 (layoutnumber, '-n numberoftimesteps   : Run the simulation until a specified step         ')
      CALL print11 (layoutnumber, '&                        either restarting if the necessary files are      ')
      CALL print11 (layoutnumber, '&                        present, or starting a fresh new one otherwise    ')
      CALL print11 (layoutnumber, '&                        Special cases: n=-1 -> Run only .h5/.nfde preproc.')
      CALL print11 (layoutnumber, '&                        Special cases: n=-2 -> Run only .h5 preprocessing ')
      CALL print11 (layoutnumber, '-s                     : Forces a fresh new simulation, erasing the        ')
      CALL print11 (layoutnumber, '&                        restarting files if they are present              ')
      CALL print11 (layoutnumber, '&                        Jointly with -n, it enforces a fresh restart      ')
      CALL print11 (layoutnumber, '&                        (erases .fields files from previous simulations)  ')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, '-pause seconds         : Wait seconds to start simulation                  ')
      CALL print11 (layoutnumber, '-prefix string         : Adds a string to the output filenames             ')
      CALL print11 (layoutnumber, '-saveall               : Saves all the observation time steps              ')
      CALL print11 (layoutnumber, '&                        (default saves only the specified windows of time)')
      CALL print11 (layoutnumber, '-singlefile            : Compacts E, H, J probes in single files to        ')
      CALL print11 (layoutnumber, '&                        overcome a large number of file openings          ')
      !!#ifdef CompileWithMPI
      !!          CALL print11 (layoutnumber, '-maxmessages number    : Buffer of messages for MPI Warnings file. Just    ')
      !!          CALL print11 (layoutnumber, '&                        increase if requested at runtime                  ')
      !!#endif

      !*********************************************************************************************************************
      !***[conformal] **************************************************************************************
      !*********************************************************************************************************************
      !conformal -help printf line   ref:  ##Confhelp##
#ifdef CompileWithConformal
      CALL print11 (layoutnumber, '-conf                    : Adds the conformal file to the simulation.'        )
#endif
      !*********************************************************************************************************************
#ifdef CompileWithNIBC
      CALL print11 (layoutnumber, '-skindepthpre          : Pre-processor for Maloney metals including skin depth.')
      CALL print11 (layoutnumber, '-mibc                  : Uses pure MIBC to deal with composites.  ')
      CALL print11 (layoutnumber, '-ade                   : Uses ADE-MIBC to deal with composites. ')
      CALL print11 (layoutnumber, '&                        Alternative to -mibc.'      )
      CALL print11 (layoutnumber, '-conformalskin         : Uses a conformal MIBC to deal with skin-depth')
      CALL print11 (layoutnumber, '&                        Do not use this switch if the problem also involves ')
      CALL print11 (layoutnumber, '&                        traditional composites, since these do not hold the right ')
      CALL print11 (layoutnumber, '&                        thickness parameter. Only use it if the problem only ')
      CALL print11 (layoutnumber, '&                        contains metals for which both the conductivity and ')
      CALL print11 (layoutnumber, '&                        thickness are CORRECTLY specified in the .nfde file. ')
      CALL print11 (layoutnumber, '-nocompomur            : Uses OLD (possibly unstable) upwinding scheme to deal with composites, ')
      CALL print11 (layoutnumber, '&                        instead of the NEW default, which uses a causal time-domain extrapolation ')
      CALL print11 (layoutnumber, '&                        of magnetic fields at the surface, by using the one-way ')
      CALL print11 (layoutnumber, '&                        advection equation (similar to 1D Mur ABCs) for its ')
      CALL print11 (layoutnumber, '&                        superior stability of the default new Mur formulation')
      CALL print11 (layoutnumber, '-attc   dissipation    : Positive factor (under 1) for stable composites,   ')
      CALL print11 (layoutnumber, '&                        permits to solve some instabilities in the simulation of MIBC materials.')
      CALL print11 (layoutnumber, '&                        It just adds a 1 cell lossy magnetic coating to the MIBC composite.')
      CALL print11 (layoutnumber, '&                        The dissipation factor is used to find the magnetic conductivity ')
      CALL print11 (layoutnumber, '&                        from the coefficient updating the current magnetic ')
      CALL print11 (layoutnumber, '&                        field from the previous one.  ')       
      write(buff,'(a,e10.2e3)')   '&                        Default= ',attfactorc
      CALL print11 (layoutnumber, buff)
#endif
      CALL print11 (layoutnumber, '-prioritizeCOMPOoverPEC: Uses Composites instead of PEC in conflicts.       ')
      CALL print11 (layoutnumber, '-prioritizeISOTROPICBODYoverall: Uses ISOTROPIC BODY FOR conflicts (JUST FOR SIVA).       ')
#ifdef CompileWithSGBC
      CALL print11 (layoutnumber, '-sgbc               : Enables the defaults SGBC model for composites. Default SGBC:')
      CALL print11 (layoutnumber, '-nosgbc             : Disables the defaults SGBC model for composites. Default SGBC:')
      CALL print11 (layoutnumber, '&                        -sgbfreq 3e9 -sgbresol 1 -sgbcrank      ')
      CALL print11 (layoutnumber, '-sgbcfreq           : Maximum frequency to consider the skin-depth       ')
      CALL print11 (layoutnumber, '-sgbcresol          : Number of cells per skin-depth a the Maximum frequency')
      CALL print11 (layoutnumber, '-sgbcyee            : Uses pure Yee ETD SGBC instead of Crank-Nicolson')
      CALL print11 (layoutnumber, '-sgbccrank          : Uses SGBC Crank-Nicolson (default)        ')
      CALL print11 (layoutnumber, '-sgbcdepth number   : Overrides automatic calculation of number of cells ')
      CALL print11 (layoutnumber, '&                        within SGBC                              ')
#endif
      CALL print11 (layoutnumber, '-pmlalpha factor order : CPML Alpha factor (>=0, <1 sug.) & polyn. grading.')
      CALL print11 (layoutnumber, '&                        alpha=factor * maximum_PML_sigma , order=polynom. ')
      write(buff,'(a,2e10.2e3)')  '&                        Default= ',alphamaxpar,alphaOrden
      CALL print11 (layoutnumber, buff)
      write(buff,'(a,e10.2e3)')   '-pmlkappa number       : CPML Kappa (>=1). Default= ',kappamaxpar
      CALL print11 (layoutnumber, buff)
      CALL print11 (layoutnumber, '-pmlcorr factor depth  : Factor for CPML enhanced stability (default none).')
      CALL print11 (layoutnumber, '&                        sigma=factor * maximum_PML_sigma, depth= # layers ')
      CALL print11 (layoutnumber, '-mur1                  : Supplement PMLs with 1st order Mur ABCs           ')
      CALL print11 (layoutnumber, '-mur2                  : Supplement PMLs with 2nd order Mur ABCs           ')
#ifdef CompileWithWires
      CALL print11 (layoutnumber, '-wiresflavor {holland.or.old} : model for the wires    ')
#endif
#ifdef CompileWithBerengerWires
      CALL print11 (layoutnumber, '-wiresflavor {berenger} : model for the wires    ')   
#endif
#ifdef CompileWithSlantedWires
      CALL print11 (layoutnumber, '-wiresflavor {new/guiffaut.or.experimental.or.slanted/transition/semistructured precision} : model for the wires    ')   
#endif
#ifdef CompileWithWires
      CALL print11 (layoutnumber, '&                        (default '//trim(adjustl(wiresflavor))//')   ')
      CALL print11 (layoutnumber, '-notaparrabos          : Do not remove extra double tails at the end of the wires ')
      CALL print11 (layoutnumber, '&                        only available for the native format.             ')
      CALL print11 (layoutnumber, '-intrawiresimplify     : Disable strict interpretation of .NFDE topology.  ')
      CALL print11 (layoutnumber, '&                        Collapse internal parallel wires and create       ')
      CALL print11 (layoutnumber, '&                        intra-wire junctions.                             ')
      CALL print11 (layoutnumber, '-nomtlnberenger        : Disables MTLN improvements for Berenger wiresflavor')
      CALL print11 (layoutnumber, '-stableradholland             : Automatic correction of radii for Holland wiresflavor')
      CALL print11 (layoutnumber, '&                        Use only in case of instabilities.  (experimental)')
      CALL print11 (layoutnumber, '-groundwires           : Ground wires touching/embedded/crossing PEC/Lossy.')
      CALL print11 (layoutnumber, '&                        Use with CAUTION. Revise *Warnings.txt file!      ')
      CALL print11 (layoutnumber, '-noguiffautcrecepelo : Ground open nodes. Experimental. Do not use.')
      CALL print11 (layoutnumber, '-connectendings        : Joins ohmicly endings nodes of adjacent segments  ')
      CALL print11 (layoutnumber, '&                        from multiwires (segments do no collapse).        ')
      CALL print11 (layoutnumber, '&                        regardless of whether they are actually connected ')
      CALL print11 (layoutnumber, '&                       through the ENL/ENR numbering ')
      CALL print11 (layoutnumber, '&                        Automatic with -a                                 ')
      CALL print11 (layoutnumber, '&                        Use with CAUTION. Revise *Warnings.txt file!      ')
      CALL print11 (layoutnumber, '-isolategroupgroups    : Detach ohmicly endings nodes of adjacent segments ')
      CALL print11 (layoutnumber, '&                        from multiwires if they are in different          ')
      !!CALL print11 (layoutnumber, '-dontsplitnodes        : Detach ohmicly endings nodes of adjacent segments ')
      !!CALL print11 (layoutnumber, '&                        with common RLC into separate RLCs                ')
      !!CALL print11 (layoutnumber, '&                        Use with CAUTION. Revise *Warnings.txt! (experim.)')
      CALL print11 (layoutnumber, '-makeholes             : Create a void 2-cell area around wire segments    ')
      CALL print11 (layoutnumber, '&                        Use with CAUTION. Revise *Warnings.txt (experim.) ')
      CALL print11 (layoutnumber, '-mindistwires dist     : Specify the min distance between wires in a       ')
      CALL print11 (layoutnumber, '&                        multiwire in new and experimental wires flavors   ')
      write(buff,'(a,e10.2e3)')   '&                        Default= ',mindistwires
      CALL print11 (layoutnumber, buff)
      CALL print11 (layoutnumber, '-inductance {ledfelt/berenger/boutayeb} : model for the self-inductance    ')
      CALL print11 (layoutnumber, '&                        (default '//trim(adjustl(inductance_model))//')   ')
      CALL print11 (layoutnumber, '-inductanceorder order : order for the self-inductance calculation for     ')
      CALL print11 (layoutnumber, '&                        slanted wires in experimental wiresflavor         ')
      write(buff,'(a,i8)')   '&                        Default= ',inductance_order
      CALL print11 (layoutnumber, '-attw   dissipation    : Positive factor (under 1) for stability in wires, ')
      write(buff,'(a,e10.2e3)')   '&                        Default= ',attfactorw                              
      CALL print11 (layoutnumber, '-maxwireradius number  : Bounds globally the wire radius                   ')
      CALL print11 (layoutnumber, '-clip                  : Permits to clip a bigger problem truncating wires.')
      CALL print11 (layoutnumber, '-wirecrank             : Uses Crank-Nicolson for wires (development)       ')
#endif
#ifdef CompileWithNF2FF
      CALL print11 (layoutnumber, '-noNF2FF string        : Supress a NF2FF plane for calculation             ')
      CALL print11 (layoutnumber, '&                        String can be: up, down, left, right, back , front')
      CALL print11 (layoutnumber, '-NF2FFdecim            : Uses decimation in NF2FF calculation (faster).    ')
      CALL print11 (layoutnumber, '&                        WARNING: High-freq aliasing may occur             ')
#endif
      CALL print11 (layoutnumber, '-vtkindex              : Output index instead of real point in 3D slices.  ')
      CALL print11 (layoutnumber, '-ignoreerrors          : Run even if errors reported in *Warnings.txt file.')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, '-cpumax minutes        : CPU runtime (useful for limited CPU queuing       ')
      CALL print11 (layoutnumber, '-noshared              : Do not waste time with shared fields              ')
      CALL print11 (layoutnumber, '-flush minutes         : Minutes between data flush of restarting fields   ')
      CALL print11 (layoutnumber, '&                        (default 0=No flush)                              ')
      CALL print11 (layoutnumber, '-flushdata minutes     : Minutes between flushing observation data         ')
      CALL print11 (layoutnumber, '&                        (default is every 5 minutes)                      ')
      CALL print11 (layoutnumber, '-map                   : Creates map ASCII files of the geometry           ')
      CALL print11 (layoutnumber, '&                        with wires and PEC                ')
      CALL print11 (layoutnumber, '&                        (in conjunction with -n 0 only creates the maps)  ')
      CALL print11 (layoutnumber, '-mapvtk                : Creates .VTK map of the PEC/wires/Surface geometry')
#ifdef CompileWithConformal
      CALL print11 (layoutnumber, '-conf file             : conformal file  ')
      CALL print11 (layoutnumber, '-abrezanjas            : Thin-gaps treated in conformal manner  ')
#endif
#ifdef CompileWithDMMA
      CALL print11 (layoutnumber, '-dmma                  : Thin-gaps treated in DMMA manner  ')
#endif
#ifdef CompileWithMPI
      CALL print11 (layoutnumber, '-mpidir {x,y,z}        : Rotate model to force MPI along z be the largest  ')
      CALL print11 (layoutnumber, '-force    cutplane     : Force a MPI layout to begin at cutplane (debug!)  ')
#endif
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, 'Control through signaling files during the simulation: (after erased)      ')
      CALL print11 (layoutnumber, '&  stop         : (void) Forces a graceful end (it Cannot be resumed)      ')
      CALL print11 (layoutnumber, '&                 No restarting data is flushed, only observation data     ')
      CALL print11 (layoutnumber, '&  stopflushing : (void) Forces a graceful end (it can be resumed)         ')
      CALL print11 (layoutnumber, '&  flush        : (void) Forces a flush of resuming fields and observation ')
      CALL print11 (layoutnumber, '&                 data in 1 minute time approx.                            ')
      CALL print11 (layoutnumber, '&  flushdata    : (void) Forces a flush only of the observation data in    ')
      CALL print11 (layoutnumber, '&                 1 minute time approx.                                    ')
      CALL print11 (layoutnumber, '&                 Both restarting and observation data are flushed         ')
      CALL print11 (layoutnumber, '&  stop_only         : Forces a graceful end (cannot be resumed) only of a ')
      CALL print11 (layoutnumber, '&                      given problem name (without the .nfde extension)    ')
      CALL print11 (layoutnumber, '&                      No restarting data is flushed, only observation data')
      CALL print11 (layoutnumber, '&  stopflushing_only : Forces a graceful end (it can be resumed) only of a ')
      CALL print11 (layoutnumber, '&                      give problem name (without the .nfde extension)     ')
      CALL print11 (layoutnumber, '&                      Both restarting and observation data is flushed     ')
      CALL print11 (layoutnumber, '&  flush_only   : Forces flush of resuming fields and observation data only')
      CALL print11 (layoutnumber, '&                 of a given problem name (without the .nfde extension)    ')
      CALL print11 (layoutnumber, '&                 in 1 minute time approx.                                 ')
      CALL print11 (layoutnumber, '&  flushdata_only : Forces a flush only of the observation data only of a  ')
      CALL print11 (layoutnumber, '&                   given problem name (without the .nfde extension)       ')
      CALL print11 (layoutnumber, '&                   in 1 minute time approx.                               ')
      CALL print11 (layoutnumber, '&                   Both restarting and observation data are flushed       ')
      CALL print11 (layoutnumber, '&  pause        : (void) While this field exist no simulation is started   ')
      CALL print11 (layoutnumber, '&  unpack       : (void) Unpacks on-the-fly .bin probes files created      ')
      CALL print11 (layoutnumber, '&                 with the -singlefile packaging option                    ')
      CALL print11 (layoutnumber, '&  postprocess  : (void) Do frequency domain and transfer function         ')
      CALL print11 (layoutnumber, '&                 postprocess on-the-fly                                   ')
      CALL print11 (layoutnumber, '&  flushxdmf    : (void) Flush .xdmf animation probes on the fly           ')
      CALL print11 (layoutnumber, '&  flushvtk     : (void) Flush .vtk  animation probes on the fly           ')
      CALL print11 (layoutnumber, '&  snap         : Creates a .h5 and .xdmf snapshot per MPI layout if the   ')
      CALL print11 (layoutnumber, '&                 field value is over the first number found in this file  ')
      CALL print11 (layoutnumber, '&                 in space steps by the 2nd integer number                 ')
      CALL print11 (layoutnumber, '&                 in time steps by the 3rd integer number (1-minute lapse) ')
      CALL print11 (layoutnumber, '&  relaunch     : Relaunches the simulation upon termination with the      ')
      CALL print11 (layoutnumber, '&                 switches read from this file. Used jointly with a        ')
      CALL print11 (layoutnumber, '&                 stop file permits to launch simulations on-demand        ')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      !
      write (buff,'(a,i14,a)') 'Max CPU time is ',topCPUtime,' seconds (can be overriden by -cpumax)'
      CALL print11 (layoutnumber, buff)
#ifdef CompileWithOpenMP
      CALL print11 (layoutnumber, 'SUPPORTED:   MultiCPU parallel simulation (OpenMP)')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: MultiCPU parallel simulation (OpenMP)')
#endif
!
#ifdef CompileWithMPI
      CALL print11 (layoutnumber, 'SUPPORTED:   MultiCPU/Multinode parallel simulation (MPI)')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: MultiCPU/Multinode parallel simulation (MPI)')
#endif
#ifdef CompileWithConformal
      CALL print11 (layoutnumber, 'SUPPORTED:   Conformal algorithm')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Conformal algorithm')
#endif
#ifdef CompileWithNF2FF
      CALL print11 (layoutnumber, 'SUPPORTED:   Near-to-Far field probes')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Near-to-Far field probes')
#endif
#ifdef CompileWithAnisotropic
      CALL print11 (layoutnumber, 'SUPPORTED:   Lossy anistropic materials, both electric and magnetic')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Lossy anistropic materials, both electric and magnetic')
#endif
#ifdef CompileWithDMMA
      CALL print11 (layoutnumber, 'SUPPORTED:   Thin Slots ')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Thin Slots ')
#endif
#ifdef CompileWithEDispersives
      CALL print11 (layoutnumber, 'SUPPORTED:   Electric and Magnetic Dispersive materials ')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Electric and Magnetic Dispersive materials ')
#endif
#ifdef CompileWithSGBC
      CALL print11 (layoutnumber, 'SUPPORTED:   Isotropic Multilayer Skin-depth Materials (SGBC)')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Isotropic Multilayer Skin-depth Materials (SGBC)')
#endif
#ifdef CompileWithNIBC
      CALL print11 (layoutnumber, 'SUPPORTED:   Isotropic Multilayer Skin-depth Materials (MIBC)')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Isotropic Multilayer Skin-depth Materials (MIBC)')
#endif

#ifdef CompileWithWires
      CALL print11 (layoutnumber, 'SUPPORTED:   Loaded and grounded thin-wires with juntions')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Loaded and grounded thin-wires with juntions')
#endif
#ifdef CompileWithNodalSources
      CALL print11 (layoutnumber, 'SUPPORTED:   Nodal hard/soft electric and magnetic sources')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Nodal hard/soft electric and magnetic sources')
#endif
#ifdef CompileWithHDF
      CALL print11 (layoutnumber, 'SUPPORTED:   .xdmf+.h5 probes ')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: .xdmf+.h5 probes ')
#endif
#ifdef CompileWithOldSaving
      CALL print11 (layoutnumber, 'SUPPORTED:   .fields.old files created (fail-safe)')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: .fields.old files created (fail-safe)')
#endif
#ifdef CompileWithStochastic
      CALL print11 (layoutnumber, 'SUPPORTED:   Stochastic analysis')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Stochastic analysis')
#endif
#ifdef CompileWithPrescale
      CALL print11 (layoutnumber, 'SUPPORTED:   Permittivity scaling accelerations')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Permittivity scaling accelerations')
#endif
#ifdef CompileWithWires
      CALL print11 (layoutnumber, 'SUPPORTED:   Holland Wires')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Holland Wires')
#endif
#ifdef CompileWithBerengerWires
      CALL print11 (layoutnumber, 'SUPPORTED:   Multi-Wires')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Multi-Wires')
#endif
#ifdef CompileWithSlantedWires
      CALL print11 (layoutnumber, 'SUPPORTED:   Slanted Wires')
#else
      CALL print11 (layoutnumber, 'UNSUPPORTED: Slanted Wires')
#endif
!!!!!!!!!!!!!!!!!
#ifdef CompileWithReal4
      CALL print11 (layoutnumber, 'Single precission simulations (reals are 4-byte)')
#endif
#ifdef CompileWithReal8
      CALL print11 (layoutnumber, 'Double precission simulations (reals are 8-byte)')
#endif
#ifdef CompileWithInt4
      CALL print11 (layoutnumber, 'Media matrices are 4 bytes')
#endif
#ifdef CompileWithInt2
      CALL print11 (layoutnumber, 'Media matrices are 2 bytes')
#endif
#ifdef CompileWithInt1
      CALL print11 (layoutnumber, 'Media matrices are 1 byte')
#endif
#ifdef CompileWithMPI
      CALL MPI_FINALIZE (ierr)
#endif
      return
   end subroutine print_help

   subroutine print_basic_help
      call print_credits
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, 'Basic usage: ')
      CALL print11 (layoutnumber, '&   For help use          -h ')
      CALL print11 (layoutnumber, '&   For launching use                     ')
      CALL print11 (layoutnumber, '&                         -i inputfile (native)')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      return
   end subroutine print_basic_help

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine default_flags
   hopf=.false.
   precision=0 !redondeo del semiestructurado
   stochastic=.false.
   chosenyesornostochastic=.false. !es un flag informativo que debe inicializarse a .false. a pesar de qu el sentido comun diga lo contrario
   simu_devia=.false. !puto bug semana santa '19 cazado a 210419
!!200918 !!!si se lanza con -pscal se overridea esto
   Eps0= 8.8541878176203898505365630317107502606083701665994498081024171524053950954599821142852891607182008932e-12
   Mu0 = 1.2566370614359172953850573533118011536788677597500423283899778369231265625144835994512139301368468271e-6
   cluz=1.0_RKIND/sqrt(eps0*mu0)
   
#ifdef CompileWithHDF
      createh5bin=.false.
#else
      createh5bin=.true.
#endif     
      lcreateh5filefromsinglebin=.false.
      permitscaling=.false.
      niapapostprocess=.false.
      planewavecorr=.false.
      prioritizeCOMPOoverPEC=.false.  !pec has default more priority than compo (para siva hay que cambiarlo)
      prioritizeISOTROPICBODYoverall=.FALSE. !PARA EL SIVA SE CAMBIA POR LINEA DE COMANDO
      mpidir=3 !DEFAULT DO NOT ROTATE GEOMETRY !JUST TO TAKE PROFIT OF MPI
      maxwireradius=-1.0_RKIND
      boundwireradius=.false.
      wirecrank=.FALSE.
      ignoreerrors=.false.
      ignoresamplingerrors=.false.
      vtkindex=.FALSE. !SOLO AFECTA A LOS VTK (SACA INDICES EN VEZ DE POSICION FISICA)
      CLIPREGION=.false.
      NF2FFDecim=.FALSE.
      facesNF2FF%tr=.true.
      facesNF2FF%fr=.true.
      facesNF2FF%iz=.true.
      facesNF2FF%de=.true.
      facesNF2FF%ab=.true.
      facesNF2FF%ar=.true.
      !defaults
      hay_slanted_wires=.false.
      forcing = .FALSE.
      resume_fromold = .FALSE.
      singlefilewrite = .FALSE.
      existeputoconf=.false.
      updateshared=.true. !040717 para que no pierda tiempo en el update shared de preprocess se crea flag -noshared que pone esta variable a false. 
                          !Esa info solo la usan anisotropic y composites. podria hacerse algo mas automatico. de momento manual.

      finaltimestep=0
      cfltemp=1.0 !dummy
      cfl=1.0 !default courant number !no tocarlo 310715 solo afecta si se usa -cfl
      forcecfl=.false.
      !PML default
      !cpml stretching maximum parameters !!alphamaxpar=StaticFrequency*2*pi*Eps0
      alphamaxpar=0.0_RKIND !0.24  !expresion 7.78 taflove 3 edic)
      alphaorden=1.0_RKIND
      kappamaxpar=1.0_RKIND !15.0_RKIND !061118 mantener a 1 por conflictos cpml and permittivity scaling
      !and final layer electric sigma
      medioextra%exists=.false.
      medioextra%index=-7 !void
      medioextra%size=-1  !void
      medioextra%sigma=-1e20 !void
      !
      MurAfterPML=.false.
      mur_second=.false.
      mur_first=.false.
      mur_exist=.false.
      !!!!!!!!!!!!
      takeintcripte=.false. !a peticion de OLD, redondear los nodos de cripte a la baja
      attfactorc=1.0_RKIND !default dissipation factor for composites
      attfactorW=1.0_RKIND!default dissipation factor for wires

      mibc=.false.
      ade=.false. !auxiliary differential equation for composites
      conformalskin=.false.
      SGBC=.true. !default is false unless required
      SGBCDispersive=.false. !default is false unless required    
      skindepthpre=.false.
      SGBCDepth=-1  ! se calcula automaticamente a menos que se use el switch
      SGBCFreq=1e9 !default es cazar el skin depth hasta 1e9
      SGBCresol=1.0 !numero de celdas por skin depth (caida a exp(-1))
      SGBCCrank=.true. !default es SGBCcrank

      fatalerror=.false.
      fatalerrornfde2sgg=.false.
      !**************************************************************************************************
      !***[conformal] *******************************************************************
      !**************************************************************************************************
      !conformal existence flags   ref: ##Confflag##
      input_conformal_flag = .false.
      !**************************************************************************************************
      !**************************************************************************************************
      !**************************************************************************************************
      !added 2701418 para prevenir conflictos de dobles mallas conformal si se usa -run_with_abrezanjas
      flag_conf_sgg=.false.
      !

      dontwritevtk=.false.

      NOcompomur=.false. !DEFAULT mi formulacion

      !default no join the wires which are adjacent (ORIGINAL election)
      !do not connect endings unless specified in ORIGINAL
      makeholes = .FALSE.
      connectendings=.false.
      isolategroupgroups=.false.
      dontsplitnodes = .true. !07/12/14 estaba puesta a .false. por defecto pero el tratamiento de las rlc terminales ya no es nodal ver comemtario en wires.f90 en call splitnodo
      strictOLD=.true. !default is strict ORIGINAL overriden manually 
      TAPARRABOS=.true. !default since 101116 !cortar los multizigzag rabitos
      mtlnberenger=.true. !solo actua si se invoca con wiresflavor berenger esto va a ser siempre true a menos que tambien se invoque con -nomtlnberenger (solo para debugeo y que coincida con Holland) 020719
      stableradholland=.false. !solo actua si se invoca con wiresflavor holland 
      fieldtotl=.false.
      experimentalVideal=.false.
      forceresampled=.false.
      factorradius=1.0e+30 !para evitar division por cero 120123
      factordelta=1.0e+30 !para evitar division por cero 120123
      !default
      groundwires = .false.
      noguiffautcrecepelo =.false. !131219 experimental niapa ojoooo
      inductance_model = 'boutayeb'
      inductance_order = 8
      wiresflavor='holland'
      mindistwires = 0.5_RKIND
      !
      MurAfterPML = .false.
      !
      createdotnfdefromdoth5=.false.
      createmap = .FALSE.
      createmapvtk = .FALSE.
      verbose = .FALSE.
      saveall = .FALSE.
      forcesteps = .FALSE.
      resume = .FALSE.
      freshstart = .FALSE.
      run = .FALSE.  !si hay .fields restartea y si no comienza
      deleteintermediates = .FALSE.
      !
      existeNFDE = .FALSE.
      existeh5  = .FALSE.
      !
      !default is NO flush fields
      flushminutesFields = 0
      !default is to flush data when the buffer is filled up
      !si se pone cada tantos minutos y se guardan las sondas en trancos!puede haber errores de redondeo porque el buffer se limpia tras cada flusheo
      flushminutesData = topCPUtime
      !
      !maximum runtime
      maxCPUtime = topCPUtime
      input_conformal_flag = .false.
      file11isopen=.false.
      file10isopen=.false.
      relaunching=.false.
      forcestop=.false.
      input_conformal_flag = .false.
!thin gaps      
#ifdef CompileWithConformal
      run_with_dmma = .false.
! todo esto para el abrezanjas. se precisa tambien el input_conformal_flag  
!!!!quitado sgg ojo 290521 esto no se ha arreglado aim... quito el abrezanjas !290521 bug
      run_with_abrezanjas = .true.
      !!! run_with_abrezanjas = .false. !0222 
      if (.NOT.input_conformal_flag) then
            conformal_file_input_name = char(0)
            input_conformal_flag = .true.;
 !!         input_conformal_flag_abrezanjas = .true.;
      end if
#else
      run_with_abrezanjas = .false.
#ifdef CompileWithDMMA
      run_with_dmma = .true.
#else
      run_with_dmma = .false.
#endif
#endif

!fin thin gaps


      return
   end subroutine default_flags

   subroutine removeintraspaces(a)
        CHARACTER (LEN=*), intent(inout):: a
        integer (Kind=4) :: i,longi
        logical correc
        correc=.true.
        do while(correc)
            correc=.false.
            a=trim(adjustl(a))
            longi=len(trim(adjustl(a)))
            buscae: do i=1,longi-1
              if ((a(i:i)==' ').and.(a(i+1:i+1)==' ')) then
                a=a(1:i-1)//a(i+1:longi) 
                correc=.true.
                exit buscae
              endif
            end do buscae
        end do
   return
   end subroutine

   subroutine insertalogtmp !para 100920
           CALL OffPrint !no reimprimas, esto ya estaba por pantalla
            OPEN (newunit=myunit11, file='SEMBA_FDTD_temp.log')
            do
                read(myunit11,'(1024a)',end=7211) dubuf
                dubuf='&'//dubuf !para respetar los espacios
                CALL print11 (layoutnumber, dubuf)
            end do
7211        CLOSE (myunit11, status='delete')   
            CALL OnPrint
            return
   end subroutine insertalogtmp

   
   
   
  END PROGRAM SEMBA_FDTD_launcher
!
