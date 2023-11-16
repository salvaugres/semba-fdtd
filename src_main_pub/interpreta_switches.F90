module interpreta_switchwes_m  

   USE FDETYPES
   USE Getargs   
   use EpsMuTimeScale_m
   use Report
   use version
   use ParseadorClass
   IMPLICIT NONE
   PRIVATE
   !   
   type entrada_t

        logical ::                            &
            forcing                         , &
            singlefilewrite                 , &
            ignoresamplingerrors            , &
            ignoreerrors                    , &
            updateshared                    , &
            prioritizeISOTROPICBODYoverall  , &
            wirecrank                       , &
            CLIPREGION                      , &
            verbose                         , &
            resume                          , &
            forcesteps                      , &
            resume_fromold                  , &
            freshstart                      , &
            run                             , &
            createmap                       , &
            dontwritevtk                    , &
            vtkindex                        , &
            createmapvtk                    , &
            hopf                            , &
            run_with_dmma                   , &
            run_with_abrezanjas             , &
            input_conformal_flag            , &
            pausar                          , &
            l_aux                           , &
            flag_conf_sgg                   , &
            takeintcripte                   , &
            skindepthpre                    , &
            SGBC                            , &
            conformalskin                   , &
            ade                             , &
            mibc                            , &
            NOcompomur                      , &
            MurAfterPML                     , &
            SGBCcrank                       , &
            sgbcDispersive                  , &
            saveall                         , &
            boundwireradius                 , &
            makeholes                       , &
            mur_first                       , &
            mur_second                      , &
            connectendings                  , &
            strictOLD                       , &
            mtlnberenger                    , &
            stableradholland                , &
            TAPARRABOS                      , &
            fieldtotl                       , &
            forceresampled                  , &
            isolategroupgroups              , &
            groundwires                     , &
            noSlantedcrecepelo              , &
            forcecfl                        , &
            niapapostprocess                , &
            planewavecorr                   , &
            permitscaling                   , &
            stochastic                      , &
            chosenyesornostochastic         , &
            prioritizeCOMPOoverPEC          , &
            createh5bin                     , &
            deleteintermediates             , &
            existeNFDE                      , &
            file11isopen                    , &
            NF2FFDecim                      , &
            existeh5                        , &
            fatalerror                      , & 
            existeconf                      , &
            existecmsh                      , &
            thereare_stoch                  , &
            creditosyaprinteados            
      
        integer (kind=4) ::                   &
            wirethickness                    ,&
            inductance_order                 ,&
            finaltimestep                    ,&
            ierr                             ,&
            layoutnumber                     ,&
            size                             ,&
            length                           ,&
            mpidir                           ,&
            flushminutesFields               ,&
            flushminutesData                 ,&
            flushsecondsFields               ,&
            flushsecondsData                 ,&
            forced                           ,&
            maxCPUtime                       ,&
            SGBCdepth                        ,&
            precision                      
                                           
        REAL (KIND=RKIND) ::                  &
            maxwireradius                    ,&
            mindistwires                     ,&
            attfactorc                       ,&
            attfactorw                       ,&
            cfltemp                          ,&
            cfl                              ,&
            SGBCfreq                         ,&
            SGBCresol                        ,&
            alphamaxpar                      ,&
            kappamaxpar                      ,&
            alphaOrden                     
                                           
        REAL (KIND=8) ::                      &
             time_begin                      ,&
             time_end                        
        real (kind=RKIND_wires) ::            &
             factorradius                    ,&
             factordelta                     
   
   end type entrada_t
       
   PUBLIC interpreta,insertalogtmp,print_help,print_basic_help,print_credits, &
       removeintraspaces,entrada_t,buscaswitchficheroinput
   !                                        
CONTAINS
    
   subroutine interpreta(sgg,chaininput,statuse,  &
                             prefixopci, prefixopci1,opcionespararesumeo, opcionesoriginales, &
                             slicesoriginales, chdummy,dubuf,conformal_file_input_name, &
                             ficherohopf ,inductance_model,wiresflavor,fichin, f, chain, buff,prefix,nEntradaRoot, &
                             nresumeable2,geomfile,opcionestotales,l )     
   
   
   
   CHARACTER (LEN=BUFSIZE_LONG) :: prefixopci, prefixopci1,opcionespararesumeo, opcionesoriginales, &
                                   slicesoriginales, chdummy      
   CHARACTER (LEN=BUFSIZE) :: chaininput,chari,dubuf  ,conformal_file_input_name   ,ficherohopf  ,&
                              inductance_model,wiresflavor , &
                              fichin, f, chain, buff,prefix,nEntradaRoot,nresumeable2,geomfile,&
                              opcionestotales    
   

   type (SGGFDTDINFO), intent(INOUT)     ::  sgg   
   type (nf2ff_T) :: facesNF2FF    
   TYPE (MedioExtra_t) :: MEDIOEXTRA    
   type (EpsMuTimeScale_input_parameters_t) :: EpsMuTimeScale_input_parameters
   type (tiempo_t)  ::  time_out2 
   
   logical :: existiarunningigual,mpidirset,resume3
   integer (kind=4) :: i,j,donde,n,statuse, newmpidir
   real (KIND=RKIND) :: pausetime
!!!!!!!!!!!!!        
   type (entrada_t) :: l
!!!!!!!!!    
!!! variables locales
        logical ::                            &
            forcing                         , &
            singlefilewrite                 , &
            ignoresamplingerrors            , &
            ignoreerrors                    , &
            updateshared                    , &
            prioritizeISOTROPICBODYoverall  , &
            wirecrank                       , &
            CLIPREGION                      , &
            verbose                         , &
            resume                          , &
            forcesteps                      , &
            resume_fromold                  , &
            freshstart                      , &
            run                             , &
            createmap                       , &
            dontwritevtk                    , &
            vtkindex                        , &
            createmapvtk                    , &
            hopf                            , &
            run_with_dmma                   , &
            run_with_abrezanjas             , &
            input_conformal_flag            , &
            pausar                          , &
            l_aux                           , &
            flag_conf_sgg                   , &
            takeintcripte                   , &
            skindepthpre                    , &
            SGBC                            , &
            conformalskin                   , &
            ade                             , &
            mibc                            , &
            NOcompomur                      , &
            MurAfterPML                     , &
            SGBCcrank                       , &
            sgbcDispersive                  , &
            saveall                         , &
            boundwireradius                 , &
            makeholes                       , &
            mur_first                       , &
            mur_second                      , &
            connectendings                  , &
            strictOLD                       , &
            mtlnberenger                    , &
            stableradholland                , &
            TAPARRABOS                      , &
            fieldtotl                       , &
            forceresampled                  , &
            isolategroupgroups              , &
            groundwires                     , &
            noSlantedcrecepelo              , &
            forcecfl                        , &
            niapapostprocess                , &
            planewavecorr                   , &
            permitscaling                   , &
            stochastic                      , &
            chosenyesornostochastic         , &
            prioritizeCOMPOoverPEC          , &
            createh5bin                     , &
            deleteintermediates             , &
            existeNFDE                      , &
            file11isopen                    , &
            NF2FFDecim                      , &
            existeh5                        , &
            fatalerror                      , &  
            existeconf                      , &
            existecmsh                      , &
            thereare_stoch                  , &
            creditosyaprinteados           
      
        integer (kind=4) ::                   &
            wirethickness                    ,&
            inductance_order                 ,&
            finaltimestep                    ,&
            ierr                             ,&
            layoutnumber                     ,&
            size                             ,&
            length                           ,&
            mpidir                           ,&
            flushminutesFields               ,&
            flushminutesData                 ,&
            flushsecondsFields               ,&
            flushsecondsData                 ,&
            forced                           ,&
            maxCPUtime                       ,&
            SGBCdepth                        ,&
            precision                      
                                    
        REAL (KIND=RKIND) ::                  &
            maxwireradius                    ,&
            mindistwires                     ,&
            attfactorc                       ,&
            attfactorw                       ,&
            cfltemp                          ,&
            cfl                              ,&
            SGBCfreq                         ,&
            SGBCresol                        ,&
            alphamaxpar                      ,&
            kappamaxpar                      ,&
            alphaOrden                     
                                           
        REAL (KIND=8) ::                      &
             time_begin                      ,&
             time_end                        
        real (kind=RKIND_wires) ::            &
             factorradius                    ,&
             factordelta                     
 !!!! 
!asigna variables locales

            forcing                                 =l%forcing                        
            singlefilewrite                         =l%singlefilewrite                
            ignoresamplingerrors                    =l%ignoresamplingerrors           
            ignoreerrors                            =l%ignoreerrors                   
            updateshared                            =l%updateshared                   
            prioritizeISOTROPICBODYoverall          =l%prioritizeISOTROPICBODYoverall 
            wirecrank                               =l%wirecrank                      
            CLIPREGION                              =l%CLIPREGION                     
            verbose                                 =l%verbose                        
            resume                                  =l%resume                         
            forcesteps                              =l%forcesteps                     
            resume_fromold                          =l%resume_fromold                 
            freshstart                              =l%freshstart                     
            run                                     =l%run                            
            createmap                               =l%createmap                      
            dontwritevtk                            =l%dontwritevtk                   
            vtkindex                                =l%vtkindex                       
            createmapvtk                            =l%createmapvtk                   
            hopf                                    =l%hopf                           
            run_with_dmma                           =l%run_with_dmma                  
            run_with_abrezanjas                     =l%run_with_abrezanjas            
            input_conformal_flag                    =l%input_conformal_flag           
            pausar                                  =l%pausar                         
            l_aux                                   =l%l_aux                          
            flag_conf_sgg                           =l%flag_conf_sgg                  
            takeintcripte                           =l%takeintcripte                  
            skindepthpre                            =l%skindepthpre                   
            SGBC                                    =l%SGBC                           
            conformalskin                           =l%conformalskin                  
            ade                                     =l%ade                            
            mibc                                    =l%mibc                           
            NOcompomur                              =l%NOcompomur                     
            MurAfterPML                             =l%MurAfterPML                    
            SGBCcrank                               =l%SGBCcrank                      
            sgbcDispersive                          =l%sgbcDispersive                 
            saveall                                 =l%saveall                        
            boundwireradius                         =l%boundwireradius                
            makeholes                               =l%makeholes                      
            mur_first                               =l%mur_first                      
            mur_second                              =l%mur_second                     
            connectendings                          =l%connectendings                 
            strictOLD                               =l%strictOLD                      
            mtlnberenger                            =l%mtlnberenger                   
            stableradholland                        =l%stableradholland               
            TAPARRABOS                              =l%TAPARRABOS                     
            fieldtotl                               =l%fieldtotl                      
            forceresampled                          =l%forceresampled                 
            isolategroupgroups                      =l%isolategroupgroups             
            groundwires                             =l%groundwires                    
            noSlantedcrecepelo                      =l%noSlantedcrecepelo             
            forcecfl                                =l%forcecfl                       
            niapapostprocess                        =l%niapapostprocess               
            planewavecorr                           =l%planewavecorr                  
            permitscaling                           =l%permitscaling                  
            stochastic                              =l%stochastic                     
            chosenyesornostochastic                 =l%chosenyesornostochastic        
            prioritizeCOMPOoverPEC                  =l%prioritizeCOMPOoverPEC         
            createh5bin                             =l%createh5bin                    
            deleteintermediates                     =l%deleteintermediates            
            existeNFDE                              =l%existeNFDE                     
            file11isopen                            =l%file11isopen                   
            NF2FFDecim                              =l%NF2FFDecim                     
            existeh5                                =l%existeh5                       
            fatalerror                              =l%fatalerror                     
            fatalerror                              =l%fatalerror                     
            existeconf                              =l%existeconf                     
            existecmsh                              =l%existecmsh                    
            thereare_stoch                          =l%thereare_stoch                 
            creditosyaprinteados                    =l%creditosyaprinteados       
            
            wirethickness                           =l%wirethickness                  
            inductance_order                        =l%inductance_order               
            finaltimestep                           =l%finaltimestep                  
            ierr                                    =l%ierr                           
            layoutnumber                            =l%layoutnumber                   
            size                                    =l%size                           
            length                                  =l%length                         
            mpidir                                  =l%mpidir                         
            flushminutesFields                      =l%flushminutesFields             
            flushminutesData                        =l%flushminutesData               
            flushsecondsFields                      =l%flushsecondsFields             
            flushsecondsData                        =l%flushsecondsData               
            forced                                  =l%forced                         
            maxCPUtime                              =l%maxCPUtime                     
            SGBCdepth                               =l%SGBCdepth                      
            precision                               =l%precision      
            
            maxwireradius                           =l%maxwireradius                  
            mindistwires                            =l%mindistwires                   
            attfactorc                              =l%attfactorc                     
            attfactorw                              =l%attfactorw                     
            cfltemp                                 =l%cfltemp                        
            cfl                                     =l%cfl                            
            SGBCfreq                                =l%SGBCfreq                       
            SGBCresol                               =l%SGBCresol                      
            alphamaxpar                             =l%alphamaxpar                    
            kappamaxpar                             =l%kappamaxpar                    
            alphaOrden                              =l%alphaOrden     
            
            time_begin                              =l%time_begin                     
            time_end                                =l%time_end   
            
            factorradius                            =l%factorradius                   
            factordelta                             =l%factordelta                    
        
!!!
   
   
   mpidirset=.false.
   existiarunningigual=.false.
   statuse=0
   !!!!!!!!!!!!!!!
   n = commandargumentcount (chaininput)
   IF (n == 0) THEN
      call print_basic_help(layoutnumber,creditosyaprinteados,time_out2) 
      call stoponerror(layoutnumber,size,'Error: NO arguments neither command line nor in launch file. Correct and remove pause...',.true.)
      statuse=-1
      !goto 666
   END IF
   opcionestotales=''
   do i=2,n
      CALL getcommandargument (chaininput, i, chain, length, statuse)
      IF (statuse /= 0) THEN
         CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
          statuse=-1
          !goto 666
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
          !goto 666
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
          goto 666
2762      CONTINUE
          if (.not.mpidirset) then
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
            mpidirset=.true.
          endif
           
!!!!!!!#ifndef CompileWithGamusino              
          case ('-pause')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=7312) pausetime
            GO TO 8312
7312        CALL stoponerror (layoutnumber, size, 'Invalid pause time',.true.)
          statuse=-1
          !goto 666
8312        IF (pausetime <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pause time',.true.)
          statuse=-1
          !goto 666
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
          !!!!goto 666
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
          !goto 666
2712      CONTINUE
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
          !goto 666
312         CONTINUE
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-singlefile')
            singlefilewrite = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))  
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
!!!!#endif        
!     
  
          CASE ('-verbose')
            verbose = .TRUE.
          CASE ('-ignoreerrors')
            ignoreerrors = .TRUE.         
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
          !goto 666
812         IF (maxCPUtime <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPU maximum time',.true.)
          statuse=-1
          !goto 666
            END IF   

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
          !goto 666
400         IF (flushminutesFields <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !goto 666
            END IF
          CASE ('-flushdata')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=301) flushminutesData
            GO TO 401
301         CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !goto 666
401         IF (flushminutesData <= 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid flushing interval',.true.)
          statuse=-1
          !goto 666
            END IF   
          CASE ('-run')
            run = .TRUE.                
          CASE ('-map')
            !dump the map files
            createmap = .TRUE.
          CASE ('-dontwritevtk')
            dontwritevtk=.true.
          CASE ('-vtkindex')
            vtkindex = .TRUE.  
          CASE ('-mapvtk')
            !dump the map files
#ifdef CompileWithVTK   
            createmapvtk = .TRUE.
#else
            createmapvtk = .FALSE.
#endif
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
          !goto 666
8621        IF (alphamaxpar < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML alpha factor',.true.)
          statuse=-1
          !goto 666
            END IF
            i = i + 1
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7121) alphaOrden
            GO TO 8121
7121        CALL stoponerror (layoutnumber, size, 'Invalid CPML order factor',.true.)
          statuse=-1
          !goto 666
8121        IF (alphaOrden < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML alpha factor',.true.)
          statuse=-1
          !goto 666
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
          !goto 666
8622        IF (kappamaxpar < 1.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid CPML kappa factor',.true.)
          statuse=-1
          !goto 666
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
          !goto 666
8672        IF (medioextra%sigma < 0.0_RKIND) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr sigma factor',.true.)
          statuse=-1
          !goto 666
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
          !goto 666
8662        IF (MEDIOEXTRA%size < 0) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid pmlcorr depth factor',.true.); statuse=-1; !goto 666
            END IF
            !          opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-attc')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=766) attfactorc
            GO TO 866
766         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !goto 666
866         IF ((attfactorc <= -1.0_RKIND ).or.(attfactorc > 1.0_RKIND)) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !goto 666
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
7466        CALL stoponerror (layoutnumber, size, 'Invalid SGBC depth ',.true.); statuse=-1; !goto 666
8466        IF (SGBCdepth < -1 ) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid SGBC depth',.true.); statuse=-1; !goto 666
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
74616       CALL stoponerror (layoutnumber, size, 'Invalid SGBC freq ',.true.); statuse=-1; !goto 666
84616       IF (SGBCfreq < 0. ) THEN
            CALL stoponerror (layoutnumber, size, 'Invalid SGBC freq',.true.); statuse=-1; !goto 666
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
74626       CALL stoponerror (layoutnumber, size, 'Invalid SGBC decay ',.true.); statuse=-1; !goto 666
84626       IF (SGBCresol < 0.0 ) THEN
            CALL stoponerror (layoutnumber, size, 'Invalid SGBC decay',.true.); statuse=-1; !goto 666
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
732         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !goto 666
832         IF ((attfactorw <= -1.0_RKIND ).or.(attfactorw > 1.0_RKIND)) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !goto 666
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-maxwireradius')
            boundwireradius=.true.
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=737) maxwireradius
            GO TO 837
737         CALL stoponerror (layoutnumber, size, 'Invalid dissipation factor',.true.); statuse=-1; !goto 666
837         IF ((maxwireradius <= 0.0_RKIND )) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid maximumwireradius',.true.); statuse=-1; !goto 666
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
          CASE ('-mindistwires')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=1732) mindistwires
            GO TO 1832
1732        CALL stoponerror (layoutnumber, size, 'Invalid minimum distance between wires',.true.); statuse=-1; !goto 666
1832        IF (mindistwires <= 0.0_RKIND ) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid minimum distance between wires',.true.); statuse=-1; !goto 666
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))
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
          !!case ('-experimentalVideal')
          !!    experimentalVideal=.true.
          !!    opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          case ('-forceresampled') !a menos que se pida explicitamente, no se resamplea 120123
              forceresampled=.true.
              opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))   
              
          CASE ('-wirethickness')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to real
            READ (f,*, ERR=7416) wirethickness
            GO TO 8416
7416        CALL stoponerror (layoutnumber, size, 'Invalid wirethickness ',.true.); statuse=-1; !goto 666
8416        IF (SGBCdepth < -1 ) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid wirethickness',.true.); statuse=-1; !goto 666
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))// ' ' // trim (adjustl(f))              
          CASE ('-wiresflavor')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
            READ (f, '(a)', ERR=3621) wiresflavor
            if (trim(adjustl(wiresflavor(1:1)))=='g') wiresflavor='slanted' 
            select case (trim(adjustl(wiresflavor)))
            case ('holland','old')
                wiresflavor='holland'
            case ('berenger','new')
                wiresflavor='berenger'
            case ('slanted','experimental')
                wiresflavor='slanted'
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
2561            CALL stoponerror (layoutnumber, size, 'Invalid precision for semistructured',.true.); statuse=-1; !goto 666
2562            IF (precision < 0 ) THEN
                    CALL stoponerror (layoutnumber, size, 'Invalid precision for semistructured',.true.); statuse=-1; !goto 666
                END IF
            !   
            end select   
            GO TO 4621
3621        CALL stoponerror (layoutnumber, size, 'Invalid wires flavor',.true.); statuse=-1; !goto 666
4621        IF ( ((trim(adjustl(wiresflavor)) /= 'holland')  .AND. &
                  (trim(adjustl(wiresflavor)) /= 'transition') .AND. &
                  (trim(adjustl(wiresflavor)) /= 'berenger') .AND.  &
                  (trim(adjustl(wiresflavor)) /= 'slanted').and. &
                  (trim(adjustl(wiresflavor))/='semistructured')) .or. &
             .not.((trim(adjustl(wiresflavor)) == 'holland')  .xor.  &
                  (trim(adjustl(wiresflavor)) == 'transition') .xor. &
                  (trim(adjustl(wiresflavor)) == 'berenger') .xor.  &
                  (trim(adjustl(wiresflavor)) == 'slanted').xor.  &
                  (trim(adjustl(wiresflavor)) =='semistructured')) )  THEN
               CALL stoponerror (layoutnumber, size, 'Invalid wires flavor->'//trim(adjustl(wiresflavor)),.true.); statuse=-1; !goto 666
            END IF    
#ifndef CompileWithThickWires
            select case (trim(adjustl(wiresflavor)))
            case ('holland','transition')
                if (wirethickness/=1) then
                    CALL stoponerror (layoutnumber, size, 'Holland wire flavor not available in this compilation',.true.); statuse=-1; !goto 666
                endif
            end select   
#endif    
#ifndef CompileWithThickWires
            select case (trim(adjustl(wiresflavor)))
            case ('holland')
                if (wirethickness/=1) then
                    CALL stoponerror (layoutnumber, size, 'Holland wire flavor thickness>1 requires recompiling',.true.); statuse=-1; !goto 666
                endif
            end select   
#endif
#ifdef CompileWithWires
            select case (trim(adjustl(wiresflavor)))
            case ('berenger','slanted','experimental','transition')   
                if (wirethickness/=1) then
                    CALL stoponerror (layoutnumber, size, 'Thickness>1 unsupported for this wireflavor',.true.); statuse=-1; !goto 666
                endif    
            end select   
#endif
#ifndef CompileWithBerengerWires
            select case (trim(adjustl(wiresflavor)))
            case ('berenger')
                CALL stoponerror (layoutnumber, size, 'Berenger wire flavor not available in this compilation',.true.); statuse=-1; !goto 666
            end select   
#endif
#ifndef CompileWithSlantedWires
            select case (trim(adjustl(wiresflavor)))
            case ('slanted','experimental')
                CALL stoponerror (layoutnumber, size, 'Experimental wire flavor not available in this compilation',.true.); statuse=-1; !goto 666
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
          CASE ('-noSlantedcrecepelo ') !opcion niapa excperimental 131219
            noSlantedcrecepelo  = .TRUE.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
          CASE ('-inductance')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f, '(a)', ERR=361) inductance_model
            GO TO 461
361         CALL stoponerror (layoutnumber, size, 'Invalid inductance model',.true.); statuse=-1; !goto 666
461         IF ((inductance_model /= 'ledfelt') .AND. (inductance_model /= 'berenger') .AND. &
            &    (inductance_model /= 'boutayeb')) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid inductance model',.true.); statuse=-1; !goto 666
            END IF
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain)) // ' ' // trim (adjustl(f))
          CASE ('-inductanceorder')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            READ (f,*, ERR=179) inductance_order
            GO TO 180
179         CALL stoponerror (layoutnumber, size, 'Invalid inductance order',.true.); statuse=-1; !goto 666
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
3762        CALL stoponerror (layoutnumber, size, 'Invalid Courant Number',.true.); statuse=-1; !goto 666
3862        IF (cfltemp <= 0.0 ) THEN
               call print11(layoutnumber,'------> Ignoring negative or null CFL Courant Number')
!!!!!!!!!               CALL stoponerror (layoutnumber, size, 'Invalid negative or null CFL Courant Number',.true.); statuse=-1; !goto 666 !!!sgg 151216 para evitar el error cfl 0 del problem-type sigue como si no estuviera
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
33762       CALL stoponerror (layoutnumber, size, 'Invalid pscale parameters',.true.); statuse=-1; !goto 666
33862       IF (EpsMuTimeScale_input_parameters%checkError()/=0) THEN
                CALL stoponerror (layoutnumber, size, &
   &'Invalid -pscale parameters: some parameters have to be greater than 0.0: -pscale t0(>=0) tend slope(>0)'&
                  &,.true.); statuse=-1; !goto 666
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
602         CALL stoponerror (layoutnumber, size, 'Invalid time step',.true.); statuse=-1; !goto 666
702         IF (finaltimestep < -2) THEN
               CALL stoponerror (layoutnumber, size, 'Invalid time step',.true.); statuse=-1; !goto 666
            END IF      
!!!!!!     
          CASE ('-factorradius')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=6032) factorradius
            GO TO 7032
6032         CALL stoponerror (layoutnumber, size, 'Invalid factorradius',.true.); statuse=-1; !goto 666
7032         continue
          CASE ('-factordelta')
            i = i + 1
            CALL getcommandargument (chaininput, i, f, length,  statuse)
            ! Converts the characters to integer
            READ (f,*, ERR=6072) factordelta
            GO TO 7072
6072         CALL stoponerror (layoutnumber, size, 'Invalid factordelta',.true.); statuse=-1; !goto 666
7072         continue
!!!!!!!!!!!!!
          CASE ('-stoch')
            stochastic=.true.
            chosenyesornostochastic=.true.
            opcionespararesumeo = trim (adjustl(opcionespararesumeo)) // ' ' // trim (adjustl(chain))
#ifndef CompileWithMPI
            CALL stoponerror (layoutnumber, size, 'Stochastic simulation unsupported without MPI compilation',.true.); statuse=-1; !goto 666
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
            CALL stoponerror (layoutnumber, size, 'Wrong switch '//trim(adjustl(chain)),.true.); statuse=-1; !goto 666
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
      CALL stoponerror (layoutnumber, size, 'strictOLD option not compatible with -connectendings',.true.); statuse=-1; !goto 666
   END IF
   IF (taparrabos .AND. (.not.strictOLD)) THEN
      CALL stoponerror (layoutnumber, size, '-nostrictOLD option requires -notaparrabos ',.true.); statuse=-1; !goto 666
   END IF
   IF (isolategroupgroups .AND. strictOLD) THEN
      CALL stoponerror (layoutnumber, size, '-intrawiresimplify option not compatible with -isolategroupgroups',.true.); statuse=-1; !goto 666
   END IF

   IF ((sgbc .AND. mibc)) THEN
      CALL stoponerror (layoutnumber, size, 'Use only one of -sgbc -mibc',.true.); statuse=-1; !goto 666
   END IF
   IF (freshstart .AND. resume) THEN
      CALL stoponerror (layoutnumber, size, 'Fresh Start option -s not compatible with restarting -r',.true.); statuse=-1; !goto 666
   END IF
   IF (freshstart .AND. resume_fromold) THEN
      CALL stoponerror (layoutnumber, size, 'Fresh Start option -s not compatible with -old',.true.); statuse=-1; !goto 666
   END IF
   IF (( .NOT. resume).and.(.not.run) .AND. resume_fromold) THEN
      CALL stoponerror (layoutnumber, size, 'Resume option -r must be used if issuing -old',.true.); statuse=-1; !goto 666
   END IF
   IF ((flushminutesFields /= 0) .AND. (deleteintermediates)) THEN
      CALL stoponerror (layoutnumber, size, '-delete is not compatible with -flush',.true.); statuse=-1; !goto 666
   END IF
   if (run_with_abrezanjas.and.run_with_dmma) then
      CALL stoponerror (layoutnumber, size, '-abrezanjas is not compatible with -dmma',.true.); statuse=-1; !goto 666
   END IF
   if (stochastic.and.(trim(adjustl(wiresflavor))/='holland')) then
      CALL stoponerror (layoutnumber, size, 'Old wires flavor is the only supported with stochastic',.true.); statuse=-1; !goto 666
   END IF
   if (stochastic.and.wirecrank) then
      CALL stoponerror (layoutnumber, size, 'Wires Crank Nicolson is unsupported with stochastic',.true.); statuse=-1; !goto 666
   END IF
   !!!si esta soportado 170719
   !! if (permitscaling.and.resume) then
   !!   CALL stoponerror (layoutnumber, size, 'Resuming with Permittivity scaling unsupported',.true.); statuse=-1; !goto 666
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
         CALL stoponerror (layoutnumber, size, 'Resume fields not present',.true.); statuse=-1; !goto 666
      END IF
      WRITE (dubuf,*) 'RESUMING simulation ', trim (adjustl(nEntradaRoot)), ' until n= ', finaltimestep
      CALL print11 (layoutnumber, dubuf)
   ELSE
      IF (resume3 .AND. ( .NOT. freshstart).and.(.not.run)) THEN
         CALL stoponerror (layoutnumber, size, 'Restarting file exists. Either specify -r to RESUME, -s to do a fresh START, or -run to run in whatever the case',.true.); statuse=-1; !goto 666
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
   if (((wiresflavor=='slanted').or.(wiresflavor=='semistructured')).AND.(mpidir/=3)) then
       continue !arreglado mpidir slanted 2019
       !         CALL stoponerror (layoutnumber, size, 'slanted wires unsupported with -mpidir {x,y}',.true.); statuse=-1; !goto 666
   endif
   if (input_conformal_flag.AND.(mpidir/=3)) then
        continue !arreglado mpidir conformal 2019
         !TODO: under test
         !26-sep-2018: lo comento 
         !CALL stoponerror (layoutnumber, size, 'CONFORMAL -conf  unsupported with -mpidir {x,y}',.true.); statuse=-1; !goto 666
   endif
   if (run_with_abrezanjas.AND.(mpidir/=3)) then
        continue !arreglado mpidir conformal 2019
         !under test
         !26-sep-2018: lo comento 
         !CALL stoponerror (layoutnumber, size, 'New abrezanjas thin gaps unsupported with -mpidir {x,y}',.true.); statuse=-1; !goto 666
   endif
   if (run_with_abrezanjas.AND.flag_conf_sgg) then
      !pass Mayo-2018
         !CALL stoponerror (layoutnumber, size, 'CONFORMAL -conf currently unsupported with new abrezanjas thin gaps (unsupported 2 simultaneous conformal meshes at this moment',.true.); statuse=-1; !goto 666
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
   !open ReportingFiles
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
            & trim(adjustl(opcionesoriginales)),.true.); statuse=-1; !goto 666
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
         if (layoutnumber==0) call insertalogtmp(layoutnumber)
      ELSE
         CLOSE (11)
         file11isopen=.false.
         OPEN (11, file=trim(adjustl(nEntradaRoot))//'_Report.txt', FORM='formatted')
         file11isopen=.true.
         if (layoutnumber==0) call insertalogtmp(layoutnumber)
      END IF
      !
      CALL get_secnds (time_out2)
      call print_credits(layoutnumber,creditosyaprinteados,time_out2)
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
      CALL stoponerror (layoutnumber, size, 'Some input file missing .h5/.nfde/.conf',.true.); statuse=-1; !goto 666
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
!!!asigna variable de salida

            l%forcing                                 =forcing                        
            l%singlefilewrite                         =singlefilewrite                
            l%ignoresamplingerrors                    =ignoresamplingerrors           
            l%ignoreerrors                            =ignoreerrors                   
            l%updateshared                            =updateshared                   
            l%prioritizeISOTROPICBODYoverall          =prioritizeISOTROPICBODYoverall 
            l%wirecrank                               =wirecrank                      
            l%CLIPREGION                              =CLIPREGION                     
            l%verbose                                 =verbose                        
            l%resume                                  =resume                         
            l%forcesteps                              =forcesteps                     
            l%resume_fromold                          =resume_fromold                 
            l%freshstart                              =freshstart                     
            l%run                                     =run                            
            l%createmap                               =createmap                      
            l%dontwritevtk                            =dontwritevtk                   
            l%vtkindex                                =vtkindex                       
            l%createmapvtk                            =createmapvtk                   
            l%hopf                                    =hopf                           
            l%run_with_dmma                           =run_with_dmma                  
            l%run_with_abrezanjas                     =run_with_abrezanjas            
            l%input_conformal_flag                    =input_conformal_flag           
            l%pausar                                  =pausar                         
            l%l_aux                                   =l_aux                          
            l%flag_conf_sgg                           =flag_conf_sgg                  
            l%takeintcripte                           =takeintcripte                  
            l%skindepthpre                            =skindepthpre                   
            l%SGBC                                    =SGBC                           
            l%conformalskin                           =conformalskin                  
            l%ade                                     =ade                            
            l%mibc                                    =mibc                           
            l%NOcompomur                              =NOcompomur                     
            l%MurAfterPML                             =MurAfterPML                    
            l%SGBCcrank                               =SGBCcrank                      
            l%sgbcDispersive                          =sgbcDispersive                 
            l%saveall                                 =saveall                        
            l%boundwireradius                         =boundwireradius                
            l%makeholes                               =makeholes                      
            l%mur_first                               =mur_first                      
            l%mur_second                              =mur_second                     
            l%connectendings                          =connectendings                 
            l%strictOLD                               =strictOLD                      
            l%mtlnberenger                            =mtlnberenger                   
            l%stableradholland                        =stableradholland               
            l%TAPARRABOS                              =TAPARRABOS                     
            l%fieldtotl                               =fieldtotl                      
            l%forceresampled                          =forceresampled                 
            l%isolategroupgroups                      =isolategroupgroups             
            l%groundwires                             =groundwires                    
            l%noSlantedcrecepelo                      =noSlantedcrecepelo             
            l%forcecfl                                =forcecfl                       
            l%niapapostprocess                        =niapapostprocess               
            l%planewavecorr                           =planewavecorr                  
            l%permitscaling                           =permitscaling                  
            l%stochastic                              =stochastic                     
            l%chosenyesornostochastic                 =chosenyesornostochastic        
            l%prioritizeCOMPOoverPEC                  =prioritizeCOMPOoverPEC         
            l%createh5bin                             =createh5bin                    
            l%deleteintermediates                     =deleteintermediates            
            l%existeNFDE                              =existeNFDE                     
            l%file11isopen                            =file11isopen                   
            l%NF2FFDecim                              =NF2FFDecim                     
            l%existeh5                                =existeh5                       
            l%fatalerror                              =fatalerror                     
            l%fatalerror                              =fatalerror                     
            l%existeconf                              =existeconf                    
            l%existecmsh                              =existecmsh                     
            l%thereare_stoch                          =thereare_stoch                 
            l%creditosyaprinteados                    =creditosyaprinteados       
            
            l%wirethickness                           =wirethickness                  
            l%inductance_order                        =inductance_order               
            l%finaltimestep                           =finaltimestep                  
            l%ierr                                    =ierr                           
            l%layoutnumber                            =layoutnumber                   
            l%size                                    =size                           
            l%length                                  =length                         
            l%mpidir                                  =mpidir                         
            l%flushminutesFields                      =flushminutesFields             
            l%flushminutesData                        =flushminutesData               
            l%flushsecondsFields                      =flushsecondsFields             
            l%flushsecondsData                        =flushsecondsData               
            l%forced                                  =forced                         
            l%maxCPUtime                              =maxCPUtime                     
            l%SGBCdepth                               =SGBCdepth                      
            l%precision                               =precision      
            
            l%maxwireradius                           =maxwireradius                  
            l%mindistwires                            =mindistwires                   
            l%attfactorc                              =attfactorc                     
            l%attfactorw                              =attfactorw                     
            l%cfltemp                                 =cfltemp                        
            l%cfl                                     =cfl                            
            l%SGBCfreq                                =SGBCfreq                       
            l%SGBCresol                               =SGBCresol                      
            l%alphamaxpar                             =alphamaxpar                    
            l%kappamaxpar                             =kappamaxpar                    
            l%alphaOrden                              =alphaOrden     
            
            l%time_begin                              =time_begin                     
            l%time_end                                =time_end   
            
            l%factorradius                            =factorradius                   
            l%factordelta                             =factordelta                    
                                         
    
!!    
666    return !el unico return que he dejado !240817
   
   end subroutine interpreta
   
   subroutine insertalogtmp(layoutnumber) !para 100920   
           CHARACTER (LEN=BUFSIZE) ::  dubuf
           integer (kind=4) :: MYUNIT11,LAYOUTNUMBER
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
   
   subroutine print_basic_help(layoutnumber,creditosyaprinteados,time_out2)     
      type (tiempo_t)  ::  time_out2 
      
      integer (kind=4) :: LAYOUTNUMBER 
      logical :: creditosyaprinteados
      call print_credits(layoutnumber,creditosyaprinteados,time_out2)
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, 'Basic usage: ')
      CALL print11 (layoutnumber, '&   For help use          -h ')
      CALL print11 (layoutnumber, '&   For launching use                     ')
      CALL print11 (layoutnumber, '&                         -i inputfile (native)')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      return
      end subroutine print_basic_help
 
   subroutine print_credits(layoutnumber,creditosyaprinteados,time_out2)
      TYPE (tiempo_t) :: time_out2
      CHARACTER (LEN=BUFSIZE) ::  dubuf
      integer (kind=4) :: layoutnumber
      logical :: creditosyaprinteados
      if (creditosyaprinteados) return
      creditosyaprinteados=.true.
      CALL print11 (layoutnumber, '=========================')
      CALL print11 (layoutnumber, 'SEMBA-FDTD SOLVER')
      CALL print11 (layoutnumber, '=========================')
      
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) trim (adjustl(dataversion))
      CALL print11 (layoutnumber, dubuf)
      WRITE (dubuf,*) SEPARADOR // SEPARADOR // SEPARADOR
      CALL print11 (layoutnumber, dubuf)
      CALL print11 (layoutnumber, 'All rights reserved by the University of Granada (Spain)')
      CALL print11 (layoutnumber, '       Contact person: Salvador G. Garcia <salva@ugr.es>')
      CALL print11 (layoutnumber, ' ')
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
#ifdef CompileWithConformal
      CALL print11 (layoutnumber, 'Compiled WITH Conformal support')
#else
      CALL print11 (layoutnumber, 'Compiled without Conformal support')
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

   subroutine print_help(layoutnumber)
      integer (kind=4) :: layoutnumber,ierr
      CHARACTER (LEN=BUFSIZE) :: buff
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, 'Command line arguments: ')
      CALL print11 (layoutnumber, '___________________________________________________________________________')
      CALL print11 (layoutnumber, '-i geometryfile        : Simulates the Native format input file            ')
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
      CALL print11 (layoutnumber, '-skindepthpre          : Pre-processor for SGBC metals including skin depth.')
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
!      write(buff,'(a,e10.2e3)')   '&                        Default= ',attfactorc
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
!      write(buff,'(a,2e10.2e3)')  '&                        Default= ',alphamaxpar,alphaOrden
      CALL print11 (layoutnumber, buff)
!      write(buff,'(a,e10.2e3)')   '-pmlkappa number       : CPML Kappa (>=1). Default= ',kappamaxpar
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
      CALL print11 (layoutnumber, '-wiresflavor {new/Slanted.or.experimental.or.slanted/transition/semistructured precision} : model for the wires    ')   
#endif
#ifdef CompileWithWires
!      CALL print11 (layoutnumber, '&                        (default '//trim(adjustl(wiresflavor))//')   ')
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
      CALL print11 (layoutnumber, '-noSlantedcrecepelo : Ground open nodes. Experimental. Do not use.')
      CALL print11 (layoutnumber, '-connectendings        : Joins ohmicly endings nodes of adjacent segments  ')
      CALL print11 (layoutnumber, '&                        from multiwires (segments do no collapse).        ')
      CALL print11 (layoutnumber, '&                        regardless of whether they are actually connected ')
      CALL print11 (layoutnumber, '&                        through the LeftEnd/RightEnd numbering ')
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
!      write(buff,'(a,e10.2e3)')   '&                        Default= ',mindistwires
      CALL print11 (layoutnumber, buff)
      CALL print11 (layoutnumber, '-inductance {ledfelt/berenger/boutayeb} : model for the self-inductance    ')
!      CALL print11 (layoutnumber, '&                        (default '//trim(adjustl(inductance_model))//')   ')
      CALL print11 (layoutnumber, '-inductanceorder order : order for the self-inductance calculation for     ')
      CALL print11 (layoutnumber, '&                        slanted wires in experimental wiresflavor         ')
!      write(buff,'(a,i8)')   '&                        Default= ',inductance_order
      CALL print11 (layoutnumber, '-attw   dissipation    : Positive factor (under 1) for stability in wires, ')
!      write(buff,'(a,e10.2e3)')   '&                        Default= ',attfactorw                              
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
   end subroutine removeintraspaces


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine buscaswitchficheroinput(chaininput,statuse,fichin,filefde,fileh5,l)     
   
   
   type (tiempo_t)  ::  time_out2   
   CHARACTER (LEN=BUFSIZE) :: chaininput,fichin,dato,buff,f,chain,fileFDE,fileH5
   
   
   integer (kind=4) :: i,n,statuse, NUM_NFDES,TEMP_NUMNFDES,p,ierr
   
  
   CHARACTER (LEN=5) :: NFDEEXTENSION, CONFEXTENSION, CMSHEXTENSION
    
!!!!!!!!!!!!!        
   type (entrada_t) :: l
!!!!!!!!!    
!!! variables locales
        logical ::   &
            ignoreerrors                    , &
            verbose                         , &
            creditosyaprinteados            , &
            existeNFDE                      , &
            existeCONF                      , &
            existeCMSH           
                    
        integer (kind=4) ::                   &
            layoutnumber                     ,&
            size                             ,&
            length                           ,&
            mpidir                           
         
 !!!! 
!asigna variables locales
           
            ignoreerrors                            =l%ignoreerrors                       
            verbose                                 =l%verbose                 
            creditosyaprinteados                    =l%creditosyaprinteados 
            
            existeNFDE                              =l%existeNFDE
            existeCONF                              =l%existeCONF
            existeCMSH                              =l%existeCMSH
                           
            layoutnumber                            =l%layoutnumber                   
            size                                    =l%size                           
            length                                  =l%length                         
            mpidir                                  =l%mpidir  
!!!
   
   NFDEEXTENSION='.nfde'; CONFEXTENSION='.conf'; CMSHEXTENSION='.cmsh'
   statuse=0
   !!!!!!!!!!!!!!!
   n = commandargumentcount (chaininput)
   IF (n == 0) THEN
      call print_basic_help(layoutnumber,creditosyaprinteados,time_out2)  
      call stoponerror(layoutnumber,size,'Error: NO arguments neither command line nor in launch file. Correct and remove pause...',.true.)
      statuse=-1
      goto 667
   END IF

   IF (n > 0) THEN
      num_nfdes=0
      i = 2
      DO while (i <= n)
         CALL getcommandargument (chaininput, i, chain, length, statuse)
         IF (statuse /= 0) THEN
            CALL stoponerror (layoutnumber, size, 'Reading input',.true.)
            goto 667
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
            goto 667
2762        CONTINUE
          CASE ('-h')
            call print_credits(layoutnumber,creditosyaprinteados,time_out2)
            call print_help(layoutnumber)
            call print_credits(layoutnumber,creditosyaprinteados,time_out2)
            STOP
          CASE ('-hh')
            call print_credits(layoutnumber,creditosyaprinteados,time_out2)
            call print_help(layoutnumber)
            call print_credits(layoutnumber,creditosyaprinteados,time_out2)
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
                goto 667
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
                  goto 667
               END IF
               INQUIRE (file=trim(adjustl(fichin))//NFDEEXTENSION, EXIST=existeNFDE)
               IF ( .NOT. existeNFDE) THEN
                  buff='The input file was not found '//trim(adjustl(fichin))//NFDEEXTENSION
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  goto 667  
               END IF
!aniadido para chequear que no haya .conf sin haber invocado el -conf 15/12/16 sgg
               INQUIRE (file=trim(adjustl(fichin))//CONFEXTENSION, EXIST=existeCONF)
               IF ((existeCONF).AND.(.not.(input_conformal_flag))) THEN
                  buff='No -conf issued but existing file '//trim(adjustl(fichin))//confEXTENSION//' . Either remove file or relaunch with -conf'
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  goto 667  
               END IF
               INQUIRE (file=trim(adjustl(fichin))//CMSHEXTENSION, EXIST=existeCMSH)
               IF ((existeCMSH).AND.(.not.(input_conformal_flag))) THEN
                  buff='No -conf issued but existing file '//trim(adjustl(fichin))//CMSHEXTENSION//' . Either remove file or relaunch with -conf'
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  goto 667  
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
            goto 667
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
                  goto 667
               END IF
               INQUIRE (file=trim(adjustl(fichin))//NFDEEXTENSION, EXIST=existeNFDE)
               IF ( .NOT. existeNFDE) THEN
                  buff='The input file was not found '//trim(adjustl(fichin))//NFDEEXTENSION
                  CALL stoponerror (layoutnumber, size, buff,.true.)
                  statuse=-1
                  goto 667
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
        CALL stoponerror (layoutnumber, size, 'ERROR! -> No input file was specified. Use -i ****.nfde',.true.); statuse=-1; goto 667
    END IF

   fileFDE = trim (adjustl(fichin)) // NFDEEXTENSION
   fileH5 = trim (adjustl(fichin)) // '.h5'
   CALL INITWARNINGFILE (layoutnumber, size, trim (adjustl(fichin))//'_tmpWarnings.txt',verbose,ignoreErrors)
   
!asigna variables locales
           
            l%ignoreerrors                            =ignoreerrors                       
            l%verbose                                 =verbose                 
            l%creditosyaprinteados                    =creditosyaprinteados     
            
            l%existeNFDE                              =existeNFDE
            l%existeCONF                              =existeCONF
            l%existeCMSH                              =existeCMSH
                           
            l%layoutnumber                            =layoutnumber                   
            l%size                                    =size                           
            l%length                                  =length                         
            l%mpidir                                  =mpidir  
!!!
667   return
  end subroutine buscaswitchficheroinput

!!!!!!!!!!!!!!!!!!   


   
   end module interpreta_switchwes_m