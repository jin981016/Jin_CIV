  ±J  ¨   k820309              2021.13.0   ºX}g                                                                                                          
       random_mod.f90 RANDOM       '       RANDOM_SPHERE RAND_NUMBER RAND_REXP RAND_R2EXP RAND_ZEXP RAND_SECH2 RAND_PERMUTATION RAND_PICK RAND_CYCLIC_PERMUTATION RAND_BINOMIAL RAND_MULTINOMIAL RAND_T RAND_SCALED_INV_CHI2 RAND_BACTRIAN RAND_STRETCH RAND_TRAVERSE MAKE_MVCOVMAT2 RANDOM_MVNORM MAKE_MVCOVMAT RAND_RESONANCE RAND_RESONANCE_RYBICKI RAND_VOIGT RAND_RAYLEIGH RAND_CAUCHY RAND_HENYEY_GREENSTEIN gen@INIT_RANDOM_SEED gen@RANDOM_SEED gen@RANDOM_NUMBER gen@RAND_RESONANCE_VZ gen@RAND_LORENTZ gen@RANDOM_T gen@RANDOM_GAUSS gen@RAND_GAUSS gen@RANDOM_3DSPHERE gen@RAND_GAMMA gen@RAND_EXP gen@RAND_INDEX gen@RAND_PLANCK gen@RAND_PLANCK_NUM            0                                                
       REAL32 REAL64 INT32 INT64                                                                u #INIT_RANDOM_MT_SEED    #         @     @                                                        #MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIFCMB5    #MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIFCMB9    #MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIV1    #MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIV2    #MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIVC    #ISEED                                                                                                                       #INIT_RANDOM_MT_SEED%MPIFCMB5%MPI_UNWEIGHTED                                                                                                                                                    #INIT_RANDOM_MT_SEED%MPIFCMB9%MPI_WEIGHTS_EMPTY                                                                                                                                                    #INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_BOTTOM    #INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_IN_PLACE 	   #INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_STATUS_IGNORE 
                                                                                                                         	                                                                    
                                p          p            p                                                                                                             #INIT_RANDOM_MT_SEED%MPIPRIV2%MPI_STATUSES_IGNORE    #INIT_RANDOM_MT_SEED%MPIPRIV2%MPI_ERRCODES_IGNORE                                                                                      p          p          p            p          p                                                                                                                   p          p            p                                                                                                             #INIT_RANDOM_MT_SEED%MPIPRIVC%MPI_ARGVS_NULL    #INIT_RANDOM_MT_SEED%MPIPRIVC%MPI_ARGV_NULL    -                                                                                  p          p          p            p          p                                  -                                                                                 p          p            p                                             @                                                                                                                   u #RANDOM_MT_SEED    #         @     @                                                         #SIZE    #PUT    #GET              F @                                                              
 @                                                                         &                                                     F @                                                                           &                                                                                                          u #RANDOM_MT32    #RANDOM_MT64    #RANDOM_MT_V32    #RANDOM_MT_V64    #RANDOM_MT_VV32    #RANDOM_MT_VV64     #RANDOM_MT_VVV32 "   #RANDOM_MT_VVV64 $   #         @     @                                                         #HARVEST              D                                              	       #         @     @                                                         #HARVEST              D                                              
       #         @     @                                                         #HARVEST              D@                                                          	               &                                           #         @     @                                                         #HARVEST              D@                                                          
               &                                           #         @     @                                                         #HARVEST           0  D@                                                          	               &                   &                                           #         @     @                                                          #HARVEST !          0  D@                                       !                   
               &                   &                                           #         @     @                                     "                    #HARVEST #          0  D@                                       #                   	               &                   &                   &                                           #         @     @                                     $                    #HARVEST %          0  D@                                       %                   
               &                   &                   &                                                                                                          u #RAND_RESONANCE_VZ_SEON &   %         @    @                                    &                    
       #X0IN '   #A (             
  @                                       '     
                
                                          (     
                                                                     u #RAND_CAUCHY )                                                                  u #RANDOM_T0 *   #RANDOM_T1 -   #         @     @                                     *                    #T +   #DOF ,             D                                         +     
                 
  @                                        ,           #         @     @                                     -                    #T .   #DOF /             D@                                       .                   
 2              &                                                     
  @                                        /                                                                          u #RANDOM_GAUSS1 0   #RANDOM_GAUSS2 2   #RANDOM_GAUSS3 4   #         @     @                                     0                    #HARVEST 1             D                                         1     
       #         @     @                                     2                    #HARVEST 3          0  D@                                       3                   
 $              &                                           #         @     @                                     4                    #HARVEST 5          0  D@                                       5                   
 ,              &                   &                                                                                                          u #RAND_GAUSS1 6   %         @    @                                    6                     
                                                                      u #RANDOM_3DSPHERE1 7   #RANDOM_3DSPHERE2 <   #         @     @                                     7                    #RADIUS 8   #X 9   #Y :   #Z ;             
                                          8     
                D                                         9     
                 D                                         :     
                 D                                         ;     
       #         @     @                                     <                    #RADIUS =   #VEC >             
                                          =     
                D                                         >                   
 .    p          p            p                                                                                         u #GAMDEV ?   #GAMDEV_R A   %         @   @                                    ?                    
       #IA @             
                                           @           %         @    @                                    A                    
       #A B             
  @                                       B     
                                                                     u #RAND_EXP32 C   #RAND_EXP64 F   %         @    @                                    C                    	       #X0 D   #XCUT E             
                                           D     	                
                                           E     	      %         @    @                                    F                    
       #X0 G   #XCUT H             
                                          G     
                
                                          H     
                                                                     u #RAND_INDEX1 I   #RAND_INDEXN K   %         @    @                                    I                           #P J          0  
 @                                       J                   
 O             &                                           (        `    @                                    K                    R                    #P L   #NTRIAL M   p          5 O p            5 O p                                 0  
 @                                       L                   
 Q             &                                                     
  @                                        M                                                                          u #RAND_PLANCK32 N   #RAND_PLANCK64 Q   %         @    @                                    N                    	       #T O   #WAVELENGTH P             
                                          O     	                
 @                                        P           %         @    @                                    Q                    
       #T R   #WAVELENGTH S             
                                          R     
                
 @                                        S                                                                          u #RAND_PLANCK_NUM32 T   #RAND_PLANCK_NUM64 W   %         @    @                                    T                    	       #T U   #WAVELENGTH V             
                                          U     	                
 @                                        V           %         @    @                                    W                    
       #T X   #WAVELENGTH Y             
                                          X     
                
 @                                        Y                          À   @                                Z     '                    #PTR [                 D                                     [                                                                         \     RANDOM_NUMBER #         @                                            ]                    #POINT ^             
D@                                       ^                   
 A              &                                           %         @                                          _                     
       %         @                                          `                    
       #RMAX a             
                                          a     
      %         @                                          b                    
       #RMAX c             
                                          c     
      %         @                                          d                    
       #ZMAX e             
                                          e     
      %         @                                          f                    
       #ZMAX g             
  @                                       g     
      (        `                                          h                    C                    #N i   p          5 O p            5 O p                                    
                                           i           (        `                                          j                    F                    #NELE k   #NPICK l   p          5 O p            5 O p                                    
                                           k                     
                                           l           (        `                                          m                    H                    #N n   p          5 O p            5 O p                                    
                                           n           %         @                                          o                           #PP p   #NTRIAL q             
  @                                       p     
                
  @                                        q           (        `                                         r                    M                    #P s   #NTRIAL t   p          H r u     7
S
O
 p        j            j                                      H r u     7
S
O
 p        j            j                                                           0  
 @                                       s                   
 L             &                                                     
                                           t           %         @                                          v                    
       #DOF w             
                                           w           %         @                                          x                    
       #DOF y   #VAR z             
                                           y                     
                                          z     
      %         @                                          {                     
       %         @                                          |                    
       #A }             
  @                                       }     
      %         @                                          ~                    
       #A              
                                               
      #         @                                                                #X    #AVG    #COV    #CHOLE              
 @                                                          
 7             &                   &                                                     D@                                                          
 8              &                                                     D@                                                          
 9              &                                                     D@                                                          
 :              &                                           #         @                                                                #CHOLE    #X    #DOF_T    #USE_BACTRIAN              
 @                                                          
 5             &                                                     D@                                                          
 6              &                                                     
 @                                                             
 @                                                   #         @                                                                #X    #AVG    #COV    #CHOLE    #UPDATE              
 @                                                          
 ;             &                                                     
D@                                                          
 <              &                                                     
D@                                                          
 =              &                                                     
D@                                                          
 >              &                                                     
 @                                                   %         @                                                              
       #E1              
                                               
      %         @                                                              
       #E1              
                                               
      %         @                                                              
       #A              
                                               
      %         @                                                               
       %         @                                         )                     
       %         @                                                              
       #G              
                                               
                    @                                     u     SIZE              fn#fn    ¾   f  b   uapp(RANDOM     $  b   J  ISO_FORTRAN_ENV %     a       gen@INIT_RANDOM_SEED $   ç  s     INIT_RANDOM_MT_SEED R   Z       MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIFCMB5+MPI_CONSTANTS=MPIFCMB5 J   ã  P     INIT_RANDOM_MT_SEED%MPIFCMB5%MPI_UNWEIGHTED+MPI_CONSTANTS R   3       MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIFCMB9+MPI_CONSTANTS=MPIFCMB9 M   ¿  P     INIT_RANDOM_MT_SEED%MPIFCMB9%MPI_WEIGHTS_EMPTY+MPI_CONSTANTS R     è     MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIV1+MPI_CONSTANTS=MPIPRIV1 F   ÷  P     INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_BOTTOM+MPI_CONSTANTS H   G  P     INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_IN_PLACE+MPI_CONSTANTS M     ¬     INIT_RANDOM_MT_SEED%MPIPRIV1%MPI_STATUS_IGNORE+MPI_CONSTANTS R   C	  Ä     MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIV2+MPI_CONSTANTS=MPIPRIV2 O   
  Ì     INIT_RANDOM_MT_SEED%MPIPRIV2%MPI_STATUSES_IGNORE+MPI_CONSTANTS O   Ó
  ¬     INIT_RANDOM_MT_SEED%MPIPRIV2%MPI_ERRCODES_IGNORE+MPI_CONSTANTS R     ¹     MPI_CONSTANTS!INIT_RANDOM_MT_SEED%MPIPRIVC+MPI_CONSTANTS=MPIPRIVC J   8  Ì     INIT_RANDOM_MT_SEED%MPIPRIVC%MPI_ARGVS_NULL+MPI_CONSTANTS I     ¬     INIT_RANDOM_MT_SEED%MPIPRIVC%MPI_ARGV_NULL+MPI_CONSTANTS *   °  H   a   INIT_RANDOM_MT_SEED%ISEED     ø  \       gen@RANDOM_SEED    T  l      RANDOM_MT_SEED $   À  H   a   RANDOM_MT_SEED%SIZE #        a   RANDOM_MT_SEED%PUT #        a   RANDOM_MT_SEED%GET "   0  â       gen@RANDOM_NUMBER      ]      RANDOM_MT32 $   o  H   a   RANDOM_MT32%HARVEST    ·  ]      RANDOM_MT64 $     H   a   RANDOM_MT64%HARVEST    \  ]      RANDOM_MT_V32 &   ¹     a   RANDOM_MT_V32%HARVEST    M  ]      RANDOM_MT_V64 &   ª     a   RANDOM_MT_V64%HARVEST    >  ]      RANDOM_MT_VV32 '     ¬   a   RANDOM_MT_VV32%HARVEST    G  ]      RANDOM_MT_VV64 '   ¤  ¬   a   RANDOM_MT_VV64%HARVEST     P  ]      RANDOM_MT_VVV32 (   ­  Ä   a   RANDOM_MT_VVV32%HARVEST     q  ]      RANDOM_MT_VVV64 (   Î  Ä   a   RANDOM_MT_VVV64%HARVEST &     d       gen@RAND_RESONANCE_VZ '   ö  i      RAND_RESONANCE_VZ_SEON ,   _  H   a   RAND_RESONANCE_VZ_SEON%X0IN )   §  H   a   RAND_RESONANCE_VZ_SEON%A !   ï  Y       gen@RAND_LORENTZ    H  f       gen@RANDOM_T    ®  `      RANDOM_T0      H   a   RANDOM_T0%T    V  H   a   RANDOM_T0%DOF      `      RANDOM_T1    þ     a   RANDOM_T1%T      H   a   RANDOM_T1%DOF !   Ú         gen@RANDOM_GAUSS    [  ]      RANDOM_GAUSS1 &   ¸  H   a   RANDOM_GAUSS1%HARVEST       ]      RANDOM_GAUSS2 &   ]     a   RANDOM_GAUSS2%HARVEST    ñ  ]      RANDOM_GAUSS3 &   N  ¬   a   RANDOM_GAUSS3%HARVEST    ú  Y       gen@RAND_GAUSS    S   X      RAND_GAUSS1 $   «   t       gen@RANDOM_3DSPHERE !   !  q      RANDOM_3DSPHERE1 (   !  H   a   RANDOM_3DSPHERE1%RADIUS #   Ø!  H   a   RANDOM_3DSPHERE1%X #    "  H   a   RANDOM_3DSPHERE1%Y #   h"  H   a   RANDOM_3DSPHERE1%Z !   °"  e      RANDOM_3DSPHERE2 (   #  H   a   RANDOM_3DSPHERE2%RADIUS %   ]#     a   RANDOM_3DSPHERE2%VEC    ù#  b       gen@RAND_GAMMA    [$  `      GAMDEV    »$  H   a   GAMDEV%IA    %  _      GAMDEV_R    b%  H   a   GAMDEV_R%A    ª%  h       gen@RAND_EXP    &  j      RAND_EXP32    |&  H   a   RAND_EXP32%X0     Ä&  H   a   RAND_EXP32%XCUT    '  j      RAND_EXP64    v'  H   a   RAND_EXP64%X0     ¾'  H   a   RAND_EXP64%XCUT    (  j       gen@RAND_INDEX    p(  _      RAND_INDEX1    Ï(     a   RAND_INDEX1%P    c)  Ï      RAND_INDEXN    2*     a   RAND_INDEXN%P #   Æ*  H   a   RAND_INDEXN%NTRIAL     +  n       gen@RAND_PLANCK    |+  o      RAND_PLANCK32     ë+  H   a   RAND_PLANCK32%T )   3,  H   a   RAND_PLANCK32%WAVELENGTH    {,  o      RAND_PLANCK64     ê,  H   a   RAND_PLANCK64%T )   2-  H   a   RAND_PLANCK64%WAVELENGTH $   z-  v       gen@RAND_PLANCK_NUM "   ð-  o      RAND_PLANCK_NUM32 $   _.  H   a   RAND_PLANCK_NUM32%T -   §.  H   a   RAND_PLANCK_NUM32%WAVELENGTH "   ï.  o      RAND_PLANCK_NUM64 $   ^/  H   a   RAND_PLANCK_NUM64%T -   ¦/  H   a   RAND_PLANCK_NUM64%WAVELENGTH $   î/  a      C_PTR+ISO_C_BINDING ,   O0  P   %   C_PTR%PTR+ISO_C_BINDING=PTR    0  N       RANDOM_NUMBER    í0  [       RANDOM_SPHERE $   H1     a   RANDOM_SPHERE%POINT    Ü1  X       RAND_NUMBER    42  b       RAND_REXP    2  H   a   RAND_REXP%RMAX    Þ2  b       RAND_R2EXP     @3  H   a   RAND_R2EXP%RMAX    3  b       RAND_ZEXP    ê3  H   a   RAND_ZEXP%ZMAX    24  b       RAND_SECH2     4  H   a   RAND_SECH2%ZMAX !   Ü4  Ã       RAND_PERMUTATION #   5  H   a   RAND_PERMUTATION%N    ç5  Ñ       RAND_PICK    ¸6  H   a   RAND_PICK%NELE      7  H   a   RAND_PICK%NPICK (   H7  Ã       RAND_CYCLIC_PERMUTATION *   8  H   a   RAND_CYCLIC_PERMUTATION%N    S8  l       RAND_BINOMIAL !   ¿8  H   a   RAND_BINOMIAL%PP %   9  H   a   RAND_BINOMIAL%NTRIAL !   O9        RAND_MULTINOMIAL #   Î:     a   RAND_MULTINOMIAL%P (   b;  H   a   RAND_MULTINOMIAL%NTRIAL    ª;  a       RAND_T    <  H   a   RAND_T%DOF %   S<  j       RAND_SCALED_INV_CHI2 )   ½<  H   a   RAND_SCALED_INV_CHI2%DOF )   =  H   a   RAND_SCALED_INV_CHI2%VAR    M=  X       RAND_BACTRIAN    ¥=  _       RAND_STRETCH    >  H   a   RAND_STRETCH%A    L>  _       RAND_TRAVERSE     «>  H   a   RAND_TRAVERSE%A    ó>  t       MAKE_MVCOVMAT2 !   g?  ¬   a   MAKE_MVCOVMAT2%X #   @     a   MAKE_MVCOVMAT2%AVG #   §@     a   MAKE_MVCOVMAT2%COV %   ;A     a   MAKE_MVCOVMAT2%CHOLE    ÏA         RANDOM_MVNORM $   NB     a   RANDOM_MVNORM%CHOLE     âB     a   RANDOM_MVNORM%X $   vC  H   a   RANDOM_MVNORM%DOF_T +   ¾C  H   a   RANDOM_MVNORM%USE_BACTRIAN    D         MAKE_MVCOVMAT     D     a   MAKE_MVCOVMAT%X "   E     a   MAKE_MVCOVMAT%AVG "   ®E     a   MAKE_MVCOVMAT%COV $   BF     a   MAKE_MVCOVMAT%CHOLE %   ÖF  H   a   MAKE_MVCOVMAT%UPDATE    G  `       RAND_RESONANCE "   ~G  H   a   RAND_RESONANCE%E1 '   ÆG  `       RAND_RESONANCE_RYBICKI *   &H  H   a   RAND_RESONANCE_RYBICKI%E1    nH  _       RAND_VOIGT    ÍH  H   a   RAND_VOIGT%A    I  X       RAND_RAYLEIGH    mI  X       RAND_CAUCHY '   ÅI  _       RAND_HENYEY_GREENSTEIN )   $J  H   a   RAND_HENYEY_GREENSTEIN%G &   lJ  E      RAND_MULTINOMIAL%SIZE 