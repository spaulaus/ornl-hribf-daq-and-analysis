*
*   Module types
*
      integer*4  PHIL
      integer*4  SILENA
      integer*4  LRS_4300
      integer*4  HHIRF_ADC
      integer*4  LRS_4508
      integer*4  LRS_4418
      integer*4  UE_CLOCK
      integer*4  PHIL_7106
      integer*4  LRS_4415A
      integer*4  LRS_3511
      integer*4  LRS_4413
      integer*4  LRS_4416B
      integer*4  LRS_3377
      integer*4  SHAPER
      integer*4  CAEN_775
      integer*4  CAEN_785
      integer*4  MCSQ

      parameter  (PHIL = 1)
      parameter  (SILENA = 2)
      parameter  (LRS_4300 = 3)
      parameter  (HHIRF_ADC = 4)
      parameter  (LRS_4508 = 5)
      parameter  (LRS_4418 = 6)
      parameter  (UE_CLOCK = 7)
      parameter  (PHIL_7106 = 8)
      parameter  (LRS_4415A = 9)
      parameter  (LRS_3511 = 10)
      parameter  (LRS_4413 = 11)
      parameter  (LRS_4416B = 12)
      parameter  (LRS_3377 = 13)
      parameter  (SHAPER = 14)
      parameter  (CAEN_775 = 15)
      parameter  (CAEN_785 = 16)
      parameter  (MCSQ = 17)

*
*   Module data structure
*
      structure /modu_data/
         integer*4 mt_type    ! PHIL, SILENA etc.
         integer*4 mt_line    ! line number
         integer*4 mt_error   ! 0 means OK, nonzero means error
         integer*4 c          ! Crate number
         integer*4 n          ! Slot number
         integer*4 low_set    ! nonzero means lower_thresholds were set
         integer*4 low(32)    ! lower_thresholds
         integer*4 up_set     ! nonzero means upper_thresholds were set
         integer*4 up(16)     ! upper_thresholds
         integer*4 ped_set    ! nonzero means pedestals were set
         integer*4 ped(16)    ! pedestals/gains
         integer*4 off_set    ! nonzero means offsets were set
         integer*4 off(16)    ! offsets
         integer*4 com_set    ! nonzero means common_threshold was set
         integer*4 com        ! common_threshold
         integer*4 enable_set ! nonzero means enables were set
         integer*4 enable(16) ! channel enables
         integer*4 gen_set    ! nonzero means generate function was specified
         integer*4 gen_type   ! generate function type
         integer*4 delay_set  ! nonzero means delays were specified
         integer*4 delay(16)  ! delays
         integer*4 mode_set   ! nonzero means mode was set
         integer*4 mode       ! mode word
         integer*4 ovfl_set   ! nonzero means overflow was set
         integer*4 ovfl       ! overflow flag
      end structure

*
*   Data types
*
      integer*4  LOW
      integer*4  UP
      integer*4  OFF
      integer*4  PED
      integer*4  COM
      integer*4  ENABLE
      integer*4  GE_ONLY
      integer*4  BGO_ONLY
      integer*4  ANTI
      integer*4  DELAY
      integer*4  MODE
      integer*4  OVFL
      integer*4  GAINS

      parameter  (LOW = 1)
      parameter  (UP = 2)
      parameter  (OFF = 3)
      parameter  (PED = 4)
      parameter  (COM = 5)
      parameter  (ENABLE = 6)
      parameter  (GE_ONLY = 7)
      parameter  (BGO_ONLY = 8)
      parameter  (ANTI = 9)
      parameter  (DELAY = 10)
      parameter  (MODE = 11)
      parameter  (OVFL = 12)
      parameter  (GAINS = 4)

*
*  Error codes
*
      integer*4  READ_EOF
      integer*4  READ_MT
      integer*4  ERR_NUM
      integer*4  ERR_C_VAL
      integer*4  ERR_N_VAL
      integer*4  ERR_2N
      integer*4  ERR_CONV
      integer*4  ERR_MOD_UNKN
      integer*4  ERR_MOD_AMBIG
      integer*4  ERR_DAT_UNKN
      integer*4  ERR_DAT_AMBIG
      integer*4  ERR_NAN
      integer*4  ERR_DATA_TYPE
      integer*4  ERR_DATA_VAL
      integer*4  ERR_DUP_DATA
      integer*4  ERR_TOO_FEW
      integer*4  ERR_PRIOR_MOD_TYPE
      integer*4  ERR_PRIOR_LOW
      integer*4  ERR_PRIOR_UP
      integer*4  ERR_PRIOR_OFF
      integer*4  ERR_PRIOR_PED
      integer*4  ERR_PRIOR_COM
      integer*4  ERR_PRIOR_ENABLE
      integer*4  ERR_PRIOR_GEN
      integer*4  ERR_PRIOR_DELAY
      integer*4  ERR_MULTI_C
      integer*4  ERR_MULTI_N
      integer*4  ERR_NO_N
      integer*4  ERR_NO_C
      integer*4  ERR_TOO_MANY
      integer*4  ERR_UNKN_KEY
      integer*4  ERR_DATA_ILL
      integer*4  ERR_READ_MT

      parameter  (READ_EOF = -2)   ! end_of_file
      parameter  (READ_MT = -1)    ! module type specification
      parameter  (ERR_NUM = 1)
      parameter  (ERR_C_VAL = 2)   ! invalid CAMAC crate number
      parameter  (ERR_N_VAL = 3)   ! invalid CAMAC slot number
      parameter  (ERR_2N = 4)      ! second CAMAC slot less than first
      parameter  (ERR_CONV = 5)    ! number conversion error
      parameter  (ERR_MOD_UNKN = 6)  ! unknown module type
      parameter  (ERR_MOD_AMBIG = 7) ! ambiguous module specification
      parameter  (ERR_DAT_UNKN = 8)  ! unknown data type
      parameter  (ERR_DAT_AMBIG = 9) ! ambiguous data type specification
      parameter  (ERR_NAN = 10)      ! not a numeric field
      parameter  (ERR_DATA_TYPE = 11)! invalid data type for this module
      parameter  (ERR_DATA_VAL = 12) ! data value out of range
      parameter  (ERR_DUP_DATA = 13) ! duplicate data for this parameter
      parameter  (ERR_TOO_FEW = 14)  ! too few data
      parameter  (ERR_PRIOR_MOD_TYPE = 15) ! prior module assign to c and n
      parameter  (ERR_PRIOR_LOW = 16) ! prior def of lower threshold
      parameter  (ERR_PRIOR_UP = 17)  ! prior def of upper threshold
      parameter  (ERR_PRIOR_OFF = 18) ! prior def of offset memory
      parameter  (ERR_PRIOR_PED = 19) ! prior def of pedestals
      parameter  (ERR_PRIOR_COM = 20) ! prior def of common threshold
      parameter  (ERR_PRIOR_ENABLE = 21) ! prior def of enables
      parameter  (ERR_PRIOR_GEN = 22) ! prior def of generate function
      parameter  (ERR_PRIOR_DELAY = 23)  ! prior def of delays
      parameter  (ERR_MULTI_C = 24)   ! Multiple Crate specs
      parameter  (ERR_MULTI_N = 25)   ! Multiple Slot specs
      parameter  (ERR_NO_N = 26)      ! No Crate spec
      parameter  (ERR_NO_C = 27)      ! No Slot spec
      parameter  (ERR_TOO_MANY = 28)  ! Too many data
      parameter  (ERR_UNKN_KEY = 29)  ! Unknown key word
      parameter  (ERR_DATA_ILL = 30)  ! Illegal value
      parameter  (ERR_READ_MT = 31)   ! Error searching for mt=
