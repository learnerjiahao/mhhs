! ��ǰ���㵥Ԫ�ı��������������в���ģ��
	Module CurrentUnitVars
	    implicit none
		! ��ǰ���㵥Ԫ
		real v_p				!������mm
		real v_pet				!Ǳ��������mm
		real v_ep_max			!maximum plant ET
		real v_t				!ƽ���¶ȣ��ȣ�
		real v_ssh				!����Сʱ����h��
		real v_hmd				!���ʪ�ȣ�-��
		real v_wnd				!���٣�m/s��
		real v_ra				!���䣨MJ/m2/day��
		
		real v_DLAT				!����γ�ȣ��ȣ�
		
		real v_ramax			!��������շ���
		
		real v_sol_cov			!residue on soil surface for current day
		real v_albday			!albedo of the day
        
        real lastflow  !��ˮģ����һʱ�εľ�����
	end module
	
	module CurrentUnitWSPDHMVars
	    implicit none
		!���̱���
		real v_sm				!��ѩ��mm

		real v_ei				!�ڲ����������mm
		real v_interc			!�ڲ������mm

		real v_netp				!������mm
		real v_run				!��ʵ����ϵ��
		real v_rs				!�ر���mm
		real v_infil			!�ر�������mm
		
		real v_ed				!�ر�����������mm
		real v_depre			!�ر�������mm
		real v_sdep1			!�ر����ݴ���mm(ʱ�γ�)
		
		real v_crain			!�ۻ��ر���mm		
		
		real v_rtdpth           !��ǰ����
		
		real v_ep              !ֲ������mm
		real v_es				!�ر�����mm
		real v_ri				!��������mm
		real v_perco			!��������ˮ��mm	
		
		real v_eg				!����������mm
		real v_rg				!���¾�����mm
		real v_qg				!���¾�����m3/s
		
		real v_r				!�ܾ�����mm
		real v_q				!�ܾ�����m3/s
		
		!״̬����
		real v_snow				!��ѩ��mm
		real v_sint				!�ڲ��������mm
		real v_sdep				!�ر����ݴ���mm
		real v_ss				!������ˮ��mm
		real v_gt				!����ˮ����mm
		real gwht
		real snowtmp
		real tmp_srf
		
		real :: p_intercap								!��������
		real :: p_lai	      	!Ҷ���ָ��
		
		real v_ovrlnd			!�������
		real snocov1,snocov2,snocov

		!================================================================================
		! ����ģ�������������
		!================================================================================
		real,dimension(:), allocatable ::  sol_fc
		real,dimension(:), allocatable ::  sol_ul
		real,dimension(:), allocatable ::  sol_hk
		real,dimension(:), allocatable ::  sol_k
		real,dimension(:), allocatable ::  crdep
		real,dimension(:), allocatable ::  volcr
		real ::  sol_avbd
		real ::  sol_sumul	
		real ::  sol_sumfc
		
        real,dimension(:), allocatable ::  sol_st       !|mm H2O        |amount of water stored in the soil layer on
        real,dimension(:), allocatable ::  flat         !|mm H2O        |lateral flow storage array
        real,dimension(:), allocatable ::  sol_prk      !|mm H2O        |percolation storage array
		real,dimension(:), allocatable ::  sol_tmp      !|deg C         |daily average temperature of soil layer
        real,dimension(:), allocatable ::  sol_z        !|mm            |depth to bottom of soil layer 
        real,dimension(:), allocatable ::  sol_fro
        real,dimension(:), allocatable ::  sol_fm
	    real,dimension(:), allocatable ::  sol_es
	    real,dimension(:), allocatable ::  sol_h
		
        real icrk            !|none          |crack flow code
        real voltot          !|mm            |total volume of cracks expressed as depth per unit area
        real latlyr          !|mm H2O        |lateral flow in soil layer for the day
        real latq            !|mm H2O        |total lateral flow in soil profile for the day in HRU
        real lyrtile         !|mm H2O        |drainage tile flow in soil layer for day
        real qtile           !|mm H2O        |drainage tile flow in soil profile for the day
        real sepday          !|mm H2O        |micropore percolation from soil layer
        real sepbtm          !|mm H2O        |percolation from bottom of soil profile for the day in HRU
        real sw_excess       !|mm H2O        |amount of water in excess of field capacity
        real volcrmin        !|mm            |minimum soil volume in profile
        real dep_imp,ddrain,sol_crk,sol_cov
	    real :: SnowTemp,SMTemp
	    real v_rchrg 	                      !����ˮ����
	    
		real UnitSlope 
		real runoff_co
		real porosity
		real fieldcap 
		real poreindex
		real lai_max
		real depression
		real rootdepth 
		real itc_max	
		real imp_w
		real wilting 
		real residual
		real Conductivity
		real UnitSlopeTan
		real sol_avpor
	    
	end module
	
	module CurrentUnitXAJVars
	    implicit none
	    REAL v_WU						! �ϲ�����ˮ��ˮ��
		REAL v_WL						! �²�����ˮ��ˮ��
		REAL v_WD						! �������ˮ��ˮ��
		REAL v_W						! ������ˮ��ˮ��
		REAL v_WWMM						! �����������ˮ��ˮ����
		REAL v_EM						! �����������
		REAL v_EU						! �ϲ�������������
		REAL v_EL						! �²�������������
		REAL v_ED						! ���������������
		REAL v_SSM						! ����������ˮ��ˮ������ĳ�������ֵ
		REAL v_PE						! ����
		REAL v_A						! ǰ������
		REAL v_AU						! ������ˮ��ˮ��S��Ӧ����ˮ�������ߵ��ݱ�ֵ
		REAL v_KSSD						! 
		REAL v_KGD						! 
		
		real v_rss				!�ر���mm
		real v_e				!�ر�����mm
		real v_rii				!��������mm
		real v_qi			!��������ˮ��mm	
		real v_rgg				!���¾�����mm
		real v_qg				!���¾�����m3/s
		real v_rr				!�ܾ�����mm
		real v_FR
		real v_s
		
	end module
	
	module CurrentUnitHymodVars
	    implicit none
        real QuickTankState1
        real QuickTankState2
        real QuickTankState3
        real outflow
        real conv
        real Precip
        real PETval
        real PrecipStorage
        real,public :: StorageCapacity
        real ExcessPrecip1
        real dummy
        real WaterStorageNew
        real ExcessPrecip2
        real evap
        
        real SlowTankInflow
        real SlowTankStateNew
        real SlowTankOutflow
        
        real QuickTankInflow(3)
        real QuickTankOutflow(3)
        real QuickTankState(3)
        real QuickTankStateNew(3)       
        
        real WaterStorage 
        real SlowTankState
        real v_loss
	
	end module
	
