! 当前计算单元的变量，独立于所有产流模型
	Module CurrentUnitVars
	    implicit none
		! 当前计算单元
		real v_p				!降雨量mm
		real v_pet				!潜在蒸发量mm
		real v_ep_max			!maximum plant ET
		real v_t				!平均温度（度）
		real v_ssh				!日照小时数（h）
		real v_hmd				!相对湿度（-）
		real v_wnd				!风速（m/s）
		real v_ra				!辐射（MJ/m2/day）
		
		real v_DLAT				!形心纬度（度）
		
		real v_ramax			!计算最大日辐射
		
		real v_sol_cov			!residue on soil surface for current day
		real v_albday			!albedo of the day
        
        real lastflow  !退水模型上一时段的径流量
	end module
	
	module CurrentUnitWSPDHMVars
	    implicit none
		!过程变量
		real v_sm				!融雪量mm

		real v_ei				!冠层截留蒸发量mm
		real v_interc			!冠层截留量mm

		real v_netp				!净雨量mm
		real v_run				!真实径流系数
		real v_rs				!地表径流mm
		real v_infil			!地表入渗量mm
		
		real v_ed				!地表填洼蒸发量mm
		real v_depre			!地表填洼量mm
		real v_sdep1			!地表填洼储量mm(时段初)
		
		real v_crain			!累积地表径流mm		
		
		real v_rtdpth           !当前根深
		
		real v_ep              !植物蒸发mm
		real v_es				!地表蒸发mm
		real v_ri				!壤中流量mm
		real v_perco			!补给地下水量mm	
		
		real v_eg				!地下蒸发量mm
		real v_rg				!地下径流量mm
		real v_qg				!地下径流量m3/s
		
		real v_r				!总径流量mm
		real v_q				!总径流量m3/s
		
		!状态变量
		real v_snow				!积雪量mm
		real v_sint				!冠层截留储量mm
		real v_sdep				!地表填洼储量mm
		real v_ss				!土壤含水量mm
		real v_gt				!地下水储量mm
		real gwht
		real snowtmp
		real tmp_srf
		
		real :: p_intercap								!截留能力
		real :: p_lai	      	!叶面积指数
		
		real v_ovrlnd			!坡面汇流
		real snocov1,snocov2,snocov

		!================================================================================
		! 土壤模块计算新增变量
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
	    real v_rchrg 	                      !地下水补给
	    
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
	    REAL v_WU						! 上层张力水蓄水量
		REAL v_WL						! 下层张力水蓄水量
		REAL v_WD						! 深层张力水蓄水量
		REAL v_W						! 总张力水蓄水量
		REAL v_WWMM						! 流域最大张力水蓄水容量
		REAL v_EM						! 最大蒸发能力
		REAL v_EU						! 上层土壤蒸发能力
		REAL v_EL						! 下层土壤蒸发能力
		REAL v_ED						! 深层土壤蒸发能力
		REAL v_SSM						! 流域上自由水蓄水量最大的某点的蓄量值
		REAL v_PE						! 净雨
		REAL v_A						! 前期雨量
		REAL v_AU						! 与自由水蓄水量S对应的蓄水容量曲线的纵标值
		REAL v_KSSD						! 
		REAL v_KGD						! 
		
		real v_rss				!地表径流mm
		real v_e				!地表蒸发mm
		real v_rii				!壤中流量mm
		real v_qi			!补给地下水量mm	
		real v_rgg				!地下径流量mm
		real v_qg				!地下径流量m3/s
		real v_rr				!总径流量mm
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
	
