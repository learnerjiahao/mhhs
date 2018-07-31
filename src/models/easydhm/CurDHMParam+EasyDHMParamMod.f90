module CurDHMParam
        integer sol_nly      !|none          |number of layers in soil profile
		! 当前计算单元参数
		! WetSpa模型 - 产流参数
		real :: p_k_run								    !rainfall intensity adjusting coefficient for runoff coefficient	 
		real :: cn2(6)							            !rainfall intensity adjusting coefficient for runoff coefficient	 
		real :: p_p_max									!maximum rainfall intensity used for calculating runoff coefficient
		real :: p_ovrlndRat								!
		real :: P_PETM
		real :: p_sno50cov
		real :: p_sftmp
		
		real  Csolf,Csolfm     !土壤冻结系数，冻融系数
		real tdrain
		real revapmn				        !threshold depth of water in shallow aquifer required to allow revap to occur
		real rchrg_dp	    
		real gw_delay	
		real alpha_bf
        real::p_SmTmp
        real::p_SmFmx
        real::p_SmFmn
        real::p_Timp
        real::p_Snocovmx
        real gwqmn
        real gw_spyld
        real ch_revap
        real watercofm
        real ModFlood
        real ModSoil
    end module 
  
    ! EasyDHM模型参数
    module EasyDHMParamMod
        use solutionMod
        use CurDHMParam
        use WaterShedMod
!        use DBSQLLinkMod
        !use StringFiltrMod
        implicit none

        ! EasyDHM模型参数
        type EasyDHMParamClass
            integer::Pid
            real::G0
            real::Gwht0
            real::Ch_revap
            real::Agw_delay
            real::Alpha_bf
            real::Agwqmn
            real::Archrg_dp
            real::Arevapmn
            real::Gw_spyld
            real::Petm
            real::Cn2(6)
            real::P_max
            real::ImpM
            real::UnitSlopeM
            real::RunoffCoeM
            real::PorosityM
            real::FieldCapM
            real::LaiMaxM(6)
            real::RtDpthM(6)
            real::DepressM(6)
            real::ItcMaxM(6)
            real::SFtmp
            real::SmTmp
            real::SmFmx
            real::SmFmn
            real::Timp
            real::Snocovmx
            real::Sno50cov
            real::Dep_impM
            real::DdrainM
            real::Atdrain
            real::Sol_crkM
            real::Solf
            real::Solfm
            real,dimension(:),allocatable::ConductM
            real,dimension(:),allocatable::Solzcoe
!            real::watercof
       end type EasyDHMParamClass

        type(EasyDHMParamClass),Dimension(:),allocatable ::EasyDHMParams
        type(EasyDHMParamClass)::m_EasyDHMParam
        character(100)::SolzcoeStr,ConducMStr
        integer::LenStr=100

        contains

            ! 读取数据库EasyDHM模型参数
!            subroutine ReadEasyDHMParam()
!                integer::i,j,length1,length2

                !INTEGER (SQLRETURN) iRet
                !INTEGER (SQLHANDLE) :: hSQLStmt
                !用WaterShed中的参数分区个数分配EasyDHMParams的大小
!                if(allocated(EasyDHMParams))deallocate(EasyDHMParams)
!                allocate(EasyDHMParams(WaterShed%NParamRange))
!                do i=1,WaterShed%NParamRange
!                    if(allocated(EasyDHMParams(i)%SolzCoe))deallocate(EasyDHMParams(i)%SolzCoe)
!                    allocate(EasyDHMParams(i)%SolzCoe(WaterShed%NLayer-1))
!                    if(allocated(EasyDHMParams(i)%ConductM))deallocate(EasyDHMParams(i)%ConductM)
!                    allocate(EasyDHMParams(i)%ConductM(WaterShed%NLayer))
!                end do
!                if(allocated(m_EasyDHMParam%SolzCoe))deallocate(m_EasyDHMParam%SolzCoe)
!                allocate(m_EasyDHMParam%SolzCoe(WaterShed%NLayer-1))
!                if(allocated(m_EasyDHMParam%ConductM))deallocate(m_EasyDHMParam%ConductM)
!                allocate(m_EasyDHMParam%ConductM(WaterShed%NLayer))

                !dcb ReadEasyDHMParam
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"SELECT * FROM ST_ParaDefEasyDHM"//Char(0),SQL_NTSL) 
!
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,m_EasyDHMParam%pid,0,cbRet)
!                iRet=SQLBindColR4(hSQLStmt,2,SQL_C_FLOAT,m_EasyDHMParam%G0,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,3,SQL_C_FLOAT,m_EasyDHMParam%Gwht0,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,4,SQL_C_FLOAT,m_EasyDHMParam%Ch_revap,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,5,SQL_C_FLOAT,m_EasyDHMParam%Agw_delay,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,6,SQL_C_FLOAT,m_EasyDHMParam%Alpha_bf,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,7,SQL_C_FLOAT,m_EasyDHMParam%Agwqmn,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,8,SQL_C_FLOAT,m_EasyDHMParam%Archrg_dp,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,9,SQL_C_FLOAT,m_EasyDHMParam%Arevapmn,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,10,SQL_C_FLOAT,m_EasyDHMParam%Gw_spyld,0,cbR4)
!                iRet=SQLBindColChar(hSQLStmt,11,SQL_C_CHAR,SolzcoeStr,LenStr,cbStr)                
!                iRet=SQLBindColR4(hSQLStmt,12,SQL_C_FLOAT,m_EasyDHMParam%Cn2,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,13,SQL_C_FLOAT,m_EasyDHMParam%P_max,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,14,SQL_C_FLOAT,m_EasyDHMParam%ImpM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,15,SQL_C_FLOAT,m_EasyDHMParam%UnitSlopeM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,16,SQL_C_FLOAT,m_EasyDHMParam%RunoffCoeM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,17,SQL_C_FLOAT,m_EasyDHMParam%PorosityM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,18,SQL_C_FLOAT,m_EasyDHMParam%FieldCapM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,19,SQL_C_FLOAT,m_EasyDHMParam%LaiMaxM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,20,SQL_C_FLOAT,m_EasyDHMParam%RtDpthM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,21,SQL_C_FLOAT,m_EasyDHMParam%DepressM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,22,SQL_C_FLOAT,m_EasyDHMParam%ItcMaxM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,27,SQL_C_FLOAT,m_EasyDHMParam%SFtmp,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,28,SQL_C_FLOAT,m_EasyDHMParam%SmTmp,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,29,SQL_C_FLOAT,m_EasyDHMParam%SmFmx,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,30,SQL_C_FLOAT,m_EasyDHMParam%SmFmn,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,31,SQL_C_FLOAT,m_EasyDHMParam%Timp,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,32,SQL_C_FLOAT,m_EasyDHMParam%Snocovmx,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,33,SQL_C_FLOAT,m_EasyDHMParam%Sno50cov,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,34,SQL_C_FLOAT,m_EasyDHMParam%Dep_impM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,35,SQL_C_FLOAT,m_EasyDHMParam%DdrainM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,36,SQL_C_FLOAT,m_EasyDHMParam%Atdrain,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,37,SQL_C_FLOAT,m_EasyDHMParam%Sol_crkM,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,38,SQL_C_FLOAT,m_EasyDHMParam%Solf,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,39,SQL_C_FLOAT,m_EasyDHMParam%Solfm,0,cbR4)
!                iRet=SQLBindColChar(hSQLStmt,40,SQL_C_CHAR,ConducMStr,LenStr,cbStr)
!                iRet=SQLBindColR4(hSQLStmt,41,SQL_C_FLOAT,m_EasyDHMParam%Petm,0,cbR4)
!                i=0
!                Do While(.True.)
!                    iRet=SQLFetch(hSQLStmt)
!                    if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                    !处理读入字符串
!                    call GetStrSplitterCount(SolzcoeStr,100,length1,',')
!                    call GetStrSplittercount(ConducMStr,100,length2,',')
!                    !将字符串中的值传给相应数组
!
!                    read(SolzcoeStr,*) (m_EasyDHMParam%Solzcoe(j),j=1,length1,1)
!                    read(ConducMStr,*) (m_EasyDHMParam%ConductM(j),j=1,length2,1)
!
!                    i=i+1
!                    EasyDHMParams(i)=m_EasyDHMParam
!                End Do
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine

            ! 获得当前参数分区EasyDHM模型参数
            subroutine GetEasyDHMParam
            
                integer :: v_PRid
                v_PRid =CurSolution%IParamRange
		        sol_nly = WaterShed%Nlayer
		        cn2(:)  = EasyDHMParams(v_PRid)%cn2(:)
		        p_p_max  = EasyDHMParams(v_PRid)%p_max
		        P_PETM = EasyDHMParams(v_PRid)%PETM
        		
		        p_ovrlndRat = 0.02
        		
		        p_sno50cov = EasyDHMParams(v_PRid)%sno50cov
		        p_sftmp = EasyDHMParams(v_PRid)%sftmp
        		p_smtmp = EasyDHMParams(v_PRid)%smtmp
        		
                tdrain  = EasyDHMParams(v_PRid)%atdrain
               
                Csolfm = EasyDHMParams(v_PRid)%solfm
!                Csolf = EasyDHMParams(v_PRid)%solf
!                ModFlood = EasyDHMParams(v_PRid)%solf
                ModSoil = EasyDHMParams(v_PRid)%solfm

		        gw_spyld = EasyDHMParams(v_PRid)%gw_spyld
		        
		        alpha_bf = EasyDHMParams(v_PRid)%alpha_bf
                gw_delay = EasyDHMParams(v_PRid)%agw_delay

	            ! alpha factor for groundwater recession curve
                gwqmn = EasyDHMParams(v_PRid)%agwqmn		! threshold depth of water in shallow aquifer required before groundwater flow will occur
                revapmn = EasyDHMParams(v_PRid)%arevapmn		!threshold depth of water in shallow aquifer required to allow revap to occur
               
                rchrg_dp = EasyDHMParams(v_PRid)%archrg_dp
                
                p_SmTmp = EasyDHMParams(v_PRid)%SmTmp
                p_SmFmx = EasyDHMParams(v_PRid)%SmFmx
                p_SmFmn = EasyDHMParams(v_PRid)%SmFmn
                p_Timp  = EasyDHMParams(v_PRid)%Timp   !滞后系数
                p_Snocovmx = EasyDHMParams(v_PRid)%Snocovmx
                ch_revap = EasyDHMParams(v_PRid)%ch_revap
!                watercofm=EasyDHMParams(v_PRid).watercof
                
            end subroutine

    end module