module SimpleVarMod
        use SolutionMod
        use UnitInfoMod
        use SubbasinMod
        use ParamRangeMod
        
        implicit none
        
        ! 计算的水文要素
        type u_SimpleClass
        
            ! 状态信息
		    real :: WaterStorage
            real :: slow
            real :: quick1
            real :: quick2
            real :: quick3
            real :: x_loss
            
            
            ! 过程信息
            real :: PET
            real :: rs
            real :: rg            
            real :: ri
            real :: r
            
            real :: netp
            real :: e

            real :: qi
            real :: qg
            
            real :: WU
            real :: WL
            real :: WD
            real :: s
            real :: FR
                        
        end type u_SimpleClass

        ! 存储每个子流域、计算单元的水循环要素
        type(u_SimpleClass), dimension(:,:), allocatable :: u_SimpleVars
        type(u_SimpleClass), dimension(:),   allocatable :: s_SimpleVars
        
        contains 
!            subroutine InitSimpleVars
!                use InitXAJStateMod
!                use InitHymodStateMod
!                use XAJParamMod
!                integer i,j,k,IParamRange,Nlayer,ix
!                
!                Nlayer = WaterShed.Nlayer
!                IParamRange = Cursolution.IParamRange
!                
!			    do ix = 1,ParamRanges(IParamRange).NPartSubbasin
!			        i = ParamRanges(IParamRange).PartSubbasins(ix)
!
!			        do j = 1, Subbasins(i).NUnits
!		                u_SimpleVars(i,j).WaterStorage = InitHymodStates(i,j).init_WaterStorage
!                        u_SimpleVars(i,j).slow         = InitHymodStates(i,j).init_slow
!                        u_SimpleVars(i,j).quick1       = InitHymodStates(i,j).init_quick1
!                        u_SimpleVars(i,j).quick2       = InitHymodStates(i,j).init_quick2
!                        u_SimpleVars(i,j).quick3       = InitHymodStates(i,j).init_quick3
!                        u_SimpleVars(i,j).x_loss       = 0.
!                        
!                        ! 过程信息
!                        u_SimpleVars(i,j).PET          = 0.
!                        u_SimpleVars(i,j).rs           = 0.
!                        u_SimpleVars(i,j).rg           = 0. 
!                        u_SimpleVars(i,j).ri           = 0.
!                        u_SimpleVars(i,j).r            = 0.
!                        
!                        u_SimpleVars(i,j).netp         = 0.
!                        u_SimpleVars(i,j).e            = 0.
!
!                        u_SimpleVars(i,j).qi           = InitXAJStates(i,j).init_Qi
!                        u_SimpleVars(i,j).qg           = InitXAJStates(i,j).init_Qg
!                        
!                        if (InitXAJStates(i,j).init_WU > XAJParams(IParamRange).WM1) InitXAJStates(i,j).init_WU = XAJParams(IParamRange).WM1
!                        if (InitXAJStates(i,j).init_WL > XAJParams(IParamRange).WM2) InitXAJStates(i,j).init_WL = XAJParams(IParamRange).WM2
!                        if (InitXAJStates(i,j).init_WD > XAJParams(IParamRange).WM3) InitXAJStates(i,j).init_WD = XAJParams(IParamRange).WM3
!                        u_SimpleVars(i,j).WU           = InitXAJStates(i,j).init_WU
!                        u_SimpleVars(i,j).WL           = InitXAJStates(i,j).init_WL
!                        u_SimpleVars(i,j).WD           = InitXAJStates(i,j).init_WD
!                        u_SimpleVars(i,j).s            = InitXAJStates(i,j).init_S
!                    enddo
!
!                enddo
!            end subroutine
            
            subroutine InitStatSimpleVars
                integer ii,IParamRange,ix
                
                IParamRange = Cursolution.IParamRange
                
			    do ix = 1,ParamRanges(IParamRange).NPartSubbasin
			        ii = ParamRanges(IParamRange).PartSubbasins(ix)
			        s_SimpleVars(ii).WaterStorage = 0.
                    s_SimpleVars(ii).slow         = 0.
                    s_SimpleVars(ii).quick1       = 0.
                    s_SimpleVars(ii).quick2       = 0.
                    s_SimpleVars(ii).quick3       = 0.
                    s_SimpleVars(ii).x_loss       = 0.
                        
                        ! 过程信息
                    s_SimpleVars(ii).PET          = 0.
                    s_SimpleVars(ii).rs           = 0.
                    s_SimpleVars(ii).rg           = 0. 
                    s_SimpleVars(ii).ri           = 0.
                    s_SimpleVars(ii).r            = 0.
                        
                    s_SimpleVars(ii).netp         = 0.
                    s_SimpleVars(ii).e            = 0.

                    s_SimpleVars(ii).qi           = 0.
                    s_SimpleVars(ii).qg           = 0.
                        
                    s_SimpleVars(ii).WU           = 0.
                    s_SimpleVars(ii).WL           = 0.
                    s_SimpleVars(ii).WD           = 0.
                    s_SimpleVars(ii).s            = 0.
                   
                enddo

            end subroutine
            
    end module