! 方案信息
module SolutionMod
    use WatershedMod
    use TimeInfoMod

    implicit none

    real :: global_msk_K    !用于保存马斯京根汇流的K值，从RtMusk.f90中的xkm变量中取值，在Table_SingleChangePara.f90的GegPrDataSinChangePara函数中将该值输出在文件“msk_K.txt”中

    ! 长系列方案也作为一个方案来保存
    type Flood_Charv
        integer Nlines
        integer Npoints
        integer IForcast
        real, dimension(:, :, :), allocatable :: yBound
        real, dimension(:, :, :), allocatable :: xBound
        real, dimension(:), allocatable :: Qmax
        real Qlimit, QLow, QHigh, Stbu, Stbl, Locbu, Locbl
    endtype

    type floodcharpointclass
        real, dimension(:, :), allocatable :: resx1, resy1, resx2, resy2, resx3, resy3
    end type

    type restypeclass
        integer Runtype, OptCharID, OptFloodID
        integer ResDHMID, ResID, StoreType, BasedCharID, IParamRange
        real Qout, Qlimit
        character(len = 20) :: Resname
        type(Flood_Charv) :: floodchar
        real, dimension(:), allocatable :: ResIn1, ResIn2, ResIn3
        real NormStorage, Storage, DeadStorage, Area, NormSt, St, DeadSt
        real resQin, LocMod, StoreMod
        integer Isign
    end type

    type SolutionClass
        integer :: SolutionId
        integer :: pid
        character(30) :: SolutionName

        integer :: RunType         ! 模型计算方式：1=普通（参数分区范围）；2=敏感性分析；3=参数优化；4=不确定性分析；5=运行最优参数；6=普通（参数分区以上所有范围）；7=RDS提取，以便加快参数敏感性分析、自动率定等计算的速度
        integer :: Rtmdy           ! 实时修正方法：0=不校正；1=计算校正系数；2=实时校正
        integer :: FloodError      ! 是否从方案目录读取水文气象数据
        integer :: TimeStepOpt     ! 敏感性/参数优化/不确定性分析时段：1=日；2=月

        integer :: IParamYear      ! 敏感性/参数优化/不确定性分析年份
        integer :: ithpid = 1

        integer :: NTimeStepInit   ! 初始化时段个数
        integer :: ITimeStepMeas   ! 实测径流数据的时段：1=日；2=月
        integer :: IParamRange     ! 当前计算参数分区
        integer :: IWeather        ! 是否从方案目录读取水文气象数据
        integer :: dtold(3)           ! 模拟时段
        integer :: dt
        integer :: TIMESALL(3)     ! 各个时期时段数

        integer :: CalYears(2)
        integer :: CalMonths(2)
        integer :: CalDays(2)
        integer :: CalHours(2)

        integer :: IRouteType      ! 河道演进算法，1=变存储法；2=马斯京干法；3=曼宁公式
        integer :: GWSimType       ! WetSpa模型地下水模拟方法：1=线性水库；2=非线性水库;3=EasyDHM地下水模型
        integer :: iPet            ! 蒸发计算方法：1=PRIESTLEY-TAYLOR；2=PENMAN-MONTEITH；3=HARGREAVES；4=Read in
        integer :: RtmdyType       ! 实时校正方法：1=线性相关法；2=误差自回归法
        integer :: AR_num          ! 误差自回归阶数
        integer :: IResetIni       ! 是否重置每年年初产流状态变量，1=重置；0=不重置
        integer :: NRunoffGenType  ! 模型数量
        integer, dimension(:), allocatable :: IRunoffGenType
        integer :: RouteDT
        integer :: obsorsim
        character(30), dimension(:), allocatable :: StartDateTime
        character(30), dimension(:), allocatable :: EndDateTime
        integer :: nflood = 0
        integer, dimension(:), allocatable :: NFloodSeries, floodloc, peakloc
        integer, dimension(:), allocatable :: rank
        real, dimension(:, :), allocatable :: flooderr
        real, dimension(:, :), allocatable :: flooderrRes
    end type SolutionClass

    type(SolutionClass), dimension(:), allocatable :: TotalSolution
    type(SolutionClass) :: CurSolution
    type(Flood_Charv) :: floodchartemp
    type(restypeclass), dimension(:), allocatable :: restype
    type(restypeclass) :: restypetemp
    type(floodcharpointclass), dimension(:), allocatable :: floodcharpoint
    integer :: GetSolutionSuc
    integer :: outsinIdx = 1
    integer :: NResOptSolution

contains

    integer function CalMD(Iyear, IM)
        integer Iyear, IM
        select case (IM)
        case(1)
            CalMD = 31
        case(2)
            !四年一闰，百年不闰，四百年再闰
            if(IYear.ne.IYear / 4 * 4) then !2月的天数
                CalMD = 28
            elseif(IYear.eq.IYear / 400 * 400) then
                CalMD = 29
            elseif(IYear.eq.IYear / 100 * 100) then
                CalMD = 28
            elseif(IYear.eq.IYear / 4 * 4) then
                CalMD = 29
            endif                       !2月的天数
        case(3)
            CalMD = 31
        case(4)
            CalMD = 30
        case(5)
            CalMD = 31
        case(6)
            CalMD = 30
        case(7)
            CalMD = 31
        case(8)
            CalMD = 31
        case(9)
            CalMD = 30
        case(10)
            CalMD = 31
        case(11)
            CalMD = 30
        case(12)
            CalMD = 31
        end select
    end function
    !
    subroutine deleteCurSolution
        if(GetSolutionSuc .eq. 1)then
            GetSolutionSuc = 0
            if(allocated(CurSolution%IRunoffGenType))deallocate(CurSolution%IRunoffGenType)
        endif
    endsubroutine
end module