 ! ��������
    module WaterShedMod
        
        implicit none
        
        ! ����
        type WaterShedClass
            integer :: NUnit           ! ���㵥Ԫ����
            integer :: NReach          ! �ӵ�����
            integer :: NSubbasin       ! ���������
            integer :: NParamRange     ! ��������������ˮ��վ���� + ���۲��ˮ�����
            integer :: NHydroStation   ! ˮ��վ����
            integer :: Nlayer          ! ��������
            
            integer :: NResRange       ! ���۲��ˮ�����
            integer :: NResIn          ! ���۲��ˮ�����
            integer :: NResOut         ! ����۲��ˮ�����
            
        end type WaterShedClass
        
        ! ����
        type(WaterShedClass) :: WaterShed

    end module
    
