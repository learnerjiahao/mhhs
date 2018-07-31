subroutine CalModelPara(CurRunoffGenType1)

    use EasyDHMParamMod
    use ReachParamMod
    use EasyDHMVarMod
    use ReachVarMod
    use ResVarMod

    integer CurRunoffGenType1

    call GetEasyDHMParam
    call InitDHMVars
    call InitReachVars(CurRunoffGenType1)
    call InitResVars

end
   
