/*
  Shugong Wang shw36@pitt.edu
  University of Pittsburgh
*/
class XinanjiangModel
{
private:
    // FORCING
    double *P;   // 降水数据
    double *EI;  // 水面蒸发数据
    //
    long m_nSteps;  // 模型要运行的步长
    // OUTPUT
    double *R;   // 流域内每一步长的产流量(径流深度)
    double *RG;  // 每一步长的地表径流深(毫米)
    double *RS;  // 每一步长的基流径流深(毫米)
    double *E;   // 每一步长的蒸发(毫米)
    double *QRS; // 流域出口地表径流量
    double *QRG; // 流域出口地下径流量
    double *Q;   // 流域出口的总流量
    //
    double m_U;     // for 24h. U=A(km^2)/3.6/delta_t
    // SOIL
    double *W;     // 流域内土壤湿度
    double *WU;	  // 流域内上层土壤湿度
    double *WL;	  // 流域内下层土壤适度
    double *WD;    // 流域内深层土壤湿度


    double WUM;	// 流域内上层土壤蓄水容量，植被良好的流域，约为20mm,差的流域,2~10mm
    double WLM;   // 流域内下层土壤蓄水容量，可取60~90mm
    double WDM;   // 流域内深层土壤蓄水容量，WDM=WM-WUM-WLM

    // EVAPORATION
    double *EU;  // 上层土壤蒸发量（毫米）
    double *EL;  // 下层土壤蒸发量（毫米）
    double *ED;  // 深层土壤蒸发量（毫米）

    // PARAMETER
    double K;     // 流域蒸散发能力与实测蒸散发值的比
    double IMP;   // 不透水面积占全流域面积之比
    double B;     // 蓄水容量曲线的方次，小流域（几平方公里）B为0.1左右，
    // 中等面积（300平方公里以内）0.2~0.3，较大面积0.3~0.4
    double WM;    // 流域平均蓄水容量（毫米）(WM=WUM+WLM+WDM)

    double C;     // 流域内深层土壤蒸发系数，江南湿润地区：0.15-0.2，华北半湿润地区：0.09-0.12
    double FC;    // 稳定入渗率，毫米/小时
    double KKG;   // 地下径流消退系数
    double KSTOR; // 脉冲汇流计算的参数,Liang
    double WMM;   // 流域内最大蓄水容量
    double m_Area;  // 流域面积
    int m_DeltaT;   // 每一步长的小时数

public:
    XinanjiangModel(void);
    ~XinanjiangModel(void);
    // 通过文件初始化模型设置，包括步长，步数，驱动数据
    XinanjiangModel(char * FileName);
    // 初始化模型
    void InitModel(long nSteps, double Area,int DeltaT,char *ForcingFile);
    // 设置模型参数
    void SetParameters(double * Params);
    // 运行新安江模型
    void RunModel(void);
    // 保存模拟结果到文件
    void SaveResults(char * FileName);
    void Runoff(double *runoff);
private:
    // 进行汇流计算，将径流深度转换为流域出口的流量
    void Routing(void);
};