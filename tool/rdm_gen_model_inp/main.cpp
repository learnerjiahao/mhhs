//
// Created by wujiahao on 18-6-28.
//

#include <iostream>
#include <fstream>
#include <string>

void writePara3(int i);
void writeInit3(int i);

void writePara2(int i);
void writeInit2(int i);

int main(int argc, char *argv[]) {
    srand((unsigned)time(NULL));
    for (int i = 1; i < 131; ++i) {
        // 1.create model parameters to *.para
        writePara2(i);
        // 2. create model parameters to *.ini
        writeInit2(i);
    }
}

void writeInit2(int i) {

    char pFileName[100];
    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".ini\0");
    std::ofstream of(pFileName, std::ios::trunc);

    char strBuff[1024];
    std::sprintf(strBuff, "%-20s %-20s\n\0", "MODEL", "xinanjiang2");
    of << strBuff;

    double WU = rand() / double(RAND_MAX) * 30;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WU", WU);
    of << strBuff;

    double WL = rand() / double(RAND_MAX) * 80;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WL", WL);
    of << strBuff;

    double WD = rand() / double(RAND_MAX) * 130;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WD", WD);
    of << strBuff;

    double QRG0 = rand() / double(RAND_MAX) * 200;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "QRG0", QRG0);
    of << strBuff;

    of.close();
}

void writePara2(int i) {

    char pFileName[100];
    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".sub\0");
    std::ifstream fin(pFileName);
    std::string lineSub;
    std::string headName;
    double headValue;
    while(fin >> headName >> headValue)
    {
        if (headName == "BAREA") {
            break;
        }
    }
    fin.close();

    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".para\0");
    std::ofstream of(pFileName, std::ios::trunc);

    char strBuff[1024];
    std::sprintf(strBuff, "%-20s %-20s\n\0", "MODEL", "xinanjiang2");
    of << strBuff;

    std::sprintf(strBuff, "%-20s %-20f\n\0", "F", headValue);
    of << strBuff;

    double WUM = 30;//rand() / double(RAND_MAX) * 30;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WUM", WUM);
    of << strBuff;

    double WLM = 80; //rand() / double(RAND_MAX) * 150;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WLM", WLM);
    of << strBuff;

    double WDM = 130; //rand() / double(RAND_MAX) * 200;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WDM", WDM);
    of << strBuff;

    double K = rand() / double(RAND_MAX) * 1.0;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "K", K);
    of << strBuff;

    double C = rand() / double(RAND_MAX) * 0.3;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "C", C);
    of << strBuff;

    double B = rand() / double(RAND_MAX) * 1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "B", B);
    of << strBuff;

    double IMP = rand() / double(RAND_MAX) * 0.5;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "IMP", IMP);
    of << strBuff;

    double FC = rand() / double(RAND_MAX) * 30;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "FC", FC);
    of << strBuff;

    double KKG = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KKG", KKG);
    of << strBuff;

    int DT = 24;
    std::sprintf(strBuff, "%-20s %-20d\n\0", "DT", DT);
    of << strBuff;

    double KSTOR = rand() / double(RAND_MAX) * 6;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KSTOR", KSTOR);
    of << strBuff;

    of.close();
}

void writeInit3(int i) {

    char pFileName[100];
    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".ini\0");
    std::ofstream of(pFileName, std::ios::trunc);

    char strBuff[1024];
    std::sprintf(strBuff, "%-20s %-20s\n\0", "MODEL", "xinanjiang3");
    of << strBuff;

    double WU = rand() / double(RAND_MAX) * 200;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WU", WU);
    of << strBuff;

    double WL = rand() / double(RAND_MAX) * 150;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WL", WL);
    of << strBuff;

    double WD = rand() / double(RAND_MAX) * 100;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WD", WD);
    of << strBuff;

    double FR = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "FR", FR);
    of << strBuff;

    double S = rand() / double(RAND_MAX) * 0.9;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "S", S);
    of << strBuff;

    double QRSS0 = rand() / double(RAND_MAX) * 2.2;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "QRSS0", QRSS0);
    of << strBuff;

    double QRG0 = rand() / double(RAND_MAX) * 2;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "QRG0", QRG0);
    of << strBuff;

    of.close();
}

void writePara3(int i) {

    char pFileName[100];
    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".sub\0");
    std::ifstream fin(pFileName);
    std::string lineSub;
    std::string headName;
    double headValue;
    while(fin >> headName >> headValue)
    {
        if (headName == "BAREA") {
            break;
        }
    }
    fin.close();

    std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".para\0");
    std::ofstream of(pFileName, std::ios::trunc);

    char strBuff[1024];
    std::sprintf(strBuff, "%-20s %-20s\n\0", "MODEL", "xinanjiang3");
    of << strBuff;

    std::sprintf(strBuff, "%-20s %-20f\n\0", "F", headValue);
    of << strBuff;

    double WUM = rand() / double(RAND_MAX) * 20;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WUM", WUM);
    of << strBuff;

    double WLM = rand() / double(RAND_MAX) * 100;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WLM", WLM);
    of << strBuff;

    double WDM = rand() / double(RAND_MAX) * 100;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "WDM", WDM);
    of << strBuff;

    double K = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "K", K);
    of << strBuff;

    double C = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "C", C);
    of << strBuff;

    double B = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "B", B);
    of << strBuff;

    double IMP = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "IMP", IMP);
    of << strBuff;

    double SM = rand() / double(RAND_MAX) * 2;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "SM", SM);
    of << strBuff;

    double EX = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "EX", EX);
    of << strBuff;

    double KG = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KG", KG);
    of << strBuff;

    double KSS = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KSS", KSS);
    of << strBuff;

    double KKG = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KKG", KKG);
    of << strBuff;

    double KKSS = rand() / double(RAND_MAX) * 0.1;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KKSS", KKSS);
    of << strBuff;

    int DT = 24;
    std::sprintf(strBuff, "%-20s %-20d\n\0", "DT", DT);
    of << strBuff;

    double KSTOR = rand() / double(RAND_MAX) * 0.01;
    std::sprintf(strBuff, "%-20s %-20f\n\0", "KSTOR", KSTOR);
    of << strBuff;

    of.close();
}