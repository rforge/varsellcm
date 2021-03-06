/*
Cette classe est héritière d'XEM et permet d'obtenir le MLE pour des données continues

Ces élements sont:
ceux de la classe Algorithm
paramCand : vecteur contenant les paramètres issus des différentes initialisations
paramCurrent_p : pointeur vers les paramètres à partir desquels TOUS les calculs sont faits
data_p : pointeur vers les données
m_weightTMP : ulilisé pour le calcul des probabilités conditionelles

*/
#ifndef XEMInteger_H
#define XEMInteger_H
#include "DataInteger.h"
#include "ParamInteger.h"
#include "XEM.h"


class XEMInteger : public XEM{
  public:
  vector<ParamInteger> paramCand;
  ParamInteger * paramCurrent_p;
  const DataInteger * data_p;
  Col<double> m_weightTMP;
  
  // Constructeurs et destructeurs par défaut (non utilisé)
  XEMInteger(){};
  ~XEMInteger(){};

  // Constructeurs avec et sans les paramètres de réglages
  XEMInteger(const DataInteger *, const colvec &, const int &);
  XEMInteger(const DataInteger *,  const S4 *);
  // Initialisation des paramètres spécifiques
  void InitSpecificParamXEMInteger(const DataInteger * datapasse);
    
  // Les trois fonction suivantes sont à redéfinir pour chaque classe héritiaire d'XEM
  // Etape M
  virtual void Mstep();
  // calcul des log-proba conditionnelles
  virtual void ComputeTmpLogProba();
  // change le pointeur des paramètres acutels (voir définietion spécifique des classes)
  virtual void SwitchParamCurrent(int);
  // Acutalise l'object S4 retourné sous R
  virtual void Output(S4 *);

};
#endif