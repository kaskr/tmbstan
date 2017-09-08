#include <TMB.hpp>

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_double;

void tmb_forward(SEXP f, const vector_double &x, vector_double &y) {
  ADFun<double>* pf;
  pf = (ADFun<double>*) R_ExternalPtrAddr(f);
  y = pf->Forward(0, x);
  // SEXP tag=R_ExternalPtrTag(f);
  // if(!strcmp(CHAR(tag), "ADFun"))
  //   return EvalADFunObjectTemplate<ADFun<double> >(f,theta,control);
  // if(!strcmp(CHAR(tag), "parallelADFun"))
  //   return EvalADFunObjectTemplate<parallelADFun<double> >(f,theta,control);
}

void tmb_reverse(SEXP f, const vector_double &x, vector_double &y) {
  ADFun<double>* pf;
  pf = (ADFun<double>*) R_ExternalPtrAddr(f);
  vector_double v(1);
  v(0) = 1.;
  y = pf->Reverse(1, v);
  // SEXP tag=R_ExternalPtrTag(f);
  // if(!strcmp(CHAR(tag), "ADFun"))
  //   return EvalADFunObjectTemplate<ADFun<double> >(f,theta,control);
  // if(!strcmp(CHAR(tag), "parallelADFun"))
  //   return EvalADFunObjectTemplate<parallelADFun<double> >(f,theta,control);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  return 0;
}
