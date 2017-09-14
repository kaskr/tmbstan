/* Plugin a custom function and gradient in a stan model  */

namespace custom_func {
  namespace {
    using stan::math::var;
    using stan::math::op_matrix_vari;
    typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_double;
    typedef Eigen::Matrix<var,Eigen::Dynamic,1> vector_var;

    // Case 1: Plugin function and gradient from R
    SEXP R_x;
    SEXP R_fcall;
    SEXP R_gcall;
    SEXP R_env;

    // Case 2: Pass a pointer to ADFun object (faster)
    SEXP R_ptr;
    typedef void * (*FUN_PTR)(SEXP f, const vector_double &x, vector_double &y);
    FUN_PTR tmb_forward;
    FUN_PTR tmb_reverse;

    extern "C" {
      SEXP set_pointers(SEXP x_, SEXP f_, SEXP g_, SEXP e_, SEXP p_, SEXP dll_ ) {
        // Case 1:
        R_x = x_; R_fcall = f_; R_gcall = g_; R_env = e_;
        // Case 2:
        R_ptr = p_;
        if (R_ExternalPtrAddr(R_ptr)) {
          tmb_forward = (FUN_PTR) R_GetCCallable(CHAR(STRING_ELT(dll_,0)), "tmb_forward");
          tmb_reverse = (FUN_PTR) R_GetCCallable(CHAR(STRING_ELT(dll_,0)), "tmb_reverse");
        }
        return R_NilValue;
      }
    }

    double custom_func_as_double(const vector_var& xvar) {
      if ( ! R_ExternalPtrAddr(R_ptr) ) { // Case 1
        // Set evaluation point
        double* px = REAL(R_x);
        for (int i = 0; i < xvar.size(); ++i) {
          px[i] = xvar(i).val();
        }
        SEXP y;
        PROTECT( y = Rf_eval(R_fcall, R_env) );
        double ans = -REAL(y)[0];
        UNPROTECT(1);
        return ans;
      } else {                            // Case 2
        vector_double x(xvar.size());
        vector_double y(1);
        for (int i = 0; i < xvar.size(); ++i) {
          x(i) = xvar(i).val();
        }
        tmb_forward(R_ptr, x, y);
        return -y(0);
      }
    }
    // Gradient
    void gradient_custom_func_as_double(const vector_double& x_,
                                        vector_double& g) {
      if ( ! R_ExternalPtrAddr(R_ptr) ) {
        // Set evaluation point
        double* px = REAL(R_x);
        for (int i = 0; i < x_.size(); ++i) {
          px[i] = x_(i);
        }
        SEXP y;
        PROTECT( y = Rf_eval(R_gcall, R_env) );
        double* py = REAL(y);
        for (int i = 0; i < x_.size(); ++i) {
          g(i) = -py[i];
        }
        UNPROTECT(1);
      } else {
        vector_double v(1);
        v(0) = 1;
        tmb_reverse(R_ptr, v, g);
        g = -g;
      }
    }
    class custom_func_vector_vari : public op_matrix_vari {
    public:
      explicit custom_func_vector_vari(const vector_var& x) :
        op_matrix_vari(custom_func_as_double(x), x) {
      }
      void chain() {
        vector_double x(size_);
        vector_double g(size_);
        for (int i = 0; i < (int) size_; ++i)
          x(i) = vis_[i]->val_;
        gradient_custom_func_as_double(x, g);
        for (int i = 0; i < (int) size_; ++i) {
          vis_[i]->adj_ += adj_ * g(i);
        }
      }
    };
  } // End anonymous namespace

  inline var custom_func(const vector_var& x) {
    return var(new custom_func_vector_vari(x));
  }
  inline double custom_func(const vector_double& x_) {
    if ( ! R_ExternalPtrAddr(R_ptr) ) {
      double* px = REAL(R_x);
      for (int i = 0; i < x_.size(); ++i) {
        px[i] = x_(i);
      }
      SEXP y;
      PROTECT( y = Rf_eval(R_fcall, R_env) );
      double ans = -REAL(y)[0];
      UNPROTECT(1);
      return ans;
    } else {
      vector_double y(1);
      tmb_forward(R_ptr, x_, y);
      return -y(0);
    }
  }
}
