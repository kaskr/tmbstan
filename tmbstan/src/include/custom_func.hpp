/* Plugin a custom function and gradient in a stan model  */

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_double;
extern void tmb_forward(SEXP f, const vector_double &x, vector_double &y);
extern void tmb_reverse(SEXP f, const vector_double &x, vector_double &y);

namespace stan {
  namespace math {
    namespace {
      // Case 1: Plugin function and gradient from R
      SEXP R_x;
      SEXP R_fcall;
      SEXP R_gcall;
      SEXP R_env;
      // Case 2: Pass a pointer to ADFun object (faster)
      SEXP R_ptr;
      extern "C" {
        SEXP set_pointers(SEXP x_, SEXP f_, SEXP g_, SEXP e_, SEXP p_ ) {
          // Case 1:
          R_x = x_; R_fcall = f_; R_gcall = g_; R_env = e_;
          // Case 2:
          R_ptr = p_;
          return R_NilValue;
        }
      }

      typedef Eigen::Matrix<var,Eigen::Dynamic,1> vector_var;
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
          tmb_reverse(R_ptr, x_, g);
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
    }
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
}
