/* Plugin a custom function and gradient in a stan model  */
namespace stan {
  namespace math {
    namespace {
      SEXP x;
      SEXP R_fcall;
      SEXP R_gcall;
      SEXP env;
      extern "C" {
        SEXP set_pointers(SEXP x_, SEXP f_, SEXP g_, SEXP e_ ) {
          x = x_; R_fcall = f_; R_gcall = g_; env = e_;
          return R_NilValue;
        }
      }
      double custom_func_as_double(const std::vector<var>& xvar) {
        // Set evaluation point
        double* px = REAL(x);
        for (size_t i = 0; i < xvar.size(); ++i) {
          px[i] = xvar[i].val();
        }
        SEXP y;
        PROTECT( y = Rf_eval(R_fcall, env) );
        double ans = -REAL(y)[0];
        UNPROTECT(1);
        return ans;
      }
      // Gradient
      void gradient_custom_func_as_double(const std::vector<double>& x_,
                                          std::vector<double>& g) {
        // Set evaluation point
        double* px = REAL(x);
        for (size_t i = 0; i < x_.size(); ++i) {
          px[i] = x_[i];
        }
        SEXP y;
        PROTECT( y = Rf_eval(R_gcall, env) );
        double* py = REAL(y);
        for (size_t i = 0; i < x_.size(); ++i) {
          g[i] = -py[i];
        }
        UNPROTECT(1);
      }
      class custom_func_vector_vari : public op_vector_vari {
      public:
        explicit custom_func_vector_vari(const std::vector<var>& x) :
          op_vector_vari(custom_func_as_double(x), x) {
        }
        void chain() {
          std::vector<double> x(size_);
          std::vector<double> g(size_);
          for (size_t i = 0; i < size_; ++i)
            x[i] = vis_[i]->val_;
          gradient_custom_func_as_double(x, g);
          for (size_t i = 0; i < size_; ++i) {
            vis_[i]->adj_ += adj_ * g[i];
          }
        }
      };
    }
    inline var custom_func(const std::vector<var>& x) {
      return var(new custom_func_vector_vari(x));
    }
    /* Make it work for Eigen vectors */
    typedef Eigen::Matrix<var,Eigen::Dynamic,1> vector_var;
    inline var custom_func(const vector_var& x_) {
      std::vector<var> x(x_.size());
      for (size_t i = 0; i < x_.size(); ++i) {x[i] = x_(i);}
      return var(new custom_func_vector_vari(x));
    }
    /* Make it work for Eigen vectors */
    typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_double;
    inline double custom_func(const vector_double& x_) {
      double* px = REAL(x);
      for (size_t i = 0; i < x_.size(); ++i) {
        px[i] = x_(i);
      }
      SEXP y;
      PROTECT( y = Rf_eval(R_fcall, env) );
      double ans = -REAL(y)[0];
      UNPROTECT(1);
      return ans;
    }
  }
}
