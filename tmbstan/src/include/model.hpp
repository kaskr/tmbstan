#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
#include "custom_func.hpp"

// Code generated by Stan version 2.16.0

#include <stan/model/model_header.hpp>

namespace model_tmb_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_d;
typedef Eigen::Matrix<double,1,Eigen::Dynamic> row_vector_d;
typedef Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic> matrix_d;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_tmb");
    reader.add_event(13, 13, "end", "model_tmb");
    return reader;
}

class model_tmb : public prob_grad {
private:
    int N;
    int have_bounds;
    vector_d lower_bound;
    vector_d upper_bound;
public:
    model_tmb(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_tmb(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_tmb_namespace::model_tmb";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        context__.validate_dims("data initialization", "N", "int", context__.to_vec());
        N = int(0);
        vals_i__ = context__.vals_i("N");
        pos__ = 0;
        N = vals_i__[pos__++];
        context__.validate_dims("data initialization", "have_bounds", "int", context__.to_vec());
        have_bounds = int(0);
        vals_i__ = context__.vals_i("have_bounds");
        pos__ = 0;
        have_bounds = vals_i__[pos__++];
        validate_non_negative_index("lower_bound", "(N * have_bounds)", (N * have_bounds));
        context__.validate_dims("data initialization", "lower_bound", "vector_d", context__.to_vec((N * have_bounds)));
        validate_non_negative_index("lower_bound", "(N * have_bounds)", (N * have_bounds));
        lower_bound = vector_d(static_cast<Eigen::VectorXd::Index>((N * have_bounds)));
        vals_r__ = context__.vals_r("lower_bound");
        pos__ = 0;
        size_t lower_bound_i_vec_lim__ = (N * have_bounds);
        for (size_t i_vec__ = 0; i_vec__ < lower_bound_i_vec_lim__; ++i_vec__) {
            lower_bound[i_vec__] = vals_r__[pos__++];
        }
        validate_non_negative_index("upper_bound", "(N * have_bounds)", (N * have_bounds));
        context__.validate_dims("data initialization", "upper_bound", "vector_d", context__.to_vec((N * have_bounds)));
        validate_non_negative_index("upper_bound", "(N * have_bounds)", (N * have_bounds));
        upper_bound = vector_d(static_cast<Eigen::VectorXd::Index>((N * have_bounds)));
        vals_r__ = context__.vals_r("upper_bound");
        pos__ = 0;
        size_t upper_bound_i_vec_lim__ = (N * have_bounds);
        for (size_t i_vec__ = 0; i_vec__ < upper_bound_i_vec_lim__; ++i_vec__) {
            upper_bound[i_vec__] = vals_r__[pos__++];
        }

        // validate, data variables
        check_greater_or_equal(function__,"N",N,1);
        check_greater_or_equal(function__,"have_bounds",have_bounds,0);
        check_less_or_equal(function__,"have_bounds",have_bounds,1);
        // initialize data variables

        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed data

        // validate, set parameter ranges
        num_params_r__ = 0U;
        param_ranges_i__.clear();
        validate_non_negative_index("y", "N", N);
        num_params_r__ += N;
    }

    ~model_tmb() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("y")))
            throw std::runtime_error("variable y missing");
        vals_r__ = context__.vals_r("y");
        pos__ = 0U;
        validate_non_negative_index("y", "N", N);
        context__.validate_dims("initialization", "y", "vector_d", context__.to_vec(N));
        // generate_declaration y
        vector_d y(static_cast<Eigen::VectorXd::Index>(N));
        for (int j1__ = 0U; j1__ < N; ++j1__)
            y(j1__) = vals_r__[pos__++];
        try {

// ====== Custom Edit Begin
if (!have_bounds) {
  writer__.vector_unconstrain(y);
} else {
  for (int j1__ = 0U; j1__ < N; ++j1__)
    writer__.scalar_lub_unconstrain(lower_bound(j1__), upper_bound(j1__), y(j1__));
}
// ====== Custom Edit End

        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable y: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        T__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        // model parameters
        stan::io::reader<T__> in__(params_r__,params_i__);

        Eigen::Matrix<T__,Eigen::Dynamic,1>  y;
        (void) y;  // dummy to suppress unused var warning
        if (jacobian__)

// ====== Custom Edit Begin
{
  if(!have_bounds) {
    y = in__.vector_constrain(N, lp__);
  } else {
    y.resize(N);
    for (int j1__ = 0U; j1__ < N; ++j1__)
      y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__), lp__);
  }
}
// ====== Custom Edit End

        else

// ====== Custom Edit Begin
{
  if(!have_bounds) {
    y = in__.vector_constrain(N);
  } else {
    y.resize(N);
    for (int j1__ = 0U; j1__ < N; ++j1__)
      y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
  }
}
// ====== Custom Edit End



        // transformed parameters


        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        const char* function__ = "validate transformed params";
        (void) function__;  // dummy to suppress unused var warning

        // model body
        try {

            current_statement_begin__ = 12;

// ====== Custom Edit Begin
lp_accum__.add(custom_func(y));
// ====== Custom Edit End

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("y");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        vars__.resize(0);
        stan::io::reader<double> in__(params_r__,params_i__);
        static const char* function__ = "model_tmb_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters

// ====== Custom Edit Begin
vector_d y;
if(!have_bounds) {
  y = in__.vector_constrain(N);
} else {
  y.resize(N);
  for (int j1__ = 0U; j1__ < N; ++j1__)
    y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
}
// ====== Custom Edit End

        for (int k_0__ = 0; k_0__ < N; ++k_0__) {
            vars__.push_back(y[k_0__]);
        }

        if (!include_tparams__) return;
        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning



        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        // write transformed parameters

        if (!include_gqs__) return;
        // declare and define generated quantities


        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate generated quantities

        // write generated quantities
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_tmb";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }

}; // model

}

typedef model_tmb_namespace::model_tmb stan_model;

