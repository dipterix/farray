// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_farray_RCPPEXPORTS_H_GEN_
#define RCPP_farray_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace farray {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("farray", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("farray", "_farray_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in farray");
            }
        }
    }

    inline R_xlen_t setFArrayBlockSize(R_xlen_t size, R_xlen_t limit = 0, R_xlen_t buf_size = 0) {
        typedef SEXP(*Ptr_setFArrayBlockSize)(SEXP,SEXP,SEXP);
        static Ptr_setFArrayBlockSize p_setFArrayBlockSize = NULL;
        if (p_setFArrayBlockSize == NULL) {
            validateSignature("R_xlen_t(*setFArrayBlockSize)(R_xlen_t,R_xlen_t,R_xlen_t)");
            p_setFArrayBlockSize = (Ptr_setFArrayBlockSize)R_GetCCallable("farray", "_farray_setFArrayBlockSize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_setFArrayBlockSize(Shield<SEXP>(Rcpp::wrap(size)), Shield<SEXP>(Rcpp::wrap(limit)), Shield<SEXP>(Rcpp::wrap(buf_size)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<R_xlen_t >(rcpp_result_gen);
    }

    inline R_xlen_t getFArrayBlockSize(int which = 0) {
        typedef SEXP(*Ptr_getFArrayBlockSize)(SEXP);
        static Ptr_getFArrayBlockSize p_getFArrayBlockSize = NULL;
        if (p_getFArrayBlockSize == NULL) {
            validateSignature("R_xlen_t(*getFArrayBlockSize)(int)");
            p_getFArrayBlockSize = (Ptr_getFArrayBlockSize)R_GetCCallable("farray", "_farray_getFArrayBlockSize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getFArrayBlockSize(Shield<SEXP>(Rcpp::wrap(which)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<R_xlen_t >(rcpp_result_gen);
    }

    inline int getFArrayThread(bool max = false) {
        typedef SEXP(*Ptr_getFArrayThread)(SEXP);
        static Ptr_getFArrayThread p_getFArrayThread = NULL;
        if (p_getFArrayThread == NULL) {
            validateSignature("int(*getFArrayThread)(bool)");
            p_getFArrayThread = (Ptr_getFArrayThread)R_GetCCallable("farray", "_farray_getFArrayThread");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getFArrayThread(Shield<SEXP>(Rcpp::wrap(max)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline int setFArrayThread(int n, SEXP reset_after_fork = R_NilValue) {
        typedef SEXP(*Ptr_setFArrayThread)(SEXP,SEXP);
        static Ptr_setFArrayThread p_setFArrayThread = NULL;
        if (p_setFArrayThread == NULL) {
            validateSignature("int(*setFArrayThread)(int,SEXP)");
            p_setFArrayThread = (Ptr_setFArrayThread)R_GetCCallable("farray", "_farray_setFArrayThread");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_setFArrayThread(Shield<SEXP>(Rcpp::wrap(n)), Shield<SEXP>(Rcpp::wrap(reset_after_fork)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline bool hasOpenMP() {
        typedef SEXP(*Ptr_hasOpenMP)();
        static Ptr_hasOpenMP p_hasOpenMP = NULL;
        if (p_hasOpenMP == NULL) {
            validateSignature("bool(*hasOpenMP)()");
            p_hasOpenMP = (Ptr_hasOpenMP)R_GetCCallable("farray", "_farray_hasOpenMP");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_hasOpenMP();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXP dropDimension(SEXP x) {
        typedef SEXP(*Ptr_dropDimension)(SEXP);
        static Ptr_dropDimension p_dropDimension = NULL;
        if (p_dropDimension == NULL) {
            validateSignature("SEXP(*dropDimension)(SEXP)");
            p_dropDimension = (Ptr_dropDimension)R_GetCCallable("farray", "_farray_dropDimension");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_dropDimension(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline int64_t prod2(SEXP x, bool na_rm = false) {
        typedef SEXP(*Ptr_prod2)(SEXP,SEXP);
        static Ptr_prod2 p_prod2 = NULL;
        if (p_prod2 == NULL) {
            validateSignature("int64_t(*prod2)(SEXP,bool)");
            p_prod2 = (Ptr_prod2)R_GetCCallable("farray", "_farray_prod2");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_prod2(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(na_rm)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int64_t >(rcpp_result_gen);
    }

    inline SEXP parseDots(Rcpp::Environment& env, bool eval) {
        typedef SEXP(*Ptr_parseDots)(SEXP,SEXP);
        static Ptr_parseDots p_parseDots = NULL;
        if (p_parseDots == NULL) {
            validateSignature("SEXP(*parseDots)(Rcpp::Environment&,bool)");
            p_parseDots = (Ptr_parseDots)R_GetCCallable("farray", "_farray_parseDots");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_parseDots(Shield<SEXP>(Rcpp::wrap(env)), Shield<SEXP>(Rcpp::wrap(eval)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError = true) {
        typedef SEXP(*Ptr_stopIfNot)(SEXP,SEXP,SEXP);
        static Ptr_stopIfNot p_stopIfNot = NULL;
        if (p_stopIfNot == NULL) {
            validateSignature("bool(*stopIfNot)(const bool,const std::string&,bool)");
            p_stopIfNot = (Ptr_stopIfNot)R_GetCCallable("farray", "_farray_stopIfNot");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_stopIfNot(Shield<SEXP>(Rcpp::wrap(isValid)), Shield<SEXP>(Rcpp::wrap(message)), Shield<SEXP>(Rcpp::wrap(stopIfError)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXPTYPE getSexpType(SEXP x) {
        typedef SEXP(*Ptr_getSexpType)(SEXP);
        static Ptr_getSexpType p_getSexpType = NULL;
        if (p_getSexpType == NULL) {
            validateSignature("SEXPTYPE(*getSexpType)(SEXP)");
            p_getSexpType = (Ptr_getSexpType)R_GetCCallable("farray", "_farray_getSexpType");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getSexpType(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXPTYPE >(rcpp_result_gen);
    }

    inline SEXP tik() {
        typedef SEXP(*Ptr_tik)();
        static Ptr_tik p_tik = NULL;
        if (p_tik == NULL) {
            validateSignature("SEXP(*tik)()");
            p_tik = (Ptr_tik)R_GetCCallable("farray", "_farray_tik");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_tik();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP tok(std::string msg, bool stop = false) {
        typedef SEXP(*Ptr_tok)(SEXP,SEXP);
        static Ptr_tok p_tok = NULL;
        if (p_tok == NULL) {
            validateSignature("SEXP(*tok)(std::string,bool)");
            p_tok = (Ptr_tok)R_GetCCallable("farray", "_farray_tok");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_tok(Shield<SEXP>(Rcpp::wrap(msg)), Shield<SEXP>(Rcpp::wrap(stop)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

}

#endif // RCPP_farray_RCPPEXPORTS_H_GEN_
