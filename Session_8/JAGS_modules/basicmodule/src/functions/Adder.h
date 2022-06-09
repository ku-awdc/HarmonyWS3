#ifndef ADDER_H_
#define ADDER_H_

#include <function/ScalarFunction.h>

namespace jags {
namespace basicmodule {

class Adder : public ScalarFunction 
{
  public:
    Adder();

    bool checkParameterValue(std::vector<double const *> const &args) const;
    double evaluate(std::vector<double const *> const &args) const;
};

}  // namespace basicmodule
}  // namespace jags

#endif /* ADDER_H_ */
