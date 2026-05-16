#include "Adder.h"
#include <util/nainf.h>

#include <cmath>

using std::vector;
using std::string;

#define arg1(args) (*args[0])
#define arg2(args) (*args[1])

namespace jags {
namespace basicmodule {

Adder::Adder() : ScalarFunction("adder", 2)
{}

bool Adder::checkParameterValue(vector<double const *> const &args) const
{
  return true;
}

double Adder::evaluate(vector<double const *> const &args) const
{
  double sum = 0.0;
  sum += arg1(args);
  sum += arg2(args);
  
  return sum;
}

}  // namespace basicmodule
}  // namespace jags
