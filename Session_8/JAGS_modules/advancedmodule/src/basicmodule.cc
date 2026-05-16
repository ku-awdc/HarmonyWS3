#include <module/Module.h>
#include <function/DFunction.h>
#include <function/PFunction.h>
#include <function/QFunction.h>

#include "functions/adder.h"
#include "distributions/DLom.h"

using std::vector;

namespace jags {
namespace basicmodule {

	class basicModule : public Module {
	  public:
	    basicModule();
	    ~basicModule();

		void Rinsert(RScalarDist *dist);
	};

basicModule::basicModule() : Module("basicmodule")
{
  // For functions:
  insert(new Adder);

  // For distributions using d/p/q/r:
  Rinsert(new DLom);
}

void basicModule::Rinsert(RScalarDist *dist)
{
	insert(dist);
	insert(new DFunction(dist));
	insert(new PFunction(dist));
	insert(new QFunction(dist));
}

basicModule::~basicModule()
{
  vector<Function*> const &fvec = functions();
  for (unsigned int i = 0; i < fvec.size(); ++i) {
    delete fvec[i];
  }
  vector<Distribution*> const &dvec = distributions();
  for (unsigned int i = 0; i < dvec.size(); ++i) {
    delete dvec[i];
  }
}

}  // namespace basicmodule
}  // namespace jags

jags::basicmodule::basicModule _basic_module;
