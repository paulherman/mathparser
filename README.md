Math Parser
========
The C++ can be used as an include library.

PHP example of how to use:
 *   $expression = "square(e^t + 2)";
 *   $parser = new CMParser();
 *   $parser->load($expression);
 *   $parser->addFunction("square", 1, function ($x) { return $x * $x; });
 *   $parser->setParameter("t", 3);
 *   echo $parser->evaluate();
