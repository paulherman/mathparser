#include "mp.hpp"

void stripSpace(string &str)
{
	for (unsigned int i = 0; i < str.size();)
	{
		if ((str[i] == ' ') || (str[i] == '\n') || (str[i] == '\t') || (str[i] == '\v') || (str[i] == '\f') || (str[i] == '\r'))
		{
			str.erase(i, 1);
			i--;
		}
		i++;
	}
}
CParser::CParser(string str)
{
	functionArgs["sqrt"] = 1;
	functionOps["sqrt"] = EOperations::osqrt;
	functionArgs["ln"] = 1;
	functionOps["ln"] = EOperations::oln;
	functionArgs["log"] = 2;
	functionOps["log"] = EOperations::olog;
	functionArgs["lg"] = 1;
	functionOps["lg"] = EOperations::olg;
	functionArgs["integrate"] = 4;
	functionOps["integrate"] = EOperations::ointegrate;
	functionArgs["derivate"] = 3;
	functionOps["derivate"] = EOperations::oderivate;
	functionArgs["sin"] = 1;
	functionOps["sin"] = EOperations::osin;
	functionArgs["cos"] = 1;
	functionOps["cos"] = EOperations::ocos;
	functionArgs["tan"] = 1;
	functionOps["tan"] = EOperations::otan;
	functionArgs["ctan"] = 1;
	functionOps["ctan"] = EOperations::octan;
	tree = NULL;
	error = false;
	position = 0;
	stripSpace(str);
	expression = str;
	tree = parse(NULL);
}
CParser::~CParser()
{
	if (tree != NULL)
		delete tree;
}
int CParser::toint(char c)
{
	return (int)(c - '0');
}
void CParser::setParameter(string name, double value)
{
	if (error == false)
	{
		parameters[name] = value;
		tree->setParameter(name, value);
	}
}
CExpression *CParser::parseTerm()
{
	if ((end(position) == true))
	{
		addError("invalid term");
		return NULL;
	}
	else
	{
		if (expression[position] == '(')
		{
			position++;
			return parse(NULL);
		}
		else
		{
			CExpression *result = new CExpression();
			result->setOp(EOperations::ovalue);
			if ((isdigit(expression[position]) == true) || (expression[position] == '-') || (expression[position] == '+'))
			{
				bool negative = false;
				float number = 0;
				if (expression[position] == '-')
				{
					position++;
					negative = true;
				}
				if (expression[position] == '+')
					position++;
				while ((end(position) == false) && (isdigit(expression[position]) == true))
				{
					number = number * 10 + toint(expression[position]);
					position++;
				}
				if ((end(position) == false) && (expression[position] == '.'))
				{
					position++;
					float point = 1;
					while ((end(position) == false) && (isdigit(expression[position]) != 0))
					{
						point /= 10;
						number = number + point * toint(expression[position]);
						position++;
					}
				}
				if (negative == true)
					number *= -1;
				result->addValue(number);
			}
			else if (isalnum(expression[position]) != 0)
			{
				unsigned int tempPosition = position;
				while ((end(tempPosition) == false) && (isalnum(expression[tempPosition]) != 0))
					tempPosition++;
				if ((end(tempPosition) == false) && (expression[tempPosition] == '('))
				{
					string function;
					for (; position < tempPosition; position++)
						function += expression[position];
					position++;
					int arguments = functionArgs[function];
					result->setOp(functionOps[function]);
					for (int i = 0; i < arguments; i++)
						result->addExp(parse(NULL));
					if ((arguments == 0) && ((end(position) == true) || (expression[position] != ')')))
						addError("function missing bracket");
					if ((arguments > 0) && ((end(position - 1) == true) || (expression[position - 1] != ')')))
					{
						position--;
						addError("function missing bracket");
					}
				}
				else
				{
					string parameter;
					for (; position < tempPosition; position++)
						parameter += expression[position];
					result->addParam(parameter);
				}
			}
			else
				addError("invalid term");
			return result;
		}
	}
}
bool CParser::end(unsigned int pos)
{
	return pos >= expression.length();
}
void CParser::addError(string message)
{
	if (end(position) == false)
		printf("ERROR[%d '%c'] %s\n", position, expression[position], message.c_str());
	else
		printf("ERROR[%d ''] %s\n", position, message.c_str());
	errors.push_back(position);
	error = true;
	position = expression.length();
}
int CParser::getOperation()
{
	int operationID = 0;
	if ((end(position) == false) && (expression[position] != ')') && (expression[position] != ','))
	{
		char operation = expression[position];
		switch (operation)
		{
			case '+':
				operationID = EOperations::oaddition;
				break;
			case '-':
				operationID = EOperations::osubstraction;
				break;
			case '*':
				operationID = EOperations::omultiplication;
				break;
			case '/':
				operationID = EOperations::odivision;
				break;
			case '^':
				operationID = EOperations::opower;
				break;
			default:
				addError("invalid operator");
				operationID = 0;
				break;
		}
		position++;
	}
	return operationID;
}
int CParser::peekOperation()
{
	int operationID = 0;
	if ((end(position) == false) && (expression[position] != ')') && (expression[position] != ','))
	{
		char operation = expression[position];
		switch (operation)
		{
			case '+':
				operationID = EOperations::oaddition;
				break;
			case '-':
				operationID = EOperations::osubstraction;
				break;
			case '*':
				operationID = EOperations::omultiplication;
				break;
			case '/':
				operationID = EOperations::odivision;
				break;
			case '^':
				operationID = EOperations::opower;
				break;
			default:
				addError("invalid operator peek");
				operationID = 0;
				break;
		}
	}
	return operationID;
}
int CParser::operatorPriority(int op)
{
	int priority = 0;
	switch (op)
	{
		case EOperations::osubstraction:
			priority = 1;
			break;
		case EOperations::oaddition:
			priority = 1;
			break;
		case EOperations::odivision:
			priority = 2;
			break;
		case EOperations::omultiplication:
			priority = 2;
			break;
		case EOperations::opower:
			priority = 3;
			break;
		default:
			addError("invalid operator priority");
			break;
	}
	return priority;
}
CExpression *CParser::parse(CExpression *oldTermOne)
{
	CExpression *termOne;
	CExpression *termTwo;
	char op;
	char opNext;
	if (oldTermOne != NULL)
		termOne = oldTermOne;
	else
		termOne = parseTerm();
	if ((end(position) == false) && (expression[position] != ')') && (expression[position] != ','))
	{
		op = getOperation();
		termTwo = parseTerm();
		if ((end(position) == false) && (expression[position] != ')') && (expression[position] != ','))
		{
			opNext = peekOperation();
			//Exponentiation is right associative so it is an excepted case
			if ((operatorPriority(opNext) > operatorPriority(op)) || (opNext == '^'))
			{
				CExpression *result = new CExpression;
				result->addExp(termOne);
				result->addExp(parse(termTwo));
				result->setOp(op);
				return result;
			}
			else
			{
				CExpression *result = new CExpression;
				result->addExp(termOne);
				result->addExp(termTwo);
				result->setOp(op);
				return parse(result);
			}
		}
		else
		{
			if ((end(position) == false) && (expression[position] == ')'))
				position++;
			if ((end(position) == false) && (expression[position] == ','))
				position++;
			CExpression *result = new CExpression;
			result->addExp(termOne);
			result->addExp(termTwo);
			result->setOp(op);
			return result;
		}
	}
	else
	{
		position++;
		return termOne;
	}
}
CExpression *CParser::emptyExpression()
{
	CExpression *empty;
	empty->setOp(EOperations::ovalue);
	empty->addValue(0);
	return empty;
}
double CParser::evaluate()
{
	if (error == false)
	{
		double result = tree->evaluate();
		if (tree->error == true)
			error = true;
		if (error == true)
			return 0;
		else
			return result;
	}
	else
		return 0;
}
void CExpression::addExp(CExpression *exp)
{
	termTypes.push_back(ETypes::expression);
	termExpressions.push_back(exp);
}
void CExpression::setOp(int op)
{
	operation = op;
}
void CExpression::addValue(float value)
{
	termTypes.push_back(ETypes::value);
	termValues.push_back(value);
}
void CExpression::addParam(string name)
{
	termTypes.push_back(ETypes::parameter);
	termParameters.push_back(name);
}
double CExpression::evaluate()
{
	error = false;
	double result = 0;
	int countVal = 0;
	int countExp = 0;
	int countParam = 0;
	vector<double> values;
	for (unsigned int i = 0; i < termTypes.size(); i++)
	{
		if (termTypes[i] == ETypes::value)
		{
			values.push_back(termValues[countVal]);
			countVal++;
		}
		if (termTypes[i] == ETypes::expression)
		{
			values.push_back(termExpressions[countExp]->evaluate());
			if (termExpressions[countExp]->error == true)
				addError("evaluation error");
			countExp++;
		}
		if (termTypes[i] == ETypes::parameter)
		{
			values.push_back(parameters[termParameters[i]]);
			countParam++;
		}
	}
	switch (operation)
	{
		case EOperations::ocos:
			result = cos(values[0]);
			break;
		case EOperations::osin:
			result = sin(values[0]);
			break;
		case EOperations::otan:
			result = tan(values[0]);
			break;
		case EOperations::octan:
			result = 1 / tan(values[0]);
			break;
		case EOperations::olg:
			result = log(values[0]) / log(10);
			break;
		case EOperations::ovalue:
			result = values[0];
			break;
		case EOperations::oaddition:
			result = values[0] + values[1];
			break;
		case EOperations::osubstraction:
			result = values[0] - values[1];
			break;
		case EOperations::omultiplication:
			result = values[0] * values[1];
			break;
		case EOperations::odivision:
			result = values[0] / values[1];
			break;
		case EOperations::oln:
			result = log(values[0]);
			break;
		case EOperations::opower:
			result = pow(values[0], values[1]);
			break;
		case EOperations::oderivate:
		{
			if (termExpressions[1]->termTypes[0] != ETypes::parameter)
				addError("derivative error");
			else
			{
				string paramName = termExpressions[1]->termParameters[0];
				double paramValue = parameters[paramName];
				double fa, fb;
				setParameter(paramName, values[2]);
				fa = termExpressions[0]->evaluate();
				if (termExpressions[0]->error == true)
					addError("derivative error");
				setParameter(paramName, values[2] + 0.00001);
				fb = termExpressions[0]->evaluate();
				if (termExpressions[0]->error == true)
					addError("derivative error");
				result = (fb - fa) / 0.00001;
				setParameter(paramName, paramValue);
			}
			break;
		}
		case EOperations::osqrt:
			result = sqrt(values[0]);
			break;
		case EOperations::olog:
			if ((values[0] <= 0) || (values[1] <= 0))
				addError("logarithm error");
			result = log(values[1]) / log(values[0]);
			break;
		case EOperations::ointegrate:
		{
			if (termExpressions[1]->termTypes[0] != ETypes::parameter)
				addError("integration error");
			else
			{
				string paramName = termExpressions[1]->termParameters[0];
				double paramValue = parameters[paramName];
				double fa, fb;
				for (double a = values[2]; a < values[3]; a += 0.00001)
				{
					setParameter(paramName, a);
					if (termTypes[0] == ETypes::value)
						fa = termValues[0];
					if (termTypes[0] == ETypes::parameter)
						fa = parameters[termParameters[0]];
					if (termTypes[0] == ETypes::expression)
						fa = termExpressions[0]->evaluate();
					if (termExpressions[0]->error == true)
						addError("integration error");
					parameters[paramName] = a + 0.0001;
					if (termTypes[0] == ETypes::value)
						fb = termValues[0];
					if (termTypes[0] == ETypes::parameter)
						fb = parameters[termParameters[0]];
					if (termTypes[0] == ETypes::expression)
						fb = termExpressions[0]->evaluate();
					if (termExpressions[0]->error == true)
						addError("integration error");
					result += 0.00001 * (fa + fb) / 2;
				}
				setParameter(paramName, paramValue);
			}
			break;
		}
	}
	return result;
}
void CExpression::addError(string message)
{
	printf("EVAL_ERROR[%s]\n", message.c_str());
	error = true;
}
CExpression::CExpression()
{
	error = false;
}
CExpression::~CExpression()
{
	for (unsigned int i = 0; i < termExpressions.size(); i++)
		if (termExpressions[i] != NULL)
			delete termExpressions[i];
}
void CExpression::setParameter(string name, double value)
{
	parameters[name] = value;
	for (unsigned int i = 0; i < termExpressions.size(); i++)
		termExpressions[i]->setParameter(name, value);
}
