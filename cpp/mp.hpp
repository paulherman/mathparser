#pragma once
#include <string>
#include <vector>
#include <cmath>
#include <iostream>
#include <cstring>
#include <map>
#include <cstdio>
#include <algorithm>
using namespace std;

enum ETypes
{
	value,
	parameter,
	expression
};
enum EOperations
{
	ovalue,
	oaddition,
	osubstraction,
	omultiplication,
	odivision,
	opower,
	osin,
	ocos,
	otan,
	octan,
	olog,
	oln,
	olg,
	ointegrate,
	oderivate,
	osqrt
};
class CExpression
{
public:
	double evaluate();
	void setParameter(string name, double value);
	void addValue(float value);
	void addParam(string name);
	void addExp(CExpression *exp);
	void setOp(int op);
	CExpression();
	~CExpression();
	bool error;
private:
	void addError(string message);
	int operation;
	vector<int> termTypes;
	vector<double> termValues;
	vector<string> termParameters;
	vector<CExpression *> termExpressions;
	map<string, int> functions;
	map<string, double> parameters;
};
class CParser
{
public:
	CParser(string str);
	~CParser();
	double evaluate();
	void setParameter(string name, double value);
	bool error;
	vector<int> errors;
private:
	void addError(string message);
	CExpression *emptyExpression();
	CExpression *tree;
	map<string, int> functionArgs;
	map<string, int> functionOps;
	map<string, double> parameters;
	bool end(unsigned int pos);
	unsigned int position;
	string expression;
	CExpression *parse(CExpression *oldTermOne);
	int toint(char c);
	CExpression *parseTerm();
	int getOperation();
	int peekOperation();
	int operatorPriority(int op);
protected:
};
