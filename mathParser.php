<?php
class CExpression
{
	private $functions;
	private $parameters;
	private $operator;
	private $terms;
	private static function funcValue($terms, $parameters)
    {
		$result = false;
		foreach ($terms as $key => $term)
		{
			$terms[$key]["eval"] = false;
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (isset($parameters[$term["value"]]) == true)
					$terms[$key]["eval"] = (double)$parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false))
			$result = $terms[0]["eval"];
		return $result;
	}
	private static function funcMul($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false))
			$result = $terms[0]["eval"] * $terms[1]["eval"];
		return $result;
	}
	private static function funcDiv($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false) && ($terms[1]["eval"] != 0))
			$result = $terms[0]["eval"] / $terms[1]["eval"];
		return $result;
	}
	private static function funcCos($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false))
			$result = cos($terms[0]["eval"]);
		return $result;
	}
	private static function funcTan($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false))
			$result = tan($terms[0]["eval"]);
		return $result;
	}
	private static function funcLg($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[0]["eval"] > 0))
			$result = log($terms[0]["eval"], 10);
		return $result;
	}
	private static function funcLn($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[0]["eval"] > 0))
			$result = log($terms[0]["eval"]);
		return $result;
	}
	private static function funcLog($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false) && ($terms[0]["eval"] > 0) && ($terms[1]["eval"] > 0) && ($terms[1]["eval"] != 1))
			$result = log($terms[1]["eval"], $terms[0]["eval"]);
		return $result;
	}
	private static function funcSqrt($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[0]["eval"] > 0))
			$result = sqrt($terms[0]["eval"]);
		return $result;
	}
	private static function funcCtan($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false))
			$result = 1 / tan($terms[0]["eval"]);
		return $result;
	}
	private static function funcSin($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && ($terms[0]["eval"] !== false))
			$result = sin($terms[0]["eval"]);
		return $result;
	}
	private static function funcSub($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false))
			$result = $terms[0]["eval"] - $terms[1]["eval"];
		return $result;
	}
	private static function funcPow($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false) && ($terms[0]["eval"] > 0))
			$result = pow($terms[0]["eval"], $terms[1]["eval"]);
		return $result;
	}
	private static function funcAdd($terms, $parameters)
	{
		$result = false;
		foreach ($terms as $key => $term)
		{
			if ($term["type"] == "value")
				$terms[$key]["eval"] = $term["value"];
			if ($term["type"] == "expression")
				$terms[$key]["eval"] = $term["value"]->evaluate();
			if ($term["type"] == "parameter")
				if (in_array($term["value"], $parameters) == true)
					$terms[$key]["eval"] = $parameters[$term["value"]];
		}
		if ((isset($terms[0]["eval"]) == true) && (isset($terms[1]["eval"]) == true) && ($terms[0]["eval"] !== false) && ($terms[1]["eval"] !== false))
			$result = $terms[0]["eval"] + $terms[1]["eval"];
		return $result;
	}
	public function evaluate()
	{
		if (isset($this->functions[$this->operator]) == true)
			return call_user_func($this->functions[$this->operator]["function"], $this->terms, $this->parameters);
		else
			return false;
	}
	public function addFunction($name, $arguments, $function)
	{
		$this->functions[$name] = array(
			"name" => $name,
			"function" => $function,
			"arguments" => $arguments
		);
		foreach ($this->terms as $term)
			if ($term["type"] == "expression")
				$term["value"]->addFunction($name, $arguments, $function);
	}
	public function setParameter($name, $value)
	{
		$this->parameters[$name] = $value;
		foreach ($this->terms as $term)
			if ($term["type"] == "expression")
				$term["value"]->setParameter($name, $value);
	}
	public function setOperator($operator)
	{
		$this->operator = $operator;
	}
	public function addExpression($expression)
	{
		$this->terms[] = array(
			"type" => "expression",
			"value" => $expression,
			"eval" => null
		);
	}
	public function addValue($value)
	{
		$this->terms[] = array(
			"type" => "value",
			"value" => $value,
			"eval" => null
		);
	}
	public function addParameter($name)
	{
		$this->terms[] = array(
			"type" => "parameter",
			"value" => $name,
			"eval" => null
		);
	}
	public function __construct()
	{
		//add basic functions
		$this->terms = array();
		$this->parameters = array();
		$this->functions = array();
	}
};
class CMParser
{
	private $operations;
	private $expression;
	private $error;
	private $position;
	private $functions;
	private $parameters;
	private $tree;
	private $length;
	private function isEnd($p)
	{
		if ($p >= $this->length)
			return true;
		else
			return false;
	}
	private function peekOperator()
	{
		$operation = $this->expression[$this->position];
		$operations = array('+', '-', '*', '/', '^');
		if (in_array($operation, $operations) == false)
		{
			$operation = 0;
			$this->setError("invalid operator peek");
		}
		return $operation;
	}
	private function getOperator()
	{
		$operation = $this->expression[$this->position];
		$operations = array('+', '-', '*', '/', '^');
		if (in_array($operation, $operations) == false)
		{
			$operation = 0;
			$this->setError("invalid operator");
		}
		$this->position++;
		return $operation;
	}
	private function operatorPriority($operator)
	{
		$priorities = array(
			'+' => 1,
			'-' => 1,
			'*' => 2,
			'/' => 2,
			'^' => 3
		);
		if (isset($priorities[$operator]) == false)
		{
			$this->setError("unknown operator priority: " . $operator);
			$priority = 0;
		}
		else
			$priority = $priorities[$operator];
		return $priority;
	}
	public function evaluate()
	{
		if ((count($this->getError()) == 0) && ($this->tree != null))
		{
			foreach ($this->functions as $function)
				$this->tree->addFunction($function["name"], $function["parameters"], $function["function"]);
			foreach ($this->parameters as $parameter => $value)
				$this->tree->setParameter($parameter, $value);
			$result = $this->tree->evaluate();
			if ($result === false)
				$this->setError("evaluation error");
			if (count($this->getError()) == 0)
				return $result;
			else
				return false;
		}
		else
			return false;
	}
	private function isAlNum($char)
	{
		if (($this->isDigit($char) == true) || (($char >= 'a') && ($char <= 'z')) || (($char >= 'A') && ($char <= 'Z')))
			return true;
		else
			return false;
	}
	private function isDigit($char)
	{
		if (($char >= '0') && ($char <= '9'))
			return true;
		else
			return false;
	}
	private function parseTerm()
	{
		if ($this->isEnd($this->position) == true)
		{
			$this->setError("invalid term - reached the end of the expression");
			return null;
		}
		if ($this->expression[$this->position] == '(')
		{
			$this->position++;
			return $this->parse(null);
		}
		$result = null;
		if (($this->expression[$this->position] == '-') || ($this->expression[$this->position] == '+') || ($this->isDigit($this->expression[$this->position]) == true))
		{
			$negative = false;
			$number = 0.0;
			if ($this->expression[$this->position] == '-')
			{
				$negative = true;
				$this->position++;
			}
			if ($this->expression[$this->position] == '+')
				$this->position++;
			while (($this->isEnd($this->position) == false) && ($this->isDigit($this->expression[$this->position]) == true))
			{
				$number = $number * 10 + (int)$this->expression[$this->position];
				$this->position++;
			}
			if (($this->isEnd($this->position) == false) && ($this->expression[$this->position] == '.'))
			{
				$floatingPoint = 1;
				$this->position++; //Skip the floating point
				while (($this->isEnd($this->position) == false) && ($this->isDigit($this->expression[$this->position]) == true))
				{
					$floatingPoint /= 10;
					$number = $number + $floatingPoint * (int)$this->expression[$this->position];
					$this->position++;
				}
			}
			if ($negative == true)
				$number *= -1;
			$result = new CExpression();
			$result->addValue($number);
			$result->setOperator("value");
		}
		else if ($this->isAlNum($this->expression[$this->position]) == true)
		{
			$endName = $this->position;
			while (($this->isEnd($endName) == false) && ($this->isAlNum($this->expression[$endName]) == true))
				$endName++;
			if (($this->isEnd($endName) == false) && ($this->expression[$endName] == '('))
			{
				$functionName = "";
				for (; $this->position < $endName; $this->position++)
					$functionName .= $this->expression[$this->position];
				if (isset($this->functions[$functionName]) == true)
				{
					$this->position++; //Skip the opening braket
					$result = new CExpression();
					$result->setOperator($functionName);
					for ($i = 0; $i < $this->functions[$functionName]["parameters"]; $i++)
						$result->addExpression($this->parse(null));
					if (($this->functions[$functionName]["parameters"] == 0) && (($this->isEnd($this->position) == true) || ($this->expression[$this->position] != ')')))
						$this->setError("missing closing bracket");
					if (($this->functions[$functionName]["parameters"] > 0) && (($this->isEnd($this->position - 1) == true) || ($this->expression[$this->position - 1] != ')')))
						$this->setError("missing closing bracket");
				}
				else
					$this->setError("unknown function: " . $functionName);
			}
			else
			{
				$parameterName = "";
				for (; $this->position < $endName; $this->position++)
					$parameterName .= $this->expression[$this->position];
				$result = new CExpression();
				$result->addParameter($parameterName);
				$result->setOperator("value");
			}
		}
		return $result;
	}
	private function parse($oldTermOne)
	{
		$operator = null;
		$nextOperator = null;
		$termOne = null;
		$termTwo = null;
		$result = null;
		if ($oldTermOne == null)
			$termOne = $this->parseTerm();
		else
			$termOne = $oldTermOne;
		if (($this->isEnd($this->position) == false) && ($this->expression[$this->position] != ')') && ($this->expression[$this->position] != ','))
		{
			$operator = $this->getOperator();
			$termTwo = $this->parseTerm();
			if (($this->isEnd($this->position) == false) && ($this->expression[$this->position] != ')') && ($this->expression[$this->position] != ','))
			{
				$nextOperator = $this->peekOperator();
				//Exponentiation is right associative so it is an excepted case
				if (($this->operatorPriority($nextOperator) > $this->operatorPriority($operator)) || ($nextOperator == '^'))
				{
					$result = new CExpression();
					$result->addExpression($termOne);
					$result->addExpression($this->parse($termTwo));
					$result->setOperator($operator);
					return $result;
				}
				else
				{
					$result = new CExpression();
					$result->addExpression($termOne);
					$result->addExpression($termTwo);
					$result->setOperator($operator);
					return $this->parse($result);
				}
			}
			else
			{
				if ($this->isEnd($this->position) == false)
					$this->position++;
				$result = new CExpression();
				$result->addExpression($termOne);
				$result->addExpression($termTwo);
				$result->setOperator($operator);
				return $result;
			}
		}
		else
		{
			$this->position++;
			return $termOne;
		}
	}
	public function setParameter($name, $value)
	{
		$this->parameters[$name] = $value;
	}
	public function addFunction($name, $function, $parameters)
	{
		$this->operations[$name] = $name;
		$this->functions[$name] = array(
			"name" => $name,
			"function" => $function,
			"parameters" => $parameters
		);
	}
	private function initFunctions()
	{
		$this->addFunction("value", "CExpression::funcValue", 1);
		$this->addFunction("+", "CExpression::funcAdd", 2);
		$this->addFunction("-", "CExpression::funcSub", 2);
		$this->addFunction("*", "CExpression::funcMul", 2);
		$this->addFunction("/", "CExpression::funcDiv", 2);
		$this->addFunction("^", "CExpression::funcPow", 2);
		$this->addFunction("sin", "CExpression::funcSin", 1);
		$this->addFunction("cos", "CExpression::funcCos", 1);
		$this->addFunction("tan", "CExpression::funcTan", 1);
		$this->addFunction("ctan", "CExpression::funcCtan", 1);
		$this->addFunction("sqrt", "CExpression::funcSqrt", 1);
		$this->addFunction("log", "CExpression::funcLog", 2);
		$this->addFunction("ln", "CExpression::funcLn", 1);
		$this->addFunction("lg", "CExpression::funcLg", 1);
	}
	private function initConstants()
	{
		$this->setParameter("pi", pi());
		$this->setParameter("e", 2.71828183);
	}
	public function load($string)
	{
		$whitespace = array(" ", "\n", "\t", "\r");
		$this->expression = str_replace($whitespace, "", $string);
		$this->length = strlen($this->expression);
		$this->error = array();
		$this->position = 0;
		$this->functions = array();
		$this->parameters = array();
		$this->initFunctions();
		$this->initConstants();
		$this->tree = $this->parse(null);
	}
	public function setError($message)
	{
		echo $this->position . " " . $message . "<br />";
		$this->error[] = array(
			"message" => $message,
			"position" => $this->position
		);
		$this->position = $this->length;
	}
	public function getError()
	{
		return $this->error;
	}
	public function isError()
	{
		if (count($this->error) > 0)
			return true;
		else
			return false;
	}
};
?>
