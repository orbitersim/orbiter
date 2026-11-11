#pragma once

#include <vector>
#include <string>

#include "Pin.h"

class Node {
public:
	std::string name;
	std::vector<Pin> inputs;
	std::vector<Pin> outputs;
	float widthName = 0.0f;
	float maxWidthInput = 0.0f;
	float maxWidthOutput = 0.0f;
	int id;
	bool dirty;
	int pinidgen;
	Node(const char *n, int id);
	virtual ~Node() = default;
	void AddInput(const char *n, Pin::type type);
	void AddOutput(const char *n, Pin::type type);
	void ComputeLayout();
	void Draw(std::function<bool(const Pin *)> filter);
};
