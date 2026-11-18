#pragma once

#include <deque>
#include <string>

#include "Pin.h"

class Node {
protected:
	bool grayed = false;
public:
	std::string name;
	// Use deque to make sure pointers to Pins stay valid
	std::deque<Pin> inputs;
	std::deque<Pin> outputs;
	float widthName = 0.0f;
	float maxWidthInput = 0.0f;
	float maxWidthOutput = 0.0f;
	int id;
	bool dirty;
	int pinidgen;
	Node(const char *n, int id);
	virtual ~Node() = default;
	void AddInput(const char *n, Pin::type type);
	Pin *AddOutput(const char *n, Pin::type type);
	void ComputeLayout();
	void UpdateInputs();
	virtual void Draw(std::function<bool(const Pin *)> filter);
	virtual void Update(bool dryrun) = 0;
};
