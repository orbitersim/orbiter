// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// qtree.h
// Quad-tree framework for planet render engines
// ==============================================================

#ifndef __QTREE_H
#define __QTREE_H

template<typename T>
class QuadTreeNode {
public:
	QuadTreeNode (QuadTreeNode<T> *_parent = NULL, T *_entry = NULL);

	~QuadTreeNode ();
	// Deleting a node

	T *Entry() const { return entry; }
	//const T *Entry() const { return entry; }
	// Returns the node contents

	inline void SetEntry (T *newentry) { entry = newentry; entry->SetNode (this); }
	// Transfers ownership of the entry to the tree

	inline QuadTreeNode *Parent() const { return parent; }
	// Returns the node's parent, or 0 if node is root

	QuadTreeNode *Ancestor(int dlvl);
	// Returns an ancestor node from the tree. dlvl specifies how many nodes to step back
	// Ancestor(1) is equivalent to Parent()
	// If the ancestor does not exist (e.g. trying to step beyond tree root), returns 0

	inline QuadTreeNode *Child (int idx) { _ASSERT(idx < 4); return child[idx]; }
	inline const QuadTreeNode *Child (int idx) const { _ASSERT(idx < 4); return child[idx]; }
	// Returns the node's idx-th child (0<=idx<4), or 0 if child doesn't exist

	QuadTreeNode<T> *AddChild (int idx, T *childentry);
	// If child idx already exists, it and its entire subtree are deleted

	bool DelChild (int idx);
	// Delete a child and its subtree. False indicates that a node in the subtree
	// was locked and not the entire subtree could be deleted.

	bool DelChildren ();
	// Delete all children and their subtrees. False indicates that a node in the subtree
	// was locked and not the entire subtree could be deleted.

	void DelAbove(int lvl);

private:
	T *entry;
	QuadTreeNode *parent;
	QuadTreeNode *child[4];
};

template<typename T>
QuadTreeNode<T>::QuadTreeNode (QuadTreeNode<T> *_parent, T *_entry):
parent(_parent), entry(_entry)
{
	for (int i = 0; i < 4; ++i) {
		child[i] = NULL;
	}
	if (entry) {
		entry->SetNode (this);
	}
}

template<typename T>
QuadTreeNode<T>::~QuadTreeNode ()
{
	if (parent) { parent = NULL; }
	for (int i = 0; i < 4; ++i) {
		if (child[i]) {
			delete child[i];
		}
	}
	if (entry) {
		delete entry;
	}
}

template<typename T>
QuadTreeNode<T> *QuadTreeNode<T>::Ancestor(int dlvl)
{
	QuadTreeNode<T> *ancestor = this;
	for (int i = 0; i < dlvl && ancestor; ++i) {
		ancestor = ancestor->parent;
	}
	return ancestor;
}

template<typename T>
QuadTreeNode<T> *QuadTreeNode<T>::AddChild (int idx, T *childentry)
{
	_ASSERT(idx < 4);
	if (child[idx]) {
		delete child[idx];
	}
	child[idx] = new QuadTreeNode<T> (this, childentry);
	return child[idx];
}

template<typename T>
bool QuadTreeNode<T>::DelChild (int idx)
{
	_ASSERT(idx < 4);
	bool ok = true;
	if (child[idx]) {
		if (child[idx]->DelChildren() && child[idx]->entry->PreDelete()) {
			delete child[idx];
			child[idx] = NULL;
		} else {
			ok = false;
		}
	}
	return ok;
}

template<typename T>
bool QuadTreeNode<T>::DelChildren ()
{
	// recursively delete the child trees extending from the node
	bool ok = true;
	for (int i = 0; i < 4; ++i) {
		if (child[i]) {
			if (child[i]->DelChildren() && child[i]->Entry()->PreDelete()) {
				delete child[i];
				child[i] = NULL;
			} else {
				ok = false;
			}
		}
	}
	return ok;
}

template<typename T>
void QuadTreeNode<T>::DelAbove(int lvl)
{
	for (auto c : child) {
		if (c) {
			if (c->entry && c->entry->Level() >= lvl) {
				c->DelChildren();
				continue;		
			}
			c->DelAbove(lvl);
		}
	}
}

#endif // !__QTREE_H
