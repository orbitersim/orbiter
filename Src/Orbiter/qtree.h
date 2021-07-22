// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __QTREE_H
#define __QTREE_H

template<typename T>
class QuadTreeNode {
public:
	QuadTreeNode (QuadTreeNode<T> *_parent = 0, T *_entry = 0);
	
	~QuadTreeNode ();
	// Deleting a node

	inline T *Entry() { return entry; }
	// Returns the node contents

	inline void SetEntry (T *newentry) { entry = newentry; entry->SetNode (this); }
	// Transfers ownership of the entry to the tree

	inline QuadTreeNode *Parent() { return parent; }
	// Returns the node's parent, or 0 if node is root

	QuadTreeNode *Ancestor(int dlvl);
	// Returns an ancestor node from the tree. dlvl specifies how many nodes to step back
	// Ancestor(1) is equivalent to Parent()
	// If the ancestor does not exist (e.g. trying to step beyond tree root), returns 0

	inline QuadTreeNode *Child (int idx) { return child[idx]; }
	// Returns the node's idx-th child (0<=idx<4), or 0 if child doesn't exist

	QuadTreeNode<T> *AddChild (int idx, T *childentry);
	// If child idx already exists, it and its entire subtree are deleted

	bool DelChild (int idx);
	// Delete a child and its subtree. False indicates that a node in the subtree
	// was locked and not the entire subtree could be deleted.

	bool DelChildren ();
	// Delete all children and their subtrees. False indicates that a node in the subtree
	// was locked and not the entire subtree could be deleted.

private:
	T *entry;
	QuadTreeNode *parent;
	QuadTreeNode *child[4];
};

template<typename T>
QuadTreeNode<T>::QuadTreeNode (QuadTreeNode<T> *_parent, T *_entry):
parent(_parent), entry(_entry)
{
	for (int i = 0; i < 4; i++)
		child[i] = 0;
	if (entry) entry->SetNode (this);
}

template<typename T>
QuadTreeNode<T>::~QuadTreeNode ()
{
	if (entry) delete entry;
	//for (int i = 0; i < 4; i++)
	//	if (child[i]) delete child[i];
}

template<typename T>
QuadTreeNode<T> *QuadTreeNode<T>::Ancestor(int dlvl)
{
	QuadTreeNode<T> *ancestor = this;
	for (int i = 0; i < dlvl && ancestor; i++)
		ancestor = ancestor->parent;
	return ancestor;
}

template<typename T>
QuadTreeNode<T> *QuadTreeNode<T>::AddChild (int idx, T *childentry)
{
	if (child[idx]) delete child[idx];
	child[idx] = new QuadTreeNode<T> (this, childentry);
	return child[idx];
}

template<typename T>
bool QuadTreeNode<T>::DelChild (int idx)
{
	bool ok = true;
	if (child[idx]) {
		if (child[idx]->DelChildren() && child[idx]->entry->PreDelete()) {
			delete child[idx];
			child[idx] = 0;
		} else
			ok = false;
	}
	return ok;
}

template<typename T>
bool QuadTreeNode<T>::DelChildren ()
{
	// recursively delete the child trees extending from the node
	bool ok = true;
	for (int i = 0; i < 4; i++)
		if (child[i]) {
			if (child[i]->DelChildren() && child[i]->Entry()->PreDelete()) {
				delete child[i];
				child[i] = 0;
			} else
				ok = false;
		}
	return ok;
}

#endif // !__QTREE_H