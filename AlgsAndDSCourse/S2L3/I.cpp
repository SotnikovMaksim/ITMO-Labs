#include <unordered_set>

#include <cstdio>

struct Node
{
	std::unordered_set< int > childs;
	int size;

	Node() = default;
};

int n, u, v;
Node* nodes;
bool *wasThere, *centr, *searched;
int* parent;

void sizeDFS(int node)
{
	if (wasThere[node] || centr[node])
	{
		return;
	}
	wasThere[node] = true;
	searched[node] = false;

	int s = 0;
	for (auto child : nodes[node].childs)
	{
		if (!wasThere[child] && !centr[child])
		{
			sizeDFS(child);
			s += nodes[child].size;
		}
	}
	nodes[node].size = s + 1;
	wasThere[node] = false;
}

int findCentroid(int node, int size)
{
	searched[node] = true;
	for (const int& child : nodes[node].childs)
	{
		if (nodes[child].size > size / 2 && !searched[child])
		{
			return findCentroid(child, size);
		}
	}

	centr[node] = true;
	for (const int& child : nodes[node].childs)
	{
		if (!centr[child])
		{
			sizeDFS(child);
			parent[findCentroid(child, nodes[child].size)] = node;
		}
	}
	return node;
}

int main()
{
	scanf("%i", &n);
	nodes = (Node*)new Node[n + 1]();
	wasThere = new bool[n + 1]();
	searched = new bool[n + 1]();
	centr = new bool[n + 1]();
	parent = new int[n + 1]();

	for (int i = 0; i < n - 1; i++)
	{
		scanf("%i %i", &u, &v);
		nodes[u].childs.insert(v);
		nodes[v].childs.insert(u);
	}

	sizeDFS(1);

	delete[] wasThere;
	wasThere = new bool[n + 1]();

	findCentroid(1, n);

	for (int i = 1; i < n + 1; i++)
	{
		printf("%i ", parent[i]);
	}

	delete[] wasThere;
	delete[] searched;
	delete[] parent;
	delete[] centr;
	delete[] nodes;
	return 0;
}
