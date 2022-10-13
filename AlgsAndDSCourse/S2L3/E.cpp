#include <cmath>
#include <cstdio>
#include <iostream>
#include <set>

struct Node
{
	std::set< int > childs;
	int parent = 1;
	int size = 1;
	int heavy = 0;
	int pathTop = 0;
	int depth = 0;

	Node() = default;
};

int tInId;
int* tIn;
Node* nodes;
int n;
int treeSize;
int from, to, m, u, v;
bool* wasThere1;
bool* wasThere2;

class ST
{
	int size;
	int elementsCount;
	int actual;
	long long* a;

  public:
	ST(int s, int ec)
	{
		size = s;
		actual = s * 2 - 1;
		elementsCount = ec;
		a = new long long[actual]();
	}

	void addImpl(int left, int right, const long long state, int lx, int rx, int x)
	{
		if (right <= lx || rx <= left)
		{
			return;
		}
		if (left <= lx && rx <= right)
		{
			if (rx - lx != 1)
			{
				propagate(x);
			}
			a[x] += state;
			return;
		}
		propagate(x);
		int mid = (lx + rx + 1) / 2;
		addImpl(left, right, state, lx, mid, 2 * x + 1);
		addImpl(left, right, state, mid, rx, 2 * x + 2);
		a[x] = 0;
	}

	void propagate(const int x)
	{
		if (a[x] != 0)
		{
			a[2 * x + 1] += a[x];
			a[2 * x + 2] += a[x];
			a[x] = 0;
		}
	}

	void propagateAll(const int x)
	{
		if (2 * x + 1 < actual)
		{
			propagate(x);
			propagateAll(2 * x + 1);
		}
		if (2 * x + 2 < actual)
		{
			propagate(x);	 // зачем? не знаю
			propagateAll(2 * x + 2);
		}
	}

	long long getImpl(const int i, int lx, int rx, int x)
	{
		if (rx - lx == 1)
		{
			return a[x];
		}
		propagate(x);
		int mid = (lx + rx + 1) / 2;
		if (i < mid)
		{
			return getImpl(i, lx, mid, 2 * x + 1);
		}
		else
		{
			return getImpl(i, mid, rx, 2 * x + 2);
		}
	}

	int zeroCount()
	{
		propagateAll(0);
		int counter = 0;
		for (int i = size - 1; i < size - 1 + elementsCount; i++)
		{
			if (a[i] == 0)
			{
				counter++;
			}
		}
		return counter;
	}

//	DEBUG FUNCTION
	void dump()
	{
		propagateAll(0);
		for (int i = size - 1; i < size - 1 + elementsCount; i++)
		{
			printf("%lli ", a[i]);
		}
		printf("\n");
	}
};

void pathTopDfs(int node, int topId)
{
	if (wasThere2[node])
	{
		return;
	}
	wasThere2[node] = true;

	tIn[node] = tInId;
	tInId += 1;

	nodes[node].pathTop = topId;
	if (!nodes[node].childs.empty())
	{
		pathTopDfs(nodes[node].heavy, topId);
	}
	for (auto child : nodes[node].childs)
	{
		if (child == nodes[node].heavy)
		{
			continue;
		}
		pathTopDfs(child, child);
	}
}

void lca(int left, int right, ST& st)
{
	if (nodes[left].pathTop == nodes[right].pathTop)
	{
		if (tIn[right] == tIn[left] && tIn[right] != 0)
		{
			st.addImpl(tIn[right] - 1, tIn[right], 1, 0, treeSize, 0);
		}
		else
		{
			st.addImpl(std::min(tIn[right], tIn[left]), std::max(tIn[right], tIn[left]), 1, 0, treeSize, 0);
		}
		return;
	}
	int top;
	while (nodes[left].pathTop != nodes[right].pathTop)
	{
		if (nodes[nodes[left].pathTop].depth > nodes[nodes[right].pathTop].depth)
		{
			top = nodes[left].pathTop;
			if (tIn[left] == tIn[top] && tIn[top] != 0)
			{
				st.addImpl(tIn[top] - 1, tIn[top], 1, 0, treeSize, 0);
			}
			else
			{
				st.addImpl(std::min(tIn[top], tIn[left]) - 1, std::max(tIn[top], tIn[left]), 1, 0, treeSize, 0);
			}
			left = nodes[top].parent;
		}
		else
		{
			top = nodes[right].pathTop;
			if (tIn[right] == tIn[top] && tIn[top] != 0)
			{
				st.addImpl(tIn[top] - 1, tIn[top], 1, 0, treeSize, 0);
			}
			else
			{
				st.addImpl(std::min(tIn[top], tIn[right]) - 1, std::max(tIn[top], tIn[right]), 1, 0, treeSize, 0);
			}
			right = nodes[top].parent;
		}
	}
	if (tIn[right] != tIn[left])
	{
		st.addImpl(std::min(tIn[right], tIn[left]), std::max(tIn[right], tIn[left]), 1, 0, treeSize, 0);
	}
}

void makeTree(int parent, int node)
{
	if (wasThere1[node])
	{
		return;
	}
	wasThere1[node] = true;
	if (parent != 0)
	{
		nodes[node].parent = parent;
		nodes[node].childs.erase(parent);
		nodes[node].depth = nodes[parent].depth + 1;
	}

	int size = 0;
	for (auto child : nodes[node].childs)
	{
		makeTree(node, child);
		nodes[node].size += nodes[child].size;
		if (nodes[child].size > size)
		{
			size = nodes[child].size;
			nodes[node].heavy = child;
		}
	}
}

int main()
{
	scanf("%i", &n);
	treeSize = (int)std::pow(2, (std::ceil(std::log2(n - 1))));
	nodes = (Node*)new Node[n + 1]();
	wasThere1 = new bool[n + 1]();
	wasThere2 = new bool[n + 1]();
	tIn = new int[n + 1]();
	tInId = 0;
	ST st = ST(treeSize, n - 1);

	for (int i = 0; i < n - 1; i++)
	{
		scanf("%i %i", &from, &to);
		nodes[from].childs.insert(to);
		nodes[to].childs.insert(from);
	}
	makeTree(0, 1);

	pathTopDfs(1, 1);

	scanf("%i", &m);

	for (int i = 0; i < m; i++)
	{
		scanf(" %i %i", &u, &v);
		lca(u, v, st);
	}

	printf("%i", st.zeroCount());
	delete[] nodes;
	delete[] wasThere1;
	delete[] wasThere2;
	delete[] tIn;
	return 0;
}