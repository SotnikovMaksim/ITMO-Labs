#include <cmath>
#include <cstdio>
#include <cstdlib>
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
char operation;
int from, to, m, l, r, v;
bool* wasThere1;
bool* wasThere2;

class ST
{
	int size;
	long long* a;

  public:
	ST(int s)
	{
		size = s;
		a = new long long[s * 2 - 1]();
	}

//	~ST() = default;

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

void lca(int u, int state, long long value, ST& st)
{
	if (nodes[u].pathTop == nodes[state].pathTop)
	{
		st.addImpl(std::min(tIn[state], tIn[u]), std::max(tIn[state], tIn[u]) + 1, value, 0, treeSize, 0);
		return;
	}
	int top;
	while (nodes[u].pathTop != nodes[state].pathTop)
	{
		if (nodes[nodes[u].pathTop].depth > nodes[nodes[state].pathTop].depth)
		{
			top = nodes[u].pathTop;
			st.addImpl(std::min(tIn[u], tIn[top]), std::max(tIn[u], tIn[top]) + 1, value, 0, treeSize, 0);
			u = nodes[top].parent;
		}
		else
		{
			top = nodes[state].pathTop;
			st.addImpl(std::min(tIn[state], tIn[top]), std::max(tIn[state], tIn[top]) + 1, value, 0, treeSize, 0);
			state = nodes[top].parent;
		}
	}
	st.addImpl(std::min(tIn[state], tIn[u]), std::max(tIn[state], tIn[u]) + 1, value, 0, treeSize, 0);
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

int main(int argc, const char* argv[])
{
//	FILE* input = fopen(argv[0], "r");
	scanf("%i", &n);
//	fscanf(input, "%i", &n);
	treeSize = (int)std::pow(2, (std::ceil(std::log2(n))));
	nodes = (Node*) new Node[n + 1]();
	wasThere1 = new bool[n + 1]();
	wasThere2 = new bool[n + 1]();
	tIn = new int[n + 1]();
	tInId = 0;
	ST st = ST(treeSize);

	for (int i = 0; i < n - 1; i++)
	{
		scanf("%i %i", &from, &to);
//		fscanf(input, "%i %i", &from, &to);
		nodes[from].childs.insert(to);
		nodes[to].childs.insert(from);
	}
	makeTree(0, 1);

	pathTopDfs(1, 1);

	scanf("%i", &m);
//	fscanf(input, "%i", &m);
	for (int i = 0; i < m; i++)
	{
		scanf(" %c", &operation);
//		fscanf(input, " %c", &operation);
		if (operation == '+')
		{
			scanf("%i %i %i", &l, &r, &v);
//			fscanf(input, "%i %i %i", &l, &r, &v);
			lca(l, r, v, st);
		}
		else
		{
			scanf("%i", &v);
//			fscanf(input, "%i", &v);
			printf("%lli\n", st.getImpl(tIn[v], 0, treeSize, 0));
		}
	}
//	fclose(input);
	delete[] nodes;
	delete[] wasThere1;
	delete[] wasThere2;
	delete[] tIn;
	return 0;
}
