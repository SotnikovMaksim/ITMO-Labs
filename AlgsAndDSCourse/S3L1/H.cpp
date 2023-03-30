#include <cstdio>
#include <vector>
#include <algorithm>

std::vector< std::vector< int > > w;
std::vector< std::vector< int > > childs;
std::vector< std::vector< int > > parents;
std::vector< int > color;
std::vector< bool > visited;
std::vector< int > revOrder;
int n, weight, u, v, ind;

void dfs(const int& v)
{
	visited[v] = true;

	for (const int& child: childs[v])
	{
		if (!visited[child])
		{
			dfs(child);
		}
	}

	revOrder[ind--] = v;
}

void dfsRev(const int& v, const int& c)
{
	color[v] = c;

	for (const int& parent: parents[v])
	{
		if (color[parent] == -1)
		{
			dfsRev(parent, c);
		}
	}
}

void kasaraja(const int& m)
{
	color = std::vector< int >(n + 1, -1);
	childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());
	parents = std::vector< std::vector< int > >(n + 1, std::vector< int >());
	visited = std::vector< bool >(n + 1, false);
	revOrder = std::vector< int >(n + 1);
	ind = n;

	for (int u = 1; u < n + 1; u++)
	{
		for (int v = 1; v < n + 1; v++)
		{
			if (w[u][v] <= m)
			{
				childs[u].push_back(v);
				parents[v].push_back(u);
			}
		}
	}

	for (int i = 1; i < n + 1; i++)
	{
		if (!visited[i])
		{
			dfs(i);
		}
	}

	for (const int& i: revOrder)
	{
		if (color[i] == -1)
		{
			dfsRev(i, i);
		}
	}
}

int main()
{
	scanf("%i", &n);

	w = std::vector< std::vector< int > >(n + 1, std::vector< int >(n + 1, 0));
	int l = 0, r;

	for (int i = 1; i < n + 1; i++)
	{
		for (int j = 1; j < n + 1; j++)
		{
			scanf("%i", &weight);

			w[i][j] = weight;

			r = std::max(r, weight);
		}
	}

	while (l < r)
	{
		int mid = (l + r) / 2;
		kasaraja(mid);

		bool flag = true;

		for (int i = 2; i < n + 1; i++)
		{
			if (color[i] != color[i - 1])
			{
				flag = false;
				break;
			}
		}

		if (flag)
		{
			r = mid;
		} else
		{
			l = mid + 1;
		}
	}

	printf("%i", r);
}
