#include <cstdio>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

struct pair_hash
{
	template<class T1, class T2>
	std::size_t operator()(const std::pair<T1, T2> &pair) const
	{
		return std::hash<T1>()(pair.first) ^ std::hash<T2>()(pair.second);
	}
};

std::vector< std::vector<int> > childs;
std::vector<bool> visited;
std::vector<int> tin;
std::vector<int> rev;
std::vector<int> color;
std::map<std::pair<int, int>, std::vector<int>> edge_index;
int current_color;
int n, m, l, r;

void dfs(int &t, const int &v, const int &p)
{
	visited[v] = true;

	tin[v] = t;
	rev[v] = t;
	t++;

	for (const int &child: childs[v])
	{
		if (child == p)
		{ continue; }

		if (visited[child])
		{
			rev[v] = std::min(rev[v], tin[child]);
		} else
		{
			dfs(t, child, v);

			rev[v] = std::min(rev[v], rev[child]);
		}
	}
}

void set_all(std::vector<int> &indexes, const int &c)
{
	for (const auto &index: indexes)
	{
		if (color[index] == -1)
		{
			color[index] = c;
		}
	}
}

void dfs2(const int &x, int c, const int &from)
{
	visited[x] = true;

	for (const int &child: childs[x])
	{
		if (child == from)
		{
			continue;
		}

		auto ind = edge_index[{x, child}];

		if (!visited[child])
		{
			if (rev[child] >= tin[x])
			{
				int newC = ++current_color;
				set_all(ind, newC);
				dfs2(child, newC, x);
			} else
			{
				set_all(ind, c);
				dfs2(child, c, x);
			}
		} else if (tin[child] < tin[x])
		{
			if (color[ind[0]] == -1)
			{
				set_all(ind, c);
			}
		}
	}
}

int main()
{
	int t = 0;
	scanf("%i %i", &n, &m);
	visited = std::vector<bool>(n + 1);
	tin = std::vector<int>(n + 1);
	rev = std::vector<int>(n + 1);
	color = std::vector<int>(m, -1);
	childs = std::vector<std::vector<int> >(n + 1, std::vector< int >());

	for (int i = 0; i < m; i++)
	{
		scanf("%i %i", &l, &r);

		childs[l].push_back(r);
		childs[r].push_back(l);

		edge_index[{l, r}].push_back(i);
		edge_index[{r, l}].push_back(i);
	}

	for (int i = 1; i < n + 1; i++)
	{
		if (!visited[i])
		{
			dfs(t, i, -1);
		}
	}

	printf("\n");

	current_color = 0;
	visited = std::vector<bool>(n + 1, false);

	for (int i = 1; i < n + 1; i++)
	{
		if (!visited[i] && !childs[i].empty())
		{
			dfs2(i, current_color, -1);
		}
	}

	printf("%lu\n", std::set<int>(color.begin(), color.end()).size());

	for (int i = 0; i < m; i++)
	{
		printf("%i ", color[i]);
	}
}
