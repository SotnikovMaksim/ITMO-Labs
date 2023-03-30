#include <cstdio>
#include <vector>
#include <algorithm>
#include <set>

std::vector< std::vector< int > > childs;
std::vector< bool > visited;
std::vector< int > tin;
std::vector< int > rev;
std::vector< int > color;
std::set< std::pair< int, int > > bridges;
int n, m, u, v;

void dfs(int& t, const int& v, const int& p)
{
	visited[v] = true;

	tin[v] = t;
	rev[v] = t;
	t++;

	for (const int& child: childs[v])
	{
		if (child == p) { continue; }

		if (visited[child])
		{
			rev[v] = std::min(rev[v], tin[child]);
		} else
		{
			dfs(t, child, v);

			rev[v] = std::min(rev[v], rev[child]);

			if ((rev[child] > tin[v]) && (std::count(childs[v].begin(), childs[v].end(), child) == 1))
			{
				bridges.insert({v, child});
				bridges.insert({child, v});
			}
		}
	}
}

void dfs2(const int& x, const int& c)
{
	color[x] = c;

	for (const int& child: childs[x])
	{
		if ((bridges.find(std::pair< int, int >(x, child)) == bridges.end()) && color[child] == -1)
		{
			dfs2(child, c);
		}
	}
}

int main()
{
	int t = 0;
	scanf("%i %i", &n, &m);
	visited = std::vector< bool >(n + 1);
	tin = std::vector< int >(n + 1);
	rev = std::vector< int >(n + 1);
	color = std::vector< int >(n + 1, -1);
	bridges = std::set< std::pair< int, int > >();
	childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());;

	for (int i = 0; i < m; i++)
	{
		scanf("%i %i", &u, &v);

		childs[u].push_back(v);
		childs[v].push_back(u);
	}

	for (int i = 1; i < n + 1; i++)
	{
		if (!visited[i])
		{
			dfs(t, i, i);
		}
	}

	int current_color = 1;

	for (int i = 1; i < n + 1; i++)
	{
		if (color[i] == -1)
		{
			dfs2(i, current_color++);
		}
	}

	printf("%i\n", current_color - 1);

	for (int i = 1; i < n + 1; i++)
	{
		printf("%i ", color[i]);
	}

}
