#include <cstdio>
#include <vector>
#include <algorithm>
#include <set>

std::vector< std::vector< int > > childs;
std::vector< std::vector< int > > parents;
std::vector< int > color;
std::vector< bool > visited;
std::vector< int > revOrder;
std::set< std::pair< int, int > > edges;
std::set< int > comps;
int n, m, u, v, ind;

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

void dfs3(const int& x, const int& pred)
{
	if (color[x] != pred)
	{
		edges.insert({pred, color[x]});
		return;
	}
	visited[x] = true;

	for (int& child: childs[x])
	{
		if (!visited[child])
		{
			dfs3(child, pred);
		}
	}
}

int main()
{
    scanf("%i %i", &n, &m);

    color = std::vector< int >(n + 1, -1);
	childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());
	parents = std::vector< std::vector< int > >(n + 1, std::vector< int >());
    visited = std::vector< bool >(n + 1, false);
	edges = std::set< std::pair< int, int > >();
	comps = std::set< int >();
	revOrder = std::vector< int >(n + 1);
    ind = n;

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);

        childs[u].push_back(v);
        parents[v].push_back(u);
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

	for (int i = 1; i < n + 1; i++)
	{
		comps.insert(color[i]);
	}


	for (const int& pred: comps)
	{
		std::fill(visited.begin(), visited.end(), false);
		dfs3(pred, pred);
	}

	printf("%lu", edges.size());

}
