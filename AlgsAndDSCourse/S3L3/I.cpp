#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>

int n, m, x, y;
std::vector< int > g;
std::vector< bool > visited;
std::vector< std::set< int > > childs;
std::vector< std::set< int > > parents;

int mex(const std::vector< int >& of)
{
    int s = *std::max_element(of.begin(), of.end());
    std::vector< bool > was = std::vector< bool >(s + 1, false);

    for (const auto& item: of)
    {
        was[item] = true;
    }

    for (int i = 0; i < s + 1; i++) {
        if (!was[i])
        {
            return i;
        }
    }

    return s + 1;
}

void dfs(const int& cur)
{
    visited[cur] = true;

    if (childs[cur].empty())
    {
        g[cur] = 0;
        return;
    }

    std::vector< int > of;

    for (const auto& child: childs[cur])
    {
        if (!visited[child])
        {
            dfs(child);
        }

        of.push_back(g[child]);
    }

    g[cur] = mex(of);
}

int main()
{
    scanf("%i %i", &n, &m);
    g = std::vector< int >(n + 1);
    visited = std::vector< bool >(n + 1, false);
    childs = std::vector< std::set< int > >(n + 1, std::set< int >());
    parents = std::vector< std::set< int > >(n + 1, std::set< int >());

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &x, &y);

        childs[x].insert(y);
        parents[y].insert(x);
    }

    for (int i = 1; i < n + 1; ++i)
    {
        if (!visited[i] && parents[i].empty())
        {
            dfs(i);
        }
    }

    for (int i = 1; i < n + 1; i++)
    {
        printf("%i\n", g[i]);
    }
}