#include <cstdio>
#include <vector>
#include <algorithm>
 
int n, m, u, v, Kmin;
std::vector< std::vector< int > > childs;
std::vector< int > color;
std::vector< int > d;
std::vector< bool > visited;
 
 
int get_min(const int& x)
{
    std::vector< bool > p = std::vector< bool >(Kmin + 1, false);
 
    for (const int& child: childs[x])
    {
        p[color[child]] = true;
    }
 
    for (int i = 1; i < Kmin + 1; i++)
    {
        if (!p[i])
        {
            return i;
        }
    }
}
 
void dfs(const int& x)
{
    color[x] = get_min(x);
    visited[x] = true;
 
    for (const auto &child: childs[x])
    {
        if (!visited[child])
        {
            dfs(child);
        }
    }
}
 
void solve()
{
    std::vector< int > vert;
 
    for (int i = 1; i < n + 1; i ++)
    {
        vert.push_back(i);
    }
 
    std::sort(vert.begin(), vert.end(), [](auto a, auto b) { return d[a] > d[b]; });
 
    int ind = 1;
 
    for (const int& cur: vert)
    {
        color[cur] = std::min(ind++, get_min(cur));
    }
}
 
int main()
{
    scanf("%i %i", &n, &m);
    childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());
    color = std::vector< int >(n + 1, 0);
    d = std::vector< int >(n + 1, 0);
    visited = std::vector< bool >(n + 1, false);
    Kmin = 0;
 
    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
 
        childs[u].push_back(v);
        childs[v].push_back(u);
        d[u]++;
        d[v]++;
    }
 
    for (int i = 1; i < n + 1; i ++)
    {
        Kmin = std::max(Kmin, d[i]);
    }
 
    Kmin = Kmin % 2 == 1 ? Kmin : Kmin + 1;
 
    dfs(1);
 
    printf("%i\n", Kmin);
 
    for (int i = 1; i < n + 1; i++)
    {
        printf("%i\n", color[i]);
    }
 
}
 