#include <cstdio>
#include <vector>
#include <algorithm>

std::vector< std::pair< int, int > > edges;
std::vector< std::vector< int > > childs;
std::vector< bool > visited;
std::vector< int > tin;
std::vector< int > rev;
std::vector< int > answer;
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

            if (rev[child] > tin[v])
            {
                auto a = std::distance( edges.begin(), std::find(edges.begin(),
                                                                edges.end(),
                                                                std::pair< int, int >(v, child)));
                if (a == m)
                {
                    a = std::distance( edges.begin(), std::find(edges.begin(),
                                                               edges.end(),
                                                               std::pair< int, int >(child, v)));
                }
                answer.push_back(a + 1);
            }
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
    childs = std::vector< std::vector< int > >(n + 1, std::vector< int >());

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);

        edges.emplace_back(u, v);

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

    std::sort(answer.begin(), answer.end());

    printf("%lu\n", answer.size());

    for (int i : answer)
    {
        printf("%i ", i);
    }

}
