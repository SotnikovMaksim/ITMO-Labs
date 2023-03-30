#include <cstdio>
#include <vector>
#include <algorithm>
#include <cmath>
#include <queue>

struct Triple
{
    int u;
    int v;
    int w;
};

int n, m, u, v, w;
std::vector< Triple > edges;
std::vector< int > rank;
std::vector< int > p;

int get(int x)
{
    if (p[x] != x)
    {
        p[x] = get(p[x]);
    }

    return p[x];
}

void unionDSU(int x, int y)
{
    int rx = get(x);
    int ry = get(y);

    if (rx == ry)
    {
        return;
    }

    if (rank[rx] > rank[ry])
    {
        std::swap(rx, ry);
    }

    p[rx] = ry;
    if (rank[rx] == rank[ry])
    {
        rank[ry] += 1;
    }
}

long long kraskal()
{
    std::sort(edges.begin(), edges.end(), [](auto a, auto b) { return a.w < b.w; });
    long long answer = 0;

    for (const auto &edge: edges)
    {
        if (get(edge.u) != get(edge.v)) {
            answer += edge.w;

            unionDSU(edge.u, edge.v);
        }
    }

    return answer;
}

int main()
{
    scanf("%i %i", &n, &m);

    rank = std::vector< int >(n + 1);
    p = std::vector< int >(n + 1);

    for (int i = 1; i < n + 1; i++)
    {
        p[i] = i;
        rank[i] = i;
    }

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i %i", &u, &v, &w);
        edges.push_back({u, v, w});
    }

    printf("%lli", kraskal());
}
