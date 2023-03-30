#include <cstdio>
#include <vector>
#include <algorithm>

#define INF 1000000000000000

struct Triple
{
    Triple(int i, int i1, int i2) {
        a = i;
        b = i1;
        w = i2;
    }

    int a;
    int b;
    int w;
};

int n, m, k, s, a, b, w;

int main()
{
    scanf("%i %i %i %i", &n, &m, &k, &s);
    auto g = std::vector<std::vector< long long > >(k + 1, std::vector< long long > (n + 1, INF));
    std::vector< Triple > edges;
    g[0][s] = 0;

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i %i", &a, &b, &w);

        edges.emplace_back(a, b, w);
    }

    for (int i = 0; i < k; i++)
    {
        for (const auto &edge: edges)
        {
            if (g[i][edge.a] != INF)
            {
                g[i + 1][edge.b] = std::min(g[i + 1][edge.b], g[i][edge.a] + edge.w);
            }
        }
    }

    for (int i = 1; i < n + 1; i++)
    {
        printf("%lld\n", g[k][i] != INF ? g[k][i] : -1);
    }
}
