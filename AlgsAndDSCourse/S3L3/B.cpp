#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>
#define INF 1000000000000000


int n, m, u, v, w;
std::vector< std::vector< std::pair< int, int > > > childs;

void dijkstra(std::vector< long long >& min_cost, int start)
{
    auto comp = [](std::pair<int, long long> const& a, std::pair<int, long long> const& b) -> bool {
        return a.first < b.first;
    };

    std::set< std::pair< int, long long >, decltype(comp) > d(comp);
    d.emplace(start, 0);
    min_cost[start] = 0;

    while (!d.empty())
    {
        auto cur = *d.begin();
        d.erase(d.begin());

        auto node = cur.first;
        auto cost = cur.second;

        for (const auto& item : childs[node])
        {
            auto child = item.first;
            auto child_cost = item.second;

            if (cost + child_cost < min_cost[child])
            {
                d.erase({child, min_cost[child]});
                min_cost[child] = cost + child_cost;
                d.insert({item.first, min_cost[child]});
            }
        }
    }
}

int main()
{
    scanf("%i %i", &n, &m);
    childs = std::vector< std::vector< std::pair< int, int > > >(n + 1);
    auto min_cost = std::vector< long long >(n + 1, INF);

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i %i", &u, &v, &w);

        childs[u].emplace_back(v, w);
        childs[v].emplace_back(u, w);
    }

    dijkstra(min_cost, 1);

    for (int i = 1; i < n + 1; i++)
    {
        printf("%lli ", min_cost[i]);
    }
}
