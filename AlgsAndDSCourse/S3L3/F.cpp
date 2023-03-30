#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>
#define INF 1000000000000000

int n, m, u, v, w, a, b, c;
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
    childs = std::vector< std::vector< std::pair< int, int > > >(n + 1, std::vector< std::pair< int, int > >());
    std::vector< int > deg(n + 1);

    for (int i = 0; i < m; i++)
    {
        scanf("%i %i %i", &u, &v, &w);

        childs[u].emplace_back(v, w);
        childs[v].emplace_back(u, w);
    }

    scanf("%i %i %i", &a, &b, &c);
    std::vector< int > abc{a, b, c};
    long long answer = INF;

    for (const auto &vertex: abc)
    {
        std::vector< long long > min_cost(n + 1, INF); // path length from start to i

        dijkstra(min_cost, vertex);

        if (!(min_cost[a] >= INF || min_cost[b] >= INF || min_cost[c] >= INF))
        {
            answer = std::min(answer, min_cost[a] + min_cost[b] + min_cost[c]);
        }
    }

    printf("%lli", answer >= INF ? -1 : answer);
}