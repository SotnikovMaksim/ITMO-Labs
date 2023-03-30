#include <cstdio>
#include <vector>
#include <algorithm>
#include <cmath>
#include <queue>

std::vector< std::pair< int, int > > coords;
std::vector< bool > inq;
int n, x, y;
std::vector< double > key;

double cw(const int& i, const int& j)
{
    return sqrt(pow(coords[i].first - coords[j].first, 2) +
                pow(coords[i].second - coords[j].second, 2));
}

double prim()
{

    std::vector< int > p(n + 1);
    key = std::vector< double >(n + 1);
    auto comp = []( int a, int b ) { return key[a] > key[b]; };
//    std::priority_queue< int, std::vector< int >, decltype( comp ) > q( comp );
    double answer = 0;

    for (int i = 0; i < n + 1; i++)
    {
        key[i] = INFINITY;
        p[i] = 0;
//        q.push(i);
    }

    key[1] = 0;

    for (int i = 0; i < n; i++)
    {
        int v = std::distance(key.begin(), std::min_element(key.begin() + 1, key.end()));
        inq[v] = false;
//        q.pop();
        answer += key[v];
        key[v] = INFINITY;

        for (int u = 1; u < n + 1; u++)
        {
            if (u == v) continue ;

            double w = cw(u, v);
            if (inq[u] && key[u] > w)
            {
                p[u] = v;
                key[u] = w;
            }
        }
    }

    return answer;
}

int main()
{
    scanf("%i", &n);

    coords = std::vector< std::pair< int, int > >(n + 1);
    inq = std::vector< bool >(n + 1, true);

    for (int i = 1; i < n + 1; i++)
    {
        scanf("%i %i", &x, &y);
        coords[i] = std::pair< int, int >(x, y);
    }

    printf("%lf", prim());
}
