#include <cstdio>
#include <vector>
#include <algorithm>

#define INF 1000

int n;
std::vector<std::vector<int> > m;

int main() {
    scanf("%i", &n);
    m = std::vector<std::vector<int> >(n + 1, std::vector<int>(n + 1, INF));

    for (int i = 1; i < n + 1; i++)
    {
        for (int j = 1; j < n + 1; ++j)
        {
            scanf("%i", &m[i][j]);
        }
    }

    for (int i = 1; i < n + 1; i++)
    {
        for (int j = 1; j < n + 1; j++)
        {
            for (int k = 1; k < n + 1; k++)
            {
                if (m[j][i] < INF && m[i][k] < INF)
                {
                    m[j][k] = std::min(m[j][k], m[j][i] + m[i][k]);
                }
            }
        }
    }

    for (int i = 1; i < n + 1; i++)
    {
        for (int j = 1; j < n + 1; ++j)
        {
            printf("%i ", m[i][j]);
        }

        printf("\n");
    }
}
