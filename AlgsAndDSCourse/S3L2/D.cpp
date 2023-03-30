#include <cstdio>
#include <vector>
#include <algorithm>

std::vector< int > g;
int n;

int mex(std::vector< int >& of)
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

void grandi()
{
    g = std::vector< int >(n + 1);
    g[0] = 0;
    g[1] = 1;
    g[2] = 1;

    for (int i = 3; i < n + 1; i++)
    {
        std::vector< int > of;

        for (int j = 0; j < i; j++)
        {
            int a = g[j], b = g[i- j - 1];

            if (j <= 1)
            {
                a = 0;
            }
            if (i - j <= 2)
            {
                b = 0;
            }

            of.push_back(a ^ b);
        }

        g[i] = mex(of);
    }
}

int main()
{
    scanf("%i", &n);

    grandi();

    if (g[n] == 0)
    {
        printf("Mueller");
    } else
    {
        std::vector< int > win;

        for (int i = 0; i < n + 1; i++)
        {
            int a = g[i], b = g[n - i - 1];

            if (i <= 1)
            {
                a = 0;
            }
            if (n - i <= 2)
            {
                b = 0;
            }

            int res = (a ^ b);

            if (res == 0)
            {
                win.push_back(i + 1);
            }
        }

        printf("Schtirlitz\n");
        for (const auto& item: win)
        {
            printf("%i\n", item);
        }
    }
}