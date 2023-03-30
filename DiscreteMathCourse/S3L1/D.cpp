#include <cstdio>
#include <vector>
#include <deque>
#include <algorithm>
 
std::vector< std::vector< int > > table;
std::deque< int > q;
int n;
char u;
 
bool is_cycle(std::vector< int >& path)
{
    bool result = true;
 
    for (int i = 1; i < n; i++)
    {
        if (!table[path[i]][path[i + 1]])
        {
            result = false;
            break;
        }
    }
 
    if (result)
    {
        result = (table[path[n]][path[1]]);
    }
 
    return result;
}
 
int main() {
    scanf("%i ", &n);
    table = std::vector < std::vector < int > > (n + 1, std::vector<int>(n + 1, 0));
    std::vector< int > path = std::vector< int >(n + 1);
 
 
    for (int i = 2; i < n + 1; i++)
    {
        for (int k = 1; k < i; k++)
        {
            scanf("%c", &u);
            if (u == '1')
            {
                table[i][k] = 1;
            } else
            {
                table[k][i] = 1;
            }
        }
        scanf("%c", &u);
    }
 
    for (int i = 1; i < n + 1; i++)
    {
        path[i] = i;
    }
 
    while (!is_cycle(path))
    {
        std::stable_sort(path.begin(), path.end(), [](auto a, auto b) {
            return table[a][b];
        });
    }
 
    for (int i = 1; i < n + 1; i++)
    {
        printf("%i ", path[i]);
    }
}