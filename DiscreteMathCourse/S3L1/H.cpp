#include <cstdio>
#include <vector>
 
int* answer;
int n, m;
 
void P(std::vector< std::vector< int > > list, bool sign)
{
    int u, v;
    bool isVoid = true;
    int vertices_count = list.size() - 1;
 
    for (int i = 1; i < vertices_count + 1; i++)
    {
        for (int j = 1; j < vertices_count + 1; j++)
        {
            if (i != j && list[i][j] == 1)
            {
                u = i; v = j;
                isVoid = false;
                break;
            }
        }
 
        if (!isVoid)
        {
            break;
        }
    }
 
    if (isVoid)
    {
        answer[vertices_count] += sign ? 1 : -1;
        return;
    }
 
    list[u][v] = 0;
    list[v][u] = 0;
 
    P(list, sign);
 
    for (int i = 1; i < vertices_count + 1; i++)
    {
        list[u][i] = list[u][i] | list[v][i];
    }
 
    for (int i = 1; i < vertices_count + 1; i++)
    {
        list[i][u] = list[i][u] | list[i][v];
    }
 
    list.erase(list.begin() + v);
 
    for (int i = 1; i < vertices_count; i++)
    {
        list[i].erase(list[i].begin() + v);
    }
 
    P(list, !sign);
}
 
int main()
{
    scanf("%i %i", &n, &m);
 
    auto childs = std::vector< std::vector< int > >(n + 1, std::vector< int >(n + 1, 0));
    answer = new int[11]();
    int u, v;
 
    for (int i = 0; i < m; i++)
    {
        scanf("%i %i", &u, &v);
 
        childs[u][v] = 1;
        childs[v][u] = 1;
    }
 
    P(childs, true);
 
    int deg = 0;
    for (int i = n; i >= 0; i--)
    {
        if (answer[i] != 0)
        {
            deg = i;
            break;
        }
    }
 
    printf("%i\n", deg);
    for (int i = deg; i >= 0; i--)
    {
        printf("%i ", answer[i]);
    }
}