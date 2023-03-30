#include <cstdio>
#include <vector>
#include <algorithm>
#include <iostream>
 
int main()
{
    int n;
 
    scanf("%i", &n);
    std::vector<int> answer(n);
    for (int i = 0; i < n; i++)
    {
        answer[i] = i + 1;
    }
 
    std::stable_sort(answer.begin(), answer.end(), [](auto a, auto b)
    {
        std::cout << "1 " << a << " " << b << std::endl;
        std::string otv;
        std::cin >> otv;
        return otv == "YES";
    });
 
    printf("0 ");
    for (const auto &item: answer)
    {
        printf("%i ", item);
    }
}