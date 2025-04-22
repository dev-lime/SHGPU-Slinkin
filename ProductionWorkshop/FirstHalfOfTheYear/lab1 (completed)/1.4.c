/*
Дана последовательность из N целочисленных значений (возможные значения N: 5,9,13 ...). Применив не более одного
условного оператора (или условной тернарной операции),
программа должна определить, верно-ли, что каждые три элемента последовательность возрастает,
а затем - убывает (синусообразная последовательность). Циклы в программе разрешается использовать только
для организации перебора элементов последовательности.
Например:
  *   *
 * * * *
*   *   *
*/

#include <stdio.h>

int main()
{
    int N;
    printf("N: ");
    scanf("%d", &N);

    int prev, current;
    int res = 0;
    int phase = 0; // 0 - возрастание, 1 - убывание

    printf("Elements:\n");
    scanf("%d", &prev);

    for (int i = 0; i < N - 1; i++)
    {
        scanf("%d", &current);

        int expected_phase = i / 2 % 2;
        int is_increasing = current > prev;
        int is_decreasing = current < prev;

        res |= ((expected_phase == 0 && !is_increasing) || (expected_phase == 1 && !is_decreasing));

        int valid_transition = (is_increasing && phase == 0) || (is_decreasing && phase == 1);

        phase = expected_phase;
        prev = current;
    }

    printf("Последовательность %sявляется верной.\n", res == 0 ? "" : "не ");

    return 0;
}
