#include <stdlib.h>

int getrand_()

{
    return (rand());
}

void setrand_(unsigned int *seed)

{
    srand(*seed);
}
