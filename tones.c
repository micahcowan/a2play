#include <stdio.h>
#include <math.h>

int
main(void)
{
    double halfStep = pow(2., 1./12.);
    double startTone = 27.5/2.; // Freq of tone A0
    double cpu = 1022727.0;     // Freq of Apple 2's 6502 CPU
    double octStart;
    unsigned int oct=0;

    printf("%f\n\n", halfStep);

    for (double octStart = startTone; octStart < 4200.0; octStart *= 2) {
        double tone = octStart;
        printf("[%u: %.2f]\n", oct, octStart);
        for (int i=0; i<13; ++i) {
            printf("(%.2f) %lu : ", tone, (unsigned long)round(cpu/tone/2.));
            tone *= halfStep;
            if (tone > 4200.0) {
                // Highest tone on a piano is 4186; next up would be
                // 4435
                break;
            }
        }
        putchar('\n');
        ++oct;
    }
    return 0;
}
