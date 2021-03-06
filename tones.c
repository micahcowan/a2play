#include <stdio.h>
#include <math.h>

#define PRINT_TONES 0

int
main(void)
{
    double halfStep = pow(2., 1./12.);
    double startTone = 27.5;    // Freq of tone A0
    double cpu = 1022727.0;     // Freq of Apple 2's 6502 CPU

    printf("%f\n\n", halfStep);

    for (double octStart = startTone; octStart < 4200.0; octStart *= 2) {
        double tone = octStart;
        printf("[%.2f]\n", octStart);
        for (int i=0; i<13; ++i) {
            if (PRINT_TONES) {
                printf("%.2f ", tone);
            }
            else {
                printf("%lu ", (unsigned long)(cpu/tone));
            }
            tone *= halfStep;
            if (tone > 4200.0) {
                // Highest tone on a piano is 4186; next up would be
                // 4435
                break;
            }
        }
        putchar('\n');
    }
    return 0;
}
