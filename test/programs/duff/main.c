#include <stdio.h>

int main (void) {
        /*  This is called duff's device and there are many great explanations online: 
            http://en.wikipedia.org/wiki/Duff%27s_device                             */
        /*  What does this do?  */
        unsigned int count = 22;
        unsigned int j = (count + 7) / 8;
        switch(count % 8) {
                case 0: do{     putchar('0' + (int)j);
                case 7:         putchar('0' + (int)j);
                case 6:         putchar('0' + (int)j);
                case 5:         putchar('0' + (int)j);
                case 4:         putchar('0' + (int)j);
                case 3:         putchar('0' + (int)j);
                case 2:         putchar('0' + (int)j);
                case 1:         putchar('0' + (int)j);
                        } while(--j > 0);
        }
        return j;
}
