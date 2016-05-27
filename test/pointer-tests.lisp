(defpackage #:vacietis.test.pointer
  (:use #:cl #:named-readtables #:eos)
  (:import-from #:vacietis.test #:eval-test)
  (:import-from #:vacietis #:string-to-char*))

(in-package #:vacietis.test.pointer)
(in-readtable vacietis:vacietis)

(in-suite vacietis.test::pointer-tests)

(eval-test pointer-lvalue1
  "int foo[3];
int *x = &foo[1];

foo[1] = 3;
foo[2] = 5;

*x;"
  3)

(eval-test pointer-lvalue2
  "int foo[3];
int *x = &foo[1];

foo[1] = 3;
foo[2] = 5;

*(x + 1);"
  5)

(eval-test pointer-lvalue
  "int i = 1;
int *j = &i;
*j += 1;
i;"
  2)

(eval-test duff "
#include <string.h>
void send (char *to, char *from, int count) {
    int n = (count + 7) / 8;
    switch (count % 8) {
    case 0: do { *to++ = *from++;
    case 7:      *to++ = *from++;
    case 6:      *to++ = *from++;
    case 5:      *to++ = *from++;
    case 4:      *to++ = *from++;
    case 3:      *to++ = *from++;
    case 2:      *to++ = *from++;
    case 1:      *to++ = *from++;
            } while (--n > 0);
    }
}
char tobuf[32];
char frombuf[32];
memset(tobuf, '\\0', 32);
char* from = \"Duff the magic dragon's device!\";
strcpy(frombuf, from);
send(tobuf, frombuf, strlen(frombuf));
strcmp(tobuf, frombuf);
"
           0)
