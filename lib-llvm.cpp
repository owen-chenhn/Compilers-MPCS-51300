/* Library functions for EK language. */
#include <iostream>
#include <cstdlib>

using namespace std;

/* Variables that store argument information. */
static int argcount;
static char **argstrings;

extern "C" {
    void run();
    int getarg(int i);
    float getargf(int i);
}

int getarg(int i) {
    if (i < 0 || i >= argcount) {
        cout << "Error: getarg()'s index out of bound.\n";
        exit(-1);
    }

    return atoi(argstrings[i]);
}

float getargf(int i) {
    if (i < 0 || i >= argcount) {
        cout << "Error: getargf()'s index out of bound.\n";
        exit(-1);
    }

    return atof(argstrings[i]);
}


int main(int argc,char *argv[]) {
    argcount = argc - 1;
    argstrings = argcount > 0 ? argv + 1 : NULL;

    run();
}
