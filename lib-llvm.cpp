/* Library functions for EK language. */
#include <iostream>
#include <cstdlib>

using namespace std;

/* Variables that store argument information. */
static int argcount;
static char **argstrings;

extern "C" {
    void run();
    int arg(int i);
    float argf(int i);
}

int arg(int i) {
    if (i < 0 || i >= argcount) {
        cout << "Error: arg()'s index out of bound.\n";
        exit(-1);
    }

    return atoi(argstrings[i]);
}

float argf(int i) {
    if (i < 0 || i >= argcount) {
        cout << "Error: argf()'s index out of bound.\n";
        exit(-1);
    }

    return atof(argstrings[i]);
}


int main(int argc,char *argv[]) {
    argcount = argc - 1;
    argstrings = argcount > 0 ? argv + 1 : NULL;

    run();
}
