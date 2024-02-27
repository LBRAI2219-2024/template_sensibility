// Copyright (C) 2016 Daniel Leitner and Andrea Schnepf. See //license.txt for details.

#include "RootSystem.h"
#include "analysis.h"

#include <iostream>
#include <fstream>
#include <unistd.h>


#include "example_lbrai2219.h"


/**
 * Starts an examples (from the examples folder)
 */
int main(int argc, char* argv[])
{
    string name="";

    if (argc>1) {
        name= argv[1];
    }

	example_lbrai2219(); // open parameter file and output txt

    return(0);

}



