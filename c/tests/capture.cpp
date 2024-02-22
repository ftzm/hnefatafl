 #include "../board.cpp"

const char* empty =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* u_input =
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char *u_expected = ".  .  .  .  .  O  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  O  .  .  .  O  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  O  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  ."
                         ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* l_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* l_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* R_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  O  .  ."
  ".  .  .  .  .  .  .  .  X  .  ."
  ".  .  .  .  .  .  O  X  .  X  O"
  ".  .  .  .  .  .  .  .  X  .  ."
  ".  .  .  .  .  .  .  .  O  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* R_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  O  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  O  .  .  .  O"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  O  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* r_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  O  .  .  .  .  .  .  .  ."
  ".  .  X  .  .  .  .  .  .  .  ."
  "O  X  .  X  O  .  .  .  .  .  ."
  ".  .  X  .  .  .  .  .  .  .  ."
  ".  .  O  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* r_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  O  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "O  .  .  .  O  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  O  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* x_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* x_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* y_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  O  X  .  X  O  .  .  ."
  ".  .  .  .  .  X  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* y_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* B_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  X  .  .  ."
  ".  .  .  .  .  O  X  .  X  O  ."
  ".  .  .  .  .  .  .  X  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* B_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  O  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* b_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  X  .  .  ."
  ".  .  .  .  .  O  X  .  X  O  ."
  ".  .  .  .  .  .  .  X  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* b_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  O  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* c62_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  .  .  .  ."
  ".  .  .  X  .  .  .  .  .  .  ."
  ".  O  X  .  X  O  .  .  .  .  ."
  ".  .  .  X  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* c62_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  O  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* n_input =
  ".  O  X  X  X  .  X  X  X  X  O"
  ".  .  O  O  O  X  O  O  O  O  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* n_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* n_input_2 =
  ".  .  X  X  X  .  X  X  X  X  O"
  ".  .  O  O  O  X  O  O  O  O  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* n_expected_2 =
  ".  .  X  X  X  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* ne_input =
  ".  .  .  X  X  X  X  O  .  .  ."
  ".  .  .  O  O  O  O  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* ne_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* nw_input =
  ".  .  O  X  X  X  X  .  .  .  ."
  ".  .  .  O  O  O  O  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* nw_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* w_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  "O  .  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  ".  X  O  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "O  .  .  .  .  .  .  .  .  .  .";

const char* w_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* wn_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  "O  .  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* wn_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* ws_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "X  O  .  .  .  .  .  .  .  .  ."
  "O  .  .  .  .  .  .  .  .  .  .";

const char* ws_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* e_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  O"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  O  X  ."
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  .  O";

const char* e_expected =
  ".  .  .  .  .  .  .  .  .  .  "
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* es_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  .  O";

const char* es_expected =
  ".  .  .  .  .  .  .  .  .  .  "
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* en_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  O"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  O  X"
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* en_expected =
  ".  .  .  .  .  .  .  .  .  .  "
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************

const char* s_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  O  .  .  .  .  ."
  ".  .  O  O  O  X  O  O  O  O  ."
  ".  O  X  X  X  .  X  X  X  X  O";

const char* s_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* se_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  O  O  O  .  .  .  .  .  ."
  ".  O  X  X  X  .  .  .  .  .  .";

const char* se_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

const char* sw_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  O  O  O  O  ."
  ".  .  .  .  .  .  X  X  X  X  O";

const char* sw_expected =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  .  .  .  .";

//******************************************************************************
// Capture destinations

const char* capture_destinations_input =
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  .  .  .  .  O  X  .  ."
  ".  .  .  .  .  .  .  .  .  .  ."
  ".  .  .  O  .  .  .  .  .  .  ."
  ".  .  .  X  .  .  O  .  .  .  ."
  ".  .  .  X  .  .  X  .  .  .  ."
  ".  O  .  .  .  .  .  .  .  .  ."
  ".  X  .  .  .  .  .  X  O  .  ."
  ".  .  .  O  .  X  .  .  .  .  ."
  ".  .  .  .  .  .  O  X  .  .  ."
  ".  .  X  .  .  .  .  .  .  .  .";


//******************************************************************************



int code = 0;

void test_layers_equal(layer x, layer y) {
  if (x[0] != y[0] || x[1] != y[1]) {
    printf("Layer mismatch.\nexpected:\n");
    print_layer(x);
    printf("got:\n");
    print_layer(y);
    code = 1;
  }
}

void test_test(void) {
  layer expected = read_layer(l_expected, 'X');
  layer expected_r = rotate_layer(expected);

  layer foes = read_layer(l_input, 'X');
  layer foes_r = rotate_layer(foes);

  layer allies = read_layer(l_input, 'O');
  layer allies_r = rotate_layer(allies);

  capture_x(allies, allies_r, foes, foes_r, 38);
  
  test_layers_equal(expected, foes);
  test_layers_equal(expected_r, foes_r);
}

void test_capture(void (*func)(const layer&, const layer&, layer&, layer&, const unsigned char),
		  const char *input_string, const char *expected_string, unsigned char pos) {
  layer expected = read_layer(expected_string, 'X');
  layer expected_r = rotate_layer(expected);

  layer foes = read_layer(input_string, 'X');
  layer foes_r = rotate_layer(foes);

  layer allies = read_layer(input_string, 'O');
  layer allies_r = rotate_layer(allies);

  func(allies, allies_r, foes, foes_r, pos);
  
  test_layers_equal(expected, foes);
  test_layers_equal(expected_r, foes_r);
}

void test_all_captures() {
  for (int i = 1; i < 120; i++) {
    //printf("-----------------------------------------------\n");
    layer allies = {ally_masks[i][0],ally_masks[i][1]};
    layer allies_r = {ally_masks_r[i][0],ally_masks_r[i][1]};
    //print_layer(al.allies);
    //print_layer(al.allies_r);
    layer foes = {foe_masks[i][0], foe_masks[i][1]};
    //print_layer(foes);
    layer foes_r = {foe_masks_r[i][0], foe_masks_r[i][1]};
    //print_layer(foes_r);
    capture_functions[i](allies, allies_r, foes, foes_r, i);
    //print_layer(foes);
    //print_layer(foes_r);

    if (foes[0] || foes[1] || foes_r[0] || foes_r[1]) {
      printf("capture failure on %d\n", i);
      code = 0;
    }

  }
}

void bench_all_captures(int count) {
  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  while (count) {
    for (int i = 1; i < 120; i++) {
      layer allies = {0,0};
      layer allies_r = {0,0};
      layer foes = {0,0};
      layer foes_r = {0,0}; 
      capture_functions[i](start_board.black, start_board.black_r, start_board.white, start_board.white_r, i);
    }
    count--;
  };

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}

void bench_all_captures_niave(int count) {
  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  while (count) {
    for (int i = 1; i < 120; i++) {
      layer friends = {0,0};
      layer foes = {0,0};
      layer output = {0,0}; 
      apply_captures_niave(start_board.black, start_board.white, output, i);
    }
    count--;
  };

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}

void bench_board_gen(int count) {
  move moves[235];
  board boards[235]; 

  move moves_2[235];
  board boards_2[235]; 

  board start_board = read_board(start_board_string);

  // begin time
  clock_t start, end;
  double cpu_time_used;
  start = clock();

  /*
  while (count) {
    get_team_moves_black(start_board, &total, moves, boards);
    count--;
  }
  */

  int sum = 0;
  while (count) {
    int total = 0;
    get_team_moves_black(start_board, &total, moves, boards);
    for (int i = 0; i < total; i++) {
      int total_2 = 0;
      get_team_moves_black(boards[i], &total_2, moves_2, boards_2);
      for (int j = 0; j < total_2; j++) {
    	board b2 = boards_2[j];
    	// const layer occ = {
    	//   b2.black[0] | b2.white[0] | b2.king[0] | corners[0],
    	//   b2.black[1] | b2.white[1] | b2.king[1] | corners[1]};
    	// const layer occ_r = {
    	//   b2.black_r[0] | b2.white_r[0] | b2.king_r[0] | corners[0],
    	//   b2.black_r[1] | b2.white_r[1] | b2.king_r[1] | corners[1]};
    	// sum += get_team_move_count(occ, b2.black, occ_r, b2.black_r);
    	sum += __builtin_popcountll(b2.black[0]) + __builtin_popcountll(b2.black[1]);
      }
      /*
      */
      // sum += total_2;
    }
    count--;
  }
  printf("all moves: %d\n", sum);

  // end time
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("bench took %f seconds to execute \n", cpu_time_used); 
}



int main(int argc, char **argv) {

  printf("Testing capture!...\n");

  gen_foe_masks();
  gen_ally_masks();
  gen_row_moves();
  gen_row_move_counts();
  gen_center_row_move_counts();

  // test_capture(capture_u, u_input, u_expected, 93);
  // test_capture(capture_l, l_input, l_expected, 38);
  // test_capture(capture_x, x_input, x_expected, 60);
  // test_capture(capture_y, y_input, y_expected, 71);
  // test_capture(capture_r, r_input, r_expected, 63);
  // test_capture(capture_R, R_input, R_expected, 57);
  // test_capture(capture_b, b_input, b_expected, 47);
  // test_capture(capture_B, B_input, B_expected, 80);
  // test_capture(capture_62, c62_input, c62_expected, 62);
  // test_capture(capture_s, s_input, s_expected, 5);
  // test_capture(capture_se, se_input, se_expected, 5);
  // test_capture(capture_sw, sw_input, sw_expected, 5);
  // test_capture(capture_e, e_input, e_expected, 55);
  // test_capture(capture_en, en_input, en_expected, 55);
  // test_capture(capture_es, es_input, es_expected, 55);
  // test_capture(capture_w, w_input, w_expected, 65);
  // test_capture(capture_wn, wn_input, wn_expected, 65);
  // test_capture(capture_ws, ws_input, ws_expected, 65);
  // test_capture(capture_n, n_input, n_expected, 115);
  // test_capture(capture_n, n_input_2, n_expected_2, 115);
  // test_capture(capture_ne, ne_input, ne_expected, 118);
  // test_capture(capture_nw, nw_input, nw_expected, 113);
  // test_all_captures();

  // bench_all_captures(1000000);
  // bench_all_captures_niave(1000000);

  /*
  board start_board = read_board(start_board_string);
  print_board(start_board);

  move moves[235];
  board boards[235];
  int total = 0;
  get_team_moves_black(start_board, &total, moves, boards);

  printf("total: %d\n", total);

  while (total) {
    move m = moves[total];
    printf("orig: %d, dest: %d\n\n", m.orig, m.dest);
    print_board(boards[total]);
    print_board_r(boards[total]);
    total--;
    printf("--------------------------------------------------\n");
  };
  printf("orig: %d, dest: %d\n\n", moves[0].orig, moves[0].dest);
  print_board(boards[0]);
  print_board_r(boards[0]);

  */

  bench_board_gen(1000);

  // print_row(get_row_moves_2(0b000000010, 5));
  // print_row(2^0b0100100);

  // layer allies = read_layer(capture_destinations_input, 'O');
  // layer foes = read_layer(capture_destinations_input, 'X');
  // layer dests = find_capture_destinations_op(allies, foes);
  // print_layer(dests);

  return code;
}
