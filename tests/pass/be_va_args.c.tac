TACO_ASSIGN
  TACA_FRAME u8 is_addr 0
  TACA_REG u8 REG_RDI
TACO_ASSIGN
  TACA_FRAME u8 is_addr 8
  TACA_REG u8 REG_RSI
TACO_ASSIGN
  TACA_FRAME u8 is_addr 16
  TACA_REG u8 REG_RDX
TACO_ASSIGN
  TACA_FRAME u8 is_addr 24
  TACA_REG u8 REG_RCX
TACO_ASSIGN
  TACA_FRAME u8 is_addr 32
  TACA_REG u8 REG_R8
TACO_ASSIGN
  TACA_FRAME u8 is_addr 40
  TACA_REG u8 REG_R9
TACO_ASSIGN
  TACA_FRAME u4 is_addr 48
  TACA_IMM i4 8
TACO_ASSIGN
  TACA_FRAME u4 is_addr 52
  TACA_IMM i4 48
TACO_ASSIGN
  TACA_FRAME u8 is_addr 56
  TACA_ARG u0 is_addr 0
TACO_ASSIGN
  TACA_FRAME u8 is_addr 64
  TACA_FRAME u0 is_addr 0
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RDI
  TACA_FRAME u8 0
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RSI
  TACA_FRAME u0 is_addr 48
TACO_CALL
  TACA_LNAME u0 is_addr g
  TACA_IMM i4 2
TACO_LT
  TACA_FRAME i4 48
  TACA_IMM i4 48
TACO_BRZ
  TACA_REF u8 13
  TACA_ALABEL u0 3
TACO_ADD
  TACA_FRAME u8 64
  TACA_FRAME i4 48
TACO_LOAD
  TACA_FRAME i4 76
  TACA_REF u8 15
TACO_ADD
  TACA_FRAME i4 48
  TACA_IMM i4 8
TACO_ASSIGN
  TACA_FRAME i4 is_addr 48
  TACA_REF i4 17
TACO_JUMP
  TACA_ALABEL u0 2
  TACA_VOID
TACO_LABEL
  TACA_ALABEL u0 3
  TACA_VOID
TACO_LOAD
  TACA_FRAME i4 76
  TACA_FRAME u8 56
TACO_ADD
  TACA_FRAME u8 56
  TACA_IMM i4 8
TACO_ASSIGN
  TACA_FRAME u8 is_addr 56
  TACA_REF u8 22
TACO_LABEL
  TACA_ALABEL u0 2
  TACA_VOID
TACO_ASSIGN
  TACA_FRAME i4 is_addr 72
  TACA_FRAME i4 76
TACO_ASSIGN
  TACA_PARAM u8 is_addr 0
  TACA_IMM i4 2
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RDI
  TACA_IMM i4 0
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RSI
  TACA_IMM i4 1
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RDX
  TACA_IMM i4 2
TACO_ASSIGN
  TACA_REG u8 is_addr REG_RCX
  TACA_IMM i4 2
TACO_ASSIGN
  TACA_REG u8 is_addr REG_R8
  TACA_IMM i4 2
TACO_ASSIGN
  TACA_REG u8 is_addr REG_R9
  TACA_IMM i4 2
TACO_CALL
  TACA_LNAME u0 is_addr f
  TACA_IMM i4 7
