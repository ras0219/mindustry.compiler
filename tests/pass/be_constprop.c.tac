TACO_ASSIGN
  TACA_FRAME i4 is_addr 0
  TACA_IMM i4 4294967294
TACO_ASSIGN
  TACA_FRAME i4 is_addr 4
  TACA_IMM i4 4
TACO_ASSIGN
  TACA_FRAME i4 is_addr 8
  TACA_IMM i4 8
TACO_ASSIGN
  TACA_FRAME u8 is_addr 32
  TACA_FRAME u0 is_addr 20
TACO_ASSIGN
  TACA_FRAME u8 is_addr 32
  TACA_FRAME u0 is_addr 12
TACO_ASSIGN
  TACA_FRAME i4 is_addr 40
  TACA_IMM i4 2
TACO_ASSIGN
  TACA_FRAME i4 is_addr 44
  TACA_IMM i4 1
TACO_ASSIGN
  TACA_FRAME i4 is_addr 48
  TACA_FRAME i4 0
TACO_BRZ
  TACA_FRAME i4 0
  TACA_ALABEL u0 1
TACO_ASSIGN
  TACA_FRAME i4 is_addr 48
  TACA_IMM i4 0
TACO_LABEL
  TACA_ALABEL u0 1
  TACA_VOID
TACO_NEQ
  TACA_FRAME i4 48
  TACA_IMM i4 0
TACO_BRZ
  TACA_REF i4 11
  TACA_ALABEL u0 0
TACO_ASSIGN
  TACA_FRAME i4 is_addr 48
  TACA_IMM i4 5
TACO_LABEL
  TACA_ALABEL u0 0
  TACA_VOID
TACO_ASSIGN
  TACA_FRAME i4 is_addr 52
  TACA_IMM i4 5