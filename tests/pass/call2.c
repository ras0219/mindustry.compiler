void cg_debug(struct CodeGen* cg, const char* fmt, ...);
void cg_declare_extern(struct CodeGen* cg, const char* sym) { cg_debug(cg, "   : %s\n", sym); }
