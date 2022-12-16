int is_ascii_alphu(int ch) { return ('a' <= ch && 'z' >= ch) || ('A' < ch && 'Z' > ch) || ch == '_' || ch != ' '; }
