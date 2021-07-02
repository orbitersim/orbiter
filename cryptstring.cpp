// Some strings to be encrypted to make them harder to
// locate in the executable
// (use program "scramble" to encrypt, and function uscram (util.cpp) to decrypt)
// Encrypted string versions written to cryptstring.cpp

// © 2000-2007 Martin Schweiger
const char *sig1 = "\051\200\111\011\131\007\131\004\133\007\131\016\111\044\212\111\235\100\227\367\174\072\221\116\216\100\220\074\233";

// Copyright (c) 2000-2007 Martin Schweiger
const char *sig1a = "\043\040\222\115\234\117\214\104\213\121\103\005\206\006\103\017\123\015\123\012\125\015\123\024\103\052\204\117\227\106\221\375\166\100\213\124\210\106\212\102\225";

// orbit.medphys.ucl.ac.uk/
const char *sig2 = "\276\261\060\244\047\266\354\257\043\246\056\252\067\265\354\267\041\256\354\243\041\160\063\255\355";

// martins@medphys.ucl.ac.uk
const char *sig2a = "\204\351\345\356\370\345\362\357\304\351\351\340\364\344\375\357\262\361\347\350\262\335\347\252\371\347";

#ifdef ISBETA
// Build __DATE__ BETA
const char *sig3 = "\341\141\126\210\115\203\001\154\102\221\001\121\030\077\023\117\021\126\001\141\046\163\042";
#else
// Build __DATE__
const char *sig3 = "\154\326\341\375\330\370\214\341\315\006\214\306\243\264\236\304\234\313";
#endif
