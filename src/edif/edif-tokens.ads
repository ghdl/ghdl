package Edif.Tokens is
   pragma Pure (Tokens);

   type Token_Type is
     (
      Tok_Keyword,      --  '(' followed by a symbol (case insensitive).
      Tok_Right_Paren,  --  ')'

      Tok_Symbol,
      Tok_String,
      Tok_Number,

      Tok_Eof
     );
end Edif.Tokens;
