ENTITY repro1_ent IS
port(	S 	: inout string 	:= "abcdef");
END repro1_ent;

ARCHITECTURE repro1_arch OF repro1_ent IS
	constant C 	: string 	:= "abcdef";
BEGIN
  assert S = C;
END repro1_arch;
