ENTITY repro_ent IS
port(	S 	: string 	:= "abcdef");
END repro_ent;

ARCHITECTURE repro_arch OF repro_ent IS
	constant C 	: string 	:= "abcdef";
BEGIN
  assert S = C;
END repro_arch;
