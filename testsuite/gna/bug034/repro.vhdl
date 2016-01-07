ENTITY ent IS
END ent;

ARCHITECTURE arch OF ent IS
  function to_lower (c : character) return character is
  begin
    if c >= 'A' and c <= 'Z' then
      return character'val (character'pos (c) + 32);
    else
      return c;
    end if;
  end to_lower;

BEGIN
	TESTING: PROCESS
	BEGIN
          assert to_lower('F') = 'f';
	wait;
	END PROCESS TESTING;
END arch;
