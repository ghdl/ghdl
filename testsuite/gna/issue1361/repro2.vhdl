ENTITY repro2 IS
END repro2;

ARCHITECTURE behav of repro2 IS
  CONSTANT AddrRANGE     : NATURAL := 16#0FFFFFF#;

  TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF bit_vector(7 downto 0);
begin
    process
      VARIABLE Mem : MemArray  := (OTHERS => x"00");
    begin
      Mem := (OTHERS => x"ff");
      WAIT;
    END PROCESS;
END behav;
