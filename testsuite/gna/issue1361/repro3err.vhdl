ENTITY repro3err IS
END repro3err;

ARCHITECTURE behav of repro3err IS
  CONSTANT AddrRANGE     : NATURAL := 16#0FFFFFF#;

  TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF bit_vector;
begin
    process
      VARIABLE Mem : MemArray(open)(7 downto 0) := (OTHERS => x"00");
    begin
      Mem := (OTHERS => x"fff");
      WAIT;
    END PROCESS;
END behav;
