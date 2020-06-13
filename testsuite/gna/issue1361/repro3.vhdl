ENTITY repro3 IS
END repro3;

ARCHITECTURE behav of repro3 IS
  CONSTANT AddrRANGE     : NATURAL := 16#0FFFFFF#;

  TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF bit_vector;
begin
    process
      VARIABLE Mem : MemArray(open)(7 downto 0) := (OTHERS => x"00");
    begin
      Mem := (OTHERS => x"ff");
      WAIT;
    END PROCESS;
END behav;
