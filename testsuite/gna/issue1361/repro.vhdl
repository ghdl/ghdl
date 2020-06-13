ENTITY repro IS
END repro;

ARCHITECTURE vhdl_behavioral of repro IS
  constant userpreload : boolean := true;
  CONSTANT MaxData       : NATURAL := 16#FF#;        --255;
  CONSTANT AddrRANGE     : NATURAL := 16#0FFFFFF#;

  TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF INTEGER RANGE -1 TO MaxData;
  SHARED VARIABLE Mem          : MemArray  := (OTHERS => MaxData);
begin
    MemPreload : PROCESS
    BEGIN
        IF UserPreload THEN
          Mem := (OTHERS => MaxData);
        end if;
        WAIT;
    END PROCESS MemPreload;
END vhdl_behavioral;
