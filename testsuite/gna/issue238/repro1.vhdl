ENTITY repro1 IS
  TYPE foo_t IS RECORD
    bar : bit_vector;
  END RECORD foo_t;
END ENTITY repro1;

ARCHITECTURE bar OF repro1 IS
BEGIN
  process
    variable baz : foo_t(bar(1 DOWNTO 0));
  begin
    wait;
  end process;

END ARCHITECTURE bar;
