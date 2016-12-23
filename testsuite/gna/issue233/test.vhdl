ENTITY test1 IS
  PORT (
    i : IN integer);
END ENTITY test1;

ENTITY test IS
  PORT (
    o : OUT integer);
END ENTITY test;

ARCHITECTURE rtl OF test IS

BEGIN

  test1_1 : ENTITY work.test1
    PORT MAP (
      i => o);

END ARCHITECTURE rtl;
