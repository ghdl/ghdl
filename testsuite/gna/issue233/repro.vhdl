ENTITY test1 IS
  PORT (
    i : IN integer);
END ENTITY test1;

architecture behav of test1 is
begin
  process
  begin
    wait for 1 ns;
    assert i = 5 report "bad signal value" severity failure;
    wait;
  end process;
end behav;

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

entity repro is
end repro;

architecture behav of repro is
  signal s : integer;
begin

  t: entity work.test port map (s);

  s <= 5;
end behav;
