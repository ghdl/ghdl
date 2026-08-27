library ieee; use ieee.std_logic_1164.all;
entity pure_via_proc is
  port (clock : in std_logic; o : out std_logic_vector(7 downto 0));
end pure_via_proc;
architecture a of pure_via_proc is
  signal s : std_logic := '0';

  --  Declared pure, reads a signal: not pure.
  pure function f return std_logic_vector is
  begin
    if s = '1' then return x"A0"; else return x"00"; end if;
  end f;

  --  A procedure calling it is impure too, and the impurity must reach it.
  procedure p (v : out std_logic_vector(7 downto 0)) is
  begin
    v := f;
  end p;
begin
  process (clock)
    variable r : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clock) then
      p (r);
      o <= r;
    end if;
  end process;
end a;
