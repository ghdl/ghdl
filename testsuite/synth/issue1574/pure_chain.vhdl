library ieee; use ieee.std_logic_1164.all;
entity pure_chain is
  port (clock : in std_logic; o : out std_logic_vector(7 downto 0));
end pure_chain;
architecture a of pure_chain is
  signal s : std_logic := '0';

  --  Declared pure and reads a signal, so not pure.
  pure function f2 return std_logic_vector is
  begin
    if s = '1' then return x"A0"; else return x"00"; end if;
  end f2;

  --  Declared pure and calls f2, so not pure either: the impurity has to
  --  propagate through the call.
  pure function f3 return std_logic_vector is
  begin
    return f2;
  end f3;
begin
  process (clock) begin
    if rising_edge(clock) then o <= f3; end if;
  end process;
end a;
