--  An 'in' conversion function on an actual associated with an unconstrained
--  formal port.  The function changes the length (17 -> 32), so the formal
--  must be constrained by the result subtype of the function and not by the
--  actual (LRM08 5.3.2.2 e) 3)).

library ieee;
use ieee.std_logic_1164.all;

entity conv_sub is
  port (pc : in std_logic_vector);
end entity;

architecture behav of conv_sub is
begin
  process (pc) is
  begin
    assert pc'length = 32
      report "bad length: " & integer'image(pc'length) severity failure;
    assert pc(31 downto 17) = "000000000000000"
      report "bad padding" severity failure;
    assert pc(16 downto 0) = "10000000000000001"
      report "bad value" severity failure;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity conv is
end entity;

architecture behav of conv is
  subtype small is std_logic_vector(16 downto 0);
  subtype wordx is std_logic_vector(31 downto 0);

  function widen (p : small) return wordx is
    variable res : wordx := (others => '0');
  begin
    res (p'range) := p;
    return res;
  end widen;

  signal pc : small := "10000000000000001";
begin
  u : entity work.conv_sub port map (pc => widen (pc));
end architecture;
