library ieee;
use ieee.std_logic_1164.all;

package pkg is

  type t_in is record
    data : std_logic_vector(3 downto 0);
  end record;

end package;


library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity issue is
  port (
    din  : in t_in;
    dout : out std_logic
  );
end entity issue;

architecture rtl of issue is

  -- This function signature works:
  -- function func (a : std_logic_vector(3 downto 0)) return std_logic_vector is

  -- This does not work:
  function func (a : din.data'subtype) return std_logic_vector is
  begin
    return a;
  end function func;

  -- This procedure signature works:
  -- procedure prod (signal a : in std_logic_vector(3 downto 0); signal b : out std_logic_vector(3 downto 0)) is

  -- This does not work:
  procedure prod (signal a : in din.data'subtype; signal b : out std_logic_vector(3 downto 0)) is
  begin
    b <= a;
  end procedure prod;

  signal f, p : std_logic_vector(3 downto 0);

begin

  f <= func(din.data);
  prod(din.data, p);

  dout <= '1' when f = p else '0';

  assert dout;

end architecture;
