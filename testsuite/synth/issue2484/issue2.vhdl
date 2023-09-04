package pkg is

  type t_in is record
    data : bit_vector(3 downto 0);
  end record;

end package;


use work.pkg.all;

entity issue is
  port (
    din  : in t_in;
    dout : out bit
  );
end entity issue;

architecture rtl of issue is

  -- This function signature works:
  -- function func (a : bit_vector(3 downto 0)) return bit_vector is

  -- This does not work:
  function func (a : din.data'subtype) return bit_vector is
  begin
    return a;
  end function func;

  -- This procedure signature works:
  -- procedure prod (signal a : in bit_vector(3 downto 0); signal b : out bit_vector(3 downto 0)) is

  -- This does not work:
  procedure prod (signal a : in din.data'subtype; signal b : out bit_vector(3 downto 0)) is
  begin
    b <= a;
  end procedure prod;

  signal f, p : bit_vector(3 downto 0);

begin

  f <= func(din.data);
  prod(din.data, p);

  dout <= '1' when f = p else '0';

  assert dout;

end architecture;
