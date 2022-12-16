library ieee;
use ieee.std_logic_1164.all;

entity fun1 is
  port (
    d : in  std_logic_vector(3 downto 0);
    q : out std_logic_vector(4 downto 0)
    );
end entity;

architecture behaviour of fun1 is

  function fp (i : natural; v : std_logic_vector(3 downto 0)) return std_logic is
  begin
    if i > 3 or i < 0
    then
      return 'X';
    else
      return v(i);
    end if;
  end function;

begin
  process (d)
    impure function get_fp1 (i : natural) return std_logic is
    begin
      return fp(i, d);
    end function;
  begin
    q <= (others => '0');
    q(2) <= get_fp1(0);
  end process;
end architecture;
