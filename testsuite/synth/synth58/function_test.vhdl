library ieee;
  use ieee.std_logic_1164.all;

entity function_test is
  generic (
    g : std_logic := '1'
  );
  port (
    i : in std_logic_vector(7 downto 0);
    o : out std_logic_vector(7 downto 0)
  );
end function_test;

architecture rtl of function_test is

  function assign_value(value : in std_logic_vector(7 downto 0);
                        invert : in std_logic)
                        return std_logic_vector is
    variable slv_out : std_logic_vector(7 downto 0);
  begin
    if invert = '0' then
      slv_out := value;
    elsif invert = '1' then
      slv_out := not value;
    end if;
    return slv_out;
  end;

begin
  o <= assign_value(i, g);
end rtl;
