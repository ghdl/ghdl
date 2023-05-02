library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

package pkg_enum is

end pkg_enum;

library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package pkg_scala2hdl is
  function pkg_unsigned (lit : unsigned) return unsigned;

end  pkg_scala2hdl;

package body pkg_scala2hdl is

  function pkg_unsigned (lit : unsigned) return unsigned is
    alias ret : unsigned(lit'length-1 downto 0) is lit;
  begin
    return unsigned(ret);
  end pkg_unsigned;

  -- Replacing the above with the below version does not trigger the error
  -- function pkg_unsigned (lit : unsigned) return unsigned is
  -- begin
  --   return unsigned(lit);
  -- end pkg_unsigned;
end pkg_scala2hdl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_scala2hdl.all;
use work.all;
use work.pkg_enum.all;


entity BlackboxTest is
  port(
    reset : in std_logic;
    clk : in std_logic;
    io_data_out : out std_logic_vector(15 downto 0);
    io_gain2 : in std_logic_vector(31 downto 0)
  );
end BlackboxTest;

architecture arch of BlackboxTest is
  signal inst1_nasreset : std_logic;
  signal inst1_data_out : signed(15 downto 0);

  component BlackboxExample is
    port( 
      clk : in std_logic;
      nasreset : in std_logic;
      data_out : out std_logic_vector(15 downto 0);
      gain : in std_logic_vector(31 downto 0) 
    );
  end component;
  

  signal zz_io_data_out : signed(15 downto 0);
begin
  inst1 : BlackboxExample
    port map ( 
      clk => clk,
      nasreset => inst1_nasreset,
      signed(data_out) => inst1_data_out,
      gain => std_logic_vector(pkg_unsigned("01000000000000000000000000000000")) 
    );
  inst1_nasreset <= (not reset);
  zz_io_data_out <= inst1_data_out;
  io_data_out <= std_logic_vector(zz_io_data_out);
end arch;


library ieee;
use ieee.std_logic_1164.all;

------------------------------------------------------------------------------------------------------------------------------------------------------

entity BlackboxExample is

    port (
      clk      : in  std_logic;
      nasreset : in  std_logic;
      data_out : out std_logic_vector(15 downto 0);
      gain     : in  std_logic_vector(31 downto 0));


end entity BlackboxExample;

------------------------------------------------------------------------------------------------------------------------------------------------------

architecture str of BlackboxExample is
begin  -- architecture str

  data_out <= X"0100";
end architecture str;
