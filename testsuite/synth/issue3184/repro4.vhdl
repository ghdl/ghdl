library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.repro4_pkg.all;

entity repro4 is
  port (
    slvo : ahb_slv_out_vector;
    haddr : std_logic_vector(15 downto 2);
    vdata : out amba_config_word);
end;

architecture behav of repro4 is
  function conv_integer(v : std_logic_vector) return integer is
  begin
    return(to_integer(unsigned(v)));
  end;

begin

  vdata <= slvo(conv_integer(haddr(2+5 downto 5))).hconfig(conv_integer(haddr(4 downto 2)));
end;
