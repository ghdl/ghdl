library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity test_addsub is
  port (
    slv : in  std_logic_vector(3 downto 0);
    nat : in  natural range 0 to 15;
    add_slvslv : out std_logic_vector(3 downto 0);
    add_slvnat : out std_logic_vector(3 downto 0);
    add_natslv : out std_logic_vector(3 downto 0);
    sub_slvslv : out std_logic_vector(3 downto 0);
    sub_slvnat : out std_logic_vector(3 downto 0);
    sub_natslv : out std_logic_vector(3 downto 0)
  );
end;

architecture rtl of test_addsub is
begin
  add_slvslv <= slv + slv;
  add_slvnat <= slv + nat;
  add_natslv <= nat + slv;
  sub_slvslv <= slv - slv;
  sub_slvnat <= slv - nat;
  sub_natslv <= nat - slv;
end;
