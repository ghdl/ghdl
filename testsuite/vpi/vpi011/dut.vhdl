library ieee;
  use ieee.std_logic_1164.all;

entity dut is
  generic(
    DATA_WIDTH : positive := 8
  );
end entity;

architecture rtl of dut is
  type t_mem is array (natural range 0 to 1) of std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal mem : t_mem  := (('0', '1', others => '0'), ('1', '0', others => '1'));
begin
end architecture;
