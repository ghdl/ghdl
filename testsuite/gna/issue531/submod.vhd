library ieee;
   use ieee.std_logic_1164.all;

entity submodule is
port (
   clk    : in  std_logic;
   arg : in  std_logic_vector(7 downto 0);
   res : out std_logic_vector(7 downto 0)
);
end submodule;

architecture rtl of submodule is
begin
  sub_proc : process(clk)
    variable last : std_logic_vector(7 downto 0);
   begin
      if rising_edge(clk) then
         res <= arg XOR last;
         last := arg;
      end if;
   end process sub_proc;
end rtl;


