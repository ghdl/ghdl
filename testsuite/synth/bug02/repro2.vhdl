library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  port (
    clk_i          : in  std_logic;
    rst_n_i        : in  std_logic;
    vec_i         : in  std_logic_vector(0 to 0)
  );
end repro2;

architecture rtl of repro2 is
  signal s_sel : natural range vec_i'range;
  signal s_true : std_logic;
begin
  s_true <= '1';

  process (clk_i)
  begin
    if rising_edge(clk_i) then
        for i in vec_i'range loop
          if s_true = '1' then
            s_sel <= i;
            exit;
          end if;
        end loop;
    end if;
  end process;
end rtl;
