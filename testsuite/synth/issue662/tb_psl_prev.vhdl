library ieee;
  use ieee.std_logic_1164.all;

entity tb_psl_prev is
end entity tb_psl_prev;


architecture psl of tb_psl_prev is
  procedure seq (s : string; signal clk : std_logic; signal o : out std_logic)
  is
  begin
    for i in s'range loop
      wait until rising_edge(clk);
      case s(i) is
        when '0' | '_' => o <= '0';
        when '1' | '-' => o <= '1';
        when others    => o <= 'X';
      end case;
    end loop;
    wait;
  end seq;

  signal a, b : std_logic := '0';
  signal clk   : std_logic := '1';

begin
  dut: entity work.psl_prev port map (clk, a, b);

  clk <= not clk after 500 ps;

  --            0123456789012345
  SEQ_A : seq ("__-__-____-__-__", clk, a);
  SEQ_B : seq ("____--_______-__", clk, b);
end architecture psl;
