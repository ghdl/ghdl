package pkg is
  type state_t is (idle, run);
end package pkg;

use work.pkg.all;

entity bar is
  port(
    clk:  in bit;
    rst:  in bit
  );
end entity bar;

architecture rtl of bar is
  signal state: state_t;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        state <= idle;
      else
        case state is
          when idle => state <= run;
          when run  => state <= idle;
        end case;
      end if;
    end if;
  end process;

end architecture rtl;

use std.env.all;
use work.pkg.all;

entity bar_sim is
end entity bar_sim;

architecture sim of bar_sim is
  signal clk, rst: bit;
begin
  dut: entity work.bar
  port map(
    clk => clk,
    rst => rst
  );

  process
  begin
    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
  end process;

  process
  begin
    rst <= '1';
    wait until rising_edge(clk);
    rst <= '0';
    for i in 1 to 10 loop
      wait until rising_edge(clk);
    end loop;
    finish;
  end process;

  process
    alias state is <<signal dut.state: state_t>>;
  begin
    wait until rising_edge(clk);
    if state = run then
      report "RUN!";
    else
      report "IDLE";
    end if;
  end process;
end architecture sim;
