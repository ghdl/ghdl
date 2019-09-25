library ieee;
use ieee.std_logic_1164.all;

entity dff is
  generic(
    formal_g : boolean := true
  );
  port(
    reset : in std_logic;
    clk : in std_logic;
    d : in std_logic;
    q : out std_logic
  );
end entity dff;

architecture rtl of dff is
  signal q_int : std_logic;
begin

  dff_proc : process(clk, reset)
  begin
    if reset = '1' then
      q_int <= '0';
    elsif rising_edge(clk) then
      q_int <= d;
    end if;
  end process dff_proc;

  -- drive q_int to output port
  q <= q_int;

  formal_gen : if formal_g = true generate
  begin
    -- set all declarations to run on clk
    default clock is rising_edge(clk);
    d_in_check : assert always {d} |=> {q_int};
    not_d_in_check : assert always {not d} |=> {not q_int};
  end generate formal_gen;

end rtl;
