library ieee;
use ieee.std_logic_1164.all;

entity mwe is
  generic (
    wait_us : integer := 400;
    clk_period : time := 10 ns
  );
end entity;

architecture sim of mwe is

  signal clk : std_logic := '0';
  signal runsim : boolean := true;

  function slv_all(constant width : in integer; constant value : in std_logic) return std_logic_vector is
    variable slv_v : std_logic_vector(width - 1 downto 0) := (others => value);
  begin
    return slv_v;
  end function;

  function slv_ones(constant width : in integer) return std_logic_vector is
  begin
    return slv_all(width, '1');
  end function;


begin

  p_clk: process
  begin
    while runsim loop
      clk <= '0';
      wait for clk_period / 2;
      clk <= '1';
      wait for clk_period / 2;
    end loop;
    wait;
  end process;


  p_check_requests: process

    function return_true return boolean is
      constant ones_c : std_logic_vector(31 downto 0) := slv_ones(32);
    begin
      return true;
    end function;

    variable ones_v : std_logic_vector(31 downto 0);
    variable result_v : boolean;
  begin
    wait until rising_edge(clk);

    while runsim loop
      wait until rising_edge(clk);

      result_v := return_true;

      -- uncommenting the following lines speeds up the design
      -- ones_v := slv_ones(32);

    end loop;

    wait;
  end process;


  p_main: process
  begin
    wait for wait_us * 1 us;
    runsim <= false;
    wait;
  end process;

end architecture;
