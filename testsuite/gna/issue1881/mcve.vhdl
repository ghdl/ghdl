library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mcve is
end mcve;

architecture sim of mcve is
  constant clock_period : time := 10 ns;
  constant width_in     : natural := 11;        -- 12 bits
  constant last_stage   : natural := 5;

  signal v_clk          : std_logic := '0';
  signal v_rst          : std_logic;

  signal counter        : unsigned (width_in downto 0);

  subtype stage_reg_t is std_logic_vector (width_in downto 0);
  type stage_regs_t is array (0 to last_stage) of stage_reg_t;
  signal stage_reg      : stage_regs_t;

begin

  v_clk <= NOT v_clk AFTER clock_period/2;

  process is
  begin
    v_rst <= '1';
    wait for 20 ns;
    v_rst <= '0';
    wait;
  end process;

  -- Generate some data that changes with time so that we can see the latency
  -- through the module.
  process (v_clk) is
  begin
    if rising_edge (v_clk) then
      if v_rst = '1' then
        counter <= (others => '0');
      else
        counter <= counter + 1;
      end if;
    end if;
  end process;

  -- First stage:
  process (v_clk) is
  begin
    if rising_edge (v_clk) then
      stage_reg (0) <= std_logic_vector(counter);
    end if;
  end process;
  
  g_stages: for ii in 1 to last_stage generate
    process (v_clk) is
    begin
      if rising_edge (v_clk) then
        stage_reg (ii) <= stage_reg (ii - 1);
      end if;
    end process;
  end generate g_stages;

end sim;
