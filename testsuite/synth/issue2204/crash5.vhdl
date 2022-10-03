library ieee;
  use ieee.std_logic_1164.all;

entity crash is
  port (
    clk  : in  std_logic
  );
end entity crash;

architecture rtl of crash is
    signal index : std_logic := '0';
begin

    -- process (clk) is
    --     begin
    --       if rising_edge(clk) then
-- >> This prints an error, but doesn't crash ghdl
    --         index <= index = index'LAST_VALUE;
    --       end if;
    --     end process;

  -- psl default clock is rising_edge(clk);

  -- psl crash_my_ghdl2 : cover
  --    {not index'active or index'event};


end architecture rtl;
