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

  -- >> These crash after printing good errors, which still seems undesirable.
  -- >> Remove the X to test them

  -- psl crash_my_ghdl1 : cover
  --    {index /= index'LAST_VALUE};
  -- Xpsl crash_my_ghdl2 : cover
  --    {index'EVENT};
  -- Xpsl crash_my_ghdl3 : cover
  --    {index'ACTIVE};


end architecture rtl;
