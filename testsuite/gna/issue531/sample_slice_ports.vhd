library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

entity sliced_ex is
port (
   clk   : in  std_logic;
   reset : in  std_logic;
   arg_a : in  std_logic_vector(3 downto 0);
   arg_b : in  std_logic_vector(3 downto 0)
);
end sliced_ex;

architecture rtl of sliced_ex is

   signal aa, ab : std_logic_vector(3 downto 0);

begin

   aa <= arg_a(aa'range);
   ab <= arg_b(ab'range);

   monitor : process(clk)
   begin
      if rising_edge(clk) then
        report "arg_a: " & integer'image(to_integer(unsigned(arg_a)))
          & ", arg_b: " & integer'image(to_integer(unsigned(arg_b)));
      end if;
   end process;

   sub_module : entity work.submodule
   port map (
      clk => clk,

      -- This version works
      --arg( 7 downto 0) => aa,
      --arg(15 downto 8) => ab,

      -- This one works
      --arg => arg_a,

      -- This one fails
      arg(3 downto 0) => arg_a(3 downto 0),
      arg(7 downto 4) => arg_b(3 downto 0),

      res => OPEN
   );

end rtl;

