library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

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

    monitor : process(clk)
   begin
      if rising_edge(clk) then
        report "arg: " & integer'image(to_integer(unsigned(arg)));
      end if;
   end process;
end rtl;


library ieee;
   use ieee.std_logic_1164.all;

entity sliced_ex is
port (
   arg_a : in  std_logic_vector(3 downto 0);
   arg_b : in  std_logic_vector(3 downto 0)
);
end sliced_ex;

architecture rtl of sliced_ex is
  signal clk   :  std_logic;
begin
   process
   begin
     clk <= '0';
     for i in 1 to 5 * 2 loop
       wait for 10 ns;
       clk <= not clk;
     end loop;
     wait;
   end process;

   sub_module : entity work.submodule
   port map (
      clk => clk,

      -- This one fails
      arg(3 downto 0) => arg_a(3 downto 0),
      arg(7 downto 4) => arg_b(3 downto 0),

      res => OPEN
   );

end rtl;
