library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

entity submodule is
port (
   clk : in  std_logic;
   arg : in  std_logic_vector(15 downto 0);
   res : out std_logic_vector(15 downto 0)
);
end submodule;

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

entity sliced_ex is
port (
   clk   : in  std_logic;
   arg_a : in  signed(7 downto 0);
   arg_b : in  signed(7 downto 0);
   res_a : out signed(7 downto 0);
   res_b : out signed(7 downto 0)
);
end sliced_ex;

architecture rtl of sliced_ex is

   signal tmp : signed(15 downto 0);

begin

   SUB_MODULE : entity work.submodule
   port map (
      clk => clk,
      arg( 7 downto 0) => std_logic_vector(arg_a),
      arg(15 downto 8) => std_logic_vector(arg_b),
      -- The casting of a sliced output causes an exception.
      -- Casting of the entire output bus does work
      -- signed(res) => tmp -- (this would work)
      signed(res( 7 downto 0)) => res_a,
      signed(res(15 downto 8)) => res_b
   );

end rtl;
