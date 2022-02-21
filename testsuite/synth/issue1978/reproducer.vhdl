--:file: Entity.vhd

library IEEE;
context IEEE.IEEE_std_context;

entity Reproducer is
  generic (
    g_Precision : natural := 11;
    g_PulsesPerRevolution : natural := 1000
  );
  port (
    CLK : in  std_logic;
    RST : in  std_logic;
    EN  : in  std_logic;
    Z   : in  std_logic;
    POS : out unsigned(g_Precision-1 downto 0)
  );
end entity;

architecture arch of Reproducer is

  signal Position : unsigned(POS'range);

  signal Direction : std_logic := '0';

begin

    PositionCounter: process(RST, Z, CLK)
      constant CountLimit : unsigned(Position'range) := to_unsigned(4*g_PulsesPerRevolution-1, Position);
    begin
      if RST or Z then
        Position <= (others => '0');
      elsif rising_edge(CLK) and EN='1' then
        Position <=
          (others=>'0') when Position = CountLimit and Direction='1' else
          CountLimit when Position = 0 and Direction='0' else
          Position+1 when Direction else
          Position-1;
      end if;
    end process;

  pos <= position;
  direction <= '0';

end architecture;
