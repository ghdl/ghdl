library ieee;
use ieee.std_logic_1164.all;
use work.all;

entity libs12 is
end entity;

architecture tb of libs12 is

  component tent
    port(
      rst_n : in std_logic;
      clk   : in std_logic;
      period : in integer;
      clk_o : out std_logic;
      op1 : out integer;
      op2 : out integer
      );
  end component;

  signal tclk : std_logic := '0';
  signal trst : std_logic := '0';
  signal tper : integer := 1;
  signal oclk : std_logic;
  signal oop1 : integer;
  signal oop2 : integer;
  signal oclk1 : std_logic;
  signal oop11 : integer;
  signal oop21 : integer;
  
begin

  process
    variable v_cnt : integer := 0;
  begin
    trst  <= '0';
    wait for 1 ns;
    trst  <= '1';
    wait for 1 ns;
    while v_cnt < 50 loop
      --report "clk";
      tclk <= not tclk;
      wait for 1 ns;
      v_cnt := v_cnt + 1;
    end loop;
  end process;


u1: entity tent(rtl)
  port map(
    rst_n  => trst,
    clk    => tclk,
    period => tper,
    clk_o  => oclk,
    op1    => oop1,
    op2    => oop2
    );

u2: entity tent(bhv)
  port map(
    rst_n  => trst,
    clk    => tclk,
    period => tper,
    clk_o  => oclk1,
    op1    => oop11,
    op2    => oop21
    );

  mon: process(oclk)
  begin
    if oclk'event and oclk = '1' then
      report "Rising edge oclk with oop1: " & integer'image(oop1) &
      " and oop2: " & integer'image(oop2);
      report "Rising edge oclk with oop11: " & integer'image(oop11) &
      " and oop12: " & integer'image(oop21);
    end if;
  end process mon;

end tb;
