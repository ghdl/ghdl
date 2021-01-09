library ieee;
use ieee.std_logic_1164.all;

entity tent is
  port(
    rst_n : in std_logic;
    clk   : in std_logic;
    period : in integer;
    clk_o : out std_logic;
    op1 : out integer;
    op2 : out integer
    );
    
begin

  clk_mon: process(clk)
    variable tl : time := 0 ns;
    variable td : time;
    variable lp : time;
  begin
    if(clk'event) then
      --report "" & time'image(now);
      if clk = '1' then
        td := now - tl;
        tl := now;
        if lp /= td then
          report "clk period is: " & time'image(td);
          lp := td;
        end if;
      end if;
      
    end if;
  end process;
  
end entity tent;

architecture rtl of tent is

  signal bv : bit_vector(15 downto 0) := (others => '1');
  signal stdv : std_logic_vector(15 downto 0);
  signal clk1  : std_logic := '1';
  
  signal cnt : integer := 0;
  signal cnt1 : integer := 0;
  
  signal operiod : time;
  
begin

  --operiod <= period * 1 ns;

  clock : process(clk)
    variable clk_v : std_logic := '0';
  begin
    if clk'event and clk = '1' then
      clk_v := not clk_v;
      clk_o <= clk_v;
    end if;
    if cnt > 20 then
      report "END  SIM ..." severity failure;
    end if;
  end process clock;

  process(clk)
  begin
    if clk'event and clk = '1' then
      cnt <= cnt + 1;
      --report "clk event ...";
      op1 <= cnt;
      clk1 <= not clk1;
    end if;
  end process;

  process(clk1)
  begin
    if(clk1'event and clk1 = '1') then
      cnt1 <= 2 ** cnt;
      --report "clk1 event ...";
      op2 <= cnt1;
    end if;
  end process;

end rtl;


architecture bhv of tent is

  signal bv : bit_vector(15 downto 0) := (others => '1');
  signal stdv : std_logic_vector(15 downto 0);
  signal clk1  : std_logic := '1';
  
  signal cnt : integer := 0;
  signal cnt1 : integer := 0;
  
  signal operiod : time;
  
begin

  process
  begin
    report "BHV ...";
    wait;
  end process;

  clock : process(clk)
    variable clk_v : std_logic := '0';
  begin
    if clk'event and clk = '1' then
      clk_v := not clk_v;
      clk_o <= clk_v;
    end if;
    if cnt > 20 then
      report "END  SIM ..." severity failure;
    end if;
  end process clock;

  process(clk)
  begin
    if clk'event and clk = '1' then
      cnt <= cnt + 1;
      --report "clk event ...";
      op1 <= cnt;
      clk1 <= not clk1;
    end if;
  end process;

  process(clk1)
  begin
    if(clk1'event and clk1 = '1') then
      cnt1 <= 2 ** cnt;
      --report "clk1 event ...";
      op2 <= cnt1;
    end if;
  end process;

end bhv;
