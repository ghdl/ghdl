library IEEE;
use IEEE.std_logic_1164.all;

entity repro2 is
end;

architecture RTL of repro2 is

  signal tready   : std_logic;
  signal cnt      : natural;
  signal cnt_next : natural;
begin

  p_main: process (all)

    procedure count (
      constant r_ctr : in  natural;
      variable v_ctr : out natural
    ) is
    begin
      if tready = '1' then
        v_ctr := r_ctr + 1;
      else
        v_ctr := r_ctr;
      end if;
    end procedure;

    variable v : natural;

  begin
    report "execution of process p_main, cnt=" & natural'image(cnt);
    count (cnt, v);

    cnt_next <= v;
  end process p_main;

  process
  begin
    tready <= '1';
    cnt <= 1;
    wait for 1 ns;
    assert cnt_next = 2 severity failure;
    cnt <= 2;
    wait for 1 ns;
    assert cnt_next = 3 severity failure;
    tready <= '0';
    wait for 1 ns;
    assert cnt_next = 2 severity failure;
    wait;
  end process;
end;
