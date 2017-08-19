entity err93 is
end err93;

architecture behav of err93 is
begin
  process (all) is
  begin
    report "not valid in vhdl-93" severity note;
  end process;
end behav;
