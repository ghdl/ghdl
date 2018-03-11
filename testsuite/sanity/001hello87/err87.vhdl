entity err87 is
end err87;

architecture behav of err87 is
begin
  process
  begin
    report "not valid in vhdl-87" severity note;
    wait;
  end process;
end behav;
