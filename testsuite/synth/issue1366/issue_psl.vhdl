vunit issue_vunit (issue(psl)) {
  
  -- VHDL declaration seem to be working
  signal a_delayed : std_logic := '0';

  -- Other VHDL code not
  -- results in parser errors
  -- during synthesis
  process is
  begin
    wait until rising_edge(clk);
    a_delayed <= a;
  end process;  


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  CHECK_a : assert always (a -> b);

  -- You can't do anything with the declared signal
  -- Can be synthesized with GHDL, however
  -- results in error in ghdl-yosys-plugin:
  -- ERROR: Assert `n.id != 0' failed in src/ghdl.cc:172
  assert always a_delayed = prev(a);

}


