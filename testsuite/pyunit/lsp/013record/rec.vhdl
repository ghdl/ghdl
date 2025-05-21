entity hello is
end hello;

package pkg is
  type myrec is record
  end record;
end;

architecture behav of hello is
begin
  assert false report "Hello VHDL world" severity note;
end behav;
