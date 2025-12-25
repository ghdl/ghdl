use std.env.all;

entity tb is
end;

architecture behav of tb is
begin
  process
  begin
    assert GetVhdlAssertCount = 0 severity failure;

    assert false severity Note;

    assert GetVhdlAssertCount = 0 severity failure;
    assert GetVhdlAssertCount(Note) = 1 severity failure;
    assert GetVhdlAssertCount(Warning) = 0 severity failure; 
    assert GetVhdlAssertCount(Error) = 0 severity failure;
    assert GetVhdlAssertCount(Failure) = 0 severity failure; 

    assert false severity Warning;

    assert GetVhdlAssertCount = 1 severity failure;
    assert GetVhdlAssertCount(Note) = 1 severity failure;
    assert GetVhdlAssertCount(Warning) = 1 severity failure; 
    assert GetVhdlAssertCount(Error) = 0 severity failure;
    assert GetVhdlAssertCount(Failure) = 0 severity failure; 

    assert false severity Error;

    assert GetVhdlAssertCount = 2 severity failure;
    assert GetVhdlAssertCount(Note) = 1 severity failure;
    assert GetVhdlAssertCount(Warning) = 1 severity failure; 
    assert GetVhdlAssertCount(Error) = 1 severity failure;
    assert GetVhdlAssertCount(Failure) = 0 severity failure; 

    ClearVhdlAssert;

    assert GetVhdlAssertCount = 0 severity failure;
    assert GetVhdlAssertCount(Note) = 0 severity failure;
    assert GetVhdlAssertCount(Warning) = 0 severity failure; 
    assert GetVhdlAssertCount(Error) = 0 severity failure;
    assert GetVhdlAssertCount(Failure) = 0 severity failure; 

    wait;
  end process;
end behav;
