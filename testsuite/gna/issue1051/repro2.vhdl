entity repro2 is
end repro2;

architecture behav of repro2 is
  type msg_t is record
    pfx : string;
    func : string;
    user : string;
  end record;

  procedure report_msg (m : msg_t) is
  begin
    report m.pfx & "." & m.func & ": " & m.user;
  end report_msg;

  procedure fill (pfx : string := "#err#"; usr : string := "none") is
  begin
    report_msg (m => (pfx, "fill", usr));
  end fill;
begin
  process
  begin
    fill;
    fill (pfx => "my err");
    wait;
  end process;
end behav;

