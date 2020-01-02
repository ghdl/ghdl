entity repro is
end repro;

architecture behav of repro is
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
    constant m : msg_t := (pfx, "fill", usr);
  begin
    report_msg (m);
  end fill;
begin
  process
  begin
    fill;
    fill (pfx => "my err");
    wait;
  end process;
end behav;

