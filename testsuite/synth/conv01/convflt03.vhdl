entity convflt03 is
  port (din : integer);
end;

architecture behav of convflt03 is
begin
  assert real(din) > 1.0;
end;
