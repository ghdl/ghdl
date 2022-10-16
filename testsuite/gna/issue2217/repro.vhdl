entity repro is
end entity repro;

architecture beh of repro is
  procedure enable_log_msg(variable msg_id_panel : boolean_vector)is begin
  end procedure;

  procedure enable_log_msg(msg       : string)is begin
  end procedure;
begin
  enable_log_msg("Test test");
end architecture beh;
