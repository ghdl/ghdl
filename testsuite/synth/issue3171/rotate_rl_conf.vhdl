configuration rotate_RL_conf of rotate_byte is
  for arch
    for rotate_RL_instanc : rotate_RL
      use entity work.rotate_RL
        generic map (
          right_not_left => true);
    end for;
  end for;
end configuration rotate_RL_conf;

