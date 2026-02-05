configuration top_level_conf of top_level is

  for arch
    for rotate_byte_instanc : rotate_byte
      use configuration work.rotate_RL_conf;
    end for;
  end for;

end configuration top_level_conf;
