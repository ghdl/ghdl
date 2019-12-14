entity repro is
end entity;

architecture tb of repro is

  type tb_cfg_t is record
    value : string;
  end record tb_cfg_t;

  constant tb_cfg: tb_cfg_t := ( value => "hello" );
begin
  assert tb_cfg.value > "a";
end tb;


