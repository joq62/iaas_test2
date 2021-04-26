{application, iaas,
 [{description, " iaas"},
  {vsn, "1.0.0"},
  {modules, [iaas_app,iaas_sup,iaas]},
  {registered, [iaas]},
  {applications, [kernel, stdlib]},
  {mod, {iaas_app, []}},
  %% Joq Erlang specific
  {nothing,glurk}
  ]}.
