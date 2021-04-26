{application, compute,
 [{description, " compute"},
  {vsn, "1.0.0"},
  {modules, [compute_app,compute_sup,compute]},
  {registered, [compute]},
  {applications, [kernel, stdlib]},
  {mod, {compute_app, []}},
  %% Joq Erlang specific
  {affinity,[{host,[any_host,host1,host2]},{capability,cap1},{zone,z2}]},
  {needed_apps,[{app1,vsn}]}
 ]}.
