let foo foo = (* Inside top-level let-binding *)
  let _ = foo (* (multiple) let bindings *)
  and _ = foo
  in
  ignore foo; (* Simple var *)
  ignore (Some foo); (* Inside constructor *)
  ignore ((fun _ -> foo) foo); (* Inside fun definition && application *)
  function (* Inside function body and guards *)
  | _ when foo -> foo
  | _ -> foo
