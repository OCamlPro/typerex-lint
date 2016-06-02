
type t = { id : int; message : string}

let my_fun () =
  ignore (1 == 1);
  ignore ("toto" == "titi");
  ignore ((1, 2) == (1, 2));
  ignore (Some (1,2) == Some (1, 2));
  ignore ({id = 1; message = "message 1"} == {id = 1; message = "message 1"});
  ignore ([|1; 2; 3;|] == [|1; 2; 3|])
