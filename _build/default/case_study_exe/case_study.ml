(* Thin wrapper to run the case-study analysis via the shared CLI logic *)

let () =
  let base_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "case studies"
  in
  Case_studies.run base_path
