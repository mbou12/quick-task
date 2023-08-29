type task = {
    mutable tid : int;
    name : string;
}
;;

let init_task =
    {
        tid = 0;
        name = "default";
    }
;;

let task_of_string (str : string) : task = 
    let sl = (String.split_on_char ':' str) in
    {
        tid = Stdlib.int_of_string @@ List.hd sl;
        name = List.hd @@ List.tl sl;
    }
;;

let string_of_task (t : task) : string = (string_of_int t.tid) ^ ":" ^ t.name

let pp_task (t : task) (cur : bool) : string =
    if cur = true then
        "[Cur] " ^ t.name
    else
        "[" ^ string_of_int t.tid ^ "] " ^ t.name
;;

let read_tasks (f : string) : task list =
    let ic = Stdlib.open_in f in
    let rec aux (ic : in_channel) : task list =
        try
            let line = Stdlib.input_line ic in
            let t = task_of_string line in
            t::(aux ic)
        with 
            (* close input channel, ignore errors *)
            | End_of_file -> close_in_noerr ic; []
    in
    aux ic
;;

let write_tasks (f : string) (tl : task list) : unit = 
    let oc = Stdlib.open_out f in
    List.iter (fun t -> Stdlib.output_string oc @@ (string_of_task t) ^ "\n") tl
;;

let display_cur_task (tl : task list) (b : bool ref) : unit = 
    if !b = true then
        match tl with
            | [] -> Printf.printf "No current task!\n"
            | t::ts -> 
                Stdlib.output_string Stdlib.stdout @@
                (pp_task t true) ^ "\n"
    else ()
;;

let display_all_tasks (tl : task list) (b : bool ref) : unit =
    if !b = true then
        match tl with
            | [] -> Printf.printf "Finished all tasks!\n"
            | t::ts -> 
                Stdlib.output_string Stdlib.stdout @@
                (pp_task t true) ^ "\n";
                List.iter (fun t -> 
                    Stdlib.output_string Stdlib.stdout @@
                    (pp_task t false) ^ "\n") ts
    else ()
;;

let number_completed (f : string) : int =
    let ic = Stdlib.open_in f in
    let rec aux (n : int) : int =
        try
            let _ = Stdlib.input_line ic in
            aux (n+1)
        with 
            (* close input channel, ignore errors *)
            | End_of_file -> close_in_noerr ic; n
    in
    aux 1
;;

let update_tasks (tl : task list) : task list =
    let rec aux (tl : task list) (id : int) =
        match tl with
            | [] -> []
            | t::ts -> t.tid <- id+1; 
                t::(aux ts (id+1))
    in
    aux tl 0
;;

let complete_task (f : string) (tl : task list) (b : bool ref) : task list =
    if !b = true then
        let cid = number_completed f in
        let oc = Stdlib.open_out_gen [Open_append;] 222 f in
        match tl with
            | [] -> [] 
            | t::ts -> t.tid <- cid;
                    Stdlib.output_string oc @@ (string_of_task t) ^ "\n"; 
                    update_tasks ts
    else tl
;;

let create_task (tl : task list) (b : bool ref) : task list =
    match !b with
        | true -> 
            Stdlib.output_string stdout "Enter name of new task: \n";
            let name = Stdlib.read_line () in
            let t = task_of_string ("0:" ^ name) in
            let tl = t::tl in 
            update_tasks tl
        | false -> tl
;;

let filter_task (tl : task list) (t : task) : task list =
  let p = (fun x -> x.name <> t.name && x.tid <> t.tid) in
  List.filter p tl
;;

let rec find_task (tl : task list) (t : task) : task option =
  match tl with
  | [] -> None
  | x::xs -> if t.name = x.name || t.tid = x.tid then Some x else
    find_task xs t
;;

let get_task (tl : task list) (choice : string) : task option =
      begin match choice with
        | "name" -> 
            let () = Stdlib.output_string stdout "Enter name of task!\n" in
            let name = Stdlib.read_line () in
            find_task tl ({ name; tid = -1 })
        | "tid" -> 
            let () = Stdlib.output_string stdout "Enter tid of task!\n" in
            let tid = Stdlib.read_line () in
            find_task tl ({ name = ""; tid = int_of_string tid })
        | _ -> None
      end
;;

let remove_task (tl : task list) (b : bool ref) : task list =
    match !b with
    | true -> 
      let () = Stdlib.output_string stdout "Remove name or tid? (name/tid)\n" in
      let choice = Stdlib.read_line () in
      begin match (get_task tl choice) with
        | None -> let () = Stdlib.output_string stdout "Invalid Input!\n" in
          tl
        | Some t -> update_tasks @@ filter_task tl t
      end
    | false -> tl
;;

let push_task (tl : task list) (b : bool ref) : task list =
    match !b with
    | true -> 
      let () = Stdlib.output_string stdout "Push name or tid? (name/tid)\n" in
      let choice = Stdlib.read_line () in
      begin match (get_task tl choice) with
        | None -> let () = Stdlib.output_string stdout "Invalid Input!\n" in
          tl
        | Some t -> let tl = filter_task tl t in
          update_tasks (t::tl)
      end
    | false -> tl
;;

let () =
    let loc = Sys.getenv "QTWD" in
    let task_in = loc ^ "/doc/task.txt" in
    let task_out = loc ^ "/doc/completed.txt" in
    if Sys.file_exists task_in && Sys.file_exists task_out then
        let tl = read_tasks task_in in

        let complete = ref false in
        let display_cur = ref false in
        let display_all = ref false in
        let new_task = ref false in
        let remove = ref false in
        let push = ref false in

        let speclist = [
            ("-c", Arg.Set complete, "complete current task");
            ("-dc", Arg.Set display_cur, "display curernt task");
            ("-da", Arg.Set display_all, "display all tasks");
            ("-n", Arg.Set new_task, "create a new task");
            ("-r", Arg.Set remove, "remove a task");
            ("-p", Arg.Set push, "push a task");
        ] in

        let anon_fun = fun s -> Printf.printf "Ivalid argument: %s\n" s in
        let usage_msg = "quicktask [OPTION]" in

        Arg.parse speclist anon_fun usage_msg;

        let tl = update_tasks tl in
        let tl = remove_task tl remove in
        let tl = create_task tl new_task in
        display_cur_task tl display_cur;
        display_all_tasks tl display_all;
        let tl = complete_task task_out tl complete in
        let tl = push_task tl push in
        write_tasks task_in tl
    else
        Stdlib.output_string stdout "Unable to find task files!\n" 
;;
