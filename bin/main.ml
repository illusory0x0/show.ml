module Show = struct
  class type show_type = object
    method output : Format.formatter -> unit
  end

  class virtual show =
    object
      method virtual output : Format.formatter -> unit
    end

  class int (value : Int.t) =
    object
      inherit show
      method output fmt = Format.pp_print_int fmt value
    end

  class float (value : Float.t) =
    object
      inherit show
      method output fmt = Format.pp_print_float fmt value
    end

  class ['a] list (value : 'a List.t) =
    object
      constraint 'a = show_type
      inherit show

      method output fmt =
        let pp_sep fmt _ = Format.pp_print_string fmt ";" in
        Format.pp_print_string fmt "[";
        Format.pp_print_list ~pp_sep
          (fun fmt (value : 'a) -> value#output fmt)
          fmt value;
        Format.pp_print_string fmt "]"
    end
end

module Test = struct
  open! Show

  let x = new int 0

  let _ =
    let xs = new list [ new int 1; new int 2; new int 3; new int 4 ] in
    xs#output Format.std_formatter;
    Format.pp_print_newline Format.std_formatter ();

    let xs =
      new list [ new float 0.1; new float 0.2; new float 0.3; new float 0.4 ]
    in
    xs#output Format.std_formatter;
    Format.pp_print_newline Format.std_formatter ();

    ()
end
