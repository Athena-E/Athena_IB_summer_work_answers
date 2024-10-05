let lex s = 
  let lex' sl = 
    let sl' = ref sl in 
    let tks = ref [] in 
    while List.length !sl' > 0 do 
      let sym = ref "" in 
      let count = ref 0 in 
      let a_pos = ref (-1) in  (*last accepted token position in string*) 
      let q = ref FAIL in 
      while !count <= List.length !sl' do 
        match is_token !sym with 
        | true -> 
            a_pos := !count; 
            q := ACCEPT; 
            count := !count + 1; 
            sym := implode (take !count !sl'); 
        | false -> 
            count := !count + 1; 
            sym := implode (take !count !sl'); 
      done; 
      match !q with 
      | ACCEPT -> 
          let a_sym = implode (take !a_pos !sl') in 
          let tk = get_token a_sym in 
          tks := tk :: !tks; 
          sl' := drop (!a_pos) !sl';
          count := 0;
          a_pos := (-1);
      | FAIL -> raise InvalidToken; 
    done; 
    List.rev !tks 
  in 
  let sl = explode s in 
  lex' sl ;;
