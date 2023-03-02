
let dfs neigbours start =
  let rec rdfs visited node =
    if not (List.mem node visited) then
        let s = neigbours node  in
        List.fold_left rdfs (node::visited) s
    else visited
  in rdfs [] start
