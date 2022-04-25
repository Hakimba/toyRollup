module type MTreeContext = sig
  type data
  type digest
  val hash : data -> digest
  val concat_hashes : digest -> digest -> digest
  val equal : digest -> digest -> bool
  val to_string : digest -> string
end

module type MerkleTree = functor (Ctx : MTreeContext) ->
sig
  type t
  type proof
  val mtree_of_txs : Ctx.data list -> t
  val witness : Ctx.data -> t -> proof
  val proof : Ctx.data -> proof -> t -> bool
  val equal : t -> t -> bool
  val to_string : t -> string
end

module MerkleTree (Ctx : MTreeContext) = struct
  type mtree = Leaf of Ctx.digest | Node of Ctx.digest * mtree * mtree
  type t = Empty | Mtree of mtree 
  type direction = L | R
  type proof = (Ctx.digest * direction) list

  let get_hash = function Node (h, _, _) | Leaf h -> h

  let mtree_of_txs txs = match txs with 
    | [] -> Empty
    | txs' -> 
    let hash_of_txs = List.map (function tx -> Leaf(Ctx.hash tx)) txs' in
    let rec combine_nodes hs = match hs with
        | [Node(_,_,_)] as nd -> nd
        (* To manage odd leaf (for example the 5th leaf of a list of 5 leafs as "txs" parameter)
           i transform it as a node, duplicate leafs and concat the same hashes*)
        | [Leaf(h)] -> [Node((Ctx.concat_hashes h h),Leaf(h),Leaf(h))]
        | (Leaf(h1) as left) :: (Leaf(h2) as right) :: tl -> let new_hash = Ctx.concat_hashes h1 h2 in 
                                        Node(new_hash,left,right) :: (combine_nodes tl)
        | (Node(h1,_,_) as left) :: (Node(h2,_,_) as right) :: tl -> let new_hash = Ctx.concat_hashes h1 h2 in 
                                        Node(new_hash,left,right) :: (combine_nodes tl)
        | _ -> []
    in let rec create_mtree mtree = match mtree with
          | [root] -> Mtree root
          | mt -> create_mtree (combine_nodes mt)
    in create_mtree (combine_nodes hash_of_txs)

  let witness lf mtree = match mtree with
    | Empty -> failwith "empty merkle tree"
    | Mtree mtree' ->
    let leaf = Leaf (Ctx.hash lf) in
    let rec loop = function
     (* its the content we want to know the minimal merkle tree of, which can prove it
        so we start to draw the path with "true" *)
     | Leaf h when (Ctx.equal h (get_hash leaf)) -> true, []
     | Leaf _ -> false, []
     | Node (_, l, r) -> (
       let (l_r,proof_l) = loop l in
       let (r_r,proof_r) = loop r in
         match (l_r, r_r) with
         | (true, false) ->
           (* if our value come from the left, we need the right hash to compute the next parent
              but we need to capture that our value was in the left, for knowing in what order
              use concat_hashes in proof function. 
              Because concat_hashes h1 h2 != concat h2 h1 if h1 and h2 are different *)
           true, ((get_hash r), L) :: (proof_l @ proof_r)
         | (false, true) ->
           true, ((get_hash l), R) :: (proof_l @ proof_r)
        (*** case where we try to prove an odd node in the tree, 
             it is duplicated so we can find twice the same leaf,
             we take one arbitrarily *)
         | (true,true) -> 
           true, ((get_hash l), R) :: (proof_l @ proof_r)
         | _ -> false, proof_l @ proof_r )
   in
   loop mtree' |> function (false, _) -> failwith "proof not found" | (true,proof) -> List.rev proof

  let proof data proof mtree = match mtree with
    | Empty -> false
    | Mtree mtree' -> match proof with
      | [] -> false
      | _ -> 
      let concat_with_dir h1 proofNode = match proofNode with
        | (h,R) -> Ctx.concat_hashes h h1
        | (h,L) -> Ctx.concat_hashes h1 h
      in let root = List.fold_left concat_with_dir (Ctx.hash data) proof
      in Ctx.equal root (get_hash mtree')

  let equal m1 m2 = match (m1,m2) with
    | (Empty,Empty) -> true
    | (Mtree mt,Mtree mt') -> 
        let rec equal_aux = function
        | (Leaf h,Leaf h') -> Ctx.equal h h'
        | (Node(h,l,r),Node(h',l',r')) -> Ctx.equal h h' && (equal_aux (l,l')) && (equal_aux (r,r'))
        | _ -> false
        in equal_aux (mt,mt')
    | _ -> false

  let to_string mtree = match mtree with
    | Empty -> ""
    | Mtree mt -> 
      let rec to_string_aux m space = match m with
        | Leaf(h) -> String.concat "" [space;Ctx.to_string h;"\n"]
        | Node(h,l,r) -> 
          let l_str = to_string_aux l (String.concat "" ["  ";space]) in
          let r_str = to_string_aux r (String.concat "" ["  ";space]) in
          String.concat "" [space;Ctx.to_string h;"\n";l_str;r_str]
      in to_string_aux mt ""
end