(* ============================================================
   Bin Packing — Heuristics + exact Branch & Bound solver
   Prep-school coursework (Lycée Montaigne, CPGE MPI, 2024-2025)
   ============================================================ *)

(********************************* Greedy heuristics **************************************)

(* An instance is a bin capacity together with the list of item sizes to pack *)
type instance = int * int list;;




(* A box (bin) tracks how much room is left and which items it currently holds *)
type box =
  {
    mutable size    : int;         (* remaining capacity *)
    mutable content : int list;    (* items placed so far *)
  }
;;




(* Places one item into the list of boxes in O(1), thanks to the list structure:
   only the most recently opened box is ever touched *)
let put_item item boxes capacity =
  match boxes with
  |[]   -> let box  = {size = capacity - item; content = [item]} in (* no box opened yet *)
           box :: boxes
  |t::q -> if t.size >= item then  (* the last opened box has enough room *)
             (
               t.size <- t.size - item;
               t.content <- item :: t.content;
               t::q
             )

           else     (* the last opened box cannot hold the item: open a new one *)
             (
               let box  = {size = capacity - item; content = [item]} in
               box :: boxes
             )
;;



(* quick check *)
put_item 4 (put_item 3 (put_item 5 [] 10) 10) 10;;


(* Next-Fit: goes through every item once and always tries the last opened box.
   O(1) per item, so O(n) overall *)
let next_fit (i : instance) : box list =
  let (capacity, items) = i in
  let rec aux l boxes =
    match l with
    |[]   -> boxes
    |t::q -> let new_boxes = put_item t boxes capacity in
             aux q new_boxes
  in
  aux items []
;;



(* quick check *)
next_fit (10, [2; 5; 4; 7; 1; 3; 8]);; (* [[2; 5]; [4]; [7; 1]; [3]; [8]] *)






(* First-Fit: same idea as put_item, but if the last box doesn't fit the item,
   we recursively look further back through every previously opened box *)
let rec put_item_rec item boxes capacity =
  match boxes with
  |[]   -> let box  = {size = capacity - item; content = [item]} in
           box :: boxes
  |t::q -> if t.size >= item then
             (
               t.size <- t.size - item;
               t.content <- item :: t.content;
               t::q
             )

           else
             t :: (put_item_rec item q capacity)
;;



(* worst case: one box per item, and every box is checked for every item -> O(n^2) *)
let first_fit (i : instance) : box list =
  let (capacity, items) = i in
  let rec aux l boxes =
    match l with
    |[]   -> boxes
    |t::q -> let new_boxes = put_item_rec t boxes capacity in
             aux q new_boxes
  in
  aux items []
;;

first_fit (10, [2; 5; 4; 7; 1; 3; 8]);; (* [[1; 5; 2]; [3; 4]; [7]; [8]] *)





(* Decreasing merge sort, used to pre-sort items from largest to smallest *)
let rec split l =
  match l with
  |[]        -> [], []
  |[a]       -> [a], []
  |t1::t2::q -> let l1, l2 = split q in
                t1::l1, t2::l2
;;

let rec merge_decreasing l1 l2 =
  match l1, l2 with
  |[], l2                       -> l2
  |l1, []                       -> l1
  |t1::q1, t2::q2 when t1 <= t2 -> t2 :: (merge_decreasing l1 q2)
  |t1::q1, t2::q2               -> t1 :: (merge_decreasing q1 l2)
;;

let rec merge_sort_decreasing l =
  match l with
  |[]  -> []
  |[a] -> [a]
  |_   -> let l1, l2 = split l in
          merge_decreasing (merge_sort_decreasing l1) (merge_sort_decreasing l2)
;;


merge_sort_decreasing [2; 5; 4; 7; 1; 3; 8];;



(* Sorting costs O(n log n), negligible next to the O(n^2) of first_fit,
   so First-Fit-Decreasing is still O(n^2) overall *)
let first_fit_decreasing (i :instance) : box list =
  let (capacity, items) = i in
  let o = merge_sort_decreasing items in
  first_fit (capacity, o)
;;

first_fit_decreasing (10, [2; 5; 4; 7; 1; 3; 8]);; (* [[2; 8]; [3; 7]; [1; 4; 5]] *)






(* Decreasing merge sort over boxes, ordered by remaining size *)
let rec boxes_split (l : box list) : box list * box list =
  match l with
  |[]        -> [], []
  |[a]       -> [a], []
  |t1::t2::q -> let l1, l2 = boxes_split q in
                t1::l1, t2::l2
;;

let rec merge_boxes_decreasing (l1 : box list) (l2 : box list) : box list =
  match l1, l2 with
  |[], l2                                 -> l2
  |l1, []                                 -> l1
  |t1::q1, t2::q2 when t1.size <= t2.size -> t2 :: (merge_boxes_decreasing l1 q2)
  |t1::q1, t2::q2                         -> t1 :: (merge_boxes_decreasing q1 l2)
;;

let rec merge_sort_boxes_decreasing (l : box list) : box list =
  match l with
  |[]  -> []
  |[a] -> [a]
  |_   -> let l1, l2 = boxes_split l in
          merge_boxes_decreasing (merge_sort_boxes_decreasing l1) (merge_sort_boxes_decreasing l2)
;;


merge_sort_boxes_decreasing [{size = 2; content = [8]}; {size = 7; content = [3]}; {size = 2; content = [1; 7]}; {size = 6; content = [4]}; {size = 3; content = [5; 2]}];;

let best_first_fit (i : instance) : box list =
  let (capacity, items) = i in
  let rec aux l boxes =
    match l with
    |[]   -> boxes
    |t::q -> let new_boxes = put_item_rec t boxes capacity in
             aux q (merge_sort_boxes_decreasing new_boxes)
  in
  aux items []
;;

(* Same idea as First-Fit-Decreasing, but the boxes are re-sorted by remaining
   size after every insertion (merge sort keeps this O(n log n) overall) *)
let best_fit_decreasing (i :instance) : box list =
  let (capacity, items) = i in
  let o = merge_sort_decreasing items in
  best_first_fit (capacity, o)
;;

best_fit_decreasing (10, [2; 5; 4; 7; 1; 3; 8]);; (* [[2; 8]; [3; 7]; [1; 4; 5]] *)






(********************************************* Exact resolution *******************************************)

(* Branch & Bound: intelligently enumerate every possible solution.
   - each node represents one step in building a solution
   - the root (depth 0) starts by placing the first item in the first box
   - a node at depth i has two children: the right child adds item i+1 to the
     current box, the left child leaves the current box untouched and defers
     that item
   - once every item has been considered for a box, we move on to the next
     box. With 4 items, the rightmost "comb" of the tree holds the nodes
     {1}, {1,2}, {1,2,3}, {1,2,3,4}, and the leftmost comb holds {1}, {1},
     {1}, {1}, then {{1},{2}}, and so on

   The tree size is exponential in the number of items, so it cannot be built
   in full. We use lower and upper bounds to prune it:
   - the lower bound is a minimum possible value of the objective function.
     Here, it is the total remaining volume divided by the box capacity.
   - the upper bound is a maximum acceptable value of the objective function.
     Here, the best solution must not exceed the value found by
     best_fit_decreasing.

   At each node we can compute a lower bound: the number of boxes already
   filled plus the total weight of the remaining items divided by the maximum
   weight a box can hold. From a node and its lower bound, we know that no
   solution below that node can beat this bound. If, at a given node, the
   lower bound is greater than or equal to the upper bound, there is no point
   exploring its children: the search tree is pruned at that node. *)

(***** tree type for the Branch & Bound search *****)

(* Empty is the empty tree, Pruned marks a branch that was cut off *)
type tree = Node of box list * tree * tree | Empty | Pruned;;



(***** helper functions *****)

(* sum and max of an int list *)
let rec list_sum (l : int list) : int =
  match l with
  |[]   -> 0
  |t::q -> t + (list_sum q)
;;

list_sum [1; 2; 3];;

let rec list_max (l : int list) : int =
  match l with
  |[]   -> 0
  |t::q -> max t (list_max q)
;;

list_max [1; 4; 7; 3; 2];;

let rec list_max_sum (l : int list) : int * int=
  match l with
  |[]   -> (0, 0)
  |t::q -> let (m, s) = list_max_sum q in
           (max t m, t + s)
;;

list_max_sum [1; 4; 7; 3; 2];;



(***** Lower and upper bounds *****)

(* is_global chooses between the global lower bound (true) and the local,
   per-node lower bound (false) *)
let compute_lower_bound (is_global : bool) (a : tree) (i : instance) : int =
  let capacity, items = i in
  let Node(boxes, _, _) = a in
  match is_global with
  |true  -> let box::_ = boxes in
            let sum = (list_sum items) + (list_sum box.content) in
            sum / capacity
  |false -> let sum = list_sum items in
            (List.length boxes) + (sum / capacity)
;;

compute_lower_bound true (Node([{size = 4; content = [1; 2; 3]}], Empty, Empty)) (10, [4; 5]);;
compute_lower_bound false (Node([{size = 4; content = [1; 2; 3]}], Empty, Empty)) (10, [4; 5]);;

(* upper bound: the number of boxes used by the best heuristic found so far *)
let compute_upper_bound (i : instance) : int =
  List.length (best_fit_decreasing i)
;;

compute_upper_bound (10, [1; 2; 3; 4; 5]);;
compute_upper_bound (101, [27; 11; 41; 43; 42; 54; 34; 11; 2; 1; 17; 56; 42; 24; 31; 17; 18; 19; 24; 35; 13; 17; 25]);;



(***** Building and walking the search tree *****)

(* pruning *)

(* true when the lower bound is strictly greater than the upper bound *)
let should_prune (upper_bound : int) (is_global : bool) (a : tree) (i : instance) =
  let lb = compute_lower_bound is_global a i in
  (lb > upper_bound)
;;

should_prune 2 false (Node([{size = 4; content = [1; 2; 3]}], Empty, Empty)) (10, [4; 5]);;



(* builds the solution tree *)

(*
   - with n items, once we reach depth n we move on to the next box, and
     there is only a right child (adding an item). depth_state is a pair
     holding the current depth modulo n and n itself. Note the target depth
     changes every time a box is closed.
   - the left child keeps the same state and skips the current item.
     However the item is not lost: it is dealt with after every other item.
*)
let rec build_tree (a : tree) (items : int list) (capacity : int) (upper_bound : int) (is_global : bool) (depth_state : int * int) : tree =
  match (items, depth_state) with
  |([], _)                                 -> a

  |(item::remaining_items, (depth, target_depth)) when depth <> target_depth ->
                                              let Node(boxes, Empty, Empty) = a in
                                              let box::remaining_boxes = boxes in

                                              (* pruning *)
                                              if should_prune upper_bound is_global a (capacity, items) then
                                                Node(boxes, Pruned, Pruned)

                                              (* the item does not fit in the current box *)
                                              else if box.size - item < 0 then
                                                let left_child = build_tree a (remaining_items @ [item]) capacity upper_bound false (depth+1, target_depth) in
                                                let right_child = Pruned in
                                                Node(boxes, left_child, right_child)

                                              (* no pruning *)
                                              else
                                                let left_child = build_tree a (remaining_items @ [item]) capacity upper_bound false (depth+1, target_depth) in
                                                let right_child = build_tree (Node(({size = box.size - item; content = item::box.content})::remaining_boxes, Empty, Empty)) remaining_items capacity upper_bound false (depth+1, target_depth) in
                                                Node(boxes, left_child, right_child)

  |(item::remaining_items, (depth, target_depth)) when depth = target_depth ->
                                             let Node(boxes, Empty, Empty) = a in

                                             (* pruning *)
                                             if should_prune upper_bound is_global a (capacity, items) then
                                               Node(boxes, Empty, Pruned)

                                             (* no pruning *)
                                             else
                                               let box = {size = capacity - item; content = [item]} in
                                               let new_target_depth = List.length items - 1 in
                                               let right_child = build_tree (Node(box::boxes, Empty, Empty)) remaining_items capacity upper_bound false (0, new_target_depth) in
                                               Node(boxes, Empty, right_child)

;;

build_tree (Node([{size = 10 - 1; content = [1]}], Empty, Empty)) [] 10 1 true (0, 0);; (* 1-item case *)
build_tree (Node([{size = 10 - 1; content = [1]}], Empty, Empty)) [2] 10 1 true (0, 1);; (* 2-item case *)
build_tree (Node([{size = 10 - 1; content = [1]}], Empty, Empty)) [2; 3] 10 1 true (0, 2);; (* 3-item case *)


(* collects every leaf that is a feasible (non-pruned) solution *)

let rec list_solutions (a : tree) : tree list =
  match a with
  |Empty |Pruned                        -> []
  |Node(_, Pruned, Pruned)              -> []
  |Node(_, Empty, Empty)                -> [a]
  |Node(_, Empty, b) |Node(_, Pruned, b) -> list_solutions b
  |Node(_, b, Empty) |Node(_, b, Pruned) -> list_solutions b
  |Node(_, b, c)                        -> (list_solutions b) @ (list_solutions c)
;;

list_solutions (build_tree (Node([{size = 10 - 1; content = [1]}], Empty, Empty)) [2; 3] 10 1 true (0, 2));;

(* picks the solution using the fewest boxes among the feasible leaves *)

let rec find_best_solution (solutions : tree list) : box list =
  match solutions with
  |[]                     -> []
  |Empty::q |Pruned::q    -> find_best_solution q
  |[Node(boxes, _, _)]    -> boxes
  |Node(boxes1,_, _)::q   -> let boxes2 = find_best_solution q in
                             if (List.length boxes1) >= (List.length boxes2) then boxes2
                             else boxes1
;;

find_best_solution (list_solutions (build_tree (Node([{size = 10 - 1; content = [1]}], Empty, Empty)) [2; 3] 10 1 true (0, 2)));;



(***** Top-level function ******)

let solve (i : instance) : box list =
  let capacity, items = i in
  let upper_bound = compute_upper_bound i in
  let is_global = false in

  let item::q = items in
  let root = Node([{size = capacity - item; content = [item]}], Empty, Empty) in
  let a = build_tree root q capacity upper_bound is_global (0, List.length items - 1) in

  let solutions = list_solutions a in
  find_best_solution solutions
;;


solve (10, [1; 2; 3]);;
solve (10, [2; 5; 4; 7; 1; 3; 8]);;

(* Known limitation: on larger/adversarial instances (e.g. 23 items, capacity
   101) the search tree grows too fast for the pruning to keep up, and
   `solve` can take a very long time or exhaust memory. The heuristics above
   stay fast regardless of instance size; only the exact solver is affected. *)



(********************************************* Demo *******************************************)

let print_boxes (boxes : box list) : unit =
  List.iteri (fun idx b -> Printf.printf "  Box %d: %s\n" (idx+1)
                (String.concat ", " (List.map string_of_int b.content)))
    (List.rev boxes)
;;

let () =
  let instance = (10, [2; 5; 4; 7; 1; 3; 8]) in
  Printf.printf "Instance: capacity = 10, items = [2; 5; 4; 7; 1; 3; 8]\n\n";

  Printf.printf "Next-Fit (%d boxes):\n" (List.length (next_fit instance));
  print_boxes (next_fit instance);

  Printf.printf "\nFirst-Fit-Decreasing (%d boxes):\n" (List.length (first_fit_decreasing instance));
  print_boxes (first_fit_decreasing instance);

  Printf.printf "\nBest-Fit-Decreasing (%d boxes):\n" (List.length (best_fit_decreasing instance));
  print_boxes (best_fit_decreasing instance);

  Printf.printf "\nExact solution via Branch & Bound (%d boxes):\n" (List.length (solve instance));
  print_boxes (solve instance)
;;
