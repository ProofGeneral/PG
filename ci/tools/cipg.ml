(* This file is part of Proof General.
 *
 * Copyright 2024  Hendrik Tews
 *
 * Authors:   Hendrik Tews
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)


(* PG repo *)
let pg_repo = ref "../.."

(* file containing the Coq/Emacs/Debian/Ubuntu release table, relative
   to pg_repo
 *)
let release_table_file = "ci/doc/coq-emacs-releases.org"

(* CI documentatioin README.md, relative to pg_repo *)
let readme_file = "ci/doc/README.md"

(* github yaml workflow file *)
let test_workflow_file = ".github/workflows/test.yml"

(* directory in which the coq-nix-docker and coq-emacs-docker repos are *)
let src_dir = ref "~/src"

(* file containing the currently needed coq-nix containers *)
let currently_used_nix_file = "ci/doc/currently-used-coq-nix-versions"

(* file containing the currently used coq-emacs containers *)
let currently_used_coq_emacs_file = "ci/doc/currently-used-coq-emacs-versions"


(* Number of years for which to build containers for all coq versions
   released since then.
 *)
let coq_full_range_years = 2.0

(* Number of years for which to run old coq versions in PG CI.
 *)
let coq_partial_range_years = 1.5

(* Number of years for which containers are build for historic pairs.
   Those historic pairs that are older than the oldest LTS version,
   are kept as passively supported historic pairs. *)
let passive_historic_pairs_years = 6.0


(* Number of month added to EOL dates. E.g., when EOL says 2023/04
   then relax_month=1 keeps it until 2023/05 *)
let eol_relax_month = 0.0

(* print stuff for container building *)
let do_containers = ref false

(* print stuff for updating PG CI *)
let print_pg_ci_config = ref false

(* update PG CI doc and config*)
let do_pg_ci_update = ref false

(* delete superfluous containers *)
let do_delete_containers = ref false


(*****************************************************************************
 *
 * translate time ranges into seconds
 *
 *****************************************************************************)

let years_to_secs y = y *. 365.2425 *. 24.0 *. 3600.0

let coq_full_range_secs = years_to_secs coq_full_range_years

let coq_partial_range_secs = years_to_secs coq_partial_range_years

let passive_historic_pairs_secs =
  years_to_secs passive_historic_pairs_years

let eol_relax_seconds = eol_relax_month *. 31. *. 24.0 *. 3600.0

                    
(*****************************************************************************
 *
 * types
 *
 *****************************************************************************)

type date = float

(* Coq and Emacs versions from the release table. *)
type version = {
    major : int;
    minor : int;
    patch : int option;
    release_candidate : bool;
  }

let null_version = {major = 0; minor = 0; patch = None;
                    release_candidate = false; }

(* record for lines of the Coq/Emacs/Debian/Ubuntu release table *)
type release_record = {
    rel_date : date;
    rel_coq : version option;
    rel_emacs : version option;
    rel_lts : string;                   (* "" if no Debian/Ubuntu release *)
    rel_eol : date option;
    rel_historic : bool;
  }

(* record for Debian/Ubuntu releases *)
type lts_record = {
    lts_coq : version;
    lts_emacs : version;
    lts_name : string;
    eol_date : date;
  }

(* enumeration type for elements of an container matrix *)
type matrix_element =
  | Unused
  | Lts                                 (* needed for Debian/Ubuntu release *)
  | Latest_versions_complete            (* needed for recent versions *)
  | History_pair                        (* needed for historic pair *)
  | Newest                              (* needed for a newest Coq or Emacs *)
  | RC                                  (* needed for release candidate *)


(*****************************************************************************
 *
 * library functions
 *
 *****************************************************************************)

(* last element of non-empty lists *)
let rec list_last = function
  | [] -> assert false
  | a :: [] -> a
  | a :: l -> list_last l

(* l1 \ l2 : returns elements of l1 not in l2 *)
let list_diff l1 l2 =
  List.filter
    (fun x1 -> not (List.mem x1 l2))
    l1

(* l1 \cap l2 : returns only those elements of l1 that are in l2 *)
let list_intersection l1 l2 =
  List.filter
    (fun x1 -> List.mem x1 l2)
    l1

(* take a list of pairs, sorted for the first element and return a
 * list in which all second elements, which were associated to one
 * first element are collected in a list.
 * [(0, a); (0, b); (1, c); ...] -> [(0, [a; b]); (1, [c; ...]); ...]
 *)
let pairs_to_keyed_list l =
  let rec doit cur_left acc_right res = function
    | [] -> List.rev_map (fun (a, l) -> (a, List.rev l))
              ((cur_left, acc_right) :: res)
    | (a, b) :: l ->
       if cur_left = a
       then doit cur_left (b :: acc_right) res l
       else doit a [b] ((cur_left, acc_right) :: res) l
  in
  match l with
    | [] -> []
    | (a, b) :: l -> doit a [b] [] l

(*****************************************************************************
 *
 * general time functions
 *
 *****************************************************************************)

module U = Unix

(* Return tm record for year and month. Year must be a four digit year
 * number, month are real-live month numbers.
 *)
let tm_date year month = {
  U.tm_sec = 0;
  tm_min = 0;
  tm_hour = 12;
  tm_mday = 15;
  tm_mon = month - 1;
  tm_year = year - 1900;
  tm_wday = 0;
  tm_yday = 0;
  tm_isdst = false;
}

let normalize_date date = fst (U.mktime date)

(* return date as float for year and month *)
let norm_tm_date year month = normalize_date (tm_date year month)


let today = U.time()

(* let today = norm_tm_date 2023 3 *)


(*****************************************************************************
 *
 * sorting
 *
 *****************************************************************************)

(* sort Debian/Ubuntu releases by their end-of-live date *)
let sort_lts = List.sort (fun a b -> compare a.eol_date b.eol_date)

(* compare version records, taking the release_candidate field into account *)
let compare_versions a b =
  let x = compare a.major b.major in
  if x = 0
  then
    let x = compare a.minor b.minor in
    if x = 0 then
      let x =
        match (a.patch, b.patch) with
          | (Some pa, Some pb) -> compare pa pb
          | (None, None) -> 0
          | (Some pa, None) -> compare pa 0
          | (None, Some pb) -> compare 0 pb
      in
      if x = 0
      then
        match (a.release_candidate, b.release_candidate) with
          | (true, false) -> -1
          | (false, true) -> 1
          | (true, true)
            | (false, false) -> 0
      else x
    else x
  else x

(* sort version records in ascending order *)
let sort_versions = List.sort compare_versions

let discard_outdated_patch_levels vl =
  let vl = List.rev (sort_versions vl) in
  let rec keep_last cur_major cur_minor res = function
    | {major; minor} as v :: vl ->
       if cur_major = major && cur_minor = minor
       then keep_last cur_major cur_minor res vl
       else keep_last major minor (v :: res) vl
    | [] -> res
  in
  match vl with
    | {major; minor} as v :: vl ->
       keep_last major minor [v] vl
    | [] -> []

(* sort vl in ascending order and return a list of (version, index) pairs *)
let sort_and_index_versions vl : (version * int) list =
  List.mapi (fun i vr -> (vr, i)) (sort_versions vl)

(* Sort a list of version pairs. In the result, the first element
 * increses and for equal first elements, the second element
 * increases.
 *)
let sort_version_pairs =
  List.sort
    (fun (a,b) (c,d) ->
      let x = compare_versions a c in
      if x = 0
      then compare_versions b d
      else x)


(*****************************************************************************
 *
 * parse table of Coq/Emacs/Debian/Ubuntu releases
 *
 *****************************************************************************)

(* string that marks the begin of the table *)
let table_start_prefix = "| date "

(* Skip in file ic to start of the body of the Coq/Emacs/Debian/Ubuntu
 * release table
 *)
let rec skip_to_table_start ic =
  let line = input_line ic in
  if String.starts_with ~prefix:table_start_prefix line
  then
    begin
      (* Printf.printf "XX found start\n%!"; *)
      ignore(input_line ic)
    end
  else
    begin
      (* Printf.printf "XX skip line %s\n%!" line; *)
      skip_to_table_start ic;
    end
      
(* scan a date of the form YYYY/mm *)
let scan_year_month_date s =
  if s = ""
  then None
  else
    Some (Scanf.sscanf s "%d/%d" norm_tm_date)

(* Scan a version in string s in the form <major>.<minor>[rc], where
   the [rc] is optional, or <major>.<minor>[.<patch>], where the patch
   level is optional. Return a version option, where None is used when
   s is empty. Trailing characters after <minor> are ignored, but if
   there is a dot, a patch level must follow <minor>. *)
let scan_version s =
  if s = ""
  then None
  else
    let s_ch = Scanf.Scanning.from_string s in
    Scanf.bscanf s_ch "%d.%d%n"
      (fun ma mi i ->
        let (rc, patch) =
          if i + 1 < String.length s
          then
            if String.sub s i 2 = "rc"
            then (true, None)
            else
              Scanf.bscanf s_ch "%c"
                (function
                 | '.' -> Scanf.bscanf s_ch "%d"
                            (fun patch -> (false, Some patch))
                 | _ -> (false, None)
                )
          else (false, None)
        in
        Some {major = ma; minor = mi;
              patch = patch;
              release_candidate = rc;
      })

(* Parse table of Coq/Emacs/Debian/Ubuntu releases in file ic,
 * accumulating the results in accu. Recognize the end of the table by
 * an empty line.
 *)
let rec parse_table_lines ic accu =
  let scanf_continuation date_s coq_s emacs_s lts_s eol_s hist_mark =
    (* Printf.printf "XX scan-cont\n%!"; *)
    let rel_date = match scan_year_month_date date_s with
        | None ->
           prerr_endline "Empty date column in version table!";
           assert false
        | Some d -> d
    in           
    let line_rec = {
        rel_date = rel_date;
        rel_coq = scan_version coq_s;
        rel_emacs = scan_version emacs_s;
        rel_lts = String.trim lts_s;
        rel_eol = scan_year_month_date eol_s;
        rel_historic = hist_mark <> ""
      }
    in
    parse_table_lines ic (line_rec :: accu)
  in        
  match input_line ic with
    | ""
    | exception End_of_file -> accu
    | line ->
       (* Printf.printf "XX parse line %s\n%!" line; *)
       Scanf.sscanf line "| %[^| ] | %[^| ] | %[^| ] | %[^|]| %[^| ] | %[^| ] |"
         scanf_continuation


(* returns the table of Coq/Emacs/Debian/Ubuntu releases as
 * release_record's in reverse order.
 *)
let parse_table () =
  let ic = open_in (Filename.concat !pg_repo release_table_file) in
  skip_to_table_start ic;
  let res = parse_table_lines ic [] in
  close_in ic;
  res


(*****************************************************************************
 *
 * collect lts, emacs and coq versions from table
 *
 *****************************************************************************)

(* Process the remainder of the Coq/Emacs/Debian/Ubuntu release table
   in form of release records. The arguments accumulate the information found.
   In particular
   - ltss: relevant Debian/Ubuntu releases
   - passive_hist: passively supported historic pairs
   - active_hist: actively supported historic pairs
   - coqs: actively and passively supported Coq versions
   - emacses: actively and passively supported Emacs versions
   See extract_lts_versions below for the return value.
 *)
let rec collect_remaining_versions
          ltss passive_hist active_hist coqs emacses = function
  | [] -> (ltss, passive_hist, active_hist, coqs, emacses)
  | { rel_date; rel_coq; rel_emacs; rel_lts; rel_eol; rel_historic } :: table ->
     let (this_coq, coqs) =
       match rel_coq with
         | None -> (List.hd coqs, coqs)
         | Some coqv ->
            (coqv, if List.mem coqv coqs then coqs else coqv :: coqs)
     in
     let (this_emacs, emacses) =
       match rel_emacs with
         | None -> (List.hd emacses, emacses)
         | Some emacs_v ->
            (emacs_v,
             if List.mem emacs_v emacses then emacses else emacs_v :: emacses)
     in
     let lts_maybe =
       match rel_eol with
         | None -> None
         | Some eol_date ->
            if eol_date +. eol_relax_seconds > today
            then
              Some { lts_coq = this_coq ;
                     lts_emacs = this_emacs;
                     lts_name = rel_lts;
                     eol_date = eol_date;
                }
            else None
     in
     let (passive_hist, active_hist) =
       match (lts_maybe, ltss) with
         | (Some _, []) -> (active_hist, [])
         | _ -> (passive_hist, active_hist)
     in
     let active_hist =
       if rel_historic
       then (this_coq, this_emacs) :: active_hist
       else active_hist
     in
     let ltss = match lts_maybe with
         | Some lts -> lts :: ltss
         | None -> ltss
     in
     collect_remaining_versions ltss passive_hist active_hist coqs emacses table

(* Extract relevant Debian/Ubuntu releases from the
 * Coq/Emacs/Debian/Ubuntu release table in form of release records.
 * Also extract the list of all supported Coq and Emacs versions and
 * two lists of historic versions pairs. The list of passively
 * supported historic pairs for old version testing before the first
 * relevant Debian/Ubuntu release and the list of actively supported
 * pairs starting with the first relevant Debian/Ubuntu release.
 * Return everything in a 5-tuple of
 * - Debian/Ubuntu releases as list of lts_records,
 * - passive_hist: passively supported historic pairs as Coq/Emacs version pairs
 * - active_hist: actively supported historic pairs as Coq/Emacs version pairs
 * - coqs: actively and passively supported Coq versions as list of versions
 * - emacses: actively and passively supported Emacs versions as list of versions
 *
 * This function processes the release records until the first
 * passively supported historic pair or the first relevant Debian/Ubuntu
 * release is found. Then collect_remaining_versions processes the
 * remainder. The arguments last_coq and last_emacs contain the last
 * Coq and Emacs versions seen, for the following table lines that do
 * not contain these versions.
 *)
let rec extract_lts_versions last_coq last_emacs = function
  | [] -> assert false
  | { rel_date; rel_coq; rel_emacs; rel_lts; rel_eol; rel_historic; } :: table ->
     let last_coq =
       match rel_coq with
         | None -> last_coq
         | Some coq_v -> coq_v
     in
     let last_emacs =
       match rel_emacs with
         | None -> last_emacs
         | Some emacs_v -> emacs_v
     in
     (* check if this line contains an LTS version with EOL date later
      * than today
      *)
     let lts_maybe =
       match rel_eol with
       | None -> []
       | Some eol_date ->
          if eol_date +. eol_relax_seconds > today
          then
            [{ lts_coq = last_coq;
               lts_emacs = last_emacs;
               lts_name = rel_lts;
               eol_date = eol_date;
            }]
          else []
     in
     (* check if this line contains an passively supported historic pair *)
     let hist_pair =
       if rel_historic && rel_date +. passive_historic_pairs_secs > today
       then [(last_coq, last_emacs)]
       else []
     in
     if lts_maybe <> [] || hist_pair <> []
     then
       collect_remaining_versions lts_maybe [] hist_pair
         [last_coq] [last_emacs] table
     else extract_lts_versions last_coq last_emacs table


(* Return the first actively supported Coq version. Note that this
 * might be a version with outdated patch level.
 *)
let get_first_active_supported_coq lts =
  let lts_coqs = List.map (fun lt -> lt.lts_coq) lts in
  List.hd (sort_versions lts_coqs)

(* Return the first actively supported Emacs version. *)
let get_first_active_supported_emacs lts =
  let lts_emacses = List.map (fun lt -> lt.lts_emacs) lts in
  List.hd (sort_versions lts_emacses)

(* Returns the first Coq version in the release table that was
 * released on a date after range seconds before now. Note that this
 * might be a Coq version with outdated patch level.
 *)
let rec get_first_range_coq range = function
  | [] -> assert false
  | { rel_date; rel_coq = Some coq_version; } :: table ->
     if rel_date +. range < today
     then get_first_range_coq range table
     else coq_version
  | _ :: table -> get_first_range_coq range table
  

(* return the two latest emacs versions with different major versions
 * (for the magic check)
 *)
let get_latest_two_emacs_major emacses =
  let emacses = List.rev emacses in
  let newest = fst (List.hd emacses) in
  let newest_major = newest.major in
  let other =
    match List.find (fun ({major;}, _) -> major <> newest_major) emacses with
      | v_i -> fst v_i
      (* seems there is only one major emacs version - this should not happen *)
      | exception Not_found -> assert false
  in
  [other; newest]


(*****************************************************************************
 *
 * container matrix - create and mark
 *
 *****************************************************************************)

(* Create a container matrix, accessed as conts.(coq_index).(emacs_index) *)
let create_container_matrix coq_number emacs_number =
  Array.init coq_number (fun _ -> Array.make emacs_number Unused)

(* Return index for version. If version is not in indexed_version_list
 * (eg., because of discarting outdated patch levels) return index
 * with same major and minor.
 *)
let get_version_index version indexed_version_list =
  match List.find_opt (fun v -> fst v = version) indexed_version_list with
    | Some (_, i) -> i
    | None ->
       snd (List.find
              (fun (v, _) -> v.major = version.major && v.minor = version.minor)
              indexed_version_list)

(* return version for index *)
let get_index_version index indexed_version_list =
  fst (List.nth indexed_version_list index)

(* Adjust version v to one in indexed version list vl. Used for Coq
 * versions to move to the latest patch level.
 *)
let adjust_patch_level v vl =
  get_index_version (get_version_index v vl) vl

(* mark LTS emacs / coq pairs.
   With future = true additionally mark all coq versions released
   after LTS
 *)
let select_lts_versions lts coqs emacses conts future =
  let last_coq_index = snd (list_last coqs) in
  List.iter
    (fun lt ->
      let coq_index = get_version_index lt.lts_coq coqs in
      let emacs_index = get_version_index lt.lts_emacs emacses in
      conts.(coq_index).(emacs_index) <- Lts;
      if future
      then
        for i = coq_index + 1 to last_coq_index do
          conts.(i).(emacs_index) <- Lts
        done;
    )
    lts


(* Mark all actively supported emacs versions with all active coq
   version of last 2 years. All emacs versions starting form the
   oldest LTS version are actively supported.
 *)
let select_all_latest_versions
      first_full_range_coq first_active_emacs coqs emacses conts =
  let last_coq_index = snd (list_last coqs) in
  let last_emacs_index = snd (list_last emacses) in
  for coq_i = get_version_index first_full_range_coq coqs to last_coq_index do
    for emacs_i = get_version_index first_active_emacs emacses
        to last_emacs_index do
      conts.(coq_i).(emacs_i) <- Latest_versions_complete
    done
  done

(* mark all LTS emacs versions with all coq version of last 1.5 years
 * except those coq versions that are older then the coq versions of
 * the respective LTS coq version
 *)
let select_lts_latest_versions
      first_partial_range_coq lts coqs emacses ci_pairs =
  let partial_coq_i = get_version_index first_partial_range_coq coqs in
  let last_coq_index = snd (list_last coqs) in
  List.iter
    (fun {lts_coq; lts_emacs;} ->
      let lts_coq_i = get_version_index lts_coq coqs in
      let emacs_i = get_version_index lts_emacs emacses in
      let coq_i =
        if lts_coq_i > partial_coq_i then lts_coq_i + 1 else partial_coq_i in
      for coq_i = coq_i to last_coq_index
      do
        ci_pairs.(coq_i).(emacs_i) <- Latest_versions_complete
      done)
    lts

(* Select historic pairs hist_pairs in container matrix conts. *)
let select_historic_pairs hist_pairs coqs emacses conts =
  List.iter
    (fun (coq_v, emacs_v) ->
      conts.(get_version_index coq_v coqs).(get_version_index emacs_v emacses)
        <- History_pair)
  hist_pairs

(* select newest emacs with all Coq versions and
 * newest coq with all emacs versions
 *
 * version lists coqs and emacses are sorted with oldest version first
 *)
let select_newest first_emacs first_coq coqs emacses conts =
  let first_emacs = get_version_index first_emacs emacses in
  let first_coq = get_version_index first_coq coqs in
  let newest_emacs = snd (list_last emacses) in
  let newest_coq = snd (list_last coqs) in
  for coq_i = first_coq to newest_coq do
    conts.(coq_i).(newest_emacs) <- Newest
  done;
  (* skip over coq RC versions *)
  let newest_non_rc_coq =
    snd (List.find
           (fun (coq_v, coq_i) -> coq_v.release_candidate = false)
           (List.rev coqs))
  in
  for em_i = first_emacs to newest_emacs do
    conts.(newest_non_rc_coq).(em_i) <- Newest
  done

(* select coq rc version with all emacs versions
 *
 * version lists coqs and emacses are sorted with oldest version first,
 * if there is an rc version, it's the last one
 *)
let select_coq_rc_version first_emacs coqs emacses conts =
  let first_emacs = get_version_index first_emacs emacses in
  let newest_emacs = snd (list_last emacses) in
  let (coq_v, coq_i) = list_last coqs in
  if coq_v.release_candidate
  then
    for em_i = first_emacs to newest_emacs do
      conts.(coq_i).(em_i) <- RC
    done
  else
    ()

(* mark all containers to build in container matrix conts *)
let select_containers first_full_range_coq first_active_emacs
      first_active_coq lts hist_pairs coqs emacses conts =
  select_historic_pairs hist_pairs coqs emacses conts;
  select_newest first_active_emacs first_active_coq coqs emacses conts;
  select_lts_versions lts coqs emacses conts true;
  select_all_latest_versions
    first_full_range_coq first_active_emacs coqs emacses conts;
  select_coq_rc_version first_active_emacs coqs emacses conts;
  ()

(* Select version pairs to be tested in github CI and mark those as
 * used in container matrix.
 *)
let select_ci_pairs first_partial_range_coq first_active_emacs
      first_active_coq lts hist_pairs coqs emacses ci_pairs =
  select_historic_pairs hist_pairs coqs emacses ci_pairs;
  select_newest first_active_emacs first_active_coq coqs emacses ci_pairs;
  select_lts_versions lts coqs emacses ci_pairs false;
  select_lts_latest_versions first_partial_range_coq lts coqs emacses ci_pairs;
  select_coq_rc_version first_active_emacs coqs emacses ci_pairs;
  ()


(* Count number of used elements in container matrix conts. *)
let count_filled_matrix_cells conts =
  Array.fold_left
    (fun count sub ->
      Array.fold_left
        (fun count cell ->
          match cell with
            | Unused -> count
            | Lts
            | Latest_versions_complete
            | History_pair
            | Newest
            | RC -> count + 1
        )
        count
        sub)
    0
    conts

(* Return a sorted list of Coq/Emacs version pairs of those elements
 * in the container matrix conts that are marked as used.
 *)
let list_of_matrix coqs emacses conts =
  let res = ref [] in
  let last_coq_index = snd (list_last coqs) in
  let last_emacs_index = snd (list_last emacses) in
  for coq_i = 0 to last_coq_index do
    for emacs_i = 0 to last_emacs_index do
      if conts.(coq_i).(emacs_i) <> Unused
      then
        res :=
          ((get_index_version coq_i coqs), (get_index_version emacs_i emacses))
          :: !res
    done
  done;
  sort_version_pairs !res
  

(*****************************************************************************
 *
 * change files
 *
 *****************************************************************************)

(* Search in file ic, starting on current position, for a line
 * identical to marker_line and return the file position directly
 * after this line. Let End_of_file escape if marker_line cannot be
 * found.
 *)
let rec search_file_pos_after ic marker_line =
  let line = input_line ic in
  if line = marker_line
  then pos_in ic
  else search_file_pos_after ic marker_line
  
(* Search in file ic, starting on current position, for a line
 * identical to marker_line and return the file position of the start
 * of this line. Let End_of_file escape if marker_line cannot be
 * found.
 *)
let rec search_file_pos_before ic marker_line =
  let pos = pos_in ic in
  let line = input_line ic in
  if line = marker_line
  then pos
  else search_file_pos_before ic marker_line

(* Return file content without a marked part as two strings (before
 * and after).
 * marker is the variable part of a CIPG change marker line, prefix is
 * the white space before the CIPG marker, including the comment
 * start, suffix is the comment termination after the CIPG marker.
 * suffix is emtpy for markdown files.
 * prefix, suffix and marker denote the portion of the file ic to be
 * replaced. This function return the content before and after that
 * portion as a pair of strings.
 * Let End_of_file escape if a CIPG marker cannot be found.
 *)
let read_file_without_marked_part ic prefix marker suffix =
  let start_marker_line =
    Printf.sprintf "%s CIPG change marker: %s%s" prefix marker suffix
  in
  let end_marker_line =
    Printf.sprintf "%s CIPG change marker end%s" prefix suffix
  in
  let start_pos = search_file_pos_after ic start_marker_line in
  let end_pos = search_file_pos_before ic end_marker_line in
  seek_in ic 0;
  let before = really_input_string ic start_pos in
  seek_in ic end_pos;
  (before,
   really_input_string ic (in_channel_length ic - end_pos))

(* Replace content between CIPG markers with output of print_fn.
 * prefix, suffix and marker denote the portion of the file ic to be
 * replaced, see function read_file_without_marked_part.
 * print_fn must be a function that takes an out_channel as argument.
 * Let End_of_file escape if a CIPG marker cannot be found.
 *)
let file_change_wrapper file prefix marker suffix print_fn =
  let ic = open_in_bin (Filename.concat !pg_repo file) in
  let (content_before, content_after) =
    read_file_without_marked_part ic prefix marker suffix in
  close_in ic;
  let oc = open_out_bin (Filename.concat !pg_repo file) in
  output_string oc content_before;
  print_fn oc;
  output_string oc content_after;
  close_out oc;
  Printf.printf "Updated %s at marker %s\n" file marker

(* Replace content between CIPG markers in markdown files with output
 * of print_fn. Specialization of file_change_wrapper for markdown,
 * see file_change_wrapper.
 *)
let md_file_change_wrapper file marker print_fn =
  file_change_wrapper file "<!--" marker " -->" print_fn

(* Replace content between CIPG markers in YAML files with output
 * of print_fn. Specialization of file_change_wrapper for YAML,
 * see file_change_wrapper.
 *)
let yml_file_change_wrapper file marker print_fn =
  file_change_wrapper file "          #" marker "" print_fn


(*****************************************************************************
 *
 * printing
 *
 *****************************************************************************)

let version_to_string v =
  Printf.sprintf "%d.%d%s%s"
    v.major v.minor
    (match v.patch with
       | None -> ""
       | Some p -> Printf.sprintf ".%d" p)
    (if v.release_candidate then "rc" else "")

(* convert version to string, ignoring the index *)
let indexed_version_to_string (v, _i) = version_to_string v

(* print a float as time YYYY/mm *)
let date_to_string d =
  let tm = Unix.localtime d in
  Printf.sprintf "%d/%02d" (tm.U.tm_year + 1900) (tm.U.tm_mon + 1)

(* convert LTS record to string *)
let lts_to_string {lts_coq; lts_emacs; lts_name; eol_date} =
  Printf.sprintf "%s with Coq %s emacs %s end of live on %s"
    lts_name
    (version_to_string lts_coq)
    (version_to_string lts_emacs)
    (date_to_string eol_date)

(* String of type matrix_element for matrix printing. *)
let string_of_matrix_element = function
  | Unused -> ""
  | Lts -> "SUP"
  | Latest_versions_complete -> "X "
  | History_pair -> "H "
  | Newest -> "N "
  | RC -> "RC"
    
(* Print version pairs as historic pairs. Use string kind to
 * disambiguate.
 *)
let report_historic_pairs kind pairs =
  Printf.printf "%s historic pairs:\n  %s\n"
    kind
    (String.concat "\n  "
       (List.rev_map
          (fun (coq_v, em_v) ->
            Printf.sprintf "%s / %s"
              (version_to_string em_v) (version_to_string coq_v))
          pairs))  

(* Report all interesting and relevant information read from the
 * Coq/Emacs/Debian/Ubuntu relase table.
 *)
let report_table_results first_emacs first_coq first_full_range_coq
      first_partial_range_coq lts passive_hist active_hist coqs emacses
      latest_two_emacs_major =
  Printf.printf "LTS versions:\n  %s\n"
    (String.concat "\n  " (List.map lts_to_string lts));
  Printf.printf "Coq versions: %s\n"
    (String.concat ", " (List.map indexed_version_to_string coqs));
  Printf.printf "Emacs versions: %s\n"
    (String.concat ", " (List.map indexed_version_to_string emacses));
  Printf.printf "First actively supported coq: %s\n"
    (version_to_string first_coq);
  Printf.printf "First actively supported emacs: %s\n"
    (version_to_string first_emacs);
  Printf.printf "Latest two different emacs major versions: %s\n"
    (String.concat ", " (List.map version_to_string latest_two_emacs_major));
  Printf.printf "First full range coq: %s\n"
    (version_to_string first_full_range_coq);
  Printf.printf "First partial range coq: %s\n"
    (version_to_string first_partial_range_coq);
  report_historic_pairs "Passively supported" passive_hist;
  report_historic_pairs "Actively supported" active_hist;
  print_endline ""

(*****************************************************************************
 *
 * print table / matrix
 *
 *****************************************************************************)

(* Print table with first actively supported Coq / Emacs versions on oc *)
let print_actively_supported_coq_emacs_table first_coq first_emacs oc =
  Printf.fprintf oc "| Coq   | %5s |\n" (version_to_string first_coq);
  output_string oc  "|-------+-------|\n";
  Printf.fprintf oc "| Emacs | %5s |\n" (version_to_string first_emacs)

(* Output only the table for matrix conts on oc *)
let output_matrix coqs emacses conts oc =
  let last_coq_index = snd (list_last coqs) in
  let last_emacs_index = snd (list_last emacses) in
  (* table header *)
  Printf.fprintf oc "|         | %s |\n"
    (String.concat " | " (List.map indexed_version_to_string emacses));
  Printf.fprintf oc "|---------+-%s-|\n"
    (String.concat "-+-" (List.rev_map (fun _ -> "----") emacses));

  (* table body *)
  for coq_i = 0 to last_coq_index do
    Printf.fprintf oc "| %7s |"
      (version_to_string (get_index_version coq_i coqs));
    for emacs_i = 0 to last_emacs_index do
      Printf.fprintf oc " %4s |"
        (string_of_matrix_element conts.(coq_i).(emacs_i))
    done;
    Printf.fprintf oc "\n";
  done

(* Print a matrix as org table. *)
let print_matrix matrix_name coqs emacses conts =
  let count = count_filled_matrix_cells conts in
  Printf.printf "%d %s:\n" count matrix_name;
  output_matrix coqs emacses conts stdout


(*****************************************************************************
 *
 * Matrix subset check
 *
 *****************************************************************************)

(* Check that pairs enabled in CI are built as containers *)
let check_matrix_subset coqs emacses ci_pairs conts =
  let is_subset = ref true in
  let last_coq_index = snd (list_last coqs) in
  let last_emacs_index = snd (list_last emacses) in
  for coq_i = 0 to last_coq_index do
    for emacs_i = 0 to last_emacs_index do
      if ci_pairs.(coq_i).(emacs_i) <> Unused
         && conts.(coq_i).(emacs_i) = Unused
      then
        begin
          is_subset := false;
          Printf.printf
            "container missing for CI pair Coq %s Emacs %s\n"
            (version_to_string (get_index_version coq_i coqs))
            (version_to_string (get_index_version emacs_i emacses))
        end
    done;
  done;
  if !is_subset then
    print_endline "CI matrix is a subset of container matrix"
  else
    print_endline "internal error: CI matrix not contained in container matrix"


(*****************************************************************************
 *
 * retrieve existing docker images
 *
 *****************************************************************************)

(* Start a curl process that outputs all tags in the docker registry
 * for proofgeneral/repo_name. One tag per line, enclosed in quotes.
 *)
let docker_tags_channel repo_name =
  U.open_process_in
    (Printf.sprintf
       ("curl -L -s "
        ^^ "'https://registry.hub.docker.com/v2/repositories/proofgeneral"
        ^^ "/%s/tags?page_size=1024' | jq -r '.\"results\"[][\"name\"]'")
       repo_name)

(* Read all coq-nix container tags from inc, accumulating the result
 * as Coq versions in nix_conts. Ignore container tags without patch
 * level.
 *)
let rec read_nix_containers inc nix_conts =
  match input_line inc with
    | line ->
       (match scan_version line with
          | None -> assert false
          | Some v ->
             if v.patch <> None
             then read_nix_containers inc (v :: nix_conts)
             else read_nix_containers inc nix_conts
       )
    | exception End_of_file -> nix_conts
       
(* Read all coq-nix container tags from the docker registry. Return
 * them as a sorted list of Coq versions.
 *)
let get_nix_containers () =
  let inc = docker_tags_channel "coq-nix" in
  let nix_conts = read_nix_containers inc [] in
  (match U.close_process_in inc with
     | WEXITED(0) -> ()
     | _ -> assert false
  );
  sort_versions nix_conts


(* Read an Coq/Emacs tag from line, return a corresponding Coq/Emacs
 * version pair. Release candidate versions are only recognized for
 * Coq. Recognized tags have the following form:
 * "coq-8.16.1-emacs-25.2"
 * "coq-8.16-emacs-25.2"
 * "coq-8.16-rc1-emacs-26.1"
 *)
let read_coq_emacs_tag line =
  (* Printf.printf "X %s\n%!" line; *)
  let sb = Scanf.Scanning.from_string line in
  Scanf.bscanf sb "coq-%d.%d%c"
    (fun coq_major coq_minor c ->
      let coq_patch = match c with
          | '.' -> Scanf.bscanf sb "%d-" (fun patch -> Some patch)
          | '-' -> None
          | _ -> raise (Scanf.Scan_failure ". or - expected after coq minor")
      in
      let (coq_rc, emacs_major, emacs_minor) =
        Scanf.bscanf sb "%[^-]"
          (function
           | "emacs" -> Scanf.bscanf sb "-%d.%d" (fun a b -> (false, a, b))
           | "rc"
           | "rc1"
           | "rc2" -> Scanf.bscanf sb "-emacs-%d.%d" (fun a b -> (true, a, b))
           | _ -> raise (Scanf.Scan_failure "rc or emacs expected")
          )
      in
      ({major = coq_major;
        minor = coq_minor;
        patch = coq_patch;
        release_candidate = coq_rc;
       },
       {major = emacs_major;
        minor = emacs_minor;
        patch = None;
        release_candidate = false;
       }
      )
    )

(* Read all coq-emacs tags from inc, accumulating read tags as
 * Coq/Emacs version pairs in coq_emacs. Ignore those tags where the
 * Coq version has no patch level.
 *)
let rec read_all_coq_emacs_tags inc coq_emacs =
  match input_line inc with
    | line ->
       let (coq_v, emacs_v) as vp = read_coq_emacs_tag line in
       let coq_emacs =
         if coq_v.patch <> None
         then vp :: coq_emacs
         else coq_emacs
       in
       read_all_coq_emacs_tags inc coq_emacs
    | exception End_of_file -> coq_emacs
  

(* Get the tags of existing coq-emacs containers from the docker
 * registry. Return them as a sorted list of Coq/Emacs version pairs.
 *)
let get_coq_emacs_containers () =
  let inc = docker_tags_channel "coq-emacs" in
  let coq_emacs = read_all_coq_emacs_tags inc [] in
  (match U.close_process_in inc with
     | WEXITED(0) -> ()
     | _ -> assert false
  );
  sort_version_pairs coq_emacs


(*****************************************************************************
 *
 * read / write currently used containers
 *
 *****************************************************************************)

(* Return the coq-emacs docker tag for version coq and emacs. *)
let coq_emacs_tag coq emacs =
  Printf.sprintf "coq-%s-emacs-%s"
    (version_to_string coq) (version_to_string emacs)

(* Read currently needed coq-nix containers from doc subdir. *)
let read_currently_used_nix_containers () =
  let ic = open_in (Filename.concat !pg_repo currently_used_nix_file) in
  let used = read_nix_containers ic [] in
  close_in ic;
  used  

(* Read currently used coq-emacs containers from doc subdir. *)
let read_currently_used_coq_emacs_containers () =
  let ic = open_in (Filename.concat !pg_repo currently_used_coq_emacs_file) in
  let used = read_all_coq_emacs_tags ic [] in
  close_in ic;
  used

(* Write the currently used coq-nix and coq-emacs containers into
 * their respective files in the doc subdir
 *)
let update_currently_used coqs coq_emacs_used =
  print_endline"\nupdate files with currently used containers\n";
  let oc = open_out_bin (Filename.concat !pg_repo currently_used_nix_file) in
  List.iter
    (fun (coq_v, _) ->
      output_string oc (version_to_string coq_v);
      output_string oc "\n";
    )
    coqs;
  close_out oc;
  let oc = open_out_bin
             (Filename.concat !pg_repo currently_used_coq_emacs_file) in
  List.iter
    (fun (coq, emacs) ->
      output_string oc (coq_emacs_tag coq emacs);
      output_string oc "\n";
    )
    coq_emacs_used;
  close_out oc;
  ()
  

(*****************************************************************************
 *
 * print docker build commands
 *
 *****************************************************************************)

(* Compare the existing coq-nix containers with the version list in
 * coqs. Print existing, missing and superfluous versions. Print
 * docker build and push commands for missing containers. Return the
 * superfluous containers as list of Coq versions.
 *)
let check_nix_containers coqs =
  let nix_containers = get_nix_containers () in
  Printf.printf "existing nix versions: %s\n"
    (String.concat " " (List.map version_to_string nix_containers));
  let missing = list_diff coqs nix_containers in
  Printf.printf "missing nix versions: %s\n"
    (String.concat " " (List.map version_to_string missing));
  let now_in_use = read_currently_used_nix_containers () in
  let not_needed = list_diff nix_containers coqs in
  let del_now = list_diff not_needed now_in_use in
  let del_soon = list_intersection not_needed now_in_use in
  Printf.printf "now superfluous nix versions: %s\n"
    (String.concat " " (List.map version_to_string del_now));
  Printf.printf "soon superfluous nix versions: %s\n\n"
    (String.concat " " (List.map version_to_string del_soon));
  if missing <> [] then
    begin
      print_endline
        "####################################################################";
      Printf.printf "# built missing coq-nix containers\n";
      print_endline
        "####################################################################\n";
      Printf.printf "pushd %s/coq-nix-docker/coq-nix\n" !src_dir;
      List.iter
        (fun coqv ->
          let coq_version = version_to_string coqv in
          Printf.printf
            ("docker image build -t proofgeneral/coq-nix:%s \\\n"
             ^^ "\t--build-arg COQV=%s \\\n"
             ^^ "\t--build-arg OCAMLV=4.13.1-flambda .\n")
            coq_version coq_version;
          Printf.printf "docker image push proofgeneral/coq-nix:%s\n\n"
            coq_version;
        )
        missing;
      print_endline "popd";
    end;
  del_now
  

(* Print docker build and push commands for a keyed Coq/Emacs version
 * list of the form [(coq_v1, [emacs_v1; v2; ...]); (coq_v2, [...]); ...]
 *)
let rec print_coq_emacs_build_commands = function
  | [] -> ()
  | (coq, emacses) :: l ->
     let coq_version = version_to_string coq in
     print_endline
       "##############################################";
     Printf.printf "# built Coq %s containers\n\n" coq_version;
     List.iter
       (fun emacs ->
         let emacs_version = version_to_string emacs in
         let coq_emacs_tag = coq_emacs_tag coq emacs in
         Printf.printf
           ("docker image build -t proofgeneral/coq-emacs:%s \\\n"
            ^^ "\t--build-arg NIX_BASE_TAG=%s \\\n"
            ^^ "\t--build-arg EMACS_VERSION=%s .\n")
           coq_emacs_tag
           coq_version
           emacs_version;
         Printf.printf
           "docker image push proofgeneral/coq-emacs:%s\n\n" coq_emacs_tag;
       )
       emacses;
     print_coq_emacs_build_commands l
  
(* Print Coq/Emacs version pairs with title. Outputs one line for each
 * Coq version with all Emacs versions for this Coq version.
 *)
let print_coq_emacs_pairs title coq_emacs =
  let coq_emacs = pairs_to_keyed_list coq_emacs in
  Printf.printf "%s:\n" title;
  List.iter
    (fun (coq, emacses) ->
      Printf.printf "   - %s: %s\n"
        (version_to_string coq)
        (String.concat " " (List.map (fun e -> version_to_string e) emacses)))
    coq_emacs
  

(* Compare existing coq-emacs containers at docker with the matrix
 * conts. Print the existing, missing and superfluous containers.
 * Print docker build commands for missing containers. Return the
 * superfluous containers as list of Coq/Emacs version pairs.
 *)
let check_coq_emacs_containers coqs emacses conts =
  let coq_emacs_existing = get_coq_emacs_containers () in
  print_coq_emacs_pairs "existing coq-emacs containers" coq_emacs_existing;
  let coq_emacs_needed = list_of_matrix coqs emacses conts in
  let missing = list_diff coq_emacs_needed coq_emacs_existing in
  let now_in_use = read_currently_used_coq_emacs_containers () in
  let not_needed = list_diff coq_emacs_existing coq_emacs_needed in
  let del_now = list_diff not_needed now_in_use in
  let del_soon = list_intersection not_needed now_in_use in
  print_coq_emacs_pairs "missing coq-emacs containers" missing;
  print_coq_emacs_pairs "now superfluous coq-emacs containers" del_now;
  print_coq_emacs_pairs "soon superfluous coq-emacs containers" del_soon;
  if missing <> [] then
    begin
      print_endline "\n";
      print_endline
        "####################################################################";
      print_endline "# build coq-emacs containers";
      print_endline
        "####################################################################\n";
      Printf.printf "pushd %s/coq-emacs-docker/coq-emacs\n\n" !src_dir;
      print_coq_emacs_build_commands (pairs_to_keyed_list missing);
      print_endline "popd"
    end;
  del_now


(*****************************************************************************
 *
 * delete containers
 *
 *****************************************************************************)

(* Read line by line the .authinfo file ic and search for host host.
 * Recognize only simple 6-element entries without quotes. Return user
 * and password as pair of strings if found. Raises End_of_file.
 *)
let rec search_authinfo ic host =
  let line = input_line ic in
  match String.split_on_char ' ' line with
    | ["machine"; ahost; "login"; user; "password"; passwd] when ahost = host ->
       close_in ic;
       (user, passwd)
    | _ -> search_authinfo ic host

(* Return username and password as a pair of strings read from file
   ~/.authinfo for host. The file must contain a simple 6-element
   entry for host without quotes. If no entry is found the program
   exits with an error.
 *)
let read_authinfo host =
  let ic = open_in (Filename.concat (Sys.getenv "HOME") ".authinfo") in
  try
    search_authinfo ic host
  with
    | End_of_file ->
       close_in ic;
       Printf.eprintf "No record for %s found in ~/.authinfo." host;
       exit 1

(* Return a docker personal access token retrieved from docker.
 * Credentials are read from file ~/.authinfo, which must contain a
 * line for hub.docker.com.
 *)
let get_personal_access_token () =
  let (user, passwd) = read_authinfo "hub.docker.com" in
  let curl_cmd =
    "curl -s -H \"Content-Type: application/json\" -X POST "
    ^ "-d \"{\\\"username\\\": \\\""
    ^ user
    ^ "\\\", \\\"password\\\": \\\""
    ^ passwd
    ^ "\\\"}\" https://hub.docker.com/v2/users/login/ | jq -e -r .token"
  in
  (* print_endline ("CMD: " ^ curl_cmd); *)
  let inc = U.open_process_in curl_cmd in
  let token = input_line inc in
  assert (token <> "");
  (* print_endline ("TOKEN: " ^ token); *)
  (match U.close_process_in inc with
     | WEXITED(0) -> ()
     | _ ->
        prerr_endline "Unable to fetch access token from hub.docker.com.";
        prerr_endline "Have you supplied valid credentials?";
        exit 1
  );
  token

(* Delete the docker container with tag tag in repo proofgeneral/repo.
 * Argument token must be a personal docker access token.
 *)
let delete_container token repo tag =
  let delete_cmd =
    "curl -i -X DELETE -H \"Accept: application/json\" "
    ^ "-H \"Authorization: JWT " ^ token ^ "\" "
    ^ "https://hub.docker.com/v2/namespaces/proofgeneral/repositories/"
    ^ repo ^ "/tags/" ^ tag
    ^ " >/dev/null 2>&1"
  in
  Printf.printf "delete container %s/%s [Y/n]? %!" repo tag;
  let do_delete = match read_line () with
      | "" | "y" | "Y" | "yes" | "Yes" -> true
      | _ -> false
  in
  (* print_endline ("delcmd: " ^ delete_cmd); *)
  if do_delete
  then
    let status = Sys.command delete_cmd in
    if status = 0
    then
      Printf.printf "container %s/%s deleted\n\n%!" repo tag
    else
      begin
        Printf.printf "deletion of %s/%s failed\n%!" repo tag;
        exit 1
      end
  else
    Printf.printf "container %s/%s is kept\n\n%!" repo tag
  

(* Delete the coq-emacs container for coq and emacs.
 * Argument token must be a personal docker access token.
 *)
let delete_coq_emacs_container token (coq, emacs) =
  let coq_emacs_tag = coq_emacs_tag coq emacs in
  delete_container token "coq-emacs" coq_emacs_tag

(* Delete coq-emacs containers for Coq/Emacs version pairs in
 * coq_emacs_pairs.
 * Argument token must be a personal docker access token.
 *)
let delete_coq_emacs_containers token coq_emacs_pairs =
  List.iter (delete_coq_emacs_container token) coq_emacs_pairs


(* Delete the coq-nix containers for Coq versions coqv.
 * Argument token must be a personal docker access token.
 *)
let delete_nix_container token coqv =
  let coq_version = version_to_string coqv in
  delete_container token "coq-nix" coq_version

(* Delete coq-nix containers for Coq versions in coqs.
 * Argument token must be a personal docker access token.
 *)
let delete_nix_containers token coqs =
  List.iter (delete_nix_container token) coqs


(*****************************************************************************
 *
 * print lines for PG CI yaml file
 *
 *****************************************************************************)

(* Output Coq/Emacs versions on channel oc for
 * .github/workflows/test.yml for those jobs that need Coq and Emacs.
 * ci_pairs is the matrix for CI and coqs and emacses are the indexed
 * version lists of Coq and Emacs.
 *)
let output_ci_coq_emacs_versions coqs emacses ci_pairs oc =
  let last_coq_index = snd (list_last coqs) in
  let last_emacs_index = snd (list_last emacses) in
  for coq_i = 0 to last_coq_index do
    let coq_version = version_to_string (get_index_version coq_i coqs) in
    for emacs_i = 0 to last_emacs_index do
      if ci_pairs.(coq_i).(emacs_i) <> Unused
      then
        let emacs_version =
          version_to_string (get_index_version emacs_i emacses) in
        Printf.fprintf oc "          - coq-%s-emacs-%s\n"
          coq_version emacs_version;
    done
  done

(* Print Coq/Emacs versions for
 * .github/workflows/test.yml for those jobs that need Coq and Emacs.
 * See output_ci_coq_emacs_versions for the other arguments.
 *)
let print_ci_coq_emacs_versions coqs emacses ci_pairs =
  print_endline
    "Coq / Emacs version pair lines for file .github/workflows/test.yml.
These lines must be copied into the coq_emacs_version field in
the jobs test, compile-tests and simple-tests.";
  output_ci_coq_emacs_versions coqs emacses ci_pairs stdout

(* Output Emacs versions on channel oc for .github/workflows/test.yml
 * for those jobs that only need Emacs. first_active_emacs is the
 * first supported Emacs version and emacses is the indexed version
 * list of Emacs.
 *)
let output_ci_compile_indent_versions first_active_emacs emacses oc =
  let last_emacs_index = snd (list_last emacses) in
  for emacs_i = get_version_index first_active_emacs emacses
      to last_emacs_index do
    Printf.fprintf oc "          - %s\n"
      (version_to_string (get_index_version emacs_i emacses));
  done

(* Print Emacs versions for .github/workflows/test.yml
 * for those jobs that only need Emacs. See
 * output_ci_compile_indent_versions.
 *)
let print_ci_compile_indent_versions first_active_emacs emacses =
  print_endline
    "Emacs version lines for compile, indentation, and qRHL tests in
.github/workflows/test.yml. These lines must be copied into the emacs_version
field in the jobs build, test-indent, and test-qrhl.";
  output_ci_compile_indent_versions first_active_emacs emacses stdout

(* Output the latest two Emacs major versions on channel oc for
 * .github/workflows/test.yml.
 *)
let output_ci_magic_versions latest_two_emacs_major oc =
  List.iter
    (fun emacs_v ->
      Printf.fprintf oc "          - %s\n" (version_to_string emacs_v))
    latest_two_emacs_major

(* Print the latest two Emacs major versions for
 * .github/workflows/test.yml.
 *)
let print_ci_magic_versions latest_two_emacs_major =
  print_endline
    "Emacs version lines for the doc magic test in .github/workflows/test.yml.
These lines must be copied into the emacs_version field in the job
check-doc-magic.";
  output_ci_magic_versions latest_two_emacs_major stdout


(*****************************************************************************
 *
 * main
 *
 *****************************************************************************)

(** Argument list for [Arg.parse] *)
let arguments =
  Arg.align [
      ("-check", Arg.Set do_containers,
       " check existence of containers and print container build commands");
      ("-ci-print", Arg.Set print_pg_ci_config,
       " print PG CI configuration");
      ("-ci-change", Arg.Set do_pg_ci_update,
       " update PG CI configuration");
      ("-delete", Arg.Set do_delete_containers,
       " delete superfluous containers");
      ("-pg-repo", Arg.Set_string pg_repo,
       "dir specify PG repository");
      ("-src-dir", Arg.Set_string src_dir,
       "dir directory of coq-nix-docker and coq-emacs-docker repos");
    ]

(** Function for anonymous arguments. Terminates the program with 
    exit status 1.
*)
let anon_fun s =
  Printf.eprintf "unrecognized argument %s\n" s;
  exit 1

(* Process command line arguments. *)
let process_command_line () =
  Arg.parse arguments anon_fun
    (Printf.sprintf "usage: cipg [options...]\ndefault PG repo: %s\noptions:"
       !pg_repo);
  if !do_delete_containers && (not !do_containers)
  then
    do_containers := true

let main() =
  process_command_line ();
  (* table contains the complete pg.org table as release_records in reverse order *)
  let table = parse_table () in
  (* lts: LTS versions with eol date in the future as lts_record list
   * passive_hist: passively supported hisoric pairs as
   *   (coq version * emacs version) list
   * active_hist: actively supported hisoric pairs as
   *   (coq version * emacs version) list
   * coqs: actively and passively supported coq versions as version list
   * emacses: actively and passively supported emacs versions as version list
   *)
  let (lts, passive_hist, active_hist, coqs, emacses) =
    extract_lts_versions null_version null_version table
  in
  (* sort LTS by ascending EOL dates *)
  let lts = sort_lts lts in
  (* only keep latest patch level for each <major>.<minor> *)
  let coqs = discard_outdated_patch_levels coqs in
  (* sorted coq versions with indices (version * int) list *)
  let coqs = sort_and_index_versions coqs in
  (* actively sorted emacs versions with indices *)
  let emacses = sort_and_index_versions emacses in
  (* oldest coq version for which to build containers for all
     actively supported emacs versions (first version with complete X line) *)
  let first_full_range_coq =
    adjust_patch_level (get_first_range_coq coq_full_range_secs table) coqs in
  (* oldest coq version which runs with all emacs LTS version in CI
     (first partial X line in CI matrix) *)
  let first_partial_range_coq =
    adjust_patch_level (get_first_range_coq coq_partial_range_secs table) coqs in
  let first_active_emacs = get_first_active_supported_emacs lts in
  let first_active_coq =
    adjust_patch_level (get_first_active_supported_coq lts) coqs in
  (* latest two emacs major versions for magic test *)
  let latest_two_emacs_major = get_latest_two_emacs_major emacses in
  report_table_results first_active_emacs first_active_coq
                       first_full_range_coq
                       first_partial_range_coq
                       lts passive_hist active_hist
                       coqs emacses latest_two_emacs_major;
  (* 2-dimensional array of matrix_element's (coq * emacs) *)
  let conts = create_container_matrix (List.length coqs) (List.length emacses) in
  let ci_pairs =
    create_container_matrix (List.length coqs) (List.length emacses) in

  select_containers first_full_range_coq first_active_emacs
                    first_active_coq lts (passive_hist @ active_hist)
                    coqs emacses conts;
  print_endline "";
  print_matrix "containers" coqs emacses conts;
  select_ci_pairs first_partial_range_coq first_active_emacs
    first_active_coq lts active_hist coqs emacses ci_pairs;
  print_endline "\n";
  print_matrix "CI pairs" coqs emacses ci_pairs;
  print_endline "";
  check_matrix_subset coqs emacses ci_pairs conts;
  if !do_containers then
    begin
      print_endline "\n\nCHECK MISSING AND SUPERFLUOUS CONTAINERS\n";
      let not_needed_nix_versions = check_nix_containers (List.map fst coqs) in
      let not_needed_ci_versions =
        check_coq_emacs_containers coqs emacses conts in
      if !do_delete_containers then
        begin
          if not_needed_nix_versions <> [] || not_needed_ci_versions <> []
          then
            begin
              print_endline"\n\nDELETE SUPERFLUOUS CONTAINERS\n";
              let token = get_personal_access_token() in
              delete_coq_emacs_containers token not_needed_ci_versions;
              delete_nix_containers token not_needed_nix_versions;
            end
          else
            print_endline"\n\nno superfluous container to delete\n";
        end;
    end;
  if !print_pg_ci_config then
    begin
      print_endline "\n";
      print_ci_coq_emacs_versions coqs emacses ci_pairs;
      print_endline "\n";
      print_ci_compile_indent_versions first_active_emacs emacses;
      print_endline "\n";
      print_ci_magic_versions latest_two_emacs_major;
    end;  
  if !do_pg_ci_update then
    begin
      (* In README.md: update Coq/Emacs oldest activley supported
       * version, the number of containers, the container table, the
       * number of tested version pairs, and the tested version pair
       * table.
       *)
      md_file_change_wrapper readme_file "coq-emacs-versions"
        (print_actively_supported_coq_emacs_table
           first_active_coq first_active_emacs);
      md_file_change_wrapper readme_file "container-number"
        (fun oc -> Printf.fprintf oc "%d\n" (count_filled_matrix_cells conts));
      md_file_change_wrapper readme_file "container-table"
        (output_matrix coqs emacses conts);
      md_file_change_wrapper readme_file "testrun-number"
        (fun oc -> Printf.fprintf oc "%d\n"
                     (count_filled_matrix_cells ci_pairs));
      md_file_change_wrapper readme_file "testrun-table"
        (output_matrix coqs emacses ci_pairs);

      (* In test.yml update the version numbers for all test jobs. *)
      List.iter
        (fun marker ->
          yml_file_change_wrapper test_workflow_file marker
            (output_ci_compile_indent_versions first_active_emacs emacses))
        ["build-emacs-versions";
         "indent-emacs-versions";
         "qrhl-emacs-versions";
        ];

      List.iter
        (fun marker ->
          yml_file_change_wrapper test_workflow_file marker
            (output_ci_coq_emacs_versions coqs emacses ci_pairs))
        ["test-coq-emacs-versions";
         "compile-coq-emacs-versions";
         "simple-coq-emacs-versions";
        ];

      yml_file_change_wrapper test_workflow_file "magic-emacs-version"
        (output_ci_magic_versions latest_two_emacs_major);

      update_currently_used coqs (list_of_matrix coqs emacses conts);
    end;
  ()
  

let _ = main ()


(*** Local Variables: ***)
(*** compile-command: "ocamlopt.opt -g -o cipg unix.cmxa cipg.ml && ./cipg" ***)
(*** End: ***)
