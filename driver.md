# How to drive `odoc`


```ocaml env=e1
(* Prelude *)
#require "bos";;
#install_printer Fpath.pp;;
open Bos;;
let (>>=) = Result.bind;;
let (>>|=) m f = m >>= fun x -> Ok (f x);;
let get_ok = function | Ok x -> x | Error (`Msg m) -> failwith m
```

```ocaml env=e1
let odoc = Cmd.v "odoc"

let compile_output = ref [ "" ]

let link_output = ref [ "" ]

let generate_output = ref [ "" ]

let add_prefixed_output cmd list prefix lines =
  if List.length lines > 0 then
    list :=
      !list
      @ Bos.Cmd.to_string cmd :: List.map (fun l -> prefix ^ ": " ^ l) lines

let compile file ?parent ?(ignore_output = false) children =
  let output_file =
    let ext = Fpath.get_ext file in
    let basename = Fpath.basename (Fpath.rem_ext file) in
    match ext with
    | ".mld" -> "page-" ^ basename ^ ".odoc"
    | ".cmt" | ".cmti" | ".cmi" -> basename ^ ".odoc"
    | _ -> failwith ("bad extension: " ^ ext)
  in
  let open Cmd in
  let cmd =
    odoc % "compile" % Fpath.to_string file % "-I" % "." % "-o" % output_file
    |> List.fold_right (fun child cmd -> cmd % "--child" % child) children
  in
  let cmd =
    match parent with
    | Some p -> cmd % "--parent" % ("page-\"" ^ p ^ "\"")
    | None -> cmd
  in
  let lines = OS.Cmd.(run_out ~err:err_run_out cmd |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let link ?(ignore_output = false) file =
  let open Cmd in
  let cmd = odoc % "link" % p file % "-I" % "." in
  let lines = OS.Cmd.(run_out ~err:err_run_out cmd |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string file) lines

let html_generate ?(ignore_output = false) file =
  let open Cmd in
  let cmd =
    odoc % "html-generate" % p file % "-o" % "html" % "--theme-uri" % "odoc"
    % "--support-uri" % "odoc"
  in
  let lines = OS.Cmd.(run_out cmd ~err:err_run_out |> to_lines) |> get_ok in
  if not ignore_output then
    add_prefixed_output cmd generate_output (Fpath.to_string file) lines

let support_files () =
  let open Cmd in
  let cmd = odoc % "support-files" % "-o" % "html/odoc" in
  OS.Cmd.(run_out cmd |> to_lines) |> get_ok
```

We'll now make some library lists. We have not only external dependency libraries, but
[odoc] itself is also separated into libraries too. These two sets of libraries will be
documented in different sections, so we'll keep them in separate lists.
Additionally we'll also construct a list containing the extra documentation pages. Finally let's create a list mapping the section to its parent, which matches
the hierarchy declared above.

```ocaml env=e1
let dep_libraries_core = [
    "odoc-parser";
    "astring";
    "cmdliner";
    "fpath";
    "result";
    "tyxml";
    "fmt";
    "stdlib";
    "yojson";
    "biniou";
];;

let extra_deps = [
    "base";
    "core_kernel";
    "bin_prot";
    "sexplib";
    "sexplib0";
    "base_quickcheck";
    "ppx_sexp_conv";
    "ppx_hash";
]

let dep_libraries = dep_libraries_core @ extra_deps

let odoc_libraries = [];;

let all_libraries = dep_libraries @ odoc_libraries;;

let extra_docs = [
]

let parents =
    let add_parent p l = List.map (fun lib -> (lib, p)) l in
    (add_parent "odoc" dep_libraries) @ (add_parent "odoc" odoc_libraries);;

```

[odoc] operates on the compiler outputs. We need to find them for both the files compiled by Dune within this project and those in libraries we compile against.
The following uses `ocamlfind` to locate the library paths for our dependencies:

```ocaml env=e1
let ocamlfind = Cmd.v "ocamlfind"

let lib_path lib =
  let cmd = Cmd.(ocamlfind % "query" % lib) in
  OS.Cmd.(run_out cmd |> to_lines >>|= List.hd)

let lib_paths =
  List.fold_right
    (fun lib acc ->
      acc >>= fun acc ->
      lib_path lib >>|= fun l -> (lib, l) :: acc)
    dep_libraries (Ok [])
  |> get_ok
```

We need a function to find `odoc` inputs given a search path. `odoc`
operates on [.cmti], [.cmt] or [.cmi] files, in order of preference, and the following
function finds all matching files given a search path. Then it returns an `Fpath.Set.t`
that contains the `Fpath.t` values representing the absolute file path, without its extension.

```ocaml env=e1
let find_units p =
  OS.Dir.fold_contents ~dotfiles:true
    (fun p acc ->
      if List.exists (fun ext -> Fpath.has_ext ext p) [ "cmt"; "cmti"; "cmi" ]
      then p :: acc
      else acc)
    [] (Fpath.v p)
  >>|= fun paths ->
  let l = List.map Fpath.rem_ext paths in
  let l =
    List.filter
      (fun f ->
        not @@ Astring.String.is_infix ~affix:"ocamldoc" (Fpath.to_string f))
      l
  in
  List.fold_right Fpath.Set.add l Fpath.Set.empty;;
```

Since the units returned by this function have their extension stripped, we need
function to find the best file to use with this basename.

```ocaml env=e1
let best_file base =
  List.map (fun ext -> Fpath.add_ext ext base) [ "cmti"; "cmt"; "cmi" ]
  |> List.find (fun f -> Bos.OS.File.exists f |> get_ok)
```

Many of the units will be 'hidden' -- that is, their name will be mangled by Dune
in order to namespace them. This is achieved by prefixing the namespace module and
a double underscore, so we can tell by the existence of a double underscore that
a module is intended to be hidden. The following predicate tests for that condition:

```ocaml env=e1
let is_hidden path = Astring.String.is_infix ~affix:"__" (Fpath.to_string path)
```


To build the documentation, we start with these files. With the following function, we'll call `odoc compile-deps` on the file to
find all other compilation units upon which it depends:

```ocaml env=e1
type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let compile_deps f =
  let cmd = Cmd.(odoc % "compile-deps" % Fpath.to_string f) in
  OS.Cmd.(run_out cmd |> to_lines)
  >>|= List.filter_map (Astring.String.cut ~sep:" ")
  >>= fun l ->
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")
```

Let's now put together a list of all possible modules. We'll keep track of
which library they're in, and whether that library is a part of `odoc` or a dependency
library.

```ocaml env=e1
let odoc_all_unit_paths = find_units ".." |> get_ok

let odoc_units =
  List.map
    (fun lib ->
      Fpath.Set.fold
        (fun p acc ->
          if Astring.String.is_infix ~affix:lib (Fpath.to_string p) then
            ("odoc", lib, p) :: acc
          else acc)
        odoc_all_unit_paths [])
    odoc_libraries
```

```ocaml env=e1
let lib_units =
  List.map
    (fun (lib, p) ->
      Fpath.Set.fold
        (fun p acc -> ("odoc", lib, p) :: acc)
        (find_units p |> get_ok)
        [])
    lib_paths

let all_units = odoc_units @ lib_units |> List.flatten
```

Now we'll compile all of the parent `.mld` files. To ensure that the parents are compiled before the children, we start with `odoc.mld`, then `deps.mld`, and so on. The result of this file is a list of the resulting `odoc` files.

```ocaml env=e1
let compile_mlds () =
  let mkpage x = "page-\"" ^ x ^ "\"" in
  let mkmod x = "module-" ^ String.capitalize_ascii x in
  let mkmld x = Fpath.(add_ext "mld" (v x)) in
  ignore
    (compile (mkmld "odoc")
       (List.map mkpage (odoc_libraries @ extra_docs @ dep_libraries)));
  let extra_odocs =
    List.map
      (fun p ->
        ignore (compile (mkmld p) ~parent:"odoc" []);
        "page-" ^ p ^ ".odoc")
      extra_docs
  in
  let odocs =
    List.map
      (fun library ->
        let parent = List.assoc library parents in
        let children =
          List.filter_map
            (fun (parent, lib, child) ->
              if lib = library then Some (Fpath.basename child |> mkmod)
              else None)
            all_units
        in
        ignore (compile (mkmld ("library_mlds/"^library)) ~parent children);
        "page-" ^ library ^ ".odoc")
      all_libraries
  in
  List.map
    (fun f -> (Fpath.v f, false))
    ("page-odoc.odoc" :: odocs @ extra_odocs)
```

Now we get to the compilation phase. For each unit, we query its dependencies, then recursively call to compile these dependencies. Once this is done we compile the unit itself. If the unit has already been compiled we don't do anything. Note that we aren't checking the hashes of the dependencies which a build system should do to ensure that the module being compiled is the correct one. Again we benefit from the fact that we're creating the docs for one leaf package and that there must be no module name clashes in its dependencies. The result of this function is a list of the resulting `odoc` files.

```ocaml env=e1
let compile_all () =
  let mld_odocs = compile_mlds () in
  let rec rec_compile parent lib file =
    let output = Fpath.(base (set_ext "odoc" file)) in
    if OS.File.exists output |> get_ok then []
    else
      let deps = compile_deps file |> get_ok in
      let files =
        List.fold_left
          (fun acc (dep_name, digest) ->
            match
              List.find_opt
                (fun (_, _, f) ->
                  Fpath.basename f |> String.capitalize_ascii = dep_name)
                all_units
            with
            | None -> acc
            | Some (parent, lib, dep_path) ->
                let file = best_file dep_path in
                rec_compile parent lib file @ acc)
          [] deps.deps
      in
      let ignore_output = false in
      ignore (compile file ~parent:lib ~ignore_output []);
      (output, ignore_output) :: files
  in
  List.fold_left
    (fun acc (parent, lib, dep) -> acc @ rec_compile parent lib (best_file dep))
    [] all_units
  @ mld_odocs
```

Linking is now straightforward. We only need to link non-hidden `odoc` files, as any hidden are almost certainly aliased inside the non-hidden ones (a result of namespacing usually, and these aliases will be expanded).

```ocaml env=e1
let link_all odoc_files =
  let not_hidden (f, _) = not (is_hidden f) in
  List.map
    (fun (odoc_file, ignore_output) ->
      ignore (link ~ignore_output odoc_file);
      Fpath.set_ext "odocl" odoc_file)
    (List.filter not_hidden odoc_files)
```

Now we simply run `odoc html-generate` over all of the resulting `odocl` files.

```ocaml env=e1
let generate_all odocl_files =
  List.iter (fun f -> ignore(html_generate f)) odocl_files;
  support_files ()
```

The following code actually executes all of the above, and we're done!

```ocaml env=e1
let compiled = compile_all () in
let linked = link_all compiled in
generate_all linked
```

Let's see if there was any output from the `odoc` invocations:
```ocaml env=e1
# !compile_output;;
- : string list = [""]
# !link_output;;
- : string list = [""]
# !generate_output;;
- : string list =
["";
 "'../src/odoc/bin/main.exe' 'html-generate' 'odoc_xref_test.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'";
 "odoc_xref_test.odocl: Warning, resolved hidden path: Odoc_model__Lang.Signature.t";
 "'../src/odoc/bin/main.exe' 'html-generate' 'odoc_examples.odocl' '-o' 'html' '--theme-uri' 'odoc' '--support-uri' 'odoc'";
 "odoc_examples.odocl: Warning, resolved hidden path: Odoc_examples__Unexposed.t"]
```
