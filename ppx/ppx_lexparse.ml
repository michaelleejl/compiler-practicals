open Ppxlib
open Ast_builder.Default

let fail_expr ~loc =
  pexp_apply ~loc
    (pexp_ident ~loc { txt = Longident.parse "raise"; loc })
    [ (Nolabel, pexp_construct ~loc (Located.mk ~loc (Lident "Fail")) None) ]

let expand_act ~loc pat body =
  let ds_pat = ppat_var ~loc (Located.mk ~loc "ds") in
  let ds_expr = pexp_ident ~loc (Located.lident ~loc "ds") in
  let good_case = case ~lhs:pat ~guard:None ~rhs:body in
  let bad_case = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(fail_expr ~loc) in
  Ast_builder.Default.pexp_fun ~loc Nolabel None ds_pat
    (Ast_builder.Default.pexp_match ~loc ds_expr [ good_case; bad_case ])

let extension =
  Extension.declare "act" Extension.Context.expression
    Ast_pattern.(
      single_expr_payload
        (pexp_function nil none
           (pfunction_cases (case ~lhs:__ ~guard:none ~rhs:__ ^:: nil) __ __)))
    (fun ~loc ~path pat body _ _ -> expand_act ~loc pat body)

let rule = Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "act"
