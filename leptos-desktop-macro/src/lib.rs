use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
  parse_macro_input, Attribute, Expr, FnArg, Ident, ItemFn, Pat, PatType, ReturnType
};

#[proc_macro_attribute]
pub fn command(args: TokenStream, item: TokenStream) -> TokenStream {
  let _args = parse_macro_input!(args with Attribute::parse_outer);
  let item = parse_macro_input!(item as ItemFn);

  let fn_name = &item.sig.ident;
  let arg_struct_name = Ident::new(
    &format!("{}Args", fn_name.to_string().to_case(Case::UpperCamel)),
    fn_name.span(),
  );

  let visibility = &item.vis;
  let asyncness = &item.sig.asyncness;

  let (backend_fields, frontend_fields, field_names) = extract_fields(item.clone());

  let block = &item.block;

  let return_type = match &item.sig.output {
    ReturnType::Default => quote!(()),
    ReturnType::Type(_, ty) => quote!(#ty),
  };

  quote! {
    #[derive(Debug, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct #arg_struct_name {
      #(#frontend_fields)*
    }

    leptos_desktop::cfg_if! {
      if #[cfg(feature = "backend")] {

        #[tauri::command]
        #visibility #asyncness fn #fn_name(#(#backend_fields)*) -> #return_type {
          #block
        }

      } else {

        #visibility async fn #fn_name(#(#frontend_fields)*) -> #return_type {
          let args = #arg_struct_name { #(#field_names),* };
          tauri_sys::core::invoke_result(stringify!(#fn_name), &args).await
        }
      
      }
    }
  }
  .into()
}

fn _extract_fields(item: ItemFn) -> Vec<proc_macro2::TokenStream> {
  let fields: Vec<_> = item
    .sig
    .inputs
    .iter()
    .filter_map(|arg| match arg {
      FnArg::Typed(pat_type) => match &*pat_type.pat {
        Pat::Ident(pat_ident) => {
          let ident = &pat_ident.ident;
          let ty = &pat_type.ty;
          Some(quote! { #ident: #ty, })
        }
        _ => None,
      },
      _ => None,
    })
    .collect();
  fields
}

fn extract_fields(item: ItemFn) -> (Vec<TokenStream2>, Vec<TokenStream2>, Vec<Ident>) {
  let mut backend_fields = Vec::new();
  let mut frontend_fields = Vec::new();
  let mut field_names = Vec::new();

  for arg in item.sig.inputs {
    if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = arg {
      let ident = match *pat {
        Pat::Ident(pat_ident) => pat_ident.ident,
        _ => continue,
      };

      let has_only_backend = attrs.iter().any(|attr| attr.path().is_ident("backend"));

      let field = quote! { #ident: #ty, };
      backend_fields.push(field.clone());

      if !has_only_backend {
        frontend_fields.push(field);
        field_names.push(ident);
      }
    }
  }

  (backend_fields, frontend_fields, field_names)
}


#[proc_macro]
pub fn expect_frontend(input: TokenStream) -> TokenStream {
  let expr = parse_macro_input!(input as Expr);

  quote! {
    { leptos_desktop::cfg_if! {
      if #[cfg(not(feature = "backend"))] {
        #expr
      } else {
        unreachable!("Frontend-only code executed on backend")
      }
    }}
  }.into()
}
